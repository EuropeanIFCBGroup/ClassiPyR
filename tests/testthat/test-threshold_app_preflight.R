# Tests for the pre-flight before a freeze in the threshold review app

# ---- thr_read_exclusions -----------------------------------------------------

test_that("thr_read_exclusions merges the file and the annotation database", {
  db_folder <- tempfile("thr_ann_")
  dir.create(db_folder)
  on.exit(unlink(db_folder, recursive = TRUE), add = TRUE)
  thr_test_execute(get_db_path(db_folder),
                   "CREATE TABLE annotations (sample_name TEXT, roi INTEGER)")
  thr_test_execute(
    get_db_path(db_folder),
    "INSERT INTO annotations VALUES ('D20240102T000000_IFCB134', 1),
       ('D20240102T000000_IFCB134', 2), ('D20990101T000000_IFCB134', 1)"
  )
  file <- thr_test_app_write_lines(c(
    "# training bins", "D20240101T000000_IFCB134", "", "not a sample",
    "D20240102T000000_IFCB134", "D20000101T000000_IFCB1"
  ))
  on.exit(unlink(file), add = TRUE)

  result <- thr_read_exclusions(file, db_folder, thr_test_a_samples)

  expect_setequal(result$samples, c(
    "D20240101T000000_IFCB134", "D20240102T000000_IFCB134",
    "D20000101T000000_IFCB1", "D20990101T000000_IFCB134"
  ))
  expect_equal(result$sources$source,
               c("Exclusion file", "Annotation database"))
  expect_equal(result$sources$n_listed, c(3L, 2L))
  expect_equal(result$sources$n_in_frame, c(2L, 1L))
  expect_equal(result$invalid, "not a sample")
  expect_equal(result$n_in_frame, 2L)
})

test_that("thr_read_exclusions reports a source that gives nothing", {
  db_folder <- tempfile("thr_ann_")
  dir.create(db_folder)
  on.exit(unlink(db_folder, recursive = TRUE), add = TRUE)

  none <- thr_read_exclusions(NULL, NULL, thr_test_a_samples)
  expect_equal(none$samples, character())
  expect_equal(nrow(none$sources), 0)
  expect_equal(none$n_in_frame, 0L)

  empty_db <- thr_read_exclusions(NULL, db_folder, thr_test_a_samples)
  expect_equal(empty_db$sources$n_listed, 0L)
  expect_match(empty_db$notices, "No annotated samples were found",
               fixed = TRUE)

  expect_error(
    thr_read_exclusions(file.path(tempdir(), "absent.txt"), NULL,
                        thr_test_a_samples),
    "Exclusion file not found"
  )
})

# ---- thr_setup_preflight -----------------------------------------------------

test_that("thr_setup_preflight refuses h5 thresholds that disagree", {
  space <- thr_test_app_setup_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)

  preflight <- thr_setup_preflight(space$index_path, thr_test_model_a_id,
                                   space$h5_folder, space$roi_folder)

  expect_false(preflight$can_freeze)
  expect_equal(preflight$n_samples, 6L)
  expect_match(preflight$problems, "2 different sets of thresholds",
               fixed = TRUE)
  expect_match(preflight$problems, "thresholds JSON", fixed = TRUE)
  # Nothing is excluded, and the reviewer is told what that means
  expect_true(any(grepl("No training samples are excluded",
                        preflight$notices, fixed = TRUE)))
})

test_that("thr_setup_preflight returns what thr_freeze_frame needs", {
  space <- thr_test_app_setup_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  file <- thr_test_app_write_lines(c(thr_test_app_odd_sample, "garbage line",
                                     "D20000101T000000_IFCB1"))
  on.exit(unlink(file), add = TRUE)

  preflight <- thr_setup_preflight(
    space$index_path, thr_test_model_a_id, space$h5_folder, space$roi_folder,
    exclusion_file = file
  )

  expect_true(preflight$can_freeze)
  expect_equal(preflight$problems, character())
  expect_equal(preflight$n_samples, 6L)
  expect_equal(preflight$n_excluded, 1L)
  expect_equal(preflight$n_no_raw, 0L)
  expect_equal(preflight$n_frame, 5L)
  expect_equal(preflight$fallback_source, "h5")
  expect_equal(preflight$invalid_lines, "garbage line")
  expect_true(any(grepl("1 line", preflight$notices, fixed = TRUE)))
  expect_false(any(grepl("No training samples", preflight$notices,
                         fixed = TRUE)))

  frozen <- do.call(thr_freeze_frame, c(
    list(db_path = space$db_path, index_path = space$index_path,
         model_id = thr_test_model_a_id),
    preflight$freeze_args
  ))
  expect_equal(frozen$n_frame_files, 5L)
  expect_equal(frozen$n_excluded_training, 1L)
  model <- thr_get_model(space$db_path, thr_test_model_a_id)
  expect_equal(model$fallback_source, "h5")
  expect_equal(model$h5_folder, space$h5_folder)
  expect_equal(model$roi_folder, thr_normalise_folder(space$roi_folder))
  expect_equal(model$fallback, preflight$freeze_args$fallback)
})

test_that("thr_setup_preflight leaves out samples without raw data", {
  # The sample with other thresholds has no raw data: left out of the frame,
  # it no longer decides whether the class-score files agree
  space <- thr_test_app_setup_space(
    raw_samples = setdiff(thr_test_a_samples, thr_test_app_odd_sample)
  )
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)

  preflight <- thr_setup_preflight(space$index_path, thr_test_model_a_id,
                                   space$h5_folder, space$roi_folder)

  expect_true(preflight$can_freeze)
  expect_equal(preflight$n_no_raw, 1L)
  expect_equal(preflight$n_frame, 5L)
  expect_equal(preflight$freeze_args$no_raw_samples, thr_test_app_odd_sample)
  expect_true(any(grepl("1 sample.* no raw data", preflight$notices)))

  frozen <- do.call(thr_freeze_frame, c(
    list(db_path = space$db_path, index_path = space$index_path,
         model_id = thr_test_model_a_id),
    preflight$freeze_args
  ))
  expect_equal(frozen$n_no_raw, 1L)
  expect_equal(frozen$n_frame_files, 5L)
})

test_that("thr_setup_preflight reads fallback thresholds from a JSON file", {
  space <- thr_test_app_setup_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  json <- file.path(space$root, "model_thresholds_and_metrics.json")
  metrics <- lapply(c(0.3, 0.05, 0.9, 0.6), function(x) list(threshold = x))
  names(metrics) <- thr_test_labels[c(1, 2, 3, 5)]
  writeLines(jsonlite::toJSON(list(class_metrics = metrics),
                              auto_unbox = TRUE), json, useBytes = TRUE)

  preflight <- thr_setup_preflight(
    space$index_path, thr_test_model_a_id, space$h5_folder, space$roi_folder,
    fallback_mode = "json", fallback_json = json
  )

  expect_true(preflight$can_freeze)
  expect_equal(preflight$fallback_source,
               paste0("json:", normalizePath(json, winslash = "/")))
  expect_equal(preflight$freeze_args$fallback, c(0.3, 0.05, 0.9, NA, 0.6))
  expect_null(attributes(preflight$freeze_args$fallback))
  expect_equal(preflight$fallback_absent, thr_test_labels[4])
  expect_true(any(grepl("1 class.* no entry", preflight$notices)))
  expect_equal(preflight$n_fallback, 4L)

  do.call(thr_freeze_frame, c(
    list(db_path = space$db_path, index_path = space$index_path,
         model_id = thr_test_model_a_id),
    preflight$freeze_args
  ))
  model <- thr_get_model(space$db_path, thr_test_model_a_id)
  expect_equal(model$fallback, c(0.3, 0.05, 0.9, NA, 0.6))
  expect_match(model$fallback_source, "^json:")
})

test_that("thr_setup_preflight turns a bad thresholds file into a problem", {
  space <- thr_test_app_setup_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  check <- function(...) {
    thr_setup_preflight(space$index_path, thr_test_model_a_id,
                        space$h5_folder, space$roi_folder,
                        fallback_mode = "json", ...)
  }

  absent <- check(fallback_json = file.path(space$root, "nope.json"))
  expect_false(absent$can_freeze)
  expect_match(absent$problems, "Thresholds file not found", fixed = TRUE)

  blank <- check(fallback_json = "  ")
  expect_false(blank$can_freeze)
  expect_match(blank$problems, "Enter the path", fixed = TRUE)
  expect_false(check(fallback_json = NULL)$can_freeze)
})

test_that("thr_setup_preflight reports every way a frame can be empty", {
  space <- thr_test_app_setup_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  everything <- thr_test_app_write_lines(thr_test_a_samples)
  on.exit(unlink(everything), add = TRUE)

  all_excluded <- thr_setup_preflight(
    space$index_path, thr_test_model_a_id, space$h5_folder, space$roi_folder,
    exclusion_file = everything
  )
  expect_false(all_excluded$can_freeze)
  expect_equal(all_excluded$n_frame, 0L)
  expect_true(any(grepl("Nothing would be left", all_excluded$problems,
                        fixed = TRUE)))

  no_raw_folder <- thr_setup_preflight(
    space$index_path, thr_test_model_a_id, space$h5_folder,
    file.path(space$root, "no_raw_here")
  )
  expect_false(no_raw_folder$can_freeze)
  expect_true(any(grepl("raw data folder", no_raw_folder$problems,
                        fixed = TRUE)))

  elsewhere <- thr_setup_preflight(
    space$index_path, thr_test_model_a_id, file.path(space$root, "other"),
    space$roi_folder
  )
  expect_false(elsewhere$can_freeze)
  expect_equal(elsewhere$n_samples, 0L)
  expect_true(any(grepl("no usable class-score files", elsewhere$problems,
                        fixed = TRUE)))

  bad_file <- thr_setup_preflight(
    space$index_path, thr_test_model_a_id, space$h5_folder, space$roi_folder,
    exclusion_file = file.path(space$root, "absent.txt")
  )
  expect_false(bad_file$can_freeze)
  expect_true(any(grepl("Exclusion file not found", bad_file$problems,
                        fixed = TRUE)))
})

test_that("thr_setup_preflight checks its arguments", {
  space <- thr_test_app_setup_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)

  expect_error(
    thr_setup_preflight(space$index_path, thr_test_model_a_id,
                        space$h5_folder, space$roi_folder,
                        fallback_mode = "guess"),
    "fallback_mode"
  )
  expect_error(
    thr_setup_preflight(file.path(space$root, "no_index.sqlite"),
                        thr_test_model_a_id, space$h5_folder,
                        space$roi_folder),
    "No class-score index"
  )
})
