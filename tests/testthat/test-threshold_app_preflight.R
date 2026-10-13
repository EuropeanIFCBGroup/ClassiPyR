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

test_that("thr_setup_preflight finds the files of a folder whose copy was indexed first", {
  space <- thr_test_db_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  folders <- thr_test_write_copies(space$h5_folder, c("a_old", "b_new"),
                                   thr_test_a_samples[1:4])
  roi_folder <- file.path(space$root, "raw")
  thr_test_app_raw_layout(roi_folder, thr_test_a_samples[1:4])
  for (folder in folders) {
    build_threshold_index(folder, space$index_path, reader = thr_test_fake_reader())
  }
  model_id <- thr_index_models(space$index_path)$model_id

  preflight <- thr_setup_preflight(space$index_path, model_id,
                                   thr_normalise_folder(folders[["b_new"]]),
                                   roi_folder)

  expect_equal(preflight$problems, character())
  expect_true(preflight$can_freeze)
  expect_equal(preflight$n_samples, 4L)
  expect_equal(preflight$n_frame, 4L)
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

test_that("thr_setup_preflight refuses to run without a class-score folder", {
  space <- thr_test_app_setup_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)

  for (no_folder in list(NA_character_, NULL, "")) {
    preflight <- thr_setup_preflight(space$index_path, thr_test_model_a_id,
                                     no_folder, space$roi_folder)

    expect_false(preflight$can_freeze)
    expect_match(preflight$problems, "No class-score folder is in use",
                 fixed = TRUE)
    # Not the samples of every folder that was ever indexed
    expect_equal(preflight$n_samples, 0L)
    expect_equal(preflight$n_frame, 0L)
  }
})

test_that("thr_setup_preflight refuses a folder that changed since it was indexed", {
  space <- thr_test_open_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  check <- function() {
    thr_setup_preflight(space$index_path, space$model_id, space$h5_folder,
                        space$roi_folder)
  }
  rebuild <- function() {
    build_threshold_index(space$h5_folder, space$index_path,
                          reader = thr_test_fake_reader())
  }
  stale <- function(preflight) {
    grep("since the index was built", preflight$problems, fixed = TRUE,
         value = TRUE)
  }
  files <- list.files(space$h5_folder, full.names = TRUE)
  expect_true(check()$can_freeze)
  expect_length(stale(check()), 0)

  # Rewritten on disk: the index still holds the old scores
  Sys.setFileTime(files[1], Sys.time() + 3600)
  changed <- check()
  expect_false(changed$can_freeze)
  expect_match(stale(changed), "1 changed", fixed = TRUE)
  expect_match(stale(changed), "Build / update index", fixed = TRUE)
  # What the index holds is still described, next to the problem
  expect_equal(changed$n_samples, 6L)
  rebuild()
  expect_true(check()$can_freeze)

  # Deleted on disk: the index still counts its ROIs
  unlink(files[2])
  expect_match(stale(check()), "1 missing", fixed = TRUE)
  expect_false(check()$can_freeze)
  rebuild()
  expect_true(check()$can_freeze)
  expect_equal(check()$n_samples, 5L)

  # Added on disk: not in the index, so it would be left out for good
  thr_test_write_fake(space$h5_folder, "D20231201T000000_IFCB134",
                      readRDS(files[1]))
  Sys.setFileTime(files[3], Sys.time() + 7200)
  unlink(files[4])
  several <- stale(check())
  expect_length(several, 1)
  expect_match(several, "1 new, 1 changed and 1 missing", fixed = TRUE)
  rebuild()
  expect_true(check()$can_freeze)
})

test_that("only the folder under review has to match the index", {
  space <- thr_test_open_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  elsewhere <- file.path(space$root, "other_scores")
  kept <- thr_test_write_fake(elsewhere, "D20220101T000000_IFCB134")
  gone <- thr_test_write_fake(elsewhere, "D20220102T000000_IFCB134")
  build_threshold_index(elsewhere, space$index_path, reader = thr_test_fake_reader())
  Sys.setFileTime(kept, Sys.time() + 3600)
  unlink(gone)

  preflight <- thr_setup_preflight(space$index_path, space$model_id,
                                   space$h5_folder, space$roi_folder)

  expect_true(preflight$can_freeze)
  expect_equal(preflight$problems, character())
})

test_that("thr_setup_preflight refuses files a link places outside the folder", {
  skip_on_os("windows")
  space <- thr_test_open_space(n_samples = 2)
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  # A year of class scores kept on another disk and linked into the folder
  elsewhere <- file.path(space$root, "otherdisk", "2024")
  linked <- thr_test_a_samples[1:4]
  for (sample_name in linked) {
    thr_test_write_fake(elsewhere, sample_name,
                        readRDS(list.files(space$h5_folder, full.names = TRUE)[1]))
  }
  thr_test_app_raw_layout(space$roi_folder, linked)
  skip_if_not(file.symlink(normalizePath(elsewhere),
                           file.path(space$h5_folder, "2024")),
              "Cannot create a symbolic link")
  result <- build_threshold_index(space$h5_folder, space$index_path,
                                  reader = thr_test_fake_reader())
  expect_identical(result$n_ok, 6L)

  preflight <- thr_setup_preflight(space$index_path, space$model_id,
                                   space$h5_folder, space$roi_folder)

  # The index stores the four files where they really are, so a frame on
  # this folder would silently hold two samples instead of six
  expect_equal(preflight$n_samples, 2L)
  expect_false(preflight$can_freeze)
  link <- grep("symbolic link", preflight$problems, fixed = TRUE, value = TRUE)
  expect_length(link, 1)
  expect_match(link, "4 class-score file", fixed = TRUE)
  expect_match(link, "otherdisk/2024", fixed = TRUE)

  # Pointed at the folder the files really are in, nothing stands in the way
  direct <- thr_setup_preflight(space$index_path, space$model_id,
                                elsewhere, space$roi_folder)
  expect_true(direct$can_freeze)
  expect_equal(direct$n_samples, 4L)
})

test_that("thr_check_index_current stats the folder and reads no file", {
  space <- thr_test_open_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  files <- list.files(space$h5_folder, full.names = TRUE)
  expect_silent(thr_check_index_current(space$index_path, space$h5_folder))

  # Same size and time, other content: only reading the file could tell,
  # and the check does not read
  info <- file.info(files[1])
  content <- readBin(files[1], "raw", info$size)
  writeBin(rev(content), files[1])
  Sys.setFileTime(files[1], info$mtime)
  expect_silent(thr_check_index_current(space$index_path, space$h5_folder))

  Sys.setFileTime(files[1], info$mtime + 60)
  expect_error(thr_check_index_current(space$index_path, space$h5_folder),
               "1 changed")
  expect_error(
    thr_check_index_current(space$index_path, file.path(space$root, "nowhere")),
    "Class-score folder not found"
  )
  # Without an index every file of the folder is new
  expect_error(
    thr_check_index_current(file.path(space$root, "none.sqlite"), space$h5_folder),
    "6 new"
  )
  expect_false(file.exists(file.path(space$root, "none.sqlite")))
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
