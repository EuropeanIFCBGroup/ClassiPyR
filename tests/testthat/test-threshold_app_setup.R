# Tests for the Setup page helpers of the threshold review app: settings,
# index messages and the frozen summary.

# ---- thr_main_app_db_folder --------------------------------------------------

test_that("thr_main_app_db_folder reads the main app's database folder", {
  settings_file <- tempfile(fileext = ".json")
  on.exit(unlink(settings_file), add = TRUE)
  jsonlite::write_json(list(db_folder = "/data/manual", csv_folder = "/x"),
                       settings_file, auto_unbox = TRUE)
  before <- readLines(settings_file)

  expect_equal(thr_main_app_db_folder(settings_file), "/data/manual")
  # Read only: the main app's file is left exactly as it was
  expect_equal(readLines(settings_file), before)
})

test_that("thr_main_app_db_folder falls back to the default folder", {
  absent <- file.path(tempdir(), "no_such_settings.json")
  broken <- tempfile(fileext = ".json")
  empty <- tempfile(fileext = ".json")
  on.exit(unlink(c(broken, empty)), add = TRUE)
  writeLines("{ this is not json", broken)
  jsonlite::write_json(list(db_folder = "", other = 1), empty,
                       auto_unbox = TRUE)

  expect_equal(thr_main_app_db_folder(absent), get_default_db_dir())
  expect_equal(thr_main_app_db_folder(broken), get_default_db_dir())
  expect_equal(thr_main_app_db_folder(empty), get_default_db_dir())
  expect_false(file.exists(absent))
})

# ---- thr_apply_settings ------------------------------------------------------

test_that("thr_apply_settings validates, creates and saves the folders", {
  root <- tempfile("thr_settings_")
  dir.create(file.path(root, "h5"), recursive = TRUE)
  dir.create(file.path(root, "raw"))
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  settings_file <- file.path(root, "config", "threshold_settings.json")
  review_dir <- file.path(root, "review")

  applied <- thr_apply_settings(
    review_dir = paste0("  ", review_dir, " "),
    h5_folder = file.path(root, "h5"), roi_folder = file.path(root, "raw"),
    annotator = " Anna ", settings_file = settings_file
  )

  expect_true(dir.exists(review_dir))
  expect_equal(applied$review_dir, normalizePath(review_dir, winslash = "/"))
  expect_equal(applied$h5_folder,
               normalizePath(file.path(root, "h5"), winslash = "/"))
  expect_equal(applied$annotator, "Anna")
  expect_equal(applied$index_path, thr_index_db_path(applied$review_dir))
  expect_equal(applied$db_path, thr_verdict_db_path(applied$review_dir))

  saved <- thr_load_settings(settings_file)
  expect_equal(saved$review_dir, applied$review_dir)
  expect_equal(saved$h5_folder, applied$h5_folder)
  expect_equal(saved$roi_folder, applied$roi_folder)
  expect_equal(saved$annotator, "Anna")
})

test_that("thr_apply_settings requires a review folder", {
  settings_file <- tempfile(fileext = ".json")

  for (missing in list(NULL, "", "   ", NA_character_)) {
    expect_error(
      thr_apply_settings(missing, settings_file = settings_file),
      "review folder"
    )
  }
  expect_false(file.exists(settings_file))
})

test_that("thr_apply_settings refuses data folders that do not exist", {
  root <- tempfile("thr_settings_")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  settings_file <- file.path(root, "threshold_settings.json")
  review_dir <- file.path(root, "review")

  expect_error(
    thr_apply_settings(review_dir, h5_folder = file.path(root, "nope"),
                       settings_file = settings_file),
    "class-score folder does not exist"
  )
  expect_error(
    thr_apply_settings(review_dir, roi_folder = file.path(root, "nope"),
                       settings_file = settings_file),
    "raw data folder does not exist"
  )
  expect_false(file.exists(settings_file))
})

test_that("thr_apply_settings lets the data folders be left empty", {
  root <- tempfile("thr_settings_")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  settings_file <- file.path(root, "threshold_settings.json")
  thr_save_settings(list(h5_folder = "/old/h5"), settings_file)

  applied <- thr_apply_settings(file.path(root, "review"), h5_folder = "",
                                roi_folder = NULL, annotator = NULL,
                                settings_file = settings_file)

  expect_null(applied$h5_folder)
  expect_null(applied$roi_folder)
  expect_equal(applied$annotator, "")
  # A folder that was cleared is no longer restored next time
  expect_null(thr_load_settings(settings_file)$h5_folder)
})

test_that("thr_apply_settings can put folders in use without saving them", {
  root <- tempfile("thr_settings_")
  dir.create(file.path(root, "review"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  settings_file <- file.path(root, "threshold_settings.json")
  thr_save_settings(list(review_dir = file.path(root, "review"),
                         h5_folder = "/archive/offline/h5", annotator = "Anna"),
                    settings_file)
  before <- readLines(settings_file)

  # What the app does at start for a data folder that is not there right now
  applied <- thr_apply_settings(file.path(root, "review"), h5_folder = "",
                                annotator = "Anna", save = FALSE,
                                settings_file = settings_file)

  expect_null(applied$h5_folder)
  expect_equal(applied$review_dir,
               normalizePath(file.path(root, "review"), winslash = "/"))
  # The saved folder is still there for when the disk is back
  expect_identical(readLines(settings_file), before)
  expect_equal(thr_load_settings(settings_file)$h5_folder, "/archive/offline/h5")

  absent <- file.path(root, "none", "threshold_settings.json")
  thr_apply_settings(file.path(root, "review"), save = FALSE,
                     settings_file = absent)
  expect_false(file.exists(absent))
})

test_that("thr_restorable_settings leaves out a data folder that is not there", {
  root <- tempfile("thr_settings_")
  dir.create(file.path(root, "review"), recursive = TRUE)
  dir.create(file.path(root, "raw"))
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  saved <- list(review_dir = file.path(root, "review"),
                h5_folder = "/archive/offline/h5",
                roi_folder = file.path(root, "raw"), annotator = " Anna ")

  restorable <- thr_restorable_settings(saved)

  # The fields show what was saved, whether or not it can be used now
  expect_equal(restorable$fields$h5_folder, "/archive/offline/h5")
  expect_equal(restorable$fields$annotator, "Anna")
  expect_true(restorable$apply)
  expect_equal(restorable$h5_folder, "")
  expect_equal(restorable$roi_folder, file.path(root, "raw"))
  expect_length(restorable$notices, 1)
  expect_match(restorable$notices, "class-score folder", fixed = TRUE)
  expect_match(restorable$notices, "/archive/offline/h5", fixed = TRUE)
  expect_match(restorable$notices, "stays saved", fixed = TRUE)

  # Nothing saved, or a review folder that has gone: nothing to put in use
  nothing <- thr_restorable_settings(list())
  expect_false(nothing$apply)
  expect_equal(nothing$fields$review_dir, "")
  expect_equal(nothing$notices, character())
  expect_false(thr_restorable_settings(
    list(review_dir = file.path(root, "gone"))
  )$apply)
  everything <- thr_restorable_settings(utils::modifyList(
    saved, list(h5_folder = file.path(root, "raw"))
  ))
  expect_equal(everything$notices, character())
  expect_equal(everything$h5_folder, file.path(root, "raw"))
})

# ---- index messages ----------------------------------------------------------

test_that("thr_index_build_message says what the build did", {
  result <- list(n_scanned = 12, n_read = 10, n_ok = 9, n_skipped = 2,
                 failures = data.frame(h5_path = c("a", "b", "c"),
                                       status = "failed", message = "m"))

  message <- thr_index_build_message(result)

  expect_match(message, "12 class-score file", fixed = TRUE)
  expect_match(message, "10 read", fixed = TRUE)
  expect_match(message, "2 unchanged", fixed = TRUE)
  expect_match(message, "9 usable", fixed = TRUE)
  expect_match(message, "3 not usable", fixed = TRUE)

  none <- thr_index_build_message(list(n_scanned = 0, n_read = 0, n_ok = 0,
                                       n_skipped = 0,
                                       failures = result$failures[0, ]))
  expect_match(none, "No class-score files", fixed = TRUE)
})

test_that("thr_failure_overview counts and explains each status", {
  failures <- data.frame(
    h5_path = paste0("/x/", 1:5, ".h5"),
    status = c("legacy", "legacy", "failed", "duplicate", "missing"),
    message = "m", stringsAsFactors = FALSE
  )

  overview <- thr_failure_overview(failures)

  expect_equal(overview$Status, c("legacy", "duplicate", "failed", "missing"))
  expect_equal(overview$Files, c(2L, 1L, 1L, 1L))
  legacy <- overview$Meaning[overview$Status == "legacy"]
  expect_match(legacy, "augment", fixed = TRUE)
  expect_match(legacy, "Re-run inference", fixed = TRUE)
  expect_equal(nrow(thr_failure_overview(failures[0, ])), 0)
  # A status this version does not know still shows up
  odd <- thr_failure_overview(transform(failures[1, ], status = "strange"))
  expect_equal(odd$Status, "strange")
  expect_equal(odd$Files, 1L)
})

test_that("thr_review_models joins the indexed models and the frozen reviews", {
  space <- thr_test_app_setup_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  indexed <- thr_index_models(space$index_path)

  open <- thr_review_models(indexed, thr_frozen_models(space$db_path))
  expect_equal(open[names(indexed)], indexed)
  expect_identical(open$indexed, c(TRUE, TRUE))
  expect_identical(open$frozen, c(FALSE, FALSE))
  expect_false(file.exists(space$db_path))

  thr_freeze_frame(space$db_path, space$index_path, thr_test_model_a_id,
                   h5_folder = space$h5_folder)
  frozen <- thr_frozen_models(space$db_path)
  both <- thr_review_models(indexed, frozen)
  expect_identical(both$frozen, both$model_id == thr_test_model_a_id)
  expect_identical(both$indexed, c(TRUE, TRUE))

  # The index is a cache: without it the frozen review is still listed
  for (no_index in list(indexed[0, ], NULL)) {
    alone <- thr_review_models(no_index, frozen)
    expect_identical(alone$model_id, thr_test_model_a_id)
    expect_identical(alone$frozen, TRUE)
    expect_identical(alone$indexed, FALSE)
    expect_identical(alone$n_files, 0L)
    expect_identical(alone$n_rois, 0L)
    expect_equal(alone$classifier_name,
                 indexed$classifier_name[indexed$model_id == thr_test_model_a_id])
  }
  # A model of the index and another that is only frozen: both are listed
  mixed <- thr_review_models(indexed[indexed$model_id == thr_test_model_b_id, ],
                             frozen)
  expect_setequal(mixed$model_id, c(thr_test_model_a_id, thr_test_model_b_id))
  expect_identical(mixed$indexed, mixed$model_id == thr_test_model_b_id)
  expect_equal(nrow(thr_review_models(NULL, NULL)), 0)
})

test_that("thr_model_choices says which models are frozen or not in the index", {
  models <- data.frame(
    model_id = c("aaaaaaaa1111", "bbbbbbbb2222", "cccccccc3333"),
    classifier_name = c("open_model", "frozen_model", "archived_model"),
    n_files = c(6L, 4L, 0L), n_rois = c(65L, 40L, 0L),
    indexed = c(TRUE, TRUE, FALSE), frozen = c(FALSE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )

  choices <- thr_model_choices(models)

  expect_equal(unname(choices), models$model_id)
  expect_false(grepl("frozen", names(choices)[1], fixed = TRUE))
  expect_match(names(choices)[1], "6 files", fixed = TRUE)
  expect_match(names(choices)[2], "4 files", fixed = TRUE)
  expect_match(names(choices)[2], "review frozen", fixed = TRUE)
  expect_match(names(choices)[3], "review frozen", fixed = TRUE)
  expect_match(names(choices)[3], "not in the index", fixed = TRUE)
  expect_false(grepl("0 files", names(choices)[3], fixed = TRUE))
  expect_match(names(choices)[3], "cccccccc", fixed = TRUE)
})

test_that("thr_models_table lists the models and whether a review started", {
  space <- thr_test_app_setup_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  indexed <- thr_index_models(space$index_path)
  listed <- function(indexed) {
    thr_review_models(indexed, thr_frozen_models(space$db_path))
  }

  before <- thr_models_table(listed(indexed))
  expect_equal(nrow(before), 2)
  expect_equal(before$Review, rep("not started", 2))
  expect_false(file.exists(space$db_path))

  thr_freeze_frame(space$db_path, space$index_path, thr_test_model_a_id,
                   h5_folder = space$h5_folder)
  after <- thr_models_table(listed(indexed))
  expect_equal(after$Review[indexed$model_id == thr_test_model_a_id], "frozen")
  expect_equal(after$Review[indexed$model_id == thr_test_model_b_id],
               "not started")
  expect_equal(after$Files, indexed$n_files)
  expect_equal(after$ROIs, indexed$n_rois)
  expect_equal(nrow(thr_models_table(listed(indexed)[0, ])), 0)

  # A frozen review whose class-score files are no longer indexed
  gone <- thr_models_table(listed(indexed[0, ]))
  expect_equal(nrow(gone), 1)
  expect_equal(gone$Review, "frozen, not in the index")
  expect_equal(gone$Files, 0L)
})

# ---- thr_frozen_summary ------------------------------------------------------

test_that("thr_frozen_summary describes a frozen frame", {
  space <- thr_test_app_setup_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  thr_freeze_frame(space$db_path, space$index_path, thr_test_model_a_id,
                   excluded_samples = thr_test_app_odd_sample,
                   no_raw_samples = "D20240104T000000_IFCB134",
                   h5_folder = space$h5_folder, roi_folder = space$roi_folder)
  model <- thr_get_model(space$db_path, thr_test_model_a_id)
  status <- thr_frame_status(space$db_path, space$index_path,
                             thr_test_model_a_id)

  summary <- thr_frozen_summary(model, status)

  facts <- stats::setNames(summary$facts$Value, summary$facts$Item)
  expect_equal(facts[["Samples in the frame"]], "4")
  expect_equal(facts[["Left out as training samples"]], "1")
  expect_equal(facts[["Left out for lack of raw data"]], "1")
  expect_equal(facts[["Classes"]], "5")
  expect_equal(facts[["Class-score folder"]], space$h5_folder)
  expect_match(facts[["Fallback thresholds"]], "none", fixed = TRUE)
  expect_equal(summary$notices, character())
})

test_that("thr_frozen_summary says what new and changed files mean", {
  model <- list(
    classifier_name = "m", model_id = "abc", frozen_at = "2026-01-01T00:00:00Z",
    h5_folder = "/h5", roi_folder = NA_character_, class_labels = letters[1:3],
    fallback_source = "json:/models/m_thresholds_and_metrics.json",
    transform = "dataset_squarepad"
  )
  status <- list(frozen = TRUE, n_frame_files = 10L, n_excluded = 2L,
                 n_new_files = 3L, n_changed_files = 1L,
                 n_excluded_training = 2L, n_no_raw = 0L)

  summary <- thr_frozen_summary(model, status)

  expect_length(summary$notices, 2)
  new <- summary$notices[grepl("3 class-score file", summary$notices,
                               fixed = TRUE)]
  expect_match(new, "not part of this review", fixed = TRUE)
  changed <- summary$notices[grepl("1 class-score file", summary$notices,
                                   fixed = TRUE)]
  expect_match(changed, "no longer drawn from", fixed = TRUE)
  expect_match(changed, "already recorded", fixed = TRUE)
  facts <- stats::setNames(summary$facts$Value, summary$facts$Item)
  expect_match(facts[["Fallback thresholds"]],
               "/models/m_thresholds_and_metrics.json", fixed = TRUE)
  expect_equal(facts[["Raw data folder"]], "not recorded")

  h5 <- thr_frozen_summary(utils::modifyList(model, list(fallback_source = "h5")),
                           utils::modifyList(status, list(n_new_files = 0L,
                                                          n_changed_files = 0L)))
  expect_equal(h5$notices, character())
  expect_match(
    stats::setNames(h5$facts$Value, h5$facts$Item)[["Fallback thresholds"]],
    "class-score files", fixed = TRUE
  )
})

test_that("thr_frozen_summary says what still works without the index", {
  space <- thr_test_app_frozen_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  model <- thr_get_model(space$db_path, space$model_id)
  summarise <- function() {
    thr_frozen_summary(model, thr_frame_status(space$db_path, space$index_path,
                                               space$model_id))
  }
  expect_equal(summarise()$notices, character())

  thr_test_lose_index(space, "archived")
  archived <- summarise()
  expect_length(archived$notices, 1)
  expect_match(archived$notices, "None of the 6 class-score file", fixed = TRUE)
  expect_match(archived$notices, "Results and Export", fixed = TRUE)
  expect_match(archived$notices, space$h5_folder, fixed = TRUE)

  thr_test_lose_index(space, "deleted")
  deleted <- summarise()
  expect_length(deleted$notices, 1)
  expect_match(deleted$notices, "no class-score index", fixed = TRUE)
  expect_match(deleted$notices, "Results and Export", fixed = TRUE)
  expect_match(deleted$notices, "Build / update index", fixed = TRUE)
  facts <- stats::setNames(deleted$facts$Value, deleted$facts$Item)
  expect_equal(facts[["Samples in the frame"]], "6")
})
