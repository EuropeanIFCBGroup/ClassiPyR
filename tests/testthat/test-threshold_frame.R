# Tests for freezing the sampling frame of the threshold review app
#
# The index databases are built from stand-in files reproducing expected.json
# (see helper-threshold-db.R), so only the one test on the real class-score
# files needs hdf5r.

library(testthat)

thr_test_frame_counts <- function(db_path) {
  vapply(c("models", "frame_files", "excluded_samples", "strata"), function(table) {
    thr_test_query(db_path, paste("SELECT COUNT(*) AS n FROM", table))$n
  }, numeric(1))
}

thr_test_strata_table <- function(db_path, model_id) {
  strata <- thr_test_query(
    db_path,
    "SELECT class_name, bin, n_population FROM strata WHERE model_id = ?",
    params = list(model_id)
  )
  strata$class_name <- enc2utf8(strata$class_name)
  strata
}

# Same rows regardless of order
thr_test_expect_same_strata <- function(actual, expected) {
  key <- function(df) paste(df$class_name, df$bin, df$n_population, sep = "\r")
  expect_setequal(key(actual), key(expected))
  expect_equal(nrow(actual), nrow(expected))
}

# --- thr_is_frozen -----------------------------------------------------------

test_that("thr_is_frozen is FALSE without a database and does not create one", {
  space <- thr_test_db_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)

  expect_false(thr_is_frozen(space$db_path, thr_test_model_a_id))
  expect_false(file.exists(space$db_path))
  expect_error(thr_is_frozen(space$db_path, c("a", "b")), "model_id")
})

test_that("thr_is_frozen tells frozen models from the others", {
  space <- thr_test_frozen_fixture()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)

  expect_true(thr_is_frozen(space$db_path, thr_test_model_a_id))
  expect_false(thr_is_frozen(space$db_path, thr_test_model_b_id))
})

# --- thr_freeze_frame: what is written ---------------------------------------

test_that("freezing writes the model, the frame files and the strata", {
  space <- thr_test_db_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  thr_test_fixture_index(space)

  result <- thr_freeze_frame(space$db_path, space$index_path, thr_test_model_a_id,
                             h5_folder = space$h5_folder)

  expect_identical(result$model_id, thr_test_model_a_id)
  expect_identical(result$n_frame_files, 6L)
  expect_identical(result$n_rois, 65L)
  expect_identical(result$n_strata, 28L)
  expect_identical(result$n_excluded_training, 0L)
  expect_identical(result$n_no_raw, 0L)

  model <- thr_test_table(space$db_path, "models")
  expect_equal(nrow(model), 1)
  expect_identical(model$model_id, thr_test_model_a_id)
  expect_identical(model$classifier_name, "fixture_A")
  expect_identical(model$transform, "dataset_squarepad")
  expect_identical(thr_labels_from_json(model$class_labels_json), thr_test_labels)
  expect_identical(thr_numbers_from_json(model$score_edges_json), THR_SCORE_EDGES)
  expect_identical(model$fallback_source, "none")
  expect_identical(thr_numbers_from_json(model$fallback_thresholds_json),
                   rep(NA_real_, 5))
  expect_identical(model$h5_folder, space$h5_folder)
  expect_true(is.na(model$roi_folder))
  frozen_at <- as.POSIXct(model$frozen_at, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  expect_lt(abs(as.numeric(difftime(Sys.time(), frozen_at, units = "secs"))), 120)

  frame <- thr_test_table(space$db_path, "frame_files")
  indexed <- thr_test_query(
    space$index_path,
    "SELECT sample_name, h5_path, size, mtime, n_rois FROM files
     WHERE model_id = ? AND status = 'ok' ORDER BY sample_name",
    params = list(thr_test_model_a_id)
  )
  frame <- frame[order(frame$sample_name), ]
  expect_setequal(frame$sample_name, thr_test_a_samples)
  expect_identical(frame$h5_path, indexed$h5_path)
  expect_identical(frame$size, indexed$size)
  expect_identical(frame$mtime, indexed$mtime)
  expect_equal(frame$n_rois, indexed$n_rois)
  expect_true(all(frame$model_id == thr_test_model_a_id))
  expect_equal(nrow(thr_test_table(space$db_path, "excluded_samples")), 0)
})

test_that("strata match the hand counts from expected.json", {
  space <- thr_test_frozen_fixture()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)

  strata <- thr_test_strata_table(space$db_path, thr_test_model_a_id)
  hand <- thr_test_hand_strata(thr_test_expected_files("model_a"),
                               thr_test_labels, THR_SCORE_EDGES)
  thr_test_expect_same_strata(strata, hand)

  # Counted independently in Python (bisect over the same edges)
  expect_equal(nrow(strata), 28)
  expect_equal(sum(strata$n_population), 65)
  count <- function(class_name, bin) {
    strata$n_population[strata$class_name == class_name & strata$bin == bin]
  }
  expect_equal(count("Debris", 15), 7)
  expect_equal(count("Snowella/Woronichinia", 15), 3)
  expect_equal(count("Pauliella_taeniata_001", 4), 3)
  expect_equal(count("Öresund_ciliate", 3), 3)
  expect_equal(count("Skeletonema_spp", 6), 4)
  expect_length(count("Skeletonema_spp", 1), 0)
  expect_equal(
    as.vector(tapply(strata$n_population, strata$class_name, sum)[thr_test_labels]),
    c(18, 7, 16, 12, 12)
  )
  # No stratum without predictions is stored
  expect_true(all(strata$n_population > 0))
})

test_that("class names are stored as the raw labels", {
  space <- thr_test_frozen_fixture()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)

  stored <- unique(thr_test_strata_table(space$db_path, thr_test_model_a_id)$class_name)
  expect_setequal(stored, thr_test_labels)
  expect_true("Snowella/Woronichinia" %in% stored)
  expect_true("Pauliella_taeniata_001" %in% stored)
  expect_true("Öresund_ciliate" %in% stored)
})

test_that("freezing works on an index built from the real class-score files", {
  skip_if_not_installed("hdf5r")
  space <- thr_test_db_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  thr_test_copy_fixtures(space$h5_folder)
  build_threshold_index(space$h5_folder, space$index_path)

  thr_freeze_frame(space$db_path, space$index_path, thr_test_model_a_id,
                   h5_folder = space$h5_folder)

  thr_test_expect_same_strata(
    thr_test_strata_table(space$db_path, thr_test_model_a_id),
    thr_test_hand_strata(thr_test_expected_files("model_a"), thr_test_labels,
                         THR_SCORE_EDGES)
  )
})

test_that("models are frozen separately and never pooled", {
  space <- thr_test_frozen_fixture()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)

  thr_freeze_frame(space$db_path, space$index_path, thr_test_model_b_id)

  strata_b <- thr_test_strata_table(space$db_path, thr_test_model_b_id)
  thr_test_expect_same_strata(
    strata_b,
    thr_test_hand_strata(thr_test_expected_files("model_b"), thr_test_labels,
                         THR_SCORE_EDGES)
  )
  expect_equal(sum(strata_b$n_population), 20)
  expect_equal(nrow(strata_b), 13)
  # Model A is untouched by the second freeze
  strata_a <- thr_test_strata_table(space$db_path, thr_test_model_a_id)
  expect_equal(sum(strata_a$n_population), 65)
  frame <- thr_test_table(space$db_path, "frame_files")
  expect_equal(sum(frame$model_id == thr_test_model_b_id), 1)
  expect_equal(sum(frame$model_id == thr_test_model_a_id), 6)
})

test_that("the frame uses the edges it was frozen with", {
  space <- thr_test_db_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  thr_test_fixture_index(space)
  edges <- c(0, 0.5, 0.9)

  thr_freeze_frame(space$db_path, space$index_path, thr_test_model_a_id,
                   edges = edges)

  strata <- thr_test_strata_table(space$db_path, thr_test_model_a_id)
  thr_test_expect_same_strata(
    strata,
    thr_test_hand_strata(thr_test_expected_files("model_a"), thr_test_labels, edges)
  )
  expect_true(all(strata$bin %in% 1:3))
  model <- thr_test_table(space$db_path, "models")
  expect_identical(thr_numbers_from_json(model$score_edges_json), edges)
  expect_true(is.na(model$h5_folder))
})

test_that("a single edge puts every ROI in one bin", {
  space <- thr_test_db_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  thr_test_fixture_index(space)

  thr_freeze_frame(space$db_path, space$index_path, thr_test_model_a_id, edges = 0)

  strata <- thr_test_strata_table(space$db_path, thr_test_model_a_id)
  expect_equal(nrow(strata), 5)
  expect_true(all(strata$bin == 1))
  expect_equal(sum(strata$n_population), 65)
})

# --- thr_freeze_frame: exclusions --------------------------------------------

test_that("excluded and no-raw samples are left out and recorded with a reason", {
  space <- thr_test_db_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  thr_test_fixture_index(space)
  training <- c(thr_test_a_samples[3], "D20190101T000000_IFCB134")
  no_raw <- c(thr_test_a_samples[4], thr_test_a_samples[3])

  result <- thr_freeze_frame(space$db_path, space$index_path, thr_test_model_a_id,
                             excluded_samples = training, no_raw_samples = no_raw)

  expect_identical(result$n_frame_files, 4L)
  expect_identical(result$n_excluded_training, 1L)
  expect_identical(result$n_no_raw, 1L)
  frame <- thr_test_table(space$db_path, "frame_files")
  expect_setequal(frame$sample_name, thr_test_a_samples[c(1, 2, 5, 6)])

  # Only names that removed an indexed sample are recorded; a sample on both
  # lists counts as training.
  excluded <- thr_test_table(space$db_path, "excluded_samples")
  excluded <- excluded[order(excluded$sample_name), ]
  expect_identical(excluded$sample_name, thr_test_a_samples[3:4])
  expect_identical(excluded$reason, c("training", "no_raw_data"))

  kept <- thr_test_expected_files("model_a")
  kept <- kept[vapply(kept, function(entry) {
    !(entry$sample_name %in% thr_test_a_samples[3:4])
  }, logical(1))]
  strata <- thr_test_strata_table(space$db_path, thr_test_model_a_id)
  thr_test_expect_same_strata(
    strata, thr_test_hand_strata(kept, thr_test_labels, THR_SCORE_EDGES)
  )
  expect_equal(sum(strata$n_population), 52)
  expect_equal(nrow(strata), 24)
})

test_that("NULL exclusion lists mean no exclusions", {
  space <- thr_test_db_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  thr_test_fixture_index(space)

  result <- thr_freeze_frame(space$db_path, space$index_path, thr_test_model_a_id,
                             excluded_samples = NULL, no_raw_samples = NULL)

  expect_identical(result$n_frame_files, 6L)
})

test_that("an exclusion of another model's sample is not recorded", {
  space <- thr_test_db_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  thr_test_fixture_index(space)

  # Model B only scored the first sample
  result <- thr_freeze_frame(space$db_path, space$index_path, thr_test_model_a_id,
                             excluded_samples = thr_test_a_samples[2])
  expect_error(
    thr_freeze_frame(space$db_path, space$index_path, thr_test_model_b_id,
                     excluded_samples = thr_test_a_samples[1]),
    "No samples are left"
  )
  thr_freeze_frame(space$db_path, space$index_path, thr_test_model_b_id,
                   excluded_samples = thr_test_a_samples[2])

  excluded <- thr_test_table(space$db_path, "excluded_samples")
  expect_identical(excluded$model_id, thr_test_model_a_id)
  expect_identical(excluded$sample_name, thr_test_a_samples[2])
})

# --- thr_freeze_frame: the class-score folder --------------------------------

test_that("the frame is restricted to files under h5_folder", {
  space <- thr_test_db_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  inside <- file.path(space$h5_folder, "scores")
  thr_test_write_fake(inside, thr_test_a_samples[1])
  thr_test_write_fake(file.path(inside, "2024"), thr_test_a_samples[2])
  # A sibling whose name merely starts with the same characters
  thr_test_write_fake(file.path(space$h5_folder, "scores2"), thr_test_a_samples[3])
  build_threshold_index(space$h5_folder, space$index_path,
                        reader = thr_test_fake_reader())
  model_id <- thr_index_models(space$index_path)$model_id
  expect_length(model_id, 1)

  # A trailing slash must not change what "under the folder" means
  result <- thr_freeze_frame(space$db_path, space$index_path, model_id,
                             h5_folder = paste0(inside, "/"))

  expect_identical(result$n_frame_files, 2L)
  frame <- thr_test_table(space$db_path, "frame_files")
  expect_setequal(frame$sample_name, thr_test_a_samples[1:2])
  expect_identical(thr_test_table(space$db_path, "models")$h5_folder, inside)
  expect_equal(sum(thr_test_table(space$db_path, "strata")$n_population), 8)
})

test_that("without h5_folder the frame takes every indexed file of the model", {
  space <- thr_test_db_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  thr_test_write_fake(file.path(space$h5_folder, "scores"), thr_test_a_samples[1])
  thr_test_write_fake(file.path(space$h5_folder, "scores2"), thr_test_a_samples[3])
  build_threshold_index(space$h5_folder, space$index_path,
                        reader = thr_test_fake_reader())
  model_id <- thr_index_models(space$index_path)$model_id

  result <- thr_freeze_frame(space$db_path, space$index_path, model_id)

  expect_identical(result$n_frame_files, 2L)
})

test_that("a sample that is usable in two folders is taken once, first by path", {
  space <- thr_test_db_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  folders <- thr_test_write_copies(space$h5_folder, c("a_old", "b_new"),
                                   thr_test_a_samples[1:3])
  thr_test_write_fake(folders[["b_new"]], thr_test_a_samples[4])
  for (folder in folders) {
    build_threshold_index(folder, space$index_path, reader = thr_test_fake_reader())
  }
  model_id <- thr_index_models(space$index_path)$model_id
  expect_equal(sum(thr_test_files_table(space$index_path)$status == "ok"), 7)

  # Over every folder, and over a folder that holds both
  for (folder in list(NA_character_, space$h5_folder)) {
    expect_identical(thr_frame_sample_names(space$index_path, model_id, folder),
                     thr_test_a_samples[1:4])
    db_path <- tempfile(fileext = ".sqlite")
    result <- thr_freeze_frame(db_path, space$index_path, model_id,
                               h5_folder = folder)
    expect_identical(result$n_frame_files, 4L)
    expect_identical(result$n_rois, 16L)
    frame <- thr_test_table(db_path, "frame_files")
    frame <- frame[order(frame$sample_name), ]
    expect_identical(basename(dirname(frame$h5_path)),
                     c("a_old", "a_old", "a_old", "b_new"))
    expect_equal(sum(thr_test_table(db_path, "strata")$n_population), 16)
    unlink(db_path)
  }

  # A frame on the second folder holds that folder's own files
  thr_freeze_frame(space$db_path, space$index_path, model_id,
                   h5_folder = folders[["b_new"]])
  frame <- thr_test_table(space$db_path, "frame_files")
  expect_identical(unique(basename(dirname(frame$h5_path))), "b_new")
  expect_equal(nrow(frame), 4)
})

test_that("the samples a frame would start from can be listed before freezing", {
  space <- thr_test_db_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  inside <- file.path(space$h5_folder, "scores")
  thr_test_write_fake(inside, thr_test_a_samples[2])
  thr_test_write_fake(file.path(inside, "2024"), thr_test_a_samples[1])
  thr_test_write_fake(file.path(space$h5_folder, "scores2"), thr_test_a_samples[3])
  # A second file for a sample is a duplicate, not another sample
  thr_test_write_fake(file.path(inside, "copy"), thr_test_a_samples[2])
  expect_error(thr_frame_sample_names(space$index_path, "any-model"),
               "build_threshold_index")
  build_threshold_index(space$h5_folder, space$index_path,
                        reader = thr_test_fake_reader())
  model_id <- thr_index_models(space$index_path)$model_id

  expect_identical(thr_frame_sample_names(space$index_path, model_id),
                   thr_test_a_samples[1:3])
  expect_identical(thr_frame_sample_names(space$index_path, model_id,
                                          h5_folder = inside),
                   thr_test_a_samples[1:2])
  expect_identical(thr_frame_sample_names(space$index_path, "no-such-model"),
                   character())
  expect_false(file.exists(space$db_path))

  # Exactly the samples a freeze on that folder then takes
  thr_freeze_frame(space$db_path, space$index_path, model_id, h5_folder = inside)
  expect_setequal(thr_test_table(space$db_path, "frame_files")$sample_name,
                  thr_frame_sample_names(space$index_path, model_id, inside))
})

test_that("a folder holding none of the model's files gives an empty frame error", {
  space <- thr_test_db_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  thr_test_fixture_index(space)
  elsewhere <- file.path(space$root, "elsewhere")
  dir.create(elsewhere)

  expect_error(
    thr_freeze_frame(space$db_path, space$index_path, thr_test_model_a_id,
                     h5_folder = elsewhere),
    "No samples are left"
  )
})

test_that("being under a folder is a matter of path components", {
  paths <- c("/data/scores/a_class.h5", "/data/scores/2024/b_class.h5",
             "/data/scores2/c_class.h5", "/data/scores", "/other/data/scores/d.h5")

  expect_identical(thr_under_folder(paths, "/data/scores"),
                   c(TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_identical(thr_under_folder(paths, NA_character_), rep(TRUE, 5))
  expect_identical(thr_under_folder(paths, "/"), rep(TRUE, 5))
  # Characters that are wildcards in SQL are plain characters here
  expect_identical(thr_under_folder(c("/d/model_a/x.h5", "/d/modelXa/x.h5",
                                      "/d/100%/x.h5"), "/d/model_a"),
                   c(TRUE, FALSE, FALSE))
  expect_identical(thr_under_folder(character(), "/data"), logical())
})

test_that("folders are normalised without a trailing slash", {
  folder <- tempfile("thr_folder_")
  dir.create(folder)
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  expected <- normalizePath(folder, winslash = "/")

  expect_identical(thr_normalise_folder(folder), expected)
  expect_identical(thr_normalise_folder(paste0(folder, "/")), expected)
  expect_identical(thr_normalise_folder(paste0(folder, "//")), expected)
  if (.Platform$OS.type != "windows") {
    expect_identical(thr_normalise_folder("/"), "/")
  }
  expect_identical(thr_check_folder_arg(NULL, "h5_folder"), NA_character_)
  expect_identical(thr_check_folder_arg(NA, "h5_folder"), NA_character_)
})

# --- thr_freeze_frame: fallback thresholds -----------------------------------

test_that("fallback thresholds and their source are stored with the model", {
  space <- thr_test_db_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  thr_test_fixture_index(space)
  fallback <- c(0.3, 0.1 + 0.2, NaN, NA, 0.9999)

  thr_freeze_frame(space$db_path, space$index_path, thr_test_model_a_id,
                   fallback = fallback, fallback_source = "json:/models/thr.json",
                   roi_folder = space$root)

  model <- thr_test_table(space$db_path, "models")
  expect_identical(model$fallback_source, "json:/models/thr.json")
  expect_identical(thr_numbers_from_json(model$fallback_thresholds_json),
                   c(0.3, 0.1 + 0.2, NA, NA, 0.9999))
  expect_identical(model$roi_folder,
                   normalizePath(space$root, winslash = "/"))
})

test_that("an inconsistent fallback is refused", {
  space <- thr_test_db_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  thr_test_fixture_index(space)
  freeze <- function(...) {
    thr_freeze_frame(space$db_path, space$index_path, thr_test_model_a_id, ...)
  }

  expect_error(freeze(fallback = c(0.1, 0.2), fallback_source = "h5"),
               "one value per class")
  expect_error(freeze(fallback = c(0.1, 0.2, 0.3, 0.4, 1.5), fallback_source = "h5"),
               "between 0 and 1")
  expect_error(freeze(fallback = letters[1:5], fallback_source = "h5"), "numeric")
  expect_error(freeze(fallback = rep(0.5, 5)), "fallback_source")
  expect_error(freeze(fallback_source = "h5"), "fallback_source")
  expect_error(freeze(fallback = rep(0.5, 5), fallback_source = "somewhere"),
               "fallback_source")
  expect_error(freeze(fallback = rep(0.5, 5), fallback_source = "json:"),
               "fallback_source")
  expect_equal(unname(thr_test_frame_counts(space$db_path)), rep(0, 4))
})

# --- thr_freeze_frame: refusals and atomicity --------------------------------

test_that("a model can be frozen only once", {
  space <- thr_test_frozen_fixture()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  before <- thr_test_frame_counts(space$db_path)

  expect_error(
    thr_freeze_frame(space$db_path, space$index_path, thr_test_model_a_id,
                     excluded_samples = thr_test_a_samples[1]),
    "already frozen"
  )
  expect_identical(thr_test_frame_counts(space$db_path), before)
  expect_equal(nrow(thr_test_table(space$db_path, "excluded_samples")), 0)
})

test_that("a model the index does not know cannot be frozen", {
  space <- thr_test_db_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  thr_test_fixture_index(space)

  expect_error(
    thr_freeze_frame(space$db_path, space$index_path, "no-such-model"),
    "not in the index"
  )
  expect_equal(unname(thr_test_frame_counts(space$db_path)), rep(0, 4))
})

test_that("freezing needs an index and does not create one", {
  space <- thr_test_db_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)

  expect_error(
    thr_freeze_frame(space$db_path, space$index_path, thr_test_model_a_id),
    "build_threshold_index"
  )
  expect_false(file.exists(space$index_path))
})

test_that("an empty frame is refused and leaves nothing behind", {
  space <- thr_test_db_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  thr_test_fixture_index(space)

  expect_error(
    thr_freeze_frame(space$db_path, space$index_path, thr_test_model_a_id,
                     excluded_samples = thr_test_a_samples[1:4],
                     no_raw_samples = thr_test_a_samples[5:6]),
    "No samples are left"
  )
  expect_equal(unname(thr_test_frame_counts(space$db_path)), rep(0, 4))
  expect_false(thr_is_frozen(space$db_path, thr_test_model_a_id))
})

test_that("a frame without any ROI is refused", {
  space <- thr_test_db_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  empty <- thr_test_summary(roi_numbers = integer(), class_idx = integer(),
                            top_score = numeric())
  thr_test_write_fake(space$h5_folder, thr_test_a_samples[1], empty)
  build_threshold_index(space$h5_folder, space$index_path,
                        reader = thr_test_fake_reader())
  model_id <- thr_index_models(space$index_path)$model_id

  expect_error(thr_freeze_frame(space$db_path, space$index_path, model_id),
               "no ROIs")
  expect_equal(unname(thr_test_frame_counts(space$db_path)), rep(0, 4))
})

test_that("a failure part-way through freezing leaves no trace", {
  space <- thr_test_db_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  thr_test_fixture_index(space)

  # The strata are written last, after the model, the frame and the exclusions
  local({
    local_mocked_bindings(
      thr_insert_strata = function(...) stop("disk full", call. = FALSE)
    )
    expect_error(
      thr_freeze_frame(space$db_path, space$index_path, thr_test_model_a_id,
                       excluded_samples = thr_test_a_samples[1]),
      "disk full"
    )
  })

  expect_equal(unname(thr_test_frame_counts(space$db_path)), rep(0, 4))
  expect_false(thr_is_frozen(space$db_path, thr_test_model_a_id))

  # The database is left usable: the same freeze now goes through
  result <- thr_freeze_frame(space$db_path, space$index_path, thr_test_model_a_id,
                             excluded_samples = thr_test_a_samples[1])
  expect_identical(result$n_frame_files, 5L)
})

test_that("an index that lost ROI rows is caught rather than frozen", {
  space <- thr_test_db_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  thr_test_fixture_index(space)
  thr_test_execute(space$index_path, "DELETE FROM rois WHERE roi_number = 2")

  expect_error(
    thr_freeze_frame(space$db_path, space$index_path, thr_test_model_a_id),
    "inconsistent"
  )
  expect_equal(unname(thr_test_frame_counts(space$db_path)), rep(0, 4))
})

test_that("a file the index has no size or time for is left out of the frame", {
  space <- thr_test_db_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  thr_test_fixture_index(space)
  thr_test_execute(space$index_path,
                   "UPDATE files SET mtime = NULL WHERE sample_name = ? AND model_id = ?",
                   params = list(thr_test_a_samples[4], thr_test_model_a_id))

  result <- thr_freeze_frame(space$db_path, space$index_path, thr_test_model_a_id)

  # It could never be told apart from a changed file afterwards
  expect_identical(result$n_frame_files, 5L)
  expect_identical(result$n_rois, 64L)
  expect_false(thr_test_a_samples[4] %in%
                 thr_test_table(space$db_path, "frame_files")$sample_name)
  status <- thr_frame_status(space$db_path, space$index_path, thr_test_model_a_id)
  expect_identical(status$n_changed_files, 0L)
})

test_that("thr_freeze_frame validates its arguments", {
  space <- thr_test_db_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  thr_test_fixture_index(space)
  freeze <- function(model_id = thr_test_model_a_id, ...) {
    thr_freeze_frame(space$db_path, space$index_path, model_id, ...)
  }

  expect_error(freeze(model_id = NA_character_), "model_id")
  expect_error(freeze(excluded_samples = 1:3), "excluded_samples")
  expect_error(freeze(no_raw_samples = list("a")), "no_raw_samples")
  expect_error(freeze(excluded_samples = c("a", NA)), "excluded_samples")
  expect_error(freeze(edges = c(0.1, 0.5)), "edges")
  expect_error(freeze(h5_folder = c("a", "b")), "h5_folder")
  expect_error(freeze(roi_folder = 3), "roi_folder")
  expect_error(thr_freeze_frame(NA_character_, space$index_path, thr_test_model_a_id),
               "db_path")
  expect_error(thr_freeze_frame(space$db_path, 5, thr_test_model_a_id),
               "index_path")
  expect_false(file.exists(space$db_path))
})

# --- thr_frame_status --------------------------------------------------------

test_that("frame status of a model that is not frozen", {
  space <- thr_test_db_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  thr_test_fixture_index(space)

  status <- thr_frame_status(space$db_path, space$index_path, thr_test_model_a_id)

  expect_false(status$frozen)
  expect_identical(status$n_frame_files, 0L)
  expect_identical(status$n_excluded, 0L)
  expect_identical(status$n_new_files, 0L)
  expect_identical(status$n_changed_files, 0L)
  expect_false(file.exists(space$db_path))
})

test_that("frame status of one model is not that of another in the same database", {
  space <- thr_test_frozen_fixture()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)

  status <- thr_frame_status(space$db_path, space$index_path, thr_test_model_b_id)

  expect_false(status$frozen)
  expect_identical(status$n_frame_files, 0L)
})

test_that("frame status counts the frame and what was left out", {
  space <- thr_test_frozen_fixture(
    excluded_samples = c(thr_test_a_samples[3], "D20190101T000000_IFCB134"),
    no_raw_samples = thr_test_a_samples[4]
  )
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)

  status <- thr_frame_status(space$db_path, space$index_path, thr_test_model_a_id)

  expect_true(status$frozen)
  expect_identical(status$n_frame_files, 4L)
  # "2 samples excluded": the two that were indexed and left out
  expect_identical(status$n_excluded, 2L)
  expect_identical(status$n_excluded_training, 1L)
  expect_identical(status$n_no_raw, 1L)
  # Excluded samples are still indexed, but they are not new
  expect_identical(status$n_new_files, 0L)
  expect_identical(status$n_changed_files, 0L)
})

test_that("frame status sees files indexed after the freeze", {
  space <- thr_test_frozen_fixture()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  expected <- thr_test_expected()$files[[1]]
  later <- thr_test_summary(checkpoint_sha256 = expected$checkpoint_sha256,
                            classifier_name = expected$classifier_name)
  thr_test_write_fake(file.path(space$h5_folder, "model_a"),
                      "D20240201T000000_IFCB134", later)
  # Same model, but outside the folder the frame was frozen on
  outside <- file.path(space$root, "other_scores")
  thr_test_write_fake(outside, "D20240202T000000_IFCB134", later)
  build_threshold_index(space$h5_folder, space$index_path,
                        reader = thr_test_fake_reader())
  build_threshold_index(outside, space$index_path, reader = thr_test_fake_reader())
  expect_equal(
    thr_index_models(space$index_path)$n_files[
      thr_index_models(space$index_path)$model_id == thr_test_model_a_id
    ], 8
  )

  status <- thr_frame_status(space$db_path, space$index_path, thr_test_model_a_id)

  expect_identical(status$n_new_files, 1L)
  expect_identical(status$n_changed_files, 0L)
  expect_identical(status$n_frame_files, 6L)
  # The frame itself did not grow
  expect_equal(nrow(thr_test_table(space$db_path, "frame_files")), 6)
})

test_that("frame status sees frame files that changed or went missing", {
  space <- thr_test_frozen_fixture()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  folder <- file.path(space$h5_folder, "model_a")
  expected <- thr_test_expected()$files[[1]]
  rescored <- thr_test_summary(checkpoint_sha256 = expected$checkpoint_sha256,
                               classifier_name = expected$classifier_name,
                               roi_numbers = 1:40, class_idx = rep(1L, 40),
                               top_score = rep(0.5, 40))
  thr_test_write_fake(folder, thr_test_a_samples[1], rescored)
  unlink(file.path(folder, paste0(thr_test_a_samples[2], "_class.h5")))
  build_threshold_index(space$h5_folder, space$index_path,
                        reader = thr_test_fake_reader())

  status <- thr_frame_status(space$db_path, space$index_path, thr_test_model_a_id)

  expect_identical(status$n_changed_files, 2L)
  expect_identical(status$n_new_files, 0L)
  expect_identical(status$n_frame_files, 6L)
})

test_that("a frame file whose only change is its modification time counts as changed", {
  space <- thr_test_frozen_fixture()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  path <- file.path(space$h5_folder, "model_a",
                    paste0(thr_test_a_samples[5], "_class.h5"))
  Sys.setFileTime(path, Sys.time() + 3600)
  build_threshold_index(space$h5_folder, space$index_path,
                        reader = thr_test_fake_reader())

  status <- thr_frame_status(space$db_path, space$index_path, thr_test_model_a_id)

  expect_identical(status$n_changed_files, 1L)
})

test_that("frame status of a frozen frame is read without the index", {
  space <- thr_test_frozen_fixture(excluded_samples = thr_test_a_samples[1],
                                   no_raw_samples = thr_test_a_samples[2])
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  with_index <- thr_frame_status(space$db_path, space$index_path,
                                 thr_test_model_a_id)
  expect_true(with_index$index_available)
  unlink(space$index_path)

  status <- thr_frame_status(space$db_path, space$index_path, thr_test_model_a_id)

  # What the verdict database holds is known; what only the index can tell
  # is not, and is not made up
  expect_true(status$frozen)
  expect_false(status$index_available)
  expect_identical(status$n_frame_files, 4L)
  expect_identical(status$n_excluded_training, 1L)
  expect_identical(status$n_no_raw, 1L)
  expect_identical(status$n_new_files, NA_integer_)
  expect_identical(status$n_changed_files, NA_integer_)
  expect_false(file.exists(space$index_path))
  expect_false(thr_frame_status(space$db_path, space$index_path,
                                thr_test_model_b_id)$frozen)
  expect_error(thr_frame_status(space$db_path, NULL, thr_test_model_a_id),
               "index_path")
})
