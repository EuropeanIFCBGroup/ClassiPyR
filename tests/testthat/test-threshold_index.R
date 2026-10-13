# Tests for the class-score index cache of the threshold review app
#
# Most tests use stand-in class-score files (an RDS of a reader summary, see
# helper-threshold-h5.R) and an injected reader, so they run without hdf5r.

library(testthat)

# --- schema ------------------------------------------------------------------

test_that("thr_init_index_schema creates the tables and is idempotent", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  thr_init_index_schema(con)
  expect_silent(thr_init_index_schema(con))

  expect_setequal(DBI::dbListTables(con), c("meta", "models", "files", "rois"))
  version <- DBI::dbGetQuery(con, "SELECT value FROM meta WHERE key = 'schema_version'")
  expect_equal(nrow(version), 1)
  expect_identical(version$value, THR_INDEX_SCHEMA_VERSION)

  expect_identical(
    DBI::dbGetQuery(con, "PRAGMA table_info(files)")$name,
    c("file_id", "h5_path", "sample_name", "size", "mtime", "model_id", "n_rois",
      "thresholds_json", "status", "message")
  )
  expect_identical(DBI::dbGetQuery(con, "PRAGMA table_info(rois)")$name,
                   c("file_id", "roi_number", "class_idx", "score"))
  expect_identical(
    DBI::dbGetQuery(con, "PRAGMA table_info(models)")$name,
    c("model_id", "classifier_name", "checkpoint_sha256", "transform",
      "class_labels_json")
  )
  indexes <- DBI::dbGetQuery(con, "SELECT name FROM sqlite_master WHERE type = 'index'")
  expect_true(all(c("rois_class_score", "files_model", "files_sample") %in%
                    indexes$name))
})

test_that("the duplicate lookup of a build is answered from an index", {
  ws <- thr_test_workspace()
  on.exit(unlink(ws$root, recursive = TRUE), add = TRUE)
  thr_test_write_fake(ws$h5_folder, thr_test_samples[1])
  build_threshold_index(ws$h5_folder, ws$index_path, reader = thr_test_fake_reader())

  # The statement runs once per sample of every batch: a table scan there
  # made a first build of 8000 files five times slower
  plan <- thr_test_query(ws$index_path,
                         paste("EXPLAIN QUERY PLAN", THR_DUPLICATE_LOOKUP_SQL),
                         params = list(thr_test_samples[1]))
  expect_match(plan$detail, "USING INDEX files_sample", fixed = TRUE)
  expect_false(any(grepl("SCAN", plan$detail, fixed = TRUE)))
})

test_that("an index file written without the sample index gets it at the next build", {
  ws <- thr_test_workspace()
  on.exit(unlink(ws$root, recursive = TRUE), add = TRUE)
  for (s in thr_test_samples[1:2]) thr_test_write_fake(ws$h5_folder, s)
  build_threshold_index(ws$h5_folder, ws$index_path, reader = thr_test_fake_reader())
  index_names <- function() {
    thr_test_query(ws$index_path,
                   "SELECT name FROM sqlite_master WHERE type = 'index'")$name
  }
  # What a file built by the first version of the index looks like
  thr_test_execute(ws$index_path, "DROP INDEX IF EXISTS files_sample")
  expect_false("files_sample" %in% index_names())

  counter <- thr_test_counting_reader()
  build_threshold_index(ws$h5_folder, ws$index_path, reader = counter$reader)

  expect_true("files_sample" %in% index_names())
  # Added in place: the cache is not thrown away for it
  expect_length(counter$paths(), 0)
  expect_identical(thr_test_files_table(ws$index_path)$status, c("ok", "ok"))
})

# --- scanning ----------------------------------------------------------------

test_that("thr_scan_h5_folder finds class-score files recursively", {
  ws <- thr_test_workspace()
  on.exit(unlink(ws$root, recursive = TRUE), add = TRUE)
  thr_test_write_fake(file.path(ws$h5_folder, "2024", "b"), thr_test_samples[2])
  thr_test_write_fake(ws$h5_folder, thr_test_samples[1])
  thr_test_write_fake(ws$h5_folder, thr_test_samples[3], suffix = "_class_v3.h5")
  writeLines("x", file.path(ws$h5_folder, paste0(thr_test_samples[4], ".csv")))
  writeLines("x", file.path(ws$h5_folder, paste0(thr_test_samples[4], "_features.h5")))
  writeLines("x", file.path(ws$h5_folder, paste0(thr_test_samples[4], "_class.h5.tmp")))

  scanned <- thr_scan_h5_folder(ws$h5_folder)

  expect_named(scanned, c("h5_path", "sample_name", "size", "mtime"))
  expect_equal(nrow(scanned), 3)
  expect_setequal(scanned$sample_name, thr_test_samples[1:3])
  expect_identical(scanned$h5_path, normalizePath(scanned$h5_path, winslash = "/"))
  expect_false(is.unsorted(scanned$h5_path, strictly = TRUE))
  expect_type(scanned$size, "double")
  expect_type(scanned$mtime, "double")
  expect_true(all(scanned$size > 0))
  expect_equal(scanned$size, file.size(scanned$h5_path))
})

test_that("thr_scan_h5_folder returns a typed empty frame for an empty folder", {
  ws <- thr_test_workspace()
  on.exit(unlink(ws$root, recursive = TRUE), add = TRUE)

  scanned <- thr_scan_h5_folder(ws$h5_folder)
  expect_equal(nrow(scanned), 0)
  expect_named(scanned, c("h5_path", "sample_name", "size", "mtime"))
  expect_type(scanned$h5_path, "character")
  expect_type(scanned$size, "double")
})

test_that("thr_scan_h5_folder rejects a folder that is not there", {
  expect_error(thr_scan_h5_folder(file.path(tempdir(), "no_such_folder_xyz")),
               "not found")
  expect_error(thr_scan_h5_folder(NULL), "single")
  expect_error(thr_scan_h5_folder(c("a", "b")), "single")
})

# --- staleness ---------------------------------------------------------------

test_that("thr_stale_files separates new, changed and missing files", {
  ws <- thr_test_workspace()
  on.exit(unlink(ws$root, recursive = TRUE), add = TRUE)
  paths <- vapply(thr_test_samples[1:4], function(s) {
    thr_test_write_fake(ws$h5_folder, s)
  }, character(1))
  build_threshold_index(ws$h5_folder, ws$index_path, reader = thr_test_fake_reader())

  Sys.setFileTime(paths[2], Sys.time() + 120)                       # changed
  unlink(paths[3])                                                  # missing
  new_path <- thr_test_write_fake(ws$h5_folder, thr_test_samples[5])  # new

  con <- DBI::dbConnect(RSQLite::SQLite(), ws$index_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  stale <- thr_stale_files(con, thr_scan_h5_folder(ws$h5_folder))

  expect_named(stale, c("new", "changed", "missing"))
  expect_identical(basename(stale$new$h5_path), basename(new_path))
  expect_identical(basename(stale$changed$h5_path), basename(paths[[2]]))
  expect_identical(basename(stale$missing), basename(paths[[3]]))
  expect_named(stale$new, c("h5_path", "sample_name", "size", "mtime"))
  expect_named(stale$changed, c("h5_path", "sample_name", "size", "mtime"))
})

test_that("thr_stale_files can be limited to the files of one folder", {
  ws <- thr_test_workspace()
  on.exit(unlink(ws$root, recursive = TRUE), add = TRUE)
  inside <- file.path(ws$h5_folder, "2024")
  outside <- file.path(ws$h5_folder, "2023")
  in_gone <- thr_test_write_fake(inside, thr_test_samples[1])
  thr_test_write_fake(inside, thr_test_samples[2])
  out_gone <- thr_test_write_fake(outside, thr_test_samples[3])
  build_threshold_index(ws$h5_folder, ws$index_path, reader = thr_test_fake_reader())
  unlink(c(in_gone, out_gone))
  con <- DBI::dbConnect(RSQLite::SQLite(), ws$index_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  scanned <- thr_scan_h5_folder(inside)

  everywhere <- thr_stale_files(con, scanned)
  expect_length(everywhere$missing, 2)

  # A file that went missing in another folder is that folder's business
  here <- thr_stale_files(con, scanned, folder = thr_normalise_folder(inside))
  expect_identical(basename(here$missing),
                   paste0(thr_test_samples[1], "_class.h5"))
  expect_equal(nrow(here$new), 0)
  expect_equal(nrow(here$changed), 0)
})

test_that("thr_stale_files treats everything as new in an empty index", {
  ws <- thr_test_workspace()
  on.exit(unlink(ws$root, recursive = TRUE), add = TRUE)
  thr_test_write_fake(ws$h5_folder, thr_test_samples[1])
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  thr_init_index_schema(con)

  stale <- thr_stale_files(con, thr_scan_h5_folder(ws$h5_folder))
  expect_equal(nrow(stale$new), 1)
  expect_equal(nrow(stale$changed), 0)
  expect_identical(stale$missing, character())
})

# --- build: contents ---------------------------------------------------------

test_that("build_threshold_index stores files, models and ROIs", {
  ws <- thr_test_workspace()
  on.exit(unlink(ws$root, recursive = TRUE), add = TRUE)
  summary_1 <- thr_test_summary()
  summary_2 <- thr_test_summary(roi_numbers = c(3, 9), class_idx = c(5, 4),
                                top_score = c(0.1 + 0.2, 0.9999))
  thr_test_write_fake(ws$h5_folder, thr_test_samples[1], summary_1)
  thr_test_write_fake(ws$h5_folder, thr_test_samples[2], summary_2)

  result <- build_threshold_index(ws$h5_folder, ws$index_path,
                                  reader = thr_test_fake_reader())

  expect_named(result, c("n_scanned", "n_read", "n_ok", "n_skipped", "failures"))
  expect_identical(result$n_scanned, 2L)
  expect_identical(result$n_read, 2L)
  expect_identical(result$n_ok, 2L)
  expect_identical(result$n_skipped, 0L)
  expect_equal(nrow(result$failures), 0)
  expect_named(result$failures, c("h5_path", "status", "message"))

  files <- thr_test_files_table(ws$index_path)
  expect_identical(files$status, c("ok", "ok"))
  expect_identical(files$sample_name, thr_test_samples[1:2])
  expect_identical(files$n_rois, c(4L, 2L))
  expect_identical(files$h5_path, normalizePath(files$h5_path, winslash = "/"))
  expect_identical(files$size, file.size(files$h5_path))
  expect_identical(files$mtime, as.numeric(file.mtime(files$h5_path)))
  expect_identical(files$model_id,
                   rep(thr_model_identity(summary_1)$model_id, 2))

  rois <- thr_test_query(ws$index_path, "
    SELECT f.sample_name, r.roi_number, r.class_idx, r.score
    FROM rois r JOIN files f ON f.file_id = r.file_id
    ORDER BY f.sample_name, r.roi_number")
  expect_identical(rois$roi_number, c(1:4, 3L, 9L))
  expect_identical(rois$class_idx, c(1L, 2L, 3L, 1L, 5L, 4L))
  # Scores must come back bit for bit: they decide the score bin
  expect_identical(rois$score, c(0.95, 0.4, 0.999, 0.15, 0.1 + 0.2, 0.9999))
})

test_that("build_threshold_index returns its result invisibly", {
  ws <- thr_test_workspace()
  on.exit(unlink(ws$root, recursive = TRUE), add = TRUE)
  thr_test_write_fake(ws$h5_folder, thr_test_samples[1])

  expect_invisible(build_threshold_index(ws$h5_folder, ws$index_path,
                                         reader = thr_test_fake_reader()))
})

test_that("class labels and thresholds round-trip through the index", {
  ws <- thr_test_workspace()
  on.exit(unlink(ws$root, recursive = TRUE), add = TRUE)
  labels <- c(thr_test_labels[1:4], "NA")
  summary <- thr_test_summary(class_labels = labels,
                              thresholds = c(0.1 + 0.2, NaN, 0.9999, NaN, 1 / 3))
  thr_test_write_fake(ws$h5_folder, thr_test_samples[1], summary)
  build_threshold_index(ws$h5_folder, ws$index_path, reader = thr_test_fake_reader())

  model_id <- thr_model_identity(summary)$model_id
  expect_identical(thr_index_class_labels(ws$index_path, model_id), labels)

  stored <- thr_test_query(ws$index_path, "SELECT class_labels_json FROM models")
  expect_match(stored$class_labels_json, "Öresund_ciliate", fixed = TRUE)
  expect_match(stored$class_labels_json, "Snowella/Woronichinia", fixed = TRUE)

  thresholds_json <- thr_test_files_table(ws$index_path)$thresholds_json
  expect_match(thresholds_json, "null", fixed = TRUE)
  expect_false(grepl("NaN", thresholds_json, fixed = TRUE))
  expect_identical(thr_numbers_from_json(thresholds_json),
                   c(0.1 + 0.2, NA, 0.9999, NA, 1 / 3))
})

test_that("models are kept apart and summarised over their ok files", {
  ws <- thr_test_workspace()
  on.exit(unlink(ws$root, recursive = TRUE), add = TRUE)
  model_a <- function(...) thr_test_summary(...)
  model_b <- function(...) {
    thr_test_summary(checkpoint_sha256 = "sha-fake-b", classifier_name = "fake_B", ...)
  }
  thr_test_write_fake(file.path(ws$h5_folder, "a"), thr_test_samples[1], model_a())
  thr_test_write_fake(file.path(ws$h5_folder, "a"), thr_test_samples[2],
                      model_a(thresholds = rep(0.5, 5)))
  thr_test_write_fake(file.path(ws$h5_folder, "a"), thr_test_samples[3],
                      model_a(roi_numbers = 1:2, class_idx = 1:2, top_score = c(0.5, 0.6)))
  # The same sample scored by another model is not a duplicate
  thr_test_write_fake(file.path(ws$h5_folder, "b"), thr_test_samples[1], model_b())
  build_threshold_index(ws$h5_folder, ws$index_path, reader = thr_test_fake_reader())

  models <- thr_index_models(ws$index_path)
  expect_named(models, c("model_id", "classifier_name", "checkpoint_sha256",
                         "transform", "n_files", "n_rois", "n_threshold_sets"))
  expect_equal(nrow(models), 2)
  expect_identical(models$classifier_name, c("fake_A", "fake_B"))
  expect_identical(models$checkpoint_sha256, c("sha-fake-a", "sha-fake-b"))
  expect_identical(models$transform, rep("dataset_squarepad", 2))
  expect_identical(models$n_files, c(3L, 1L))
  expect_identical(models$n_rois, c(10L, 4L))
  expect_identical(models$n_threshold_sets, c(2L, 1L))
  expect_identical(models$model_id,
                   c(thr_model_identity(model_a())$model_id,
                     thr_model_identity(model_b())$model_id))
  expect_identical(thr_test_files_table(ws$index_path)$status, rep("ok", 4))
})

test_that("a model without a checkpoint hash is stored with a NULL hash", {
  ws <- thr_test_workspace()
  on.exit(unlink(ws$root, recursive = TRUE), add = TRUE)
  thr_test_write_fake(ws$h5_folder, thr_test_samples[1],
                      thr_test_summary(checkpoint_sha256 = NULL))
  build_threshold_index(ws$h5_folder, ws$index_path, reader = thr_test_fake_reader())

  models <- thr_index_models(ws$index_path)
  expect_identical(models$checkpoint_sha256, NA_character_)
  expect_identical(models$model_id, "f3736312a735109ab5149ff7c987cc65")
})

test_that("a file with no ROIs is indexed as ok with no ROI rows", {
  ws <- thr_test_workspace()
  on.exit(unlink(ws$root, recursive = TRUE), add = TRUE)
  thr_test_write_fake(ws$h5_folder, thr_test_samples[1],
                      thr_test_summary(roi_numbers = integer(), class_idx = integer(),
                                       top_score = numeric()))
  result <- build_threshold_index(ws$h5_folder, ws$index_path,
                                  reader = thr_test_fake_reader())

  expect_identical(result$n_ok, 1L)
  files <- thr_test_files_table(ws$index_path)
  expect_identical(files$status, "ok")
  expect_identical(files$n_rois, 0L)
  expect_equal(thr_test_query(ws$index_path, "SELECT COUNT(*) AS n FROM rois")$n, 0)
  expect_identical(thr_index_models(ws$index_path)$n_rois, 0L)
})

# --- build: incremental ------------------------------------------------------

test_that("a second build reads nothing", {
  ws <- thr_test_workspace()
  on.exit(unlink(ws$root, recursive = TRUE), add = TRUE)
  for (s in thr_test_samples[1:3]) thr_test_write_fake(ws$h5_folder, s)
  counter <- thr_test_counting_reader()

  first <- build_threshold_index(ws$h5_folder, ws$index_path, reader = counter$reader)
  expect_length(counter$paths(), 3)
  before <- thr_test_files_table(ws$index_path)
  counter$reset()

  second <- build_threshold_index(ws$h5_folder, ws$index_path, reader = counter$reader)
  expect_length(counter$paths(), 0)
  expect_identical(second$n_scanned, 3L)
  expect_identical(second$n_read, 0L)
  expect_identical(second$n_skipped, 3L)
  expect_identical(second$n_ok, 3L)
  expect_identical(thr_test_files_table(ws$index_path), before)
  expect_identical(first$n_ok, 3L)
})

test_that("a touched file is re-read and its old rows are replaced", {
  ws <- thr_test_workspace()
  on.exit(unlink(ws$root, recursive = TRUE), add = TRUE)
  paths <- vapply(thr_test_samples[1:3], function(s) {
    thr_test_write_fake(ws$h5_folder, s)
  }, character(1))
  counter <- thr_test_counting_reader()
  build_threshold_index(ws$h5_folder, ws$index_path, reader = counter$reader)
  ids_before <- thr_test_files_table(ws$index_path)$file_id
  counter$reset()

  # Same sample re-scored: fewer ROIs, and one ROI number that was not there
  saveRDS(thr_test_summary(roi_numbers = c(2, 7), class_idx = c(4, 4),
                           top_score = c(0.25, 0.75)), paths[2])
  Sys.setFileTime(paths[2], Sys.time() + 120)
  result <- build_threshold_index(ws$h5_folder, ws$index_path, reader = counter$reader)

  expect_identical(counter$paths(), basename(paths[[2]]))
  expect_identical(result$n_read, 1L)
  expect_identical(result$n_skipped, 2L)

  files <- thr_test_files_table(ws$index_path)
  expect_identical(files$file_id, ids_before)
  expect_identical(files$n_rois, c(4L, 2L, 4L))
  expect_identical(files$mtime[2], as.numeric(file.mtime(paths[2])))
  rois <- thr_test_query(ws$index_path,
                         "SELECT roi_number, class_idx, score FROM rois
                          WHERE file_id = ? ORDER BY roi_number",
                         params = list(files$file_id[2]))
  expect_identical(rois$roi_number, c(2L, 7L))
  expect_identical(rois$class_idx, c(4L, 4L))
  expect_identical(rois$score, c(0.25, 0.75))
})

test_that("a file whose size changed is re-read even with the same mtime", {
  ws <- thr_test_workspace()
  on.exit(unlink(ws$root, recursive = TRUE), add = TRUE)
  path <- thr_test_write_fake(ws$h5_folder, thr_test_samples[1])
  build_threshold_index(ws$h5_folder, ws$index_path, reader = thr_test_fake_reader())
  mtime <- file.mtime(path)

  saveRDS(thr_test_summary(roi_numbers = 1:40, class_idx = rep(1:5, 8),
                           top_score = seq(0.1, 0.9, length.out = 40)), path)
  Sys.setFileTime(path, mtime)
  result <- build_threshold_index(ws$h5_folder, ws$index_path,
                                  reader = thr_test_fake_reader())

  expect_identical(result$n_read, 1L)
  expect_identical(thr_test_files_table(ws$index_path)$n_rois, 40L)
})

test_that("a deleted file becomes missing and loses its ROI rows", {
  ws <- thr_test_workspace()
  on.exit(unlink(ws$root, recursive = TRUE), add = TRUE)
  paths <- vapply(thr_test_samples[1:2], function(s) {
    thr_test_write_fake(ws$h5_folder, s)
  }, character(1))
  build_threshold_index(ws$h5_folder, ws$index_path, reader = thr_test_fake_reader())
  kept_summary <- readRDS(paths[2])

  unlink(paths[1])
  counter <- thr_test_counting_reader()
  result <- build_threshold_index(ws$h5_folder, ws$index_path, reader = counter$reader)

  expect_length(counter$paths(), 0)
  expect_identical(result$n_scanned, 1L)
  expect_identical(result$n_ok, 1L)
  files <- thr_test_files_table(ws$index_path)
  expect_identical(files$status, c("missing", "ok"))
  expect_identical(result$failures$status, "missing")
  expect_identical(basename(result$failures$h5_path), basename(paths[[1]]))
  expect_match(result$failures$message, "no longer")

  rois <- thr_test_query(ws$index_path, "SELECT DISTINCT file_id FROM rois")
  expect_identical(rois$file_id, files$file_id[2])
  expect_identical(thr_index_models(ws$index_path)$n_files, 1L)
  expect_identical(thr_index_models(ws$index_path)$n_rois,
                   length(kept_summary$roi_numbers))

  # A third build has nothing more to do about the missing file
  third <- build_threshold_index(ws$h5_folder, ws$index_path, reader = counter$reader)
  expect_identical(third$n_read, 0L)
  expect_identical(third$failures$status, "missing")
})

test_that("a missing file that comes back is indexed again", {
  ws <- thr_test_workspace()
  on.exit(unlink(ws$root, recursive = TRUE), add = TRUE)
  path <- thr_test_write_fake(ws$h5_folder, thr_test_samples[1])
  build_threshold_index(ws$h5_folder, ws$index_path, reader = thr_test_fake_reader())
  parked <- file.path(ws$root, "parked.rds")
  file.copy(path, parked, copy.date = TRUE)
  unlink(path)
  build_threshold_index(ws$h5_folder, ws$index_path, reader = thr_test_fake_reader())
  expect_identical(thr_test_files_table(ws$index_path)$status, "missing")

  # Restored with the very same size and mtime: still has to be re-read
  file.copy(parked, path, copy.date = TRUE)
  result <- build_threshold_index(ws$h5_folder, ws$index_path,
                                  reader = thr_test_fake_reader())

  expect_identical(result$n_read, 1L)
  files <- thr_test_files_table(ws$index_path)
  expect_identical(files$status, "ok")
  expect_true(is.na(files$message))
  expect_equal(thr_test_query(ws$index_path, "SELECT COUNT(*) AS n FROM rois")$n, 4)
})

test_that("files indexed from another folder stay while they are on disk", {
  ws <- thr_test_workspace()
  on.exit(unlink(ws$root, recursive = TRUE), add = TRUE)
  folder_2023 <- file.path(ws$h5_folder, "2023")
  folder_2024 <- file.path(ws$h5_folder, "2024")
  thr_test_write_fake(folder_2023, thr_test_samples[1])
  thr_test_write_fake(folder_2024, thr_test_samples[2])

  build_threshold_index(folder_2023, ws$index_path, reader = thr_test_fake_reader())
  result <- build_threshold_index(folder_2024, ws$index_path,
                                  reader = thr_test_fake_reader())

  expect_identical(result$n_scanned, 1L)
  expect_identical(result$n_ok, 2L)
  expect_identical(thr_test_files_table(ws$index_path)$status, c("ok", "ok"))
})

# --- build: files that are not indexed ---------------------------------------

test_that("a legacy file is recorded but never indexed", {
  ws <- thr_test_workspace()
  on.exit(unlink(ws$root, recursive = TRUE), add = TRUE)
  thr_test_write_fake(ws$h5_folder, thr_test_samples[1])
  thr_test_write_fake(ws$h5_folder, thr_test_samples[2],
                      thr_test_summary(transform = NULL, checkpoint_sha256 = NULL,
                                       classifier_name = "old_model"))
  counter <- thr_test_counting_reader()
  result <- build_threshold_index(ws$h5_folder, ws$index_path, reader = counter$reader)

  files <- thr_test_files_table(ws$index_path)
  expect_identical(files$status, c("ok", "legacy"))
  expect_true(is.na(files$model_id[2]))
  expect_match(files$message[2], "transform")
  expect_identical(result$n_ok, 1L)
  expect_identical(result$failures$status, "legacy")

  expect_equal(thr_test_query(ws$index_path, "SELECT COUNT(*) AS n FROM models")$n, 1)
  rois <- thr_test_query(ws$index_path, "SELECT DISTINCT file_id FROM rois")
  expect_identical(rois$file_id, files$file_id[1])
  expect_equal(nrow(thr_index_models(ws$index_path)), 1)

  # Unchanged, so not read again
  counter$reset()
  build_threshold_index(ws$h5_folder, ws$index_path, reader = counter$reader)
  expect_length(counter$paths(), 0)
})

test_that("a failing reader does not stop the build", {
  ws <- thr_test_workspace()
  on.exit(unlink(ws$root, recursive = TRUE), add = TRUE)
  for (s in thr_test_samples[1:5]) thr_test_write_fake(ws$h5_folder, s)
  bad <- file.path(ws$h5_folder, paste0(thr_test_samples[c(2, 4)], "_class.h5"))
  for (path in bad) writeLines("truncated", path)

  result <- build_threshold_index(ws$h5_folder, ws$index_path,
                                  reader = thr_test_fake_reader(), batch_size = 2)

  expect_identical(result$n_scanned, 5L)
  expect_identical(result$n_read, 5L)
  expect_identical(result$n_ok, 3L)
  expect_identical(thr_test_files_table(ws$index_path)$status,
                   c("ok", "failed", "ok", "failed", "ok"))
  expect_identical(result$failures$status, c("failed", "failed"))
  expect_identical(basename(result$failures$h5_path), basename(bad))
  expect_match(result$failures$message, "fake reader could not read")
  expect_identical(thr_index_failures(ws$index_path), result$failures)
  expect_equal(thr_test_query(ws$index_path,
                              "SELECT COUNT(DISTINCT file_id) AS n FROM rois")$n, 3)
})

test_that("a failed file is retried only once it has changed", {
  ws <- thr_test_workspace()
  on.exit(unlink(ws$root, recursive = TRUE), add = TRUE)
  path <- file.path(ws$h5_folder, paste0(thr_test_samples[1], "_class.h5"))
  writeLines("truncated", path)
  counter <- thr_test_counting_reader()

  build_threshold_index(ws$h5_folder, ws$index_path, reader = counter$reader)
  build_threshold_index(ws$h5_folder, ws$index_path, reader = counter$reader)
  expect_length(counter$paths(), 1)

  # The pipeline finishes writing the file
  saveRDS(thr_test_summary(), path)
  Sys.setFileTime(path, Sys.time() + 120)
  result <- build_threshold_index(ws$h5_folder, ws$index_path, reader = counter$reader)

  expect_length(counter$paths(), 2)
  files <- thr_test_files_table(ws$index_path)
  expect_identical(files$status, "ok")
  expect_true(is.na(files$message))
  expect_equal(nrow(result$failures), 0)
})

test_that("a file whose name is not a sample name fails without being read", {
  ws <- thr_test_workspace()
  on.exit(unlink(ws$root, recursive = TRUE), add = TRUE)
  thr_test_write_fake(ws$h5_folder, thr_test_samples[1])
  thr_test_write_fake(ws$h5_folder, "merged_summer_2024")
  counter <- thr_test_counting_reader()

  result <- build_threshold_index(ws$h5_folder, ws$index_path, reader = counter$reader)

  expect_identical(counter$paths(), paste0(thr_test_samples[1], "_class.h5"))
  expect_identical(result$failures$status, "failed")
  expect_match(result$failures$message, "sample name")
  expect_match(result$failures$message, "merged_summer_2024", fixed = TRUE)
  expect_identical(result$n_ok, 1L)
})

test_that("a summary the index cannot store fails that file only", {
  bad_summaries <- list(
    "missing" = thr_test_summary(top_score = c(0.9, NaN, 0.5, 0.2)),
    "between 0 and 1" = thr_test_summary(top_score = c(0.9, 7.5, 0.5, 0.2)),
    "more than once" = thr_test_summary(roi_numbers = c(1, 2, 2, 3)),
    "class_idx" = thr_test_summary(class_idx = c(1, 2, 6, 1)),
    "same length" = thr_test_summary(top_score = c(0.9, 0.5)),
    "class_labels" = thr_test_summary(class_labels = c("a", "b", "b", "c", "d")),
    "thresholds" = thr_test_summary(thresholds = c(0.5, 0.5)),
    "roi_numbers" = thr_test_summary(roi_numbers = c(1, 2, NA, 4)),
    "list" = "not a summary"
  )

  for (expected_message in names(bad_summaries)) {
    ws <- thr_test_workspace()
    thr_test_write_fake(ws$h5_folder, thr_test_samples[1])
    thr_test_write_fake(ws$h5_folder, thr_test_samples[2],
                        bad_summaries[[expected_message]])
    thr_test_write_fake(ws$h5_folder, thr_test_samples[3])

    result <- build_threshold_index(ws$h5_folder, ws$index_path,
                                    reader = thr_test_fake_reader())

    expect_identical(thr_test_files_table(ws$index_path)$status,
                     c("ok", "failed", "ok"), info = expected_message)
    expect_match(result$failures$message, expected_message, fixed = TRUE)
    expect_equal(thr_test_query(ws$index_path, "SELECT COUNT(*) AS n FROM rois")$n, 8,
                 info = expected_message)
    unlink(ws$root, recursive = TRUE)
  }
})

test_that("a tiny floating-point overshoot above 1 is not a bad score", {
  ws <- thr_test_workspace()
  on.exit(unlink(ws$root, recursive = TRUE), add = TRUE)
  overshoot <- 1 + 2 * .Machine$double.eps
  thr_test_write_fake(ws$h5_folder, thr_test_samples[1],
                      thr_test_summary(top_score = c(overshoot, 0.4, 0.999, 0)))
  result <- build_threshold_index(ws$h5_folder, ws$index_path,
                                  reader = thr_test_fake_reader())

  expect_identical(result$n_ok, 1L)
  expect_identical(
    thr_test_query(ws$index_path, "SELECT score FROM rois ORDER BY roi_number")$score,
    c(overshoot, 0.4, 0.999, 0)
  )
})
