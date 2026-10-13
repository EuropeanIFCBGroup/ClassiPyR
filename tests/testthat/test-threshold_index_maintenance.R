# Tests for keeping the class-score index cache consistent: duplicates,
# batching and resuming, schema changes, argument checks and the queries
# the app reads the index through
#
# Like test-threshold_index.R, most tests use stand-in class-score files and
# an injected reader, so they run without hdf5r.

library(testthat)

# --- build: duplicates -------------------------------------------------------

test_that("a second file for the same model and sample is a duplicate", {
  ws <- thr_test_workspace()
  on.exit(unlink(ws$root, recursive = TRUE), add = TRUE)
  first <- thr_test_write_fake(file.path(ws$h5_folder, "run1"), thr_test_samples[1])
  second <- thr_test_write_fake(
    file.path(ws$h5_folder, "run2"), thr_test_samples[1],
    thr_test_summary(roi_numbers = 1:2, class_idx = 1:2, top_score = c(0.5, 0.6))
  )
  result <- build_threshold_index(ws$h5_folder, ws$index_path,
                                  reader = thr_test_fake_reader())

  files <- thr_test_files_table(ws$index_path)
  expect_identical(files$status, c("ok", "duplicate"))
  expect_identical(basename(dirname(files$h5_path)), c("run1", "run2"))
  expect_match(files$message[2], "run1", fixed = TRUE)
  expect_identical(result$n_ok, 1L)
  expect_identical(result$failures$status, "duplicate")

  rois <- thr_test_query(ws$index_path, "SELECT file_id FROM rois")
  expect_identical(rois$file_id, rep(files$file_id[1], 4))
  expect_identical(thr_index_models(ws$index_path)$n_files, 1L)

  # Stable: nothing is re-read and the roles do not flip
  counter <- thr_test_counting_reader()
  build_threshold_index(ws$h5_folder, ws$index_path, reader = counter$reader)
  expect_length(counter$paths(), 0)
  expect_identical(thr_test_files_table(ws$index_path)$status, c("ok", "duplicate"))
  expect_true(file.exists(first) && file.exists(second))
})

test_that("duplicates split across batches are still resolved by path order", {
  ws <- thr_test_workspace()
  on.exit(unlink(ws$root, recursive = TRUE), add = TRUE)
  for (run in c("run1", "run2", "run3")) {
    thr_test_write_fake(file.path(ws$h5_folder, run), thr_test_samples[1])
  }
  build_threshold_index(ws$h5_folder, ws$index_path,
                        reader = thr_test_fake_reader(), batch_size = 1)

  expect_identical(thr_test_files_table(ws$index_path)$status,
                   c("ok", "duplicate", "duplicate"))
  expect_equal(thr_test_query(ws$index_path, "SELECT COUNT(*) AS n FROM rois")$n, 4)
})

test_that("a later file that sorts first takes over from the indexed one", {
  ws <- thr_test_workspace()
  on.exit(unlink(ws$root, recursive = TRUE), add = TRUE)
  thr_test_write_fake(file.path(ws$h5_folder, "run2"), thr_test_samples[1])
  build_threshold_index(ws$h5_folder, ws$index_path, reader = thr_test_fake_reader())

  thr_test_write_fake(
    file.path(ws$h5_folder, "run1"), thr_test_samples[1],
    thr_test_summary(roi_numbers = 1:2, class_idx = 1:2, top_score = c(0.5, 0.6))
  )
  build_threshold_index(ws$h5_folder, ws$index_path, reader = thr_test_fake_reader())

  files <- thr_test_files_table(ws$index_path)
  expect_identical(basename(dirname(files$h5_path)), c("run1", "run2"))
  expect_identical(files$status, c("ok", "duplicate"))
  rois <- thr_test_query(ws$index_path, "SELECT file_id, roi_number FROM rois")
  expect_identical(rois$file_id, rep(files$file_id[1], 2))
})

test_that("a duplicate is promoted when the kept file disappears", {
  ws <- thr_test_workspace()
  on.exit(unlink(ws$root, recursive = TRUE), add = TRUE)
  first <- thr_test_write_fake(file.path(ws$h5_folder, "run1"), thr_test_samples[1])
  thr_test_write_fake(
    file.path(ws$h5_folder, "run2"), thr_test_samples[1],
    thr_test_summary(roi_numbers = 1:2, class_idx = 1:2, top_score = c(0.5, 0.6))
  )
  thr_test_write_fake(file.path(ws$h5_folder, "run3"), thr_test_samples[1])
  build_threshold_index(ws$h5_folder, ws$index_path, reader = thr_test_fake_reader())

  unlink(first)
  counter <- thr_test_counting_reader()
  result <- build_threshold_index(ws$h5_folder, ws$index_path, reader = counter$reader)

  files <- thr_test_files_table(ws$index_path)
  expect_identical(files$status, c("missing", "ok", "duplicate"))
  rois <- thr_test_query(ws$index_path, "SELECT file_id, roi_number FROM rois")
  expect_identical(rois$file_id, rep(files$file_id[2], 2))
  # The unchanged duplicates had to be read again to get their ROI rows
  expect_length(counter$paths(), 2)
  expect_identical(result$n_read, 2L)
  expect_identical(result$n_skipped, 0L)
  expect_identical(result$n_ok, 1L)
})

test_that("at most one ok file exists per model and sample after any build", {
  ws <- thr_test_workspace()
  on.exit(unlink(ws$root, recursive = TRUE), add = TRUE)
  for (run in c("run3", "run1", "run2")) {
    thr_test_write_fake(file.path(ws$h5_folder, run), thr_test_samples[1])
    build_threshold_index(ws$h5_folder, ws$index_path,
                          reader = thr_test_fake_reader())
    per_sample <- thr_test_query(ws$index_path, "
      SELECT model_id, sample_name, COUNT(*) AS n FROM files
      WHERE status = 'ok' GROUP BY model_id, sample_name")
    expect_identical(per_sample$n, 1L, info = run)
  }
  files <- thr_test_files_table(ws$index_path)
  expect_identical(files$status, c("ok", "duplicate", "duplicate"))
})

# --- build: duplicates are a matter of one folder ----------------------------

test_that("copies in a folder indexed earlier do not make a folder's files duplicates", {
  ws <- thr_test_workspace()
  on.exit(unlink(ws$root, recursive = TRUE), add = TRUE)
  folders <- thr_test_write_copies(ws$h5_folder, c("a_old", "b_new"),
                                   thr_test_samples[1:4])
  build_threshold_index(folders[["a_old"]], ws$index_path,
                        reader = thr_test_fake_reader())
  result <- build_threshold_index(folders[["b_new"]], ws$index_path,
                                  reader = thr_test_fake_reader())

  # A frame is frozen on one folder, so the files of another do not compete
  files <- thr_test_files_table(ws$index_path)
  expect_identical(files$status, rep("ok", 8))
  expect_identical(result$n_ok, 8L)
  expect_equal(nrow(result$failures), 0)
  expect_equal(thr_test_query(ws$index_path, "SELECT COUNT(*) AS n FROM rois")$n, 32)

  # A sample scored in two folders is still one sample of the model
  model <- thr_index_models(ws$index_path)
  expect_identical(model$n_files, 4L)
  expect_identical(model$n_rois, 16L)
  expect_identical(
    thr_frame_sample_names(ws$index_path, model$model_id, folders[["b_new"]]),
    thr_test_samples[1:4]
  )
})

test_that("building a folder that sorts first leaves the files of another alone", {
  ws <- thr_test_workspace()
  on.exit(unlink(ws$root, recursive = TRUE), add = TRUE)
  folders <- thr_test_write_copies(ws$h5_folder, c("backup", "scores"),
                                   thr_test_samples[1:3])
  build_threshold_index(folders[["scores"]], ws$index_path,
                        reader = thr_test_fake_reader())
  before <- thr_test_query(ws$index_path, "SELECT * FROM rois ORDER BY 1, 2")

  build_threshold_index(folders[["backup"]], ws$index_path,
                        reader = thr_test_fake_reader())

  files <- thr_test_files_table(ws$index_path)
  expect_identical(files$status, rep("ok", 6))
  kept <- thr_test_query(ws$index_path, "
    SELECT r.* FROM rois r JOIN files f ON f.file_id = r.file_id
    WHERE f.h5_path LIKE '%/scores/%' ORDER BY 1, 2")
  expect_identical(kept, before)
})

test_that("files set aside for a copy in another folder are used after a rebuild", {
  ws <- thr_test_workspace()
  on.exit(unlink(ws$root, recursive = TRUE), add = TRUE)
  folders <- thr_test_write_copies(ws$h5_folder, c("a_old", "b_new"),
                                   thr_test_samples[1:4])
  build_threshold_index(ws$h5_folder, ws$index_path, reader = thr_test_fake_reader())
  # Within the one folder that was built, the copies are duplicates. This is
  # also what an index holds that resolved duplicates across every folder.
  files <- thr_test_files_table(ws$index_path)
  expect_identical(files$status, rep(c("ok", "duplicate"), each = 4))

  counter <- thr_test_counting_reader()
  result <- build_threshold_index(folders[["b_new"]], ws$index_path,
                                  reader = counter$reader)

  expect_length(counter$paths(), 4)
  expect_identical(result$n_read, 4L)
  files <- thr_test_files_table(ws$index_path)
  expect_identical(files$status, rep("ok", 8))
  expect_true(all(is.na(files$message)))
  expect_equal(thr_test_query(ws$index_path, "SELECT COUNT(*) AS n FROM rois")$n, 32)

  # Stable from here on, whichever of the folders is built again
  for (folder in c(ws$h5_folder, folders[["a_old"]], folders[["b_new"]])) {
    counter$reset()
    build_threshold_index(folder, ws$index_path, reader = counter$reader)
    expect_length(counter$paths(), 0)
    expect_identical(thr_test_files_table(ws$index_path)$status, rep("ok", 8),
                     info = folder)
  }
})

test_that("a new file in an enclosing folder still takes over by path order", {
  ws <- thr_test_workspace()
  on.exit(unlink(ws$root, recursive = TRUE), add = TRUE)
  child <- file.path(ws$h5_folder, "run2")
  thr_test_write_fake(child, thr_test_samples[1])
  build_threshold_index(child, ws$index_path, reader = thr_test_fake_reader())
  thr_test_write_fake(file.path(ws$h5_folder, "run1"), thr_test_samples[1])

  # Both files are inside the folder being built, so they compete
  build_threshold_index(ws$h5_folder, ws$index_path, reader = thr_test_fake_reader())
  files <- thr_test_files_table(ws$index_path)
  expect_identical(basename(dirname(files$h5_path)), c("run1", "run2"))
  expect_identical(files$status, c("ok", "duplicate"))

  # Inside its own folder the second file has no competitor
  counter <- thr_test_counting_reader()
  build_threshold_index(child, ws$index_path, reader = counter$reader)
  expect_length(counter$paths(), 1)
  expect_identical(thr_test_files_table(ws$index_path)$status, c("ok", "ok"))
})

test_that("indexing a copy in a sibling folder leaves a frozen frame intact", {
  space <- thr_test_app_frozen_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  # "backup" sorts before "h5", the folder the frame was frozen on
  backup <- file.path(space$root, "backup")
  dir.create(backup)
  file.copy(list.files(space$h5_folder, full.names = TRUE), backup,
            copy.date = TRUE)

  result <- build_threshold_index(backup, space$index_path,
                                  reader = thr_test_fake_reader())

  expect_identical(result$n_read, 6L)
  expect_equal(nrow(result$failures), 0)
  status <- thr_frame_status(space$db_path, space$index_path, space$model_id)
  expect_identical(status$n_changed_files, 0L)
  expect_identical(status$n_new_files, 0L)
  # The review goes on: pages are still drawn from the frame's own files
  page <- thr_next_page(space$db_path, space$index_path, space$model_id,
                        thr_test_labels[1], page_size = 30, seed = 1)
  expect_equal(nrow(page), 30)
})

test_that("a frame set aside by a build of an enclosing folder comes back", {
  space <- thr_test_app_frozen_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  sibling <- file.path(space$root, "backup")
  dir.create(sibling)
  file.copy(list.files(space$h5_folder, full.names = TRUE), sibling,
            copy.date = TRUE)

  # Built as one folder, the copies in "backup" sort first and are kept, as
  # for any two files of a sample inside one folder
  build_threshold_index(space$root, space$index_path,
                        reader = thr_test_fake_reader())
  status <- thr_frame_status(space$db_path, space$index_path, space$model_id)
  expect_identical(status$n_changed_files, 6L)

  # Building the frame's own folder again makes its files usable as frozen
  build_threshold_index(space$h5_folder, space$index_path,
                        reader = thr_test_fake_reader())
  status <- thr_frame_status(space$db_path, space$index_path, space$model_id)
  expect_identical(status$n_changed_files, 0L)
  expect_identical(status$n_new_files, 0L)
  page <- thr_next_page(space$db_path, space$index_path, space$model_id,
                        thr_test_labels[1], page_size = 30, seed = 1)
  expect_equal(nrow(page), 30)
})

# --- build: batches, progress, resuming --------------------------------------

test_that("progress is reported per file and ends at the total", {
  ws <- thr_test_workspace()
  on.exit(unlink(ws$root, recursive = TRUE), add = TRUE)
  for (s in thr_test_samples[1:5]) thr_test_write_fake(ws$h5_folder, s)
  calls <- list()
  progress <- function(done, total) calls[[length(calls) + 1]] <<- c(done, total)

  build_threshold_index(ws$h5_folder, ws$index_path, reader = thr_test_fake_reader(),
                        progress = progress, batch_size = 2)

  done <- vapply(calls, `[`, numeric(1), 1)
  total <- vapply(calls, `[`, numeric(1), 2)
  expect_identical(done, as.numeric(1:5))
  expect_identical(total, rep(5, 5))
  expect_equal(thr_test_query(ws$index_path,
                              "SELECT COUNT(*) AS n FROM files WHERE status = 'ok'")$n, 5)

  # Nothing to read: progress has nothing to report
  calls <- list()
  build_threshold_index(ws$h5_folder, ws$index_path, reader = thr_test_fake_reader(),
                        progress = progress)
  expect_length(calls, 0)
})

test_that("an error in the progress callback does not stop the build", {
  ws <- thr_test_workspace()
  on.exit(unlink(ws$root, recursive = TRUE), add = TRUE)
  for (s in thr_test_samples[1:3]) thr_test_write_fake(ws$h5_folder, s)

  result <- build_threshold_index(
    ws$h5_folder, ws$index_path, reader = thr_test_fake_reader(),
    progress = function(done, total) stop("progress bar is gone")
  )
  expect_identical(result$n_ok, 3L)
})

test_that("an interrupted build keeps finished batches and resumes", {
  ws <- thr_test_workspace()
  on.exit(unlink(ws$root, recursive = TRUE), add = TRUE)
  for (s in thr_test_samples[1:5]) thr_test_write_fake(ws$h5_folder, s)

  # Not an error, so the per-file error handling must not swallow it: this is
  # what a user interrupt looks like to the build.
  fake <- thr_test_fake_reader()
  n_calls <- 0
  interrupting <- function(h5_path) {
    n_calls <<- n_calls + 1
    if (n_calls == 4) {
      stop(structure(class = c("thr_test_interrupt", "condition"),
                     list(message = "interrupted", call = NULL)))
    }
    fake(h5_path)
  }
  outcome <- tryCatch(
    build_threshold_index(ws$h5_folder, ws$index_path, reader = interrupting,
                          batch_size = 2),
    thr_test_interrupt = function(cond) "interrupted"
  )
  expect_identical(outcome, "interrupted")

  # Batch 1 (two files) was committed; batch 2 was not
  files <- thr_test_files_table(ws$index_path)
  expect_identical(files$sample_name, thr_test_samples[1:2])
  expect_equal(thr_test_query(ws$index_path,
                              "SELECT COUNT(DISTINCT file_id) AS n FROM rois")$n, 2)

  counter <- thr_test_counting_reader()
  result <- build_threshold_index(ws$h5_folder, ws$index_path, reader = counter$reader,
                                  batch_size = 2)
  expect_identical(counter$paths(), paste0(thr_test_samples[3:5], "_class.h5"))
  expect_identical(result$n_read, 3L)
  expect_identical(result$n_skipped, 2L)
  expect_identical(result$n_ok, 5L)
})

test_that("many files over several batches are all indexed", {
  ws <- thr_test_workspace()
  on.exit(unlink(ws$root, recursive = TRUE), add = TRUE)
  samples <- sprintf("D2024%02d%02dT000000_IFCB134", rep(1:3, each = 20), rep(1:20, 3))
  n_rois <- 300
  for (s in samples) {
    thr_test_write_fake(file.path(ws$h5_folder, substr(s, 1, 7)), s,
                        thr_test_summary(roi_numbers = seq_len(n_rois),
                                         class_idx = rep_len(1:5, n_rois),
                                         top_score = seq(0, 1, length.out = n_rois)))
  }
  result <- build_threshold_index(ws$h5_folder, ws$index_path,
                                  reader = thr_test_fake_reader(), batch_size = 25)

  expect_identical(result$n_ok, 60L)
  expect_equal(thr_test_query(ws$index_path, "SELECT COUNT(*) AS n FROM rois")$n,
               60 * n_rois)
  models <- thr_index_models(ws$index_path)
  expect_identical(models$n_files, 60L)
  expect_identical(models$n_rois, as.integer(60 * n_rois))
})

# --- build: another session using the index ----------------------------------

test_that("a build waits for another process that holds the index", {
  skip_on_cran()
  ws <- thr_test_workspace()
  on.exit(unlink(ws$root, recursive = TRUE), add = TRUE)
  thr_test_write_fake(ws$h5_folder, thr_test_samples[1])
  build_threshold_index(ws$h5_folder, ws$index_path, reader = thr_test_fake_reader())
  thr_test_write_fake(ws$h5_folder, thr_test_samples[2])

  # The app, or a scripted build, writing a batch while this build starts
  locked <- thr_test_lock_from_other_process(ws$index_path, seconds = 1.5)
  skip_if_not(locked, "Could not start a second R process")
  result <- build_threshold_index(ws$h5_folder, ws$index_path,
                                  reader = thr_test_fake_reader())

  expect_identical(result$n_read, 1L)
  expect_identical(result$n_ok, 2L)
})

# --- build: schema version ---------------------------------------------------

test_that("an index with another schema version is rebuilt from scratch", {
  ws <- thr_test_workspace()
  on.exit(unlink(ws$root, recursive = TRUE), add = TRUE)
  for (s in thr_test_samples[1:3]) thr_test_write_fake(ws$h5_folder, s)
  build_threshold_index(ws$h5_folder, ws$index_path, reader = thr_test_fake_reader())

  con <- DBI::dbConnect(RSQLite::SQLite(), ws$index_path)
  DBI::dbExecute(con, "UPDATE meta SET value = 'older' WHERE key = 'schema_version'")
  # A column an older layout might have had, to prove the table is recreated
  DBI::dbExecute(con, "ALTER TABLE files ADD COLUMN obsolete TEXT")
  DBI::dbDisconnect(con)

  counter <- thr_test_counting_reader()
  result <- build_threshold_index(ws$h5_folder, ws$index_path, reader = counter$reader)

  expect_length(counter$paths(), 3)
  expect_identical(result$n_read, 3L)
  expect_identical(result$n_ok, 3L)
  expect_false("obsolete" %in% names(thr_test_files_table(ws$index_path)))
  expect_identical(
    thr_test_query(ws$index_path,
                   "SELECT value FROM meta WHERE key = 'schema_version'")$value,
    THR_INDEX_SCHEMA_VERSION
  )
  expect_equal(thr_test_query(ws$index_path, "SELECT COUNT(*) AS n FROM rois")$n, 12)
})

test_that("an index that lost its schema version is rebuilt too", {
  ws <- thr_test_workspace()
  on.exit(unlink(ws$root, recursive = TRUE), add = TRUE)
  thr_test_write_fake(ws$h5_folder, thr_test_samples[1])
  build_threshold_index(ws$h5_folder, ws$index_path, reader = thr_test_fake_reader())
  con <- DBI::dbConnect(RSQLite::SQLite(), ws$index_path)
  DBI::dbExecute(con, "DELETE FROM meta")
  DBI::dbDisconnect(con)

  counter <- thr_test_counting_reader()
  build_threshold_index(ws$h5_folder, ws$index_path, reader = counter$reader)
  expect_length(counter$paths(), 1)
})

# --- build: arguments --------------------------------------------------------

test_that("build_threshold_index validates its arguments", {
  ws <- thr_test_workspace()
  on.exit(unlink(ws$root, recursive = TRUE), add = TRUE)
  reader <- thr_test_fake_reader()

  expect_error(build_threshold_index(file.path(ws$root, "nope"), ws$index_path,
                                     reader = reader), "not found")
  expect_error(build_threshold_index(ws$h5_folder, NULL, reader = reader), "index_path")
  expect_error(build_threshold_index(ws$h5_folder, c("a", "b"), reader = reader),
               "index_path")
  expect_error(build_threshold_index(ws$h5_folder, ws$index_path, reader = "reader"),
               "reader")
  expect_error(build_threshold_index(ws$h5_folder, ws$index_path, reader = reader,
                                     progress = "bar"), "progress")
  for (bad_size in list(0, -1, 2.5, NA, "ten", c(1, 2))) {
    expect_error(build_threshold_index(ws$h5_folder, ws$index_path, reader = reader,
                                       batch_size = bad_size), "batch_size")
  }
  # Nothing was created by the rejected calls
  expect_false(file.exists(ws$index_path))
})

test_that("an empty folder gives an empty index", {
  ws <- thr_test_workspace()
  on.exit(unlink(ws$root, recursive = TRUE), add = TRUE)

  result <- build_threshold_index(ws$h5_folder, ws$index_path,
                                  reader = thr_test_fake_reader())
  expect_identical(result$n_scanned, 0L)
  expect_identical(result$n_read, 0L)
  expect_identical(result$n_ok, 0L)
  expect_true(file.exists(ws$index_path))
  expect_equal(nrow(thr_index_models(ws$index_path)), 0)
})

# --- index queries -----------------------------------------------------------

test_that("index queries return typed empty results when there is no index", {
  index_path <- file.path(tempfile("thr_no_index_"), "threshold_index.sqlite")

  models <- thr_index_models(index_path)
  expect_equal(nrow(models), 0)
  expect_named(models, c("model_id", "classifier_name", "checkpoint_sha256",
                         "transform", "n_files", "n_rois", "n_threshold_sets"))
  expect_type(models$model_id, "character")
  expect_type(models$n_files, "integer")

  failures <- thr_index_failures(index_path)
  expect_equal(nrow(failures), 0)
  expect_named(failures, c("h5_path", "status", "message"))

  # Asking must not create the file as a side effect
  expect_false(file.exists(index_path))
})

test_that("index queries treat a database without index tables as empty", {
  other_db <- tempfile(fileext = ".sqlite")
  on.exit(unlink(other_db), add = TRUE)
  con <- DBI::dbConnect(RSQLite::SQLite(), other_db)
  DBI::dbExecute(con, "CREATE TABLE unrelated (x INTEGER)")
  DBI::dbDisconnect(con)

  expect_equal(nrow(thr_index_models(other_db)), 0)
  expect_equal(nrow(thr_index_failures(other_db)), 0)
  expect_error(thr_index_class_labels(other_db, "abc"), "build_threshold_index")
  expect_identical(thr_test_query(other_db, "SELECT name FROM sqlite_master")$name,
                   "unrelated")
})

test_that("index queries wait for a writer as long as the verdict database does", {
  ws <- thr_test_workspace()
  on.exit(unlink(ws$root, recursive = TRUE), add = TRUE)
  thr_test_write_fake(ws$h5_folder, thr_test_samples[1])
  build_threshold_index(ws$h5_folder, ws$index_path, reader = thr_test_fake_reader())

  timeout <- thr_with_index(ws$index_path, function(con) {
    DBI::dbGetQuery(con, "PRAGMA busy_timeout")[[1]]
  })

  expect_equal(timeout, THR_DB_BUSY_MS)
})

test_that("index queries reject a malformed index path", {
  expect_error(thr_index_models(NULL), "index_path")
  expect_error(thr_index_failures(c("a", "b")), "index_path")
  expect_error(thr_index_class_labels(NA_character_, "abc"), "index_path")
})

test_that("thr_index_class_labels reports an unknown model or missing index", {
  ws <- thr_test_workspace()
  on.exit(unlink(ws$root, recursive = TRUE), add = TRUE)
  expect_error(thr_index_class_labels(ws$index_path, "abc"), "build_threshold_index")

  thr_test_write_fake(ws$h5_folder, thr_test_samples[1])
  build_threshold_index(ws$h5_folder, ws$index_path, reader = thr_test_fake_reader())
  expect_error(thr_index_class_labels(ws$index_path, "no-such-model"), "no-such-model")
  expect_error(thr_index_class_labels(ws$index_path, c("a", "b")), "model_id")
})

test_that("thr_index_failures lists every file that is not ok, by path", {
  ws <- thr_test_workspace()
  on.exit(unlink(ws$root, recursive = TRUE), add = TRUE)
  thr_test_write_fake(file.path(ws$h5_folder, "run1"), thr_test_samples[1])
  thr_test_write_fake(file.path(ws$h5_folder, "run2"), thr_test_samples[1])
  thr_test_write_fake(ws$h5_folder, thr_test_samples[2],
                      thr_test_summary(transform = NULL))
  writeLines("truncated", file.path(ws$h5_folder,
                                    paste0(thr_test_samples[3], "_class.h5")))
  build_threshold_index(ws$h5_folder, ws$index_path, reader = thr_test_fake_reader())

  failures <- thr_index_failures(ws$index_path)
  expect_named(failures, c("h5_path", "status", "message"))
  expect_setequal(failures$status, c("legacy", "failed", "duplicate"))
  expect_false(is.unsorted(failures$h5_path))
  expect_false(anyNA(failures$message))
})

# --- build: the pipeline's own files -----------------------------------------

test_that("the fixture folder indexes into two models and a refused legacy file", {
  skip_if_not_installed("hdf5r")
  h5_folder <- thr_test_copy_fixtures()
  on.exit(unlink(h5_folder, recursive = TRUE), add = TRUE)
  # In a folder of its own: its sample name is also in model_a
  legacy_dir <- file.path(h5_folder, "legacy")
  dir.create(legacy_dir)
  file.copy(thr_test_legacy_h5(), legacy_dir)
  index_path <- file.path(h5_folder, "review", "threshold_index.sqlite")

  result <- build_threshold_index(h5_folder, index_path)

  expect_identical(result$n_scanned, 8L)
  expect_identical(result$n_read, 8L)
  expect_identical(result$n_ok, 7L)
  expect_identical(result$failures$status, "legacy")
  expect_identical(basename(dirname(result$failures$h5_path)), "legacy")

  models <- thr_index_models(index_path)
  models <- models[order(models$classifier_name), ]
  expect_identical(models$classifier_name, c("fixture_A", "fixture_B"))
  expect_identical(models$model_id, c(thr_test_model_a_id, thr_test_model_b_id))
  expect_identical(models$n_files, c(6L, 1L))
  expect_identical(models$n_rois, c(65L, 20L))
  expect_identical(models$n_threshold_sets, c(2L, 1L))
  expect_identical(models$transform, rep("dataset_squarepad", 2))
  expect_identical(thr_index_class_labels(index_path, thr_test_model_a_id),
                   thr_test_labels)
  expect_identical(thr_index_class_labels(index_path, thr_test_model_b_id),
                   thr_test_labels)

  # The legacy file's 109 labels never reach the index
  expect_equal(thr_test_query(index_path, "SELECT COUNT(*) AS n FROM models")$n, 2)
  expect_equal(thr_test_query(index_path, "SELECT COUNT(*) AS n FROM rois")$n, 85)

  # Second run reads nothing
  second <- build_threshold_index(h5_folder, index_path)
  expect_identical(second$n_read, 0L)
  expect_identical(second$n_skipped, 8L)
})

test_that("indexed ROIs equal what numpy computed, file by file", {
  skip_if_not_installed("hdf5r")
  expected <- thr_test_expected()
  # A copy, so files other tests add next to the fixtures cannot interfere
  h5_folder <- thr_test_copy_fixtures()
  on.exit(unlink(h5_folder, recursive = TRUE), add = TRUE)
  index_path <- file.path(h5_folder, "review", "threshold_index.sqlite")
  build_threshold_index(h5_folder, index_path)

  fixture_root <- normalizePath(h5_folder, winslash = "/")
  for (rel_path in names(expected$files)) {
    want <- expected$files[[rel_path]]
    file <- thr_test_query(index_path, "SELECT * FROM files WHERE h5_path = ?",
                           params = list(paste(fixture_root, rel_path, sep = "/")))
    expect_equal(nrow(file), 1, info = rel_path)
    expect_identical(file$status, "ok", info = rel_path)
    expect_identical(file$sample_name, want$sample_name, info = rel_path)
    expect_identical(thr_numbers_from_json(file$thresholds_json),
                     as.numeric(want$thresholds), info = rel_path)

    rois <- thr_test_query(index_path, "
      SELECT roi_number, class_idx, score FROM rois WHERE file_id = ?
      ORDER BY roi_number", params = list(file$file_id))
    by_roi <- order(want$roi_numbers)
    expect_identical(rois$roi_number, as.integer(want$roi_numbers)[by_roi],
                     info = rel_path)
    expect_identical(rois$class_idx, as.integer(want$class_idx)[by_roi], info = rel_path)
    expect_identical(rois$score, as.numeric(want$top_score)[by_roi], info = rel_path)
  }
})
