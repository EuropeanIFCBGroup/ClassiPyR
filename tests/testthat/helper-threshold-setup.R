# Shared test helpers for the Setup side of the threshold review app: the
# index cache as other sessions and other folders see it, and the models a
# review folder knows. Everything is prefixed thr_test_ because testthat
# sources every helper file into one environment.

# Hold an exclusive lock on a SQLite file from another R process for
# `seconds`, the way a second session writing to it does. Returns TRUE once
# the lock is held, FALSE when the other process could not be started.
thr_test_lock_from_other_process <- function(db_path, seconds = 1.5) {
  marker <- tempfile("thr_lock_")
  script <- tempfile("thr_lock_", fileext = ".R")
  writeLines(c(
    "args <- commandArgs(trailingOnly = TRUE)",
    "con <- DBI::dbConnect(RSQLite::SQLite(), args[1])",
    "DBI::dbExecute(con, 'BEGIN EXCLUSIVE')",
    "invisible(file.create(args[2]))",
    "Sys.sleep(as.numeric(args[3]))",
    "DBI::dbExecute(con, 'COMMIT')",
    "DBI::dbDisconnect(con)"
  ), script)
  system2(
    file.path(R.home("bin"), "Rscript"),
    c(shQuote(script), shQuote(db_path), shQuote(marker), seconds),
    wait = FALSE, stdout = FALSE, stderr = FALSE,
    env = paste0("R_LIBS=", paste(.libPaths(), collapse = .Platform$path.sep))
  )
  deadline <- Sys.time() + 20
  while (!file.exists(marker) && Sys.time() < deadline) {
    Sys.sleep(0.05)
  }
  file.exists(marker)
}

# Write the same stand-in class-score files of one model into several
# subfolders of `root`, as a copied or re-run folder holds them. Returns the
# folder paths, named by subfolder.
thr_test_write_copies <- function(root, subfolders, sample_names,
                                  summary = thr_test_summary()) {
  folders <- stats::setNames(file.path(root, subfolders), subfolders)
  for (folder in folders) {
    for (sample_name in sample_names) {
      thr_test_write_fake(folder, sample_name, summary)
    }
  }
  folders
}

# A workspace with a synthetic model that is indexed and has raw files for
# every sample, but no frozen frame yet: what Check and Freeze start from.
# The caller removes space$root.
thr_test_open_space <- function(n_samples = 6, n_rois = 200) {
  space <- thr_test_db_space()
  space$roi_folder <- file.path(space$root, "raw")
  space$model_id <- thr_test_app_write_synthetic(space$h5_folder, n_samples,
                                                 n_rois)
  build_threshold_index(space$h5_folder, space$index_path,
                        reader = thr_test_fake_reader())
  thr_test_app_raw_layout(space$roi_folder, thr_test_app_samples(n_samples))
  space
}

# Take the class-score index away from a frozen review, in one of the two
# ways it happens: the index file is deleted ("deleted"), or the class-score
# folder is archived and the index rebuilt on what is left ("archived").
thr_test_lose_index <- function(space, how = c("deleted", "archived")) {
  how <- match.arg(how)
  if (how == "deleted") {
    unlink(space$index_path)
  } else {
    file.rename(space$h5_folder, paste0(space$h5_folder, "_archived"))
    dir.create(space$h5_folder)
    build_threshold_index(space$h5_folder, space$index_path,
                          reader = thr_test_fake_reader())
  }
  invisible(space)
}
