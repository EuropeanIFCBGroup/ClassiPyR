# Shared test helpers for the verdict database, the sampling frame, the draws
# and the verdicts of the threshold review app.
#
# Index databases are built with build_threshold_index() and the fake reader of
# helper-threshold-h5.R, from stand-in files that reproduce expected.json. They
# carry the real checkpoint hash and transform, so the model ids are the real
# ones and none of this needs hdf5r.

# A fresh class-score folder plus index and verdict database locations.
# The caller removes `root`.
thr_test_db_space <- function() {
  root <- tempfile("thr_db_")
  h5_folder <- file.path(root, "h5")
  review <- file.path(root, "review")
  dir.create(h5_folder, recursive = TRUE)
  dir.create(review, recursive = TRUE)
  list(
    root = root,
    h5_folder = normalizePath(h5_folder, winslash = "/"),
    index_path = file.path(review, "threshold_index.sqlite"),
    db_path = file.path(review, "threshold_verdicts.sqlite")
  )
}

# Sample names of the model A fixtures, in the order used by the tests
thr_test_a_samples <- c(
  sprintf("D202401%02dT000000_IFCB134", 1:5), "D20220522T000439_IFCB134"
)

# Entries of expected.json for one model folder ("model_a" or "model_b")
thr_test_expected_files <- function(model_dir = "model_a") {
  files <- thr_test_expected()$files
  files[startsWith(names(files), paste0(model_dir, "/"))]
}

# Write stand-in class-score files reproducing expected.json under h5_folder,
# in the same model_a/ and model_b/ subfolders as the real fixtures.
thr_test_write_expected_fakes <- function(h5_folder) {
  expected <- thr_test_expected()
  for (relative in names(expected$files)) {
    entry <- expected$files[[relative]]
    thresholds <- as.numeric(entry$thresholds)
    thresholds[is.na(thresholds)] <- NaN
    summary <- thr_test_summary(
      roi_numbers = entry$roi_numbers,
      class_idx = entry$class_idx,
      top_score = entry$top_score,
      class_labels = expected$class_labels,
      classifier_name = entry$classifier_name,
      thresholds = thresholds,
      checkpoint_sha256 = entry$checkpoint_sha256,
      transform = entry$transform
    )
    thr_test_write_fake(file.path(h5_folder, dirname(relative)),
                        entry$sample_name, summary)
  }
  invisible(h5_folder)
}

# Build the index of the expected.json stand-ins in a workspace
thr_test_fixture_index <- function(space) {
  thr_test_write_expected_fakes(space$h5_folder)
  build_threshold_index(space$h5_folder, space$index_path,
                        reader = thr_test_fake_reader())
  invisible(space)
}

# Population per (class, bin) counted by hand from expected.json entries,
# without thr_score_bin(): a score's bin is the number of edges at or below it.
thr_test_hand_strata <- function(files, class_labels, edges) {
  class_idx <- unlist(lapply(files, `[[`, "class_idx"))
  score <- unlist(lapply(files, `[[`, "top_score"))
  bin <- vapply(score, function(s) sum(edges <= s), integer(1))
  counts <- as.data.frame(table(class_idx = class_idx, bin = bin),
                          stringsAsFactors = FALSE)
  counts <- counts[counts$Freq > 0, , drop = FALSE]
  counts$class_idx <- as.integer(counts$class_idx)
  counts$bin <- as.integer(counts$bin)
  counts <- counts[order(counts$class_idx, counts$bin), , drop = FALSE]
  data.frame(
    class_name = class_labels[counts$class_idx],
    bin = counts$bin,
    n_population = as.integer(counts$Freq),
    stringsAsFactors = FALSE
  )
}

# Deterministic scores spread over (0, 1] without touching the RNG
thr_test_spread_scores <- function(n, offset = 0) {
  (((seq_len(n) + offset) * 7919) %% 10007 + 1) / 10007
}

# A larger synthetic model: `n_samples` stand-in files of `n_rois` ROIs each,
# classes assigned round-robin, scores spread over all bins. Returns the model
# id. Extra arguments go to thr_test_summary() (e.g. another checkpoint).
thr_test_synthetic_index <- function(space, n_samples = 6, n_rois = 200,
                                     subfolder = "synthetic", ...) {
  samples <- sprintf("D2023%02d01T000000_IFCB134", seq_len(n_samples))
  for (i in seq_len(n_samples)) {
    summary <- thr_test_summary(
      roi_numbers = seq_len(n_rois),
      class_idx = (seq_len(n_rois) + i) %% length(thr_test_labels) + 1L,
      top_score = thr_test_spread_scores(n_rois, offset = i * 1000),
      ...
    )
    thr_test_write_fake(file.path(space$h5_folder, subfolder), samples[i],
                        summary)
  }
  build_threshold_index(space$h5_folder, space$index_path,
                        reader = thr_test_fake_reader())
  thr_model_identity(summary)$model_id
}

# Freeze model A of the fixture index with defaults; returns the workspace
thr_test_frozen_fixture <- function(space = thr_test_db_space(), ...) {
  thr_test_fixture_index(space)
  thr_freeze_frame(space$db_path, space$index_path, thr_test_model_a_id,
                   h5_folder = space$h5_folder, ...)
  space
}

# All rows of one verdict database table
thr_test_table <- function(db_path, table) {
  thr_test_query(db_path, paste("SELECT * FROM", table))
}

# Run a statement against a SQLite file (thr_test_query is for SELECTs)
thr_test_execute <- function(db_path, sql, params = NULL) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, sql, params = params)
}

# Resolve a page as a browser that loaded every extracted image would report it
thr_test_confirm_page <- function(db_path, page, wrong = integer(),
                                  annotator = "tester",
                                  token = thr_new_page_token()) {
  page$extracted <- TRUE
  resolved <- thr_resolve_page_verdicts(page, loaded = page$draw_id,
                                        failed = integer(), wrong = wrong,
                                        token = token, expected_token = token)
  thr_save_verdicts(db_path, resolved, annotator, token)
  token
}
