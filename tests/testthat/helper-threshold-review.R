# Shared test helpers for the Review page of the threshold review app when it
# is run through its Shiny server (tests/testthat/test-threshold-app.R).
# Everything is prefixed thr_test_review_ because testthat sources every
# helper file into one environment.

# Folders for an app session: class-score files of the synthetic model (six
# samples, every class predicted 240 times) and raw files for every sample.
# The review folder is left for the app to create. The caller removes `root`.
thr_test_review_workspace <- function() {
  root <- tempfile("thr_review_app_")
  space <- list(root = root, h5 = file.path(root, "h5"),
                raw = file.path(root, "raw"),
                review = file.path(root, "review"))
  dir.create(space$h5, recursive = TRUE)
  dir.create(space$raw)
  space$model_id <- thr_test_app_write_synthetic(space$h5)
  thr_test_app_raw_layout(space$raw, thr_test_app_samples())
  space
}

# Take a session under shiny::testServer() from nothing to a frozen frame:
# apply the folders, build the index, pick the model, check and freeze.
thr_test_review_freeze <- function(session, space) {
  session$setInputs(thr_review_dir = space$review, thr_h5_folder = space$h5,
                    thr_roi_folder = space$raw, thr_annotator = "Tester")
  session$setInputs(thr_apply_settings = 1)
  session$setInputs(thr_build_index = 1)
  session$setInputs(thr_model = space$model_id)
  session$setInputs(thr_fallback_mode = "h5")
  session$setInputs(thr_preflight = 1)
  session$setInputs(thr_freeze = 1)
  session$setInputs(thr_freeze_confirm = 1)
  invisible(session)
}

# Every draw in the verdict database of a workspace
thr_test_review_draws <- function(space) {
  thr_test_query(thr_verdict_db_path(space$review),
                 "SELECT * FROM draws ORDER BY draw_id")
}
