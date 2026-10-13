# Shared test helpers for the image extraction, the launcher and the Shiny app
# of the threshold review (section D). Everything is prefixed thr_test_app_
# because testthat sources every helper file into one environment.

# The one real raw bin in the test data: 8 ROIs, of which ROI 1 has no image
thr_test_app_raw_sample <- "D20220522T000439_IFCB134"

thr_test_app_raw_folder <- function() {
  normalizePath(testthat::test_path("test_data", "raw"), winslash = "/")
}

thr_test_app_raw_roi <- function() {
  file.path(thr_test_app_raw_folder(), "2022", "D20220522",
            paste0(thr_test_app_raw_sample, ".roi"))
}

# A real PNG (160 x 74 pixels) for fake extractors to hand out
thr_test_app_png <- function() {
  testthat::test_path("test_data", "png", thr_test_app_raw_sample,
                      paste0(thr_test_app_raw_sample, "_00002.png"))
}

# A page in the shape thr_next_page() returns, for one or more samples
thr_test_app_page <- function(sample_name = thr_test_app_raw_sample,
                              roi_number = 2:4, score = NULL) {
  n <- length(roi_number)
  if (is.null(score)) {
    score <- seq(0.99, 0.5, length.out = n)
  }
  data.frame(
    draw_id = seq_len(n),
    class_name = rep("Skeletonema_spp", n),
    bin = rep(1L, n),
    sample_name = rep_len(sample_name, n),
    roi_number = as.integer(roi_number),
    score = score,
    is_new = rep(TRUE, n),
    stringsAsFactors = FALSE
  )
}

# Stand-in for iRfcb::ifcb_extract_pngs() that copies a real PNG to every
# requested file name and records its calls. `fail` names samples whose calls
# throw; `refuse` lists ROI numbers that make a call throw when asked for
# together with others or alone, like an out-of-range ROI does in iRfcb.
thr_test_app_fake_extractor <- function(fail = character(), refuse = integer()) {
  log <- new.env(parent = emptyenv())
  log$calls <- list()
  extractor <- function(roi_file, out_folder, ROInumbers, verbose = FALSE, ...) {
    sample_name <- tools::file_path_sans_ext(basename(roi_file))
    log$calls <- c(log$calls, list(list(sample_name = sample_name,
                                        rois = ROInumbers)))
    if (sample_name %in% fail) {
      stop("fake extractor cannot read ", basename(roi_file), call. = FALSE)
    }
    if (any(ROInumbers %in% refuse)) {
      stop("ROI numbers out of range", call. = FALSE)
    }
    sample_dir <- file.path(out_folder, sample_name)
    dir.create(sample_dir, recursive = TRUE, showWarnings = FALSE)
    file.copy(thr_test_app_png(),
              file.path(sample_dir, sprintf("%s_%05d.png", sample_name,
                                            as.integer(ROInumbers))),
              overwrite = TRUE)
    invisible(NULL)
  }
  list(extractor = extractor, calls = function() log$calls,
       n_calls = function() length(log$calls))
}

# Create empty .roi files for samples in the standard IFCB folder layout
thr_test_app_raw_layout <- function(roi_folder, sample_names) {
  for (sample_name in sample_names) {
    folder <- file.path(roi_folder, substr(sample_name, 2, 5),
                        substr(sample_name, 1, 9))
    dir.create(folder, recursive = TRUE, showWarnings = FALSE)
    file.create(file.path(folder, paste0(sample_name, ".roi")))
  }
  invisible(roi_folder)
}

# A fixture index (model A: six samples, model B: one) with raw files for
# every model A sample. The caller removes space$root.
thr_test_app_setup_space <- function(raw_samples = thr_test_a_samples) {
  space <- thr_test_fixture_index(thr_test_db_space())
  space$roi_folder <- file.path(space$root, "raw")
  dir.create(space$roi_folder)
  thr_test_app_raw_layout(space$roi_folder, raw_samples)
  space
}

# Sample of model A whose class-score file stores other thresholds
thr_test_app_odd_sample <- "D20240103T000000_IFCB134"

thr_test_app_write_lines <- function(lines) {
  path <- tempfile("thr_exclude_", fileext = ".txt")
  writeLines(lines, path)
  path
}

# Sample names of the synthetic model written by thr_test_app_write_synthetic()
thr_test_app_samples <- function(n_samples = 6) {
  sprintf("D2023%02d01T000000_IFCB134", seq_len(n_samples))
}

# Write stand-in class-score files for a synthetic model without indexing
# them, so that an app under test can build the index itself. Classes are
# assigned round-robin and scores are spread over every bin; with the
# defaults each class is predicted 240 times. Returns the model id.
thr_test_app_write_synthetic <- function(h5_folder, n_samples = 6,
                                         n_rois = 200, ...) {
  samples <- thr_test_app_samples(n_samples)
  for (i in seq_len(n_samples)) {
    summary <- thr_test_summary(
      roi_numbers = seq_len(n_rois),
      class_idx = (seq_len(n_rois) + i) %% length(thr_test_labels) + 1L,
      top_score = thr_test_spread_scores(n_rois, offset = i * 1000),
      ...
    )
    thr_test_write_fake(h5_folder, samples[i], summary)
  }
  thr_model_identity(summary)$model_id
}

# A workspace with a synthetic model, raw files for every sample and a frozen
# frame: what the review, results and export helpers start from.
thr_test_app_frozen_space <- function(n_samples = 6, n_rois = 200, ...) {
  space <- thr_test_db_space()
  space$roi_folder <- file.path(space$root, "raw")
  space$image_folder <- file.path(space$root, "images")
  dir.create(space$image_folder)
  space$model_id <- thr_test_app_write_synthetic(space$h5_folder, n_samples,
                                                 n_rois)
  build_threshold_index(space$h5_folder, space$index_path,
                        reader = thr_test_fake_reader())
  thr_test_app_raw_layout(space$roi_folder, thr_test_app_samples(n_samples))
  thr_freeze_frame(space$db_path, space$index_path, space$model_id,
                   h5_folder = space$h5_folder, roi_folder = space$roi_folder,
                   ...)
  space
}

# Confirm every draw of a class in one page, marking those scoring below
# `wrong_below` as wrong. Returns the page token.
thr_test_app_review_class <- function(space, class_name, wrong_below = 0.5) {
  page <- thr_next_page(space$db_path, space$index_path, space$model_id,
                        class_name, page_size = 1000)
  thr_test_confirm_page(space$db_path, page,
                        wrong = page$draw_id[page$score < wrong_below])
}

# Evaluate `code` with the ClassiPyR config dir pointed at tempdir(), the way
# get_config_dir() does under R CMD check, so no test touches the real
# settings of the person running it.
thr_test_app_with_config <- function(code) {
  old <- Sys.getenv("_R_CHECK_PACKAGE_NAME_", unset = NA)
  Sys.setenv("_R_CHECK_PACKAGE_NAME_" = "ClassiPyR")
  on.exit({
    if (is.na(old)) {
      Sys.unsetenv("_R_CHECK_PACKAGE_NAME_")
    } else {
      Sys.setenv("_R_CHECK_PACKAGE_NAME_" = old)
    }
  }, add = TRUE)
  unlink(file.path(get_config_dir(), "threshold_settings.json"))
  force(code)
}

# Directory of the threshold app, or "" when it is not installed
thr_test_app_dir <- function() {
  system.file("threshold-app", package = "ClassiPyR")
}

# Source the app's server.R the way tests/testthat/test-app.R does for the
# main app: into a fresh environment below the global one.
thr_test_app_source_server <- function() {
  app_env <- new.env(parent = globalenv())
  source(file.path(thr_test_app_dir(), "server.R"), local = app_env)
  app_env
}

# The text of a rendered shiny tag or tag list
thr_test_app_html <- function(tag) {
  paste(as.character(tag), collapse = "\n")
}

# What a browser sends on "Confirm page"
thr_test_app_browser <- function(token, loaded, wrong = integer(),
                                 failed = integer()) {
  list(token = token, loaded = as.list(loaded), failed = as.list(failed),
       wrong = as.list(wrong), nonce = stats::runif(1))
}
