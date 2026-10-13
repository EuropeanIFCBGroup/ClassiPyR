# Shared test helpers for the threshold-review class-score reader and index.
#
# Everything here is prefixed thr_test_ because testthat sources every helper
# file into one environment.

# Class labels of the fixtures written by data-raw/make_threshold_fixtures.py.
# The slash, the three-digit suffix and the non-ASCII letter are deliberate.
thr_test_labels <- c(
  "Skeletonema_spp", "Snowella/Woronichinia", "Pauliella_taeniata_001",
  "Öresund_ciliate", "Debris"
)

# model_id of the two fixture checkpoints, computed outside R with Python's
# hashlib so the R implementation is checked against an independent one.
thr_test_model_a_id <- "35f6df0e31cb0efd8f7f65bb945f0495"
thr_test_model_b_id <- "ca6aa7cb1f0e9bf8db52cb3aa5315a87"

thr_test_fixture_dir <- function() {
  testthat::test_path("test_data", "threshold_review")
}

thr_test_legacy_h5 <- function() {
  testthat::test_path("test_data", "D20220522T000439_IFCB134_class.h5")
}

# expected.json: list(class_labels = <chr>, files = <named list per h5 file>)
thr_test_expected <- function() {
  jsonlite::fromJSON(file.path(thr_test_fixture_dir(), "expected.json"),
                     simplifyVector = TRUE)
}

# Copy the fixture tree (model_a/, model_b/) into a fresh temp folder so tests
# can touch, delete and add files without changing the tracked fixtures.
thr_test_copy_fixtures <- function(dest = tempfile("thr_fixtures_")) {
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  for (model_dir in c("model_a", "model_b")) {
    file.copy(file.path(thr_test_fixture_dir(), model_dir), dest,
              recursive = TRUE, copy.date = TRUE)
  }
  dest
}

# A summary in exactly the shape read_class_scores_summary() returns.
# transform = NULL gives a legacy file; checkpoint_sha256 = NULL gives a file
# whose checkpoint could not be hashed.
thr_test_summary <- function(roi_numbers = 1:4,
                             class_idx = c(1L, 2L, 3L, 1L),
                             top_score = c(0.95, 0.4, 0.999, 0.15),
                             class_labels = thr_test_labels,
                             classifier_name = "fake_A",
                             thresholds = c(0.3, 0.05, 0.9, NaN, 0.6),
                             checkpoint_sha256 = "sha-fake-a",
                             transform = "dataset_squarepad") {
  attrs <- list(ifcb_classify_version = "0.1.0", model_architecture = "resnet50")
  if (!is.null(transform)) attrs$transform <- transform
  if (!is.null(checkpoint_sha256)) attrs$checkpoint_sha256 <- checkpoint_sha256
  list(
    roi_numbers = as.integer(roi_numbers),
    class_idx = as.integer(class_idx),
    top_score = as.numeric(top_score),
    class_labels = class_labels,
    classifier_name = classifier_name,
    thresholds = as.numeric(thresholds),
    attrs = attrs
  )
}

# Write a stand-in class-score file: an RDS of a summary under the file name
# the scanner looks for. thr_test_fake_reader() reads it back, so index tests
# need neither hdf5r nor real HDF5 files.
thr_test_write_fake <- function(folder, sample_name, summary = thr_test_summary(),
                                suffix = "_class.h5") {
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(folder, paste0(sample_name, suffix))
  saveRDS(summary, path)
  path
}

# Stand-in for read_class_scores_summary(); errors on anything that is not an
# RDS file, like the real reader does on a corrupt HDF5 file.
thr_test_fake_reader <- function() {
  function(h5_path) {
    tryCatch(
      readRDS(h5_path),
      error = function(e) stop("fake reader could not read ", basename(h5_path),
                               call. = FALSE),
      warning = function(w) stop("fake reader could not read ", basename(h5_path),
                                 call. = FALSE)
    )
  }
}

# Wrap a reader so a test can see which files were read, in order.
# Returns list(reader = <function>, paths = <function returning chr>,
# reset = <function>).
thr_test_counting_reader <- function(reader = thr_test_fake_reader()) {
  seen <- new.env(parent = emptyenv())
  seen$paths <- character()
  list(
    reader = function(h5_path) {
      seen$paths <- c(seen$paths, basename(h5_path))
      reader(h5_path)
    },
    paths = function() seen$paths,
    reset = function() seen$paths <- character()
  )
}

# Run one query against an index (or any SQLite) file.
thr_test_query <- function(db_path, sql, params = NULL) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbGetQuery(con, sql, params = params)
}

# Write a small class-scores HDF5 file with hdf5r. `scores` is classes x ROIs,
# the layout hdf5r presents the pipeline's files in. Only for cases the real
# fixtures cannot cover (asymmetric square matrix, inconsistent dimensions).
thr_test_write_h5 <- function(path, scores, class_labels, roi_numbers,
                              classifier_name = "hdf5r_test",
                              thresholds = rep(NaN, length(class_labels)),
                              attrs = list(transform = "dataset_squarepad")) {
  h5 <- hdf5r::H5File$new(path, "w")
  on.exit(h5$close_all(), add = TRUE)
  h5[["output_scores"]] <- scores
  h5[["class_labels"]] <- class_labels
  h5[["roi_numbers"]] <- as.integer(roi_numbers)
  h5[["classifier_name"]] <- classifier_name
  h5[["thresholds"]] <- thresholds
  for (key in names(attrs)) {
    hdf5r::h5attr(h5, key) <- attrs[[key]]
  }
  path
}
