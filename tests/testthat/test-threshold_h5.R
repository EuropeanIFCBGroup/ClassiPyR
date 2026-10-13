# Tests for the class-score reader used by the threshold review app

library(testthat)

# --- thr_top1 ---------------------------------------------------------------

test_that("thr_top1 returns the winning class and its score per column", {
  scores <- cbind(c(0.1, 0.7, 0.2), c(0.6, 0.3, 0.1), c(0.2, 0.2, 0.6))
  top <- thr_top1(scores)

  expect_identical(top$class_idx, c(2L, 1L, 3L))
  expect_identical(top$top_score, c(0.7, 0.6, 0.6))
})

test_that("thr_top1 reads columns as ROIs even for a square matrix", {
  # Asymmetric on purpose: reading rows as ROIs gives a different answer
  scores <- rbind(c(0.9, 0.8, 0.7),
                  c(0.1, 0.1, 0.2),
                  c(0.0, 0.1, 0.1))
  top <- thr_top1(scores)

  expect_identical(top$class_idx, c(1L, 1L, 1L))
  expect_identical(top$top_score, c(0.9, 0.8, 0.7))
  expect_false(identical(thr_top1(t(scores))$class_idx, top$class_idx))
})

test_that("thr_top1 gives an exact tie to the first class, like numpy argmax", {
  scores <- cbind(c(0.4, 0.4, 0.2), c(0.2, 0.4, 0.4), c(1 / 3, 1 / 3, 1 / 3))
  expect_identical(thr_top1(scores)$class_idx, c(1L, 2L, 1L))
})

test_that("thr_top1 does not treat a near tie as a tie", {
  # A tolerance-based argmax (max.col's default) would pick class 1 here
  scores <- cbind(c(0.4, 0.4 + 1e-9, 0.1), c(0.5, 0.5 - 1e-12, 0))
  top <- thr_top1(scores)

  expect_identical(top$class_idx, c(2L, 1L))
  expect_identical(top$top_score, c(0.4 + 1e-9, 0.5))
})

test_that("thr_top1 handles one ROI, one class and no ROIs", {
  one_roi <- thr_top1(matrix(c(0.2, 0.5, 0.3), ncol = 1))
  expect_identical(one_roi$class_idx, 2L)
  expect_identical(one_roi$top_score, 0.5)

  one_class <- thr_top1(matrix(c(0.2, 0.5, 0.3), nrow = 1))
  expect_identical(one_class$class_idx, c(1L, 1L, 1L))
  expect_identical(one_class$top_score, c(0.2, 0.5, 0.3))

  none <- thr_top1(matrix(numeric(), nrow = 3, ncol = 0))
  expect_identical(none$class_idx, integer())
  expect_identical(none$top_score, numeric())
})

test_that("thr_top1 rejects input it cannot rank", {
  expect_error(thr_top1(c(0.2, 0.8)), "matrix")
  expect_error(thr_top1(matrix("a", 2, 2)), "matrix")
  expect_error(thr_top1(matrix(numeric(), nrow = 0, ncol = 3)), "no classes")
  expect_error(thr_top1(cbind(c(0.2, NaN), c(0.5, 0.5))), "missing")
})

# --- read_class_scores_summary: against the pipeline's own files -------------

test_that("the reader reproduces what numpy computed for every fixture", {
  skip_if_not_installed("hdf5r")
  expected <- thr_test_expected()
  expect_length(expected$files, 7)

  for (rel_path in names(expected$files)) {
    want <- expected$files[[rel_path]]
    got <- read_class_scores_summary(file.path(thr_test_fixture_dir(), rel_path))

    expect_identical(got$roi_numbers, as.integer(want$roi_numbers), info = rel_path)
    expect_identical(got$class_idx, as.integer(want$class_idx), info = rel_path)
    # Full precision: the score stored in the index decides the score bin
    expect_identical(got$top_score, as.numeric(want$top_score), info = rel_path)
    expect_identical(got$class_labels, expected$class_labels, info = rel_path)
    expect_identical(got$class_labels[got$class_idx], want$class_name_auto,
                     info = rel_path)
    expect_identical(got$classifier_name, want$classifier_name, info = rel_path)
    expect_identical(got$attrs$transform, want$transform, info = rel_path)
    expect_identical(got$attrs$checkpoint_sha256, want$checkpoint_sha256,
                     info = rel_path)
  }
})

test_that("the reader returns the documented shape", {
  skip_if_not_installed("hdf5r")
  got <- read_class_scores_summary(
    file.path(thr_test_fixture_dir(), "model_a", "D20240101T000000_IFCB134_class.h5")
  )

  expect_named(got, c("roi_numbers", "class_idx", "top_score", "class_labels",
                      "classifier_name", "thresholds", "attrs"))
  expect_type(got$roi_numbers, "integer")
  expect_type(got$class_idx, "integer")
  expect_type(got$top_score, "double")
  expect_type(got$class_labels, "character")
  expect_type(got$classifier_name, "character")
  expect_length(got$classifier_name, 1)
  expect_type(got$thresholds, "double")
  expect_type(got$attrs, "list")
  expect_true(all(vapply(got$attrs, is.character, logical(1))))
  expect_setequal(names(got$attrs),
                  c("ifcb_classify_version", "python_version", "torch_version",
                    "torchvision_version", "transform", "model_architecture",
                    "checkpoint_sha256"))
})

test_that("the reader keeps awkward class labels untouched", {
  skip_if_not_installed("hdf5r")
  got <- read_class_scores_summary(
    file.path(thr_test_fixture_dir(), "model_b", "D20240101T000000_IFCB134_class.h5")
  )

  expect_identical(got$class_labels, thr_test_labels)
  expect_identical(Encoding(got$class_labels[4]), "UTF-8")
})

test_that("the reader keeps NaN thresholds as NaN", {
  skip_if_not_installed("hdf5r")
  expected <- thr_test_expected()

  for (rel_path in names(expected$files)) {
    want <- as.numeric(expected$files[[rel_path]]$thresholds)
    got <- read_class_scores_summary(file.path(thr_test_fixture_dir(), rel_path))

    expect_identical(is.nan(got$thresholds), is.na(want), info = rel_path)
    expect_identical(got$thresholds[!is.na(want)], want[!is.na(want)], info = rel_path)
  }

  alt <- read_class_scores_summary(
    file.path(thr_test_fixture_dir(), "model_a", "D20240103T000000_IFCB134_class.h5")
  )
  expect_identical(alt$thresholds, rep(0.5, 5))
})

test_that("the reader resolves an exact tie the way numpy did", {
  skip_if_not_installed("hdf5r")
  h5_path <- file.path(thr_test_fixture_dir(), "model_a",
                       "D20240105T000000_IFCB134_class.h5")
  got <- read_class_scores_summary(h5_path)

  # ROI 1 scores 0.4 for both class 1 and class 2
  expect_identical(got$class_idx[1], 1L)
  expect_identical(got$top_score[1], 0.4)

  # The pipeline's own argmax label, stored in the file, is the reference
  h5 <- hdf5r::H5File$new(h5_path, "r")
  on.exit(h5$close_all(), add = TRUE)
  expect_identical(got$class_labels[got$class_idx], h5[["class_name_auto"]]$read())
})

test_that("the reader handles a single-ROI file", {
  skip_if_not_installed("hdf5r")
  got <- read_class_scores_summary(
    file.path(thr_test_fixture_dir(), "model_a", "D20240104T000000_IFCB134_class.h5")
  )

  expect_identical(got$roi_numbers, 7L)
  expect_identical(got$class_idx, 4L)
  expect_length(got$top_score, 1)
  expect_length(got$class_labels, 5)
})

test_that("the reader does not guess the orientation of a square matrix", {
  skip_if_not_installed("hdf5r")
  h5_path <- tempfile(fileext = ".h5")
  on.exit(unlink(h5_path), add = TRUE)
  # The 5 x 5 fixture has the same argmax and top score either way round, so
  # orientation needs a matrix whose transpose gives a different answer.
  scores <- rbind(c(0.9, 0.8, 0.7),
                  c(0.1, 0.1, 0.2),
                  c(0.0, 0.1, 0.1))
  thr_test_write_h5(h5_path, scores, c("a", "b", "c"), c(11, 12, 13))

  got <- read_class_scores_summary(h5_path)
  expect_identical(got$class_idx, c(1L, 1L, 1L))
  expect_identical(got$top_score, c(0.9, 0.8, 0.7))
  expect_identical(got$roi_numbers, c(11L, 12L, 13L))
})

test_that("the reader flags the legacy file by its missing transform", {
  skip_if_not_installed("hdf5r")
  got <- read_class_scores_summary(thr_test_legacy_h5())

  expect_length(got$class_labels, 109)
  expect_identical(got$roi_numbers, 2:8)
  expect_length(got$attrs, 0)
  expect_null(got$attrs$transform)
})

# --- read_class_scores_summary: files it must refuse -------------------------

test_that("the reader names the dataset a file is missing", {
  skip_if_not_installed("hdf5r")
  source_h5 <- file.path(thr_test_fixture_dir(), "model_a",
                         "D20240101T000000_IFCB134_class.h5")

  for (dataset in c("output_scores", "class_labels", "roi_numbers")) {
    h5_path <- tempfile(fileext = ".h5")
    file.copy(source_h5, h5_path)
    h5 <- hdf5r::H5File$new(h5_path, "r+")
    h5$link_delete(dataset)
    h5$close_all()

    expect_error(read_class_scores_summary(h5_path), dataset, fixed = TRUE)
    unlink(h5_path)
  }
})

test_that("the reader tolerates a file without classifier_name or thresholds", {
  skip_if_not_installed("hdf5r")
  h5_path <- tempfile(fileext = ".h5")
  on.exit(unlink(h5_path), add = TRUE)
  file.copy(file.path(thr_test_fixture_dir(), "model_a",
                      "D20240101T000000_IFCB134_class.h5"), h5_path)
  h5 <- hdf5r::H5File$new(h5_path, "r+")
  h5$link_delete("classifier_name")
  h5$link_delete("thresholds")
  h5$close_all()

  got <- read_class_scores_summary(h5_path)
  expect_identical(got$classifier_name, NA_character_)
  expect_identical(got$thresholds, rep(NaN, 5))
  expect_length(got$roi_numbers, 20)
})

test_that("the reader refuses a file that is not HDF5", {
  skip_if_not_installed("hdf5r")
  h5_path <- tempfile(fileext = ".h5")
  on.exit(unlink(h5_path), add = TRUE)
  writeLines("this is not an HDF5 file", h5_path)

  expect_error(read_class_scores_summary(h5_path), "Could not open")
  expect_error(read_class_scores_summary(h5_path), basename(h5_path), fixed = TRUE)
})

test_that("the reader reports a missing file and a malformed path", {
  skip_if_not_installed("hdf5r")
  expect_error(read_class_scores_summary(file.path(tempdir(), "nope_class.h5")),
               "not found")
  expect_error(read_class_scores_summary(NULL), "single")
  expect_error(read_class_scores_summary(c("a.h5", "b.h5")), "single")
  expect_error(read_class_scores_summary(NA_character_), "single")
})

test_that("the reader refuses scores that do not match labels and ROI numbers", {
  skip_if_not_installed("hdf5r")
  h5_path <- tempfile(fileext = ".h5")
  on.exit(unlink(h5_path), add = TRUE)
  scores <- matrix(c(0.6, 0.3, 0.1, 0.2, 0.2, 0.6), nrow = 3)   # 3 classes x 2 ROIs

  # Transposed storage would look like this: 2 labels, 3 ROIs
  thr_test_write_h5(h5_path, scores, c("a", "b"), c(1, 2, 3))
  expect_error(read_class_scores_summary(h5_path), "class_labels")

  thr_test_write_h5(h5_path, scores, c("a", "b", "c"), c(1, 2, 3))
  expect_error(read_class_scores_summary(h5_path), "roi_numbers")

  thr_test_write_h5(h5_path, scores, c("a", "b", "c"), c(1, 2), thresholds = c(0.5, 0.5))
  expect_error(read_class_scores_summary(h5_path), "thresholds")
})

test_that("the reader refuses a score dataset that is not a matrix", {
  skip_if_not_installed("hdf5r")
  h5_path <- tempfile(fileext = ".h5")
  on.exit(unlink(h5_path), add = TRUE)
  # Stored as a plain 1-D dataset: there is no ROI dimension to check
  thr_test_write_h5(h5_path, c(0.6, 0.3, 0.1), c("a", "b", "c"), 1)

  expect_error(read_class_scores_summary(h5_path), "dimension")
})

test_that("the reader handles a file with no ROIs", {
  skip_if_not_installed("hdf5r")
  h5_path <- tempfile(fileext = ".h5")
  on.exit(unlink(h5_path), add = TRUE)
  h5 <- hdf5r::H5File$new(h5_path, "w")
  h5$create_dataset("output_scores", dtype = hdf5r::h5types$H5T_NATIVE_DOUBLE,
                    space = hdf5r::H5S$new("simple", dims = c(3, 0), maxdims = c(3, 0)),
                    chunk_dims = NULL)
  h5$create_dataset("roi_numbers", dtype = hdf5r::h5types$H5T_NATIVE_INT32,
                    space = hdf5r::H5S$new("simple", dims = 0, maxdims = 0),
                    chunk_dims = NULL)
  h5[["class_labels"]] <- c("a", "b", "c")
  hdf5r::h5attr(h5, "transform") <- "dataset_squarepad"
  h5$close_all()

  got <- read_class_scores_summary(h5_path)
  expect_identical(got$roi_numbers, integer())
  expect_identical(got$class_idx, integer())
  expect_identical(got$top_score, numeric())
  expect_identical(got$class_labels, c("a", "b", "c"))
  expect_silent(thr_check_summary(got))
})

test_that("the reader refuses scores with missing values", {
  skip_if_not_installed("hdf5r")
  h5_path <- tempfile(fileext = ".h5")
  on.exit(unlink(h5_path), add = TRUE)
  scores <- matrix(c(0.6, NaN, 0.1, 0.2, 0.2, 0.6), nrow = 3)
  thr_test_write_h5(h5_path, scores, c("a", "b", "c"), c(1, 2))

  expect_error(read_class_scores_summary(h5_path), "missing")
})

test_that("the reader leaves no HDF5 handle open, whether it succeeds or fails", {
  skip_if_not_installed("hdf5r")
  h5_path <- tempfile(fileext = ".h5")
  on.exit(unlink(h5_path), add = TRUE)
  file.copy(file.path(thr_test_fixture_dir(), "model_a",
                      "D20240101T000000_IFCB134_class.h5"), h5_path)

  open_objects <- function() {
    h5 <- hdf5r::H5File$new(h5_path, "r")
    on.exit(h5$close_all(), add = TRUE)
    # Counts every open identifier on the file, including this handle
    as.numeric(h5$get_obj_count(hdf5r::h5const$H5F_OBJ_ALL))
  }

  read_class_scores_summary(h5_path)
  expect_equal(open_objects(), 1)

  h5 <- hdf5r::H5File$new(h5_path, "r+")
  h5$link_delete("roi_numbers")
  h5$close_all()
  expect_error(read_class_scores_summary(h5_path), "roi_numbers")
  expect_equal(open_objects(), 1)
})

# --- thr_model_identity ------------------------------------------------------

test_that("model identity is the same across one model's files", {
  skip_if_not_installed("hdf5r")
  model_a_files <- list.files(file.path(thr_test_fixture_dir(), "model_a"),
                              full.names = TRUE)
  expect_length(model_a_files, 6)

  ids <- vapply(model_a_files, function(path) {
    thr_model_identity(read_class_scores_summary(path))$model_id
  }, character(1))

  # Includes the file written with a different thresholds vector
  expect_identical(unname(ids), rep(thr_test_model_a_id, 6))
})

test_that("model identity differs for another checkpoint", {
  skip_if_not_installed("hdf5r")
  identity <- thr_model_identity(read_class_scores_summary(
    file.path(thr_test_fixture_dir(), "model_b", "D20240101T000000_IFCB134_class.h5")
  ))

  expect_identical(identity$model_id, thr_test_model_b_id)
  expect_identical(identity$classifier_name, "fixture_B")
  expect_identical(
    identity$checkpoint_sha256,
    "ac7ee122aa3635bc1087aead15c72e0c7ff5478ad39586951aa16b4182a6b2ef"
  )
  expect_identical(identity$transform, "dataset_squarepad")
  expect_false(identity$is_legacy)
})

test_that("the real legacy file is flagged and gets no model id", {
  skip_if_not_installed("hdf5r")
  identity <- thr_model_identity(read_class_scores_summary(thr_test_legacy_h5()))

  expect_true(identity$is_legacy)
  expect_identical(identity$model_id, NA_character_)
  expect_identical(identity$transform, NA_character_)
  expect_identical(identity$checkpoint_sha256, NA_character_)
  expect_identical(identity$classifier_name, "SMHI NIVA ResNet50 V5")
})

test_that("model identity follows the documented recipe", {
  identity <- thr_model_identity(thr_test_summary(checkpoint_sha256 = "sha-x"))

  expect_named(identity, c("model_id", "classifier_name", "checkpoint_sha256",
                           "transform", "is_legacy"))
  expect_identical(
    identity$model_id,
    thr_string_md5(paste("sha-x", "dataset_squarepad",
                         paste(thr_test_labels, collapse = "\n"), sep = "\n"))
  )
  expect_identical(identity$checkpoint_sha256, "sha-x")
  expect_false(identity$is_legacy)
})

test_that("model identity falls back to the classifier name without a hash", {
  identity <- thr_model_identity(thr_test_summary(checkpoint_sha256 = NULL))

  # md5 of "name:fake_A\ndataset_squarepad\n<labels>", from Python's hashlib
  expect_identical(identity$model_id, "f3736312a735109ab5149ff7c987cc65")
  expect_identical(identity$checkpoint_sha256, NA_character_)

  renamed <- thr_model_identity(thr_test_summary(checkpoint_sha256 = NULL,
                                                 classifier_name = "fake_B"))
  expect_false(identical(renamed$model_id, identity$model_id))
})

test_that("model identity ignores the name when the checkpoint is hashed", {
  base <- thr_model_identity(thr_test_summary())
  renamed <- thr_model_identity(thr_test_summary(classifier_name = "renamed_folder"))
  other_thresholds <- thr_model_identity(thr_test_summary(thresholds = rep(0.5, 5)))

  expect_identical(renamed$model_id, base$model_id)
  expect_identical(other_thresholds$model_id, base$model_id)
})

test_that("model identity changes with checkpoint, transform and class labels", {
  base <- thr_model_identity(thr_test_summary())$model_id

  expect_false(identical(
    thr_model_identity(thr_test_summary(checkpoint_sha256 = "sha-other"))$model_id, base
  ))
  expect_false(identical(
    thr_model_identity(thr_test_summary(transform = "dataset_resize"))$model_id, base
  ))
  expect_false(identical(
    thr_model_identity(thr_test_summary(class_labels = rev(thr_test_labels)))$model_id,
    base
  ))
  expect_false(identical(
    thr_model_identity(thr_test_summary(
      class_labels = sub("/", "_", thr_test_labels, fixed = TRUE)
    ))$model_id,
    base
  ))
})

test_that("a summary without a transform is legacy", {
  for (transform in list(NULL, NA_character_, "")) {
    summary <- thr_test_summary(transform = NULL)
    summary$attrs$transform <- transform
    identity <- thr_model_identity(summary)

    expect_true(identity$is_legacy)
    expect_identical(identity$model_id, NA_character_)
    expect_identical(identity$transform, NA_character_)
  }
})

test_that("model identity refuses a model it cannot name", {
  nameless <- thr_test_summary(checkpoint_sha256 = NULL, classifier_name = NA_character_)
  expect_error(thr_model_identity(nameless), "classifier_name")

  expect_error(thr_model_identity(list(class_labels = "a")), "attrs")
  expect_error(thr_model_identity("not a summary"), "summary")
})
