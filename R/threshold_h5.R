# Class-score reader for the threshold review app
#
# Reads only what a threshold fit needs from an ifcb-classify class-scores
# file: per ROI, the winning class and its score, plus what identifies the
# model that produced them. ifcb-classify applies a threshold to the argmax
# winner only, so the rest of the score matrix is never stored.

# Softmax scores can overshoot 1 by a few ulp; anything further out is not a
# probability and has no score bin.
THR_SCORE_TOLERANCE <- 1e-6

#' Stop unless hdf5r is installed
#'
#' @return \code{NULL}, invisibly.
#' @noRd
thr_require_hdf5r <- function() {
  if (!requireNamespace("hdf5r", quietly = TRUE)) {
    stop("Package 'hdf5r' is required to read H5 class-score files. ",
         "Install it with: install.packages('hdf5r')", call. = FALSE)
  }
  invisible(NULL)
}

#' Read the top-1 summary of a class-scores HDF5 file
#'
#' Reads an ifcb-classify \code{*_class.h5} file and reduces it to the winning
#' class and score per ROI. The score matrix is always taken as classes x ROIs,
#' the layout hdf5r presents the file in; its stored shape is checked against
#' the class labels and ROI numbers and never guessed from the dimensions.
#'
#' @param h5_path Path to a class-scores HDF5 file.
#' @return A list with \code{roi_numbers} (integer), \code{class_idx} (integer,
#'   1-based index into \code{class_labels}; ties go to the first class, as
#'   numpy's argmax does), \code{top_score} (numeric), \code{class_labels}
#'   (character, as stored), \code{classifier_name} (one string, \code{NA} when
#'   the file has none), \code{thresholds} (numeric, \code{NaN} where the model
#'   has no threshold) and \code{attrs} (named list of the file's root
#'   attributes as character).
#' @noRd
read_class_scores_summary <- function(h5_path) {
  thr_require_hdf5r()
  if (!thr_is_single_string(h5_path)) {
    stop("`h5_path` must be a single file path.", call. = FALSE)
  }
  if (!file.exists(h5_path)) {
    stop("Class-score file not found: ", h5_path, call. = FALSE)
  }
  h5 <- tryCatch(
    hdf5r::H5File$new(h5_path, "r"),
    error = function(e) {
      stop("Could not open ", basename(h5_path), " as an HDF5 file. It may be ",
           "truncated or still being written (", h5_path, ").", call. = FALSE)
    }
  )
  on.exit(h5$close_all(), add = TRUE)

  thr_check_h5_datasets(h5, h5_path)
  class_labels <- enc2utf8(as.character(h5[["class_labels"]]$read()))
  roi_numbers <- as.integer(h5[["roi_numbers"]]$read())
  top <- thr_top1(thr_read_h5_scores(h5, length(class_labels),
                                     length(roi_numbers), h5_path))

  list(
    roi_numbers = roi_numbers,
    class_idx = top$class_idx,
    top_score = top$top_score,
    class_labels = class_labels,
    classifier_name = thr_read_h5_classifier_name(h5),
    thresholds = thr_read_h5_thresholds(h5, length(class_labels), h5_path),
    attrs = thr_read_h5_attrs(h5)
  )
}

#' Stop when a class-scores file lacks a required dataset
#'
#' @param h5 An open \code{hdf5r::H5File}.
#' @param h5_path Path of the file, for the error message.
#' @return \code{NULL}, invisibly.
#' @noRd
thr_check_h5_datasets <- function(h5, h5_path) {
  required <- c("output_scores", "class_labels", "roi_numbers")
  absent <- setdiff(required, names(h5))
  if (length(absent) > 0) {
    stop("Missing dataset ", paste0("'", absent, "'", collapse = ", "), " in ",
         basename(h5_path), ". It is not a class-scores file that ",
         "ifcb-classify wrote (", h5_path, ").", call. = FALSE)
  }
  invisible(NULL)
}

#' Read the score matrix of a class-scores file as classes x ROIs
#'
#' @param h5 An open \code{hdf5r::H5File}.
#' @param n_classes Number of class labels in the file.
#' @param n_rois Number of ROI numbers in the file.
#' @param h5_path Path of the file, for error messages.
#' @return Numeric matrix with \code{n_classes} rows and \code{n_rois} columns.
#' @noRd
thr_read_h5_scores <- function(h5, n_classes, n_rois, h5_path) {
  dataset <- h5[["output_scores"]]
  dims <- dataset$dims
  if (length(dims) != 2) {
    stop("output_scores in ", basename(h5_path), " has ", length(dims),
         " dimension(s); expected a classes x ROIs matrix.", call. = FALSE)
  }
  if (dims[1] != n_classes) {
    stop("output_scores in ", basename(h5_path), " has ", dims[1],
         " class rows but class_labels lists ", n_classes,
         " classes. The file is inconsistent; re-run the classification.",
         call. = FALSE)
  }
  if (dims[2] != n_rois) {
    stop("output_scores in ", basename(h5_path), " has ", dims[2],
         " ROI columns but roi_numbers lists ", n_rois,
         " ROIs. The file is inconsistent; re-run the classification.",
         call. = FALSE)
  }
  if (n_rois == 0) {
    return(matrix(numeric(), nrow = n_classes, ncol = 0))
  }
  scores <- dataset$read()
  # hdf5r drops the ROI dimension of a single-ROI file. Restore the stored
  # shape rather than inferring one from the length.
  dim(scores) <- dims
  scores
}

#' Read the classifier name of a class-scores file
#'
#' @param h5 An open \code{hdf5r::H5File}.
#' @return One string, or \code{NA} when the file has no classifier name.
#' @noRd
thr_read_h5_classifier_name <- function(h5) {
  if (!"classifier_name" %in% names(h5)) {
    return(NA_character_)
  }
  thr_scalar_or_na(h5[["classifier_name"]]$read())
}

#' Read the per-class thresholds of a class-scores file
#'
#' @param h5 An open \code{hdf5r::H5File}.
#' @param n_classes Number of class labels in the file.
#' @param h5_path Path of the file, for error messages.
#' @return Numeric vector of length \code{n_classes}; \code{NaN} where no
#'   threshold is set, and all \code{NaN} when the file stores none.
#' @noRd
thr_read_h5_thresholds <- function(h5, n_classes, h5_path) {
  if (!"thresholds" %in% names(h5)) {
    return(rep(NaN, n_classes))
  }
  thresholds <- as.numeric(h5[["thresholds"]]$read())
  if (length(thresholds) != n_classes) {
    stop("thresholds in ", basename(h5_path), " has ", length(thresholds),
         " values but class_labels lists ", n_classes,
         " classes. The file is inconsistent; re-run the classification.",
         call. = FALSE)
  }
  thresholds
}

#' Read the root attributes of a class-scores file
#'
#' @param h5 An open \code{hdf5r::H5File}.
#' @return Named list of character vectors; empty for a file without
#'   attributes.
#' @noRd
thr_read_h5_attrs <- function(h5) {
  keys <- hdf5r::h5attr_names(h5)
  values <- lapply(keys, function(key) {
    enc2utf8(as.character(hdf5r::h5attr(h5, key)))
  })
  stats::setNames(values, keys)
}

#' Winning class and score per ROI
#'
#' @param scores Numeric matrix of classes x ROIs (column j is ROI j).
#' @return A list with \code{class_idx} (integer, 1-based row of the highest
#'   score in each column; an exact tie goes to the first class, as numpy's
#'   argmax does) and \code{top_score} (that score).
#' @noRd
thr_top1 <- function(scores) {
  if (!is.matrix(scores) || !is.numeric(scores)) {
    stop("`scores` must be a numeric matrix of classes x ROIs.", call. = FALSE)
  }
  if (nrow(scores) == 0) {
    stop("`scores` has no classes (zero rows).", call. = FALSE)
  }
  if (anyNA(scores)) {
    stop("output_scores contains missing values (NaN), so the winning class ",
         "is undefined. Re-run the classification for this sample.",
         call. = FALSE)
  }
  if (ncol(scores) == 0) {
    return(list(class_idx = integer(), top_score = numeric()))
  }
  # ties.method = "first" compares exactly; the default ("random") would treat
  # scores within a relative 1e-5 as tied.
  class_idx <- max.col(t(scores), ties.method = "first")
  list(
    class_idx = class_idx,
    top_score = scores[cbind(class_idx, seq_len(ncol(scores)))]
  )
}

#' Identify the model behind a class-scores summary
#'
#' A file without a \code{transform} root attribute is legacy: it was scored
#' before ifcb-classify stopped applying training augmentation at inference,
#' so its scores do not match current inference and it gets no model
#' identifier. Otherwise the identifier is the MD5 of the checkpoint hash (or,
#' when the checkpoint could not be hashed, the classifier name), the
#' transform and the class labels.
#'
#' @param summary A list as returned by \code{read_class_scores_summary()}.
#' @return A list with \code{model_id} (\code{NA} for a legacy file),
#'   \code{classifier_name}, \code{checkpoint_sha256} (\code{NA} if absent),
#'   \code{transform} (\code{NA} for a legacy file) and \code{is_legacy}.
#' @noRd
thr_model_identity <- function(summary) {
  if (!is.list(summary)) {
    stop("`summary` must be the list returned by read_class_scores_summary().",
         call. = FALSE)
  }
  if (!is.list(summary$attrs)) {
    stop("`summary` has no `attrs` list of root attributes; it must be the ",
         "list returned by read_class_scores_summary().", call. = FALSE)
  }
  transform <- thr_scalar_or_na(summary$attrs$transform)
  checkpoint_sha256 <- thr_scalar_or_na(summary$attrs$checkpoint_sha256)
  classifier_name <- thr_scalar_or_na(summary$classifier_name)
  is_legacy <- is.na(transform)

  model_id <- NA_character_
  if (!is_legacy) {
    if (is.na(checkpoint_sha256) && is.na(classifier_name)) {
      stop("The file has neither a checkpoint_sha256 attribute nor a ",
           "classifier_name, so the model that scored it cannot be identified.",
           call. = FALSE)
    }
    key <- if (is.na(checkpoint_sha256)) {
      paste0("name:", classifier_name)
    } else {
      checkpoint_sha256
    }
    model_id <- thr_string_md5(paste(
      key, transform, paste(summary$class_labels, collapse = "\n"), sep = "\n"
    ))
  }

  list(
    model_id = model_id,
    classifier_name = classifier_name,
    checkpoint_sha256 = checkpoint_sha256,
    transform = transform,
    is_legacy = is_legacy
  )
}

#' Stop when a reader summary cannot be stored in the index
#'
#' The index tables reject missing scores and repeated ROI numbers, which
#' would roll back a whole batch of files. Checking here turns such a file
#' into a single failure instead.
#'
#' @param summary A list as returned by \code{read_class_scores_summary()}.
#' @return \code{NULL}, invisibly.
#' @noRd
thr_check_summary <- function(summary) {
  fail <- function(...) stop(..., call. = FALSE)
  if (!is.list(summary)) {
    fail("The reader did not return a list for this file.")
  }
  labels <- summary$class_labels
  if (!is.character(labels) || length(labels) == 0 || anyNA(labels) ||
        anyDuplicated(labels) > 0) {
    fail("class_labels must be distinct, non-missing class names.")
  }
  rois <- summary$roi_numbers
  if (!is.numeric(rois) || anyNA(rois) || any(rois != round(rois))) {
    fail("roi_numbers must be whole numbers without missing values.")
  }
  if (anyDuplicated(rois) > 0) {
    fail("roi_numbers lists ROI ", rois[anyDuplicated(rois)], " more than once.")
  }
  idx <- summary$class_idx
  score <- summary$top_score
  if (length(idx) != length(rois) || length(score) != length(rois)) {
    fail("roi_numbers, class_idx and top_score must have the same length.")
  }
  if (!is.numeric(idx) || anyNA(idx) || any(idx < 1 | idx > length(labels)) ||
        any(idx != round(idx))) {
    fail("class_idx must index the ", length(labels), " class labels.")
  }
  if (!is.numeric(score) || anyNA(score)) {
    fail("top_score has missing values (NaN).")
  }
  if (any(score < -THR_SCORE_TOLERANCE | score > 1 + THR_SCORE_TOLERANCE)) {
    fail("Scores are not between 0 and 1, so they are not class ",
         "probabilities (range ", min(score), " to ", max(score), ").")
  }
  if (length(summary$thresholds) != length(labels)) {
    fail("thresholds must have one value per class label.")
  }
  invisible(NULL)
}

#' First element as a string, or NA
#'
#' @param x A value read from a class-scores file, possibly absent.
#' @return One string; \code{NA} when \code{x} is \code{NULL}, empty, missing
#'   or an empty string.
#' @noRd
thr_scalar_or_na <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA_character_)
  }
  value <- enc2utf8(as.character(x[[1]]))
  if (is.na(value) || !nzchar(value)) NA_character_ else value
}
