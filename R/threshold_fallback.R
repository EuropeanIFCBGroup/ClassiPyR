# Fallback thresholds and exclusion lists for the threshold review app
#
# A class that the review cannot fit keeps the threshold the model already
# has. That threshold comes from the model's training thresholds JSON when the
# user points at it, and otherwise from the thresholds stored in the
# class-score files. The readers for both are here, together with the reader
# for a plain-text list of samples to leave out of a review.

#' Read a text file as one UTF-8 string
#'
#' @param path Path to the file.
#' @return One string.
#' @noRd
thr_read_text <- function(path) {
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

#' Replace bare NaN tokens in JSON text by null
#'
#' Python's \code{json.dump()} writes a missing number as a bare \code{NaN},
#' which no JSON parser accepts. Only tokens outside string literals are
#' touched, so a class label that happens to contain "NaN" is left as it is.
#'
#' @param text JSON text, one string.
#' @return The text with every bare \code{NaN} replaced by \code{null}.
#' @noRd
thr_null_bare_nan <- function(text) {
  strings <- gregexpr('"(?:[^"\\\\]|\\\\.)*"', text, perl = TRUE)
  outside <- regmatches(text, strings, invert = TRUE)[[1]]
  regmatches(text, strings, invert = TRUE) <- list(
    gsub("\\bNaN\\b", "null", outside, perl = TRUE)
  )
  text
}

#' Parse a thresholds JSON file
#'
#' @param json_path Path to the file.
#' @return The parsed content as nested lists.
#' @noRd
thr_parse_thresholds_json <- function(json_path) {
  text <- thr_read_text(json_path)
  # parse_json() only ever parses the text; fromJSON() would also follow a URL
  parse <- function(json) {
    tryCatch(jsonlite::parse_json(json, simplifyVector = FALSE),
             error = function(e) e)
  }
  parsed <- parse(text)
  if (inherits(parsed, "error")) {
    reason <- strsplit(conditionMessage(parsed), "\n", fixed = TRUE)[[1]][1]
    parsed <- parse(thr_null_bare_nan(text))
  }
  if (inherits(parsed, "error")) {
    non_finite <- grepl("\\bInfinity\\b", text, perl = TRUE)
    stop("The thresholds file is not valid JSON and cannot be read: ",
         json_path, " (", reason, "). ",
         if (non_finite) {
           paste("It contains Infinity, which Python writes but JSON does",
                 "not allow; a threshold must be a number between 0 and 1.")
         } else {
           paste("Point to the model's *_thresholds_and_metrics.json as",
                 "written by ifcb-classify.")
         },
         call. = FALSE)
  }
  parsed
}

#' Take the class metrics out of a parsed thresholds file
#'
#' @param parsed Parsed content of the file.
#' @param json_path Path to the file, for error messages.
#' @return Named list with one entry per class in the file.
#' @noRd
thr_fallback_class_metrics <- function(parsed, json_path) {
  metrics <- if (is.list(parsed)) parsed[["class_metrics"]]
  keys <- names(metrics)
  if (!is.list(metrics) || length(metrics) == 0 || is.null(keys)) {
    stop("The thresholds file has no `class_metrics` object keyed by class ",
         "label: ", json_path, ". Point to the model's ",
         "*_thresholds_and_metrics.json as written by ifcb-classify.",
         call. = FALSE)
  }
  stats::setNames(metrics, enc2utf8(keys))
}

#' The threshold of one class entry of a thresholds file
#'
#' @param entry The class's entry in \code{class_metrics}.
#' @param class_name The class label, for error messages.
#' @param json_path Path to the file, for error messages.
#' @return One number, \code{NA} when the entry has no threshold.
#' @noRd
thr_entry_threshold <- function(entry, class_name, json_path) {
  fail <- function(what) {
    stop("The threshold of class '", class_name, "' in ", json_path, " ", what,
         ". A threshold must be a single number between 0 and 1.",
         call. = FALSE)
  }
  if (!is.list(entry)) {
    fail("is not stored in an entry with a `threshold` field")
  }
  threshold <- entry[["threshold"]]
  if (is.null(threshold)) {
    return(NA_real_)
  }
  if (!is.numeric(threshold) || length(threshold) != 1) {
    fail("is not a single number")
  }
  if (threshold < 0 || threshold > 1) {
    fail(paste("is", format(threshold)))
  }
  as.numeric(threshold)
}

#' Read fallback thresholds from a model's thresholds JSON
#'
#' Reads the format ifcb-classify writes at training
#' (\code{*_thresholds_and_metrics.json}) and that this app exports: an object
#' \code{class_metrics} keyed by class label, each entry with a
#' \code{threshold}. Classes are matched by label, exactly; files keyed by
#' class index (an old format) are refused rather than matched by position.
#'
#' @param json_path Path to the thresholds JSON file.
#' @param class_labels The model's class labels, in model order.
#' @return Numeric vector aligned with \code{class_labels}: the threshold of
#'   each class, \code{NA} where the file has no entry for the class or no
#'   threshold in it (\code{null}, or the bare \code{NaN} Python writes). The
#'   attribute \code{"absent"} lists the labels the file has no entry for.
#'   Stops when the file cannot be parsed, has no \code{class_metrics}, holds
#'   a threshold outside 0 to 1, or matches none of the labels.
#' @noRd
thr_read_fallback_json <- function(json_path, class_labels) {
  if (!thr_is_single_string(json_path)) {
    stop("`json_path` must be a single file path.", call. = FALSE)
  }
  if (!is.character(class_labels) || length(class_labels) == 0 ||
        anyNA(class_labels)) {
    stop("`class_labels` must be a character vector with the model's class ",
         "labels.", call. = FALSE)
  }
  if (!file.exists(json_path) || dir.exists(json_path)) {
    stop("Thresholds file not found: ", json_path, call. = FALSE)
  }
  metrics <- thr_fallback_class_metrics(thr_parse_thresholds_json(json_path),
                                        json_path)
  class_labels <- enc2utf8(class_labels)
  at <- match(class_labels, names(metrics))
  if (all(is.na(at))) {
    thr_stop_no_label_match(names(metrics), class_labels, json_path)
  }
  thresholds <- vapply(seq_along(class_labels), function(i) {
    if (is.na(at[i])) {
      return(NA_real_)
    }
    thr_entry_threshold(metrics[[at[i]]], class_labels[i], json_path)
  }, numeric(1))
  structure(thresholds, absent = class_labels[is.na(at)])
}

#' Stop because a thresholds file matches none of the model's classes
#'
#' @param keys Class keys found in the file.
#' @param class_labels The model's class labels.
#' @param json_path Path to the file.
#' @return Does not return.
#' @noRd
thr_stop_no_label_match <- function(keys, class_labels, json_path) {
  if (all(grepl("^[0-9]+$", keys))) {
    stop("The thresholds file is keyed by class indices, not class labels: ",
         json_path, ". That old format cannot be matched to the model's ",
         "classes safely; use a file keyed by class label.", call. = FALSE)
  }
  stop("None of the ", length(class_labels), " classes of the model has an ",
       "entry in ", json_path, " (its classes start with: ",
       paste(utils::head(keys, 3), collapse = ", "), "). It looks like the ",
       "thresholds file of another model.", call. = FALSE)
}

#' Fallback thresholds stored in the class-score files
#'
#' Each class-score file carries the thresholds the model was run with. They
#' can stand in for the model's thresholds file only when every file agrees.
#'
#' @param index_path Path to the index SQLite file.
#' @param model_id Model identifier, as listed by \code{thr_index_models()}.
#' @param sample_names \code{NULL} for every usable file of the model, or the
#'   sample names to look at (the samples of a frame).
#' @param h5_folder Class-score folder the review covers, or \code{NA} for
#'   every indexed file of the model. The index keeps files of every folder
#'   it was built on, and files elsewhere must not decide whether the files
#'   of this review agree. A sample that is usable in more than one indexed
#'   folder is looked at once, in its first file by path.
#' @return A list with \code{thresholds} (numeric aligned with the model's
#'   class labels, \code{NA} where the files hold no threshold; \code{NULL}
#'   when the files disagree or none matched) and \code{n_sets} (the number
#'   of distinct thresholds vectors found).
#' @noRd
thr_h5_fallback <- function(index_path, model_id, sample_names = NULL,
                            h5_folder = NA_character_) {
  thr_check_model_id(model_id)
  thr_check_index_exists(index_path)
  if (!is.null(sample_names) && (!is.character(sample_names) || anyNA(sample_names))) {
    stop("`sample_names` must be NULL or a character vector of sample names ",
         "without missing values.", call. = FALSE)
  }
  folder <- thr_check_folder_arg(h5_folder, "h5_folder")
  n_classes <- length(thr_index_class_labels(index_path, model_id))
  files <- thr_with_index(index_path, function(con) {
    DBI::dbGetQuery(con, "
      SELECT h5_path, sample_name, thresholds_json FROM files
      WHERE model_id = ? AND status = 'ok'
      ORDER BY h5_path", params = list(model_id))
  })
  files <- files[thr_under_folder(files$h5_path, folder), , drop = FALSE]
  # A sample can be usable in two indexed folders. The frame takes its first
  # file by path (thr_frame_candidates()), so only that file is looked at.
  files <- files[!duplicated(files$sample_name), , drop = FALSE]
  if (!is.null(sample_names)) {
    files <- files[files$sample_name %in% sample_names, , drop = FALSE]
  }
  # Equal vectors were written as equal text, so the text can be compared
  sets <- unique(files$thresholds_json)
  thresholds <- NULL
  if (length(sets) == 1) {
    thresholds <- thr_numbers_from_json(sets)
    if (is.null(thresholds)) {
      thresholds <- rep(NA_real_, n_classes)
    }
  }
  list(thresholds = thresholds, n_sets = length(sets))
}

#' Read a list of sample names to leave out of a review
#'
#' The file has one sample name per line. Blank lines and anything from a
#' \code{#} to the end of a line are ignored.
#'
#' @param path Path to a plain-text file.
#' @return Character vector of the distinct valid sample names
#'   (\code{DYYYYMMDDTHHMMSS_IFCBNNN}), in file order. The attribute
#'   \code{"invalid"} lists the distinct lines that are not sample names.
#' @noRd
thr_read_exclusion_file <- function(path) {
  if (!thr_is_single_string(path)) {
    stop("`path` must be a single file path.", call. = FALSE)
  }
  if (!file.exists(path) || dir.exists(path)) {
    stop("Exclusion file not found: ", path, call. = FALSE)
  }
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  if (length(lines) > 0) {
    # A byte order mark would otherwise make the first name invalid
    lines[1] <- sub("^\xef\xbb\xbf", "", lines[1], useBytes = TRUE)
  }
  # Bytes that are not UTF-8 (a comment saved in another encoding) must not
  # stop the pattern matching below
  lines <- iconv(lines, from = "UTF-8", to = "UTF-8", sub = "?")
  lines <- trimws(sub("#.*$", "", lines))
  lines <- unique(lines[nzchar(lines)])
  valid <- grepl(VALID_SAMPLE_NAME_PATTERN, lines)
  structure(lines[valid], invalid = lines[!valid])
}
