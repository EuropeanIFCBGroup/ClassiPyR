# Paths, settings and encoding helpers for the threshold review app
#
# The review folder holds two SQLite files: the verdict database (the durable
# record of what a reviewer decided) and the index cache (a rebuildable copy of
# the class-score files). Settings live next to the main app's settings, in a
# file of their own so neither app can overwrite the other's keys.

#' Path to the verdict database in a review folder
#'
#' @param review_dir Path to the review folder.
#' @return Path to the verdict SQLite file.
#' @noRd
thr_verdict_db_path <- function(review_dir) {
  file.path(review_dir, THR_VERDICT_DB_FILE)
}

#' Path to the class-score index cache in a review folder
#'
#' @param review_dir Path to the review folder.
#' @return Path to the index SQLite file.
#' @noRd
thr_index_db_path <- function(review_dir) {
  file.path(review_dir, THR_INDEX_DB_FILE)
}

#' Path to the threshold app settings file
#'
#' Creates the configuration directory if it does not exist.
#'
#' @return Path to \code{threshold_settings.json}.
#' @noRd
thr_settings_path <- function() {
  config_dir <- get_config_dir()
  if (!dir.exists(config_dir)) {
    dir.create(config_dir, recursive = TRUE, showWarnings = FALSE)
  }
  file.path(config_dir, "threshold_settings.json")
}

#' Load the threshold app settings
#'
#' @param settings_file Path to the settings JSON file.
#' @return Named list of settings; an empty list when the file is absent, not
#'   valid JSON, or not a JSON object.
#' @noRd
thr_load_settings <- function(settings_file = thr_settings_path()) {
  if (!file.exists(settings_file)) {
    return(list())
  }
  settings <- tryCatch(
    jsonlite::fromJSON(settings_file),
    error = function(e) list()
  )
  # A JSON array or scalar parses fine but cannot hold named settings
  if (!is.list(settings) || is.null(names(settings))) {
    return(list())
  }
  settings
}

#' Save threshold app settings
#'
#' Merges the given keys into the settings file, leaving the other keys as
#' they are. A key with a \code{NULL} value is removed.
#'
#' @param settings Named list of settings to update.
#' @param settings_file Path to the settings JSON file.
#' @return Invisibly, the full merged settings list.
#' @noRd
thr_save_settings <- function(settings, settings_file = thr_settings_path()) {
  setting_names <- names(settings)
  is_named <- is.list(settings) &&
    (length(settings) == 0 ||
       (!is.null(setting_names) && !anyNA(setting_names) && all(nzchar(setting_names))))
  if (!is_named) {
    stop("`settings` must be a named list, e.g. list(h5_folder = \"/data/h5\").",
         call. = FALSE)
  }
  dir.create(dirname(settings_file), recursive = TRUE, showWarnings = FALSE)
  update_settings_file(settings, settings_file)
}

#' Validate a review folder
#'
#' @param review_dir Path to the folder holding the verdict database and the
#'   index cache.
#' @param create If \code{TRUE}, create the folder when it does not exist.
#' @return The normalised absolute path, with forward slashes. Stops with an
#'   explanation when the folder is absent, is a file, cannot be created or
#'   cannot be written to.
#' @noRd
thr_validate_review_dir <- function(review_dir, create = FALSE) {
  if (!thr_is_single_string(review_dir)) {
    stop("`review_dir` must be a single folder path.", call. = FALSE)
  }
  if (file.exists(review_dir) && !dir.exists(review_dir)) {
    stop("Review folder path is a file, not a folder: ", review_dir,
         ". Choose a folder for the review databases.", call. = FALSE)
  }
  if (!dir.exists(review_dir)) {
    if (!isTRUE(create)) {
      stop("Review folder does not exist: ", review_dir,
           ". Create it first, or choose an existing folder.", call. = FALSE)
    }
    dir.create(review_dir, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(review_dir)) {
      stop("Could not create the review folder: ", review_dir,
           ". Check that the parent folder exists and is writable.", call. = FALSE)
    }
  }
  # The verdict database and the index are both written here
  if (file.access(review_dir, mode = 2) != 0) {
    stop("Review folder is not writable: ", review_dir,
         ". Choose a folder you can write to; the review databases are ",
         "stored there.", call. = FALSE)
  }
  normalizePath(review_dir, winslash = "/", mustWork = TRUE)
}

#' MD5 of one string
#'
#' Hashes the UTF-8 bytes of the string, so the digest does not depend on the
#' platform's native encoding. Used for model identifiers, which must come out
#' the same on every machine that reads the same class-score files.
#'
#' @param x A single string.
#' @return Lower-case hexadecimal MD5 digest.
#' @noRd
thr_string_md5 <- function(x) {
  if (!is.character(x) || length(x) != 1 || is.na(x)) {
    stop("`x` must be a single string.", call. = FALSE)
  }
  # tools::md5sum() only hashes files before R 4.5, so go through a temp file
  tmp <- tempfile("thr_md5_")
  on.exit(unlink(tmp), add = TRUE)
  writeBin(charToRaw(enc2utf8(x)), tmp)
  unname(tools::md5sum(tmp))
}

#' Check for a single non-empty string
#'
#' @param x Object to check.
#' @return \code{TRUE} when \code{x} is one non-missing, non-empty string.
#' @noRd
thr_is_single_string <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x) && nzchar(x)
}

#' Store class labels as JSON text
#'
#' Labels are written exactly as given, in order. A single label stays a JSON
#' array.
#'
#' @param labels Character vector of class labels.
#' @return A JSON array as one UTF-8 string.
#' @noRd
thr_labels_to_json <- function(labels) {
  enc2utf8(as.character(jsonlite::toJSON(enc2utf8(as.character(labels)))))
}

#' Read class labels stored as JSON text
#'
#' @param json A JSON array of strings, as written by
#'   \code{thr_labels_to_json()}.
#' @return Character vector of class labels.
#' @noRd
thr_labels_from_json <- function(json) {
  # Unsimplified, so a label that reads "NA" can never be taken for a missing value
  labels <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  vapply(labels, function(label) as.character(label), character(1))
}

#' Store a numeric vector as JSON text
#'
#' Values keep 17 significant digits, enough to read back the identical
#' double. \code{NaN} and \code{NA} (no threshold set) are written as
#' \code{null}.
#'
#' @param x Numeric vector.
#' @return A JSON array as one string.
#' @noRd
thr_numbers_to_json <- function(x) {
  as.character(jsonlite::toJSON(as.numeric(x), digits = I(17), na = "null"))
}

#' Read a numeric vector stored as JSON text
#'
#' @param json A JSON array, as written by \code{thr_numbers_to_json()}, or
#'   \code{NA}/\code{NULL} when nothing was stored.
#' @return Numeric vector with \code{NA} where the JSON has \code{null};
#'   \code{NULL} when nothing was stored.
#' @noRd
thr_numbers_from_json <- function(json) {
  if (is.null(json) || length(json) != 1 || is.na(json)) {
    return(NULL)
  }
  # An all-null array parses as logical and an empty one as a list
  as.numeric(unlist(jsonlite::fromJSON(json)))
}
