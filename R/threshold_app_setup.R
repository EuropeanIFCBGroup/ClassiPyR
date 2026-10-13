# Setup page of the threshold review app (run_threshold_app())
#
# Applying the folders, describing what an index build found, and describing
# a frozen frame. The pre-flight before a freeze is in
# threshold_app_preflight.R.

# What each status of a class-score file that cannot be used means
THR_FILE_STATUS_MEANING <- c(
  legacy = paste(
    "Scored before ifcb-classify stopped applying training augmentation at",
    "inference, so the scores do not match current inference. Re-run",
    "inference on these bins to review them."
  ),
  duplicate = paste(
    "A second file of the same model for a sample that is already indexed.",
    "The first file by path is used."
  ),
  failed = paste(
    "Could not be read, is inconsistent, or is not named after an IFCB",
    "sample. The message of each file says why."
  ),
  missing = "Was indexed earlier and is no longer on disk."
)

#' A path typed into the app, cleaned
#'
#' @param path Value of a text input.
#' @return The trimmed path, or \code{NULL} when nothing was entered.
#' @noRd
thr_clean_path_input <- function(path) {
  if (!is.character(path) || length(path) != 1 || is.na(path)) {
    return(NULL)
  }
  path <- trimws(path)
  if (nzchar(path)) path else NULL
}

#' Database folder of the main app, to offer for the exclusion list
#'
#' Reads the main app's settings file and nothing else: the threshold app
#' never writes to it.
#'
#' @param settings_file Path to the main app's settings JSON file.
#' @return The folder its annotation database is in, or the default database
#'   folder when the settings do not name one.
#' @noRd
thr_main_app_db_folder <- function(settings_file = get_settings_path()) {
  folder <- if (file.exists(settings_file)) {
    tryCatch(jsonlite::fromJSON(settings_file)$db_folder,
             error = function(e) NULL)
  }
  folder <- thr_clean_path_input(folder)
  if (is.null(folder)) get_default_db_dir() else folder
}

#' Check a data folder entered in the app
#'
#' @param folder Value of a folder input; may be empty.
#' @param what What the folder is, for the error message.
#' @return The normalised folder, or \code{NULL} when none was entered.
#' @noRd
thr_check_data_folder <- function(folder, what) {
  folder <- thr_clean_path_input(folder)
  if (is.null(folder)) {
    return(NULL)
  }
  if (!dir.exists(folder)) {
    stop("The ", what, " does not exist: ", folder, ". Enter an existing ",
         "folder, or leave the field empty.", call. = FALSE)
  }
  thr_normalise_folder(folder)
}

#' Validate and save the folders of the threshold app
#'
#' The review folder is required and is created when it does not exist. The
#' two data folders may be left empty, which is enough to look at the results
#' of a review or to export them; a folder that is given must exist. Nothing
#' is saved unless everything is valid.
#'
#' @param review_dir Folder for the verdict database and the index cache.
#' @param h5_folder Folder with the class-score files, or empty.
#' @param roi_folder Folder with the raw IFCB data, or empty.
#' @param annotator Name of the reviewer, or empty.
#' @param settings_file Path to the threshold app's settings file.
#' @return A list with \code{review_dir}, \code{h5_folder} and
#'   \code{roi_folder} (normalised; \code{NULL} when left empty),
#'   \code{annotator} (trimmed, possibly \code{""}), \code{index_path} and
#'   \code{db_path}.
#' @noRd
thr_apply_settings <- function(review_dir, h5_folder = NULL, roi_folder = NULL,
                               annotator = NULL,
                               settings_file = thr_settings_path()) {
  review_dir <- thr_clean_path_input(review_dir)
  if (is.null(review_dir)) {
    stop("Choose a review folder first. The verdicts of the review and the ",
         "index of the class-score files are stored there.", call. = FALSE)
  }
  h5_folder <- thr_check_data_folder(h5_folder, "class-score folder")
  roi_folder <- thr_check_data_folder(roi_folder, "raw data folder")
  annotator <- thr_clean_path_input(annotator)
  review_dir <- thr_validate_review_dir(review_dir, create = TRUE)
  thr_save_settings(
    list(review_dir = review_dir, h5_folder = h5_folder,
         roi_folder = roi_folder,
         annotator = if (is.null(annotator)) "" else annotator),
    settings_file
  )
  list(
    review_dir = review_dir, h5_folder = h5_folder, roi_folder = roi_folder,
    annotator = if (is.null(annotator)) "" else annotator,
    index_path = thr_index_db_path(review_dir),
    db_path = thr_verdict_db_path(review_dir)
  )
}

#' Describe what an index build did
#'
#' @param result The list \code{build_threshold_index()} returns.
#' @return One string.
#' @noRd
thr_index_build_message <- function(result) {
  if (result$n_scanned == 0) {
    return(paste("No class-score files (*_class*.h5) were found in the",
                 "class-score folder."))
  }
  sprintf(
    paste("Found %d class-score file(s): %d read, %d unchanged since the",
          "last build. The index now holds %d usable file(s) and %d not",
          "usable."),
    as.integer(result$n_scanned), as.integer(result$n_read),
    as.integer(result$n_skipped), as.integer(result$n_ok),
    nrow(result$failures)
  )
}

#' Count the files that cannot be used, by reason
#'
#' @param failures Data frame from \code{thr_index_failures()}.
#' @return Data frame with \code{Status}, \code{Files} and \code{Meaning},
#'   one row per status that occurs.
#' @noRd
thr_failure_overview <- function(failures) {
  thr_check_columns(failures, "status", "failures")
  known <- names(THR_FILE_STATUS_MEANING)
  statuses <- c(intersect(known, failures$status),
                sort(setdiff(unique(failures$status), known)))
  meaning <- unname(THR_FILE_STATUS_MEANING[statuses])
  meaning[is.na(meaning)] <- "See the message of each file."
  data.frame(
    Status = statuses,
    Files = vapply(statuses, function(status) sum(failures$status == status),
                   integer(1), USE.NAMES = FALSE),
    Meaning = meaning,
    stringsAsFactors = FALSE
  )
}

#' The indexed models, for display
#'
#' @param models Data frame from \code{thr_index_models()}.
#' @param db_path Path to the verdict SQLite file, which may not exist yet.
#' @return Data frame with one row per model: \code{Classifier},
#'   \code{Model id}, \code{Files}, \code{ROIs}, \code{Threshold sets},
#'   \code{Transform} and \code{Review} (\code{"frozen"} or
#'   \code{"not started"}). Models are never pooled.
#' @noRd
thr_models_table <- function(models, db_path) {
  frozen <- vapply(models$model_id, function(model_id) {
    thr_is_frozen(db_path, model_id)
  }, logical(1), USE.NAMES = FALSE)
  table <- data.frame(
    Classifier = as.character(models$classifier_name),
    id = substr(models$model_id, 1, 8),
    Files = as.integer(models$n_files),
    ROIs = as.integer(models$n_rois),
    sets = as.integer(models$n_threshold_sets),
    Transform = as.character(models$transform),
    Review = ifelse(frozen, "frozen", "not started"),
    stringsAsFactors = FALSE
  )
  names(table)[c(2, 5)] <- c("Model id", "Threshold sets")
  table
}

#' Where a frame's fallback thresholds came from, in words
#'
#' @param fallback_source Stored source: \code{"h5"}, \code{"json:<path>"}
#'   or \code{"none"}.
#' @return One string.
#' @noRd
thr_fallback_source_label <- function(fallback_source) {
  if (identical(fallback_source, "h5")) {
    return("the thresholds stored in the class-score files")
  }
  if (thr_is_single_string(fallback_source) &&
      startsWith(fallback_source, "json:")) {
    return(paste("the thresholds file", substring(fallback_source, 6)))
  }
  "none: a class the review cannot fit gets 0 (no threshold)"
}

#' Describe a frozen frame
#'
#' @param model List from \code{thr_get_model()}.
#' @param status List from \code{thr_frame_status()}.
#' @return A list with \code{facts} (data frame with \code{Item} and
#'   \code{Value}) and \code{notices}: plain statements about class-score
#'   files that were indexed after the freeze and about frame files that
#'   changed since, empty when there are none.
#' @noRd
thr_frozen_summary <- function(model, status) {
  text <- function(value) {
    if (is.null(value) || is.na(value)) "not recorded" else as.character(value)
  }
  facts <- c(
    "Classifier" = text(model$classifier_name),
    "Model id" = text(model$model_id),
    "Frozen" = text(model$frozen_at),
    "Class-score folder" = text(model$h5_folder),
    "Raw data folder" = text(model$roi_folder),
    "Samples in the frame" = as.character(status$n_frame_files),
    "Left out as training samples" = as.character(status$n_excluded_training),
    "Left out for lack of raw data" = as.character(status$n_no_raw),
    "Classes" = as.character(length(model$class_labels)),
    "Fallback thresholds" = thr_fallback_source_label(model$fallback_source)
  )
  notices <- c(
    if (status$n_new_files > 0) sprintf(
      paste("%d class-score file(s) were indexed after this frame was",
            "frozen. They are not part of this review: no images are drawn",
            "from them and they do not count in the estimates."),
      status$n_new_files
    ),
    if (status$n_changed_files > 0) sprintf(
      paste("%d class-score file(s) of the frame have changed, moved or been",
            "removed since it was frozen. They are no longer drawn from.",
            "Verdicts already recorded for them stay valid."),
      status$n_changed_files
    )
  )
  list(
    facts = data.frame(Item = names(facts), Value = unname(facts),
                       stringsAsFactors = FALSE),
    notices = as.character(notices)
  )
}
