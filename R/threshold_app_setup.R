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
    "A second file of the same model for the same sample within the",
    "class-score folder that was indexed. The first file by path is used."
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

#' Folders saved last time, as far as they can be put in use at start
#'
#' A data folder that was saved and is not there right now (a disk that is
#' not mounted, an archive that is offline) is left out of use and reported.
#' It is not forgotten: the start of the app saves nothing.
#'
#' @param saved List from \code{thr_load_settings()}.
#' @return A list with \code{fields} (what the four text fields show:
#'   \code{review_dir}, \code{h5_folder}, \code{roi_folder} and
#'   \code{annotator}, \code{""} when not saved), \code{apply} (whether the
#'   saved review folder exists, so the folders can be put in use),
#'   \code{h5_folder} and \code{roi_folder} (the saved folder when it exists,
#'   otherwise \code{""}) and \code{notices} (one sentence per saved data
#'   folder that is not there).
#' @noRd
thr_restorable_settings <- function(saved) {
  keys <- c("review_dir", "h5_folder", "roi_folder", "annotator")
  fields <- vapply(keys, function(key) {
    value <- thr_clean_path_input(saved[[key]])
    if (is.null(value)) "" else value
  }, character(1))
  what <- c(h5_folder = "class-score folder", roi_folder = "raw data folder")
  gone <- names(what)[nzchar(fields[names(what)]) &
                        !dir.exists(fields[names(what)])]
  usable <- fields[names(what)]
  usable[gone] <- ""
  list(
    fields = as.list(fields),
    apply = nzchar(fields[["review_dir"]]) && dir.exists(fields[["review_dir"]]),
    h5_folder = usable[["h5_folder"]],
    roi_folder = usable[["roi_folder"]],
    notices = sprintf(
      paste("The saved %s is not there right now and is not in use: %s. It",
            "stays saved; press Apply once it is available again, or enter",
            "another folder."),
      what[gone], fields[gone]
    )
  )
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
#' @param save Whether to save the folders. \code{FALSE} when the app puts
#'   the saved folders in use at start: saving then would erase a data folder
#'   that is only unavailable for the moment.
#' @return A list with \code{review_dir}, \code{h5_folder} and
#'   \code{roi_folder} (normalised; \code{NULL} when left empty),
#'   \code{annotator} (trimmed, possibly \code{""}), \code{index_path} and
#'   \code{db_path}.
#' @noRd
thr_apply_settings <- function(review_dir, h5_folder = NULL, roi_folder = NULL,
                               annotator = NULL,
                               settings_file = thr_settings_path(),
                               save = TRUE) {
  review_dir <- thr_clean_path_input(review_dir)
  if (is.null(review_dir)) {
    stop("Choose a review folder first. The verdicts of the review and the ",
         "index of the class-score files are stored there.", call. = FALSE)
  }
  h5_folder <- thr_check_data_folder(h5_folder, "class-score folder")
  roi_folder <- thr_check_data_folder(roi_folder, "raw data folder")
  annotator <- thr_clean_path_input(annotator)
  review_dir <- thr_validate_review_dir(review_dir, create = TRUE)
  if (isTRUE(save)) {
    thr_save_settings(
      list(review_dir = review_dir, h5_folder = h5_folder,
           roi_folder = roi_folder,
           annotator = if (is.null(annotator)) "" else annotator),
      settings_file
    )
  }
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

#' The models of a review folder
#'
#' The models the index holds usable class-score files of, together with the
#' models whose review is frozen in the verdict database. The second group
#' matters when the index was deleted, or was rebuilt after the class-score
#' files were archived: the review is still there and must stay reachable.
#'
#' @param indexed Data frame from \code{thr_index_models()}, or \code{NULL}
#'   when the index could not be read.
#' @param frozen Data frame from \code{thr_frozen_models()}, or \code{NULL}
#'   when the verdict database could not be read.
#' @return The columns of \code{thr_index_models()} for every model of either
#'   group, sorted by classifier name, with \code{indexed} (the index holds
#'   usable files of the model) and \code{frozen} (its review has started). A
#'   model that is not indexed has 0 files and ROIs.
#' @noRd
thr_review_models <- function(indexed, frozen) {
  if (is.null(indexed)) {
    indexed <- thr_no_index_models()
  }
  if (is.null(frozen)) {
    frozen <- thr_no_frozen_models()
  }
  only_frozen <- frozen[!(frozen$model_id %in% indexed$model_id), , drop = FALSE]
  models <- rbind(indexed, data.frame(
    model_id = only_frozen$model_id,
    classifier_name = only_frozen$classifier_name,
    checkpoint_sha256 = only_frozen$checkpoint_sha256,
    transform = only_frozen$transform,
    n_files = rep(0L, nrow(only_frozen)), n_rois = rep(0L, nrow(only_frozen)),
    n_threshold_sets = rep(0L, nrow(only_frozen)),
    stringsAsFactors = FALSE
  ))
  # The order the index lists its models in
  models <- models[order(models$classifier_name, models$model_id,
                         method = "radix", na.last = FALSE), , drop = FALSE]
  models$indexed <- models$model_id %in% indexed$model_id
  models$frozen <- models$model_id %in% frozen$model_id
  thr_reset_rownames(models)
}

#' The models of a review folder, for display
#'
#' @param models Data frame from \code{thr_review_models()}.
#' @return Data frame with one row per model: \code{Classifier},
#'   \code{Model id}, \code{Files}, \code{ROIs}, \code{Threshold sets},
#'   \code{Transform} and \code{Review} (\code{"not started"},
#'   \code{"frozen"}, or \code{"frozen, not in the index"} for a review whose
#'   class-score files the index no longer holds). \code{Files} and
#'   \code{ROIs} count each sample of a model once, over every folder the
#'   index was built on. Models are never pooled.
#' @noRd
thr_models_table <- function(models) {
  thr_check_columns(models, c("model_id", "indexed", "frozen"), "models")
  review <- ifelse(!models$frozen, "not started",
                   ifelse(models$indexed, "frozen", "frozen, not in the index"))
  table <- data.frame(
    Classifier = as.character(models$classifier_name),
    id = substr(models$model_id, 1, 8),
    Files = as.integer(models$n_files),
    ROIs = as.integer(models$n_rois),
    sets = as.integer(models$n_threshold_sets),
    Transform = as.character(models$transform),
    Review = as.character(review),
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
#'   files that were indexed after the freeze, about frame files that changed
#'   since and about an index that is gone, empty when there are none.
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
  list(
    facts = data.frame(Item = names(facts), Value = unname(facts),
                       stringsAsFactors = FALSE),
    notices = thr_frozen_notices(model, status)
  )
}

#' What a reviewer should know about the files behind a frozen frame
#'
#' @param model List from \code{thr_get_model()}.
#' @param status List from \code{thr_frame_status()}.
#' @return Character vector of plain statements, empty when the index holds
#'   every file of the frame as it was frozen and nothing was added.
#' @noRd
thr_frozen_notices <- function(model, status) {
  folder <- if (is.null(model$h5_folder) || is.na(model$h5_folder)) {
    "the class-score folder"
  } else {
    model$h5_folder
  }
  if (isFALSE(status$index_available)) {
    return(paste0(
      "There is no class-score index in the review folder, so no new images ",
      "can be drawn for this review. Verdicts already recorded are kept, and ",
      "Results and Export work without the index. To go on reviewing, put ",
      folder, " in use as the class-score folder under Folders and press ",
      "\"Build / update index\"."
    ))
  }
  none_left <- status$n_frame_files > 0 &&
    status$n_changed_files >= status$n_frame_files
  as.character(c(
    if (status$n_new_files > 0) sprintf(
      paste("%d class-score file(s) were indexed after this frame was",
            "frozen. They are not part of this review: no images are drawn",
            "from them and they do not count in the estimates."),
      status$n_new_files
    ),
    if (none_left) sprintf(
      paste0("None of the %d class-score file(s) of this frame are in the ",
             "index as they were frozen: they have changed, moved or been ",
             "removed. No new images can be drawn, so the Review page has ",
             "nothing left to show. Verdicts already recorded stay valid, and ",
             "Results and Export work as before. If the files are only out ",
             "of reach, bring them back under %s and press \"Build / update ",
             "index\"."),
      status$n_frame_files, folder
    ) else if (status$n_changed_files > 0) sprintf(
      paste("%d class-score file(s) of the frame have changed, moved or been",
            "removed since it was frozen. They are no longer drawn from.",
            "Verdicts already recorded for them stay valid."),
      status$n_changed_files
    )
  ))
}
