# Pre-flight of a freeze in the threshold review app (run_threshold_app())
#
# A frame cannot be changed once it is frozen, so everything that goes into
# it is worked out here first: the training samples to leave out, the samples
# without raw data and the fallback thresholds. The result is shown to the
# reviewer and only then handed to thr_freeze_frame().

#' Read the samples to leave out of a review as training samples
#'
#' @param exclusion_file \code{NULL}, or the path of a text file with one
#'   sample name per line (see \code{thr_read_exclusion_file()}).
#' @param annotations_folder \code{NULL}, or a folder holding the main app's
#'   \code{annotations.sqlite}: every sample annotated there is left out.
#' @param frame_samples Samples a frame of the model would start from.
#' @return A list with \code{samples} (every name listed by either source),
#'   \code{sources} (data frame with \code{source}, \code{n_listed} and
#'   \code{n_in_frame}, one row per source that was given), \code{n_in_frame}
#'   (distinct frame samples that are excluded), \code{invalid} (lines of the
#'   file that are not sample names) and \code{notices}.
#' @noRd
thr_read_exclusions <- function(exclusion_file, annotations_folder,
                                frame_samples) {
  listed <- list()
  invalid <- character()
  notices <- character()
  exclusion_file <- thr_clean_path_input(exclusion_file)
  if (!is.null(exclusion_file)) {
    from_file <- thr_read_exclusion_file(exclusion_file)
    invalid <- attr(from_file, "invalid")
    listed[["Exclusion file"]] <- as.character(from_file)
  }
  annotations_folder <- thr_clean_path_input(annotations_folder)
  if (!is.null(annotations_folder)) {
    annotated <- list_annotated_samples_db(get_db_path(annotations_folder))
    if (length(annotated) == 0) {
      notices <- paste0("No annotated samples were found in ",
                        get_db_path(annotations_folder), ".")
    }
    listed[["Annotation database"]] <- as.character(annotated)
  }
  samples <- unique(unlist(listed, use.names = FALSE))
  if (is.null(samples)) {
    samples <- character()
  }
  list(
    samples = samples,
    sources = data.frame(
      source = names(listed),
      n_listed = as.integer(lengths(listed)),
      n_in_frame = vapply(listed, function(names) {
        sum(frame_samples %in% names)
      }, integer(1), USE.NAMES = FALSE),
      stringsAsFactors = FALSE
    ),
    n_in_frame = sum(frame_samples %in% samples),
    invalid = invalid,
    notices = notices
  )
}

#' Work out the fallback thresholds of a frame
#'
#' @param index_path Path to the index SQLite file.
#' @param model_id Model identifier.
#' @param mode \code{"h5"} for the thresholds stored in the class-score
#'   files, \code{"json"} for a thresholds JSON file.
#' @param json_path Path of the thresholds JSON file, for \code{"json"}.
#' @param sample_names Samples the frame would hold. The class-score files
#'   only have to agree among these.
#' @param h5_folder Class-score folder the review covers.
#' @return A list with \code{fallback} (numeric aligned with the class
#'   labels, without attributes), \code{source} (as
#'   \code{thr_freeze_frame()} takes it) and \code{absent} (labels the JSON
#'   file has no entry for). Stops with an explanation when the fallback
#'   cannot be determined.
#' @noRd
thr_preflight_fallback <- function(index_path, model_id, mode, json_path,
                                   sample_names, h5_folder) {
  if (identical(mode, "json")) {
    json_path <- thr_clean_path_input(json_path)
    if (is.null(json_path)) {
      stop("Enter the path of the model's thresholds JSON file, or choose ",
           "the values stored in the class-score files.", call. = FALSE)
    }
    labels <- thr_index_class_labels(index_path, model_id)
    fallback <- thr_read_fallback_json(json_path, labels)
    source <- paste0("json:", normalizePath(json_path, winslash = "/"))
    return(list(fallback = as.numeric(fallback), source = source,
                absent = attr(fallback, "absent")))
  }
  stored <- thr_h5_fallback(index_path, model_id, sample_names = sample_names,
                            h5_folder = h5_folder)
  if (is.null(stored$thresholds)) {
    stop("The class-score files of this frame carry ", stored$n_sets,
         " different sets of thresholds, so they cannot say which thresholds ",
         "the model has. Point to the model's thresholds JSON file instead.",
         call. = FALSE)
  }
  list(fallback = as.numeric(stored$thresholds), source = "h5",
       absent = character())
}

#' Notices of a pre-flight
#'
#' @param exclusions List from \code{thr_read_exclusions()}.
#' @param n_no_raw Number of samples without raw data.
#' @param roi_folder Raw data folder.
#' @param absent Class labels the thresholds file has no entry for.
#' @return Character vector of things the reviewer should know before
#'   freezing; none of them prevents it.
#' @noRd
thr_preflight_notices <- function(exclusions, n_no_raw, roi_folder, absent) {
  shown <- utils::head(exclusions$invalid, 5)
  c(
    if (exclusions$n_in_frame == 0) paste(
      "No training samples are excluded. If any sample of this frame was",
      "used to train the model, the estimated precision will be too high.",
      "Continue only if the model was trained on other samples."
    ),
    exclusions$notices,
    if (length(exclusions$invalid) > 0) sprintf(
      "%d line(s) of the exclusion file are not sample names and were ignored: %s%s",
      length(exclusions$invalid), paste(shown, collapse = ", "),
      if (length(exclusions$invalid) > length(shown)) ", ..." else ""
    ),
    if (n_no_raw > 0) sprintf(
      paste("%d sample(s) have no raw data under %s and are left out of the",
            "frame, because their images could not be shown."),
      n_no_raw, roi_folder
    ),
    if (length(absent) > 0) sprintf(
      paste("%d class(es) have no entry in the thresholds file. Where the",
            "review cannot fit one of them, 0 is written (no threshold): %s%s"),
      length(absent), paste(utils::head(absent, 5), collapse = ", "),
      if (length(absent) > 5) ", ..." else ""
    )
  )
}

#' Run a pre-flight step, collecting its error as a problem
#'
#' @param step Function of no arguments.
#' @return A list with \code{value} (\code{NULL} when the step failed) and
#'   \code{problem} (the error message, or \code{NULL}).
#' @noRd
thr_preflight_step <- function(step) {
  tryCatch(
    list(value = step(), problem = NULL),
    error = function(e) list(value = NULL, problem = conditionMessage(e))
  )
}

#' Pre-flight of a freeze
#'
#' Works out everything a frame would be frozen with, without writing
#' anything: the samples excluded as training samples, the samples without
#' raw data and the fallback thresholds. Whatever stands in the way is
#' returned as a problem instead of being raised, so the app can show all of
#' it at once.
#'
#' @param index_path Path to the index SQLite file.
#' @param model_id Model identifier.
#' @param h5_folder Class-score folder the review covers.
#' @param roi_folder Raw data folder.
#' @inheritParams thr_read_exclusions
#' @param fallback_mode \code{"h5"} or \code{"json"}; see
#'   \code{thr_preflight_fallback()}.
#' @param fallback_json Path of the thresholds JSON file.
#' @return A list with \code{n_samples} (usable samples of the model in the
#'   folder), \code{exclusions} (the source table of
#'   \code{thr_read_exclusions()}), \code{invalid_lines}, \code{n_excluded}
#'   and \code{n_no_raw} (samples left out, for either reason),
#'   \code{n_frame} (samples left), \code{fallback_source},
#'   \code{n_fallback} (classes with a fallback threshold),
#'   \code{fallback_absent}, \code{problems}, \code{notices},
#'   \code{can_freeze} and \code{freeze_args}: the arguments
#'   \code{excluded_samples}, \code{no_raw_samples}, \code{fallback},
#'   \code{fallback_source}, \code{h5_folder} and \code{roi_folder} of
#'   \code{thr_freeze_frame()}.
#' @noRd
thr_setup_preflight <- function(index_path, model_id, h5_folder, roi_folder,
                                exclusion_file = NULL,
                                annotations_folder = NULL,
                                fallback_mode = "h5", fallback_json = NULL) {
  if (!thr_is_single_string(fallback_mode) ||
      !(fallback_mode %in% c("h5", "json"))) {
    stop("`fallback_mode` must be \"h5\" or \"json\".", call. = FALSE)
  }
  samples <- thr_frame_sample_names(index_path, model_id, h5_folder)
  read <- thr_preflight_step(function() {
    thr_read_exclusions(exclusion_file, annotations_folder, samples)
  })
  exclusions <- if (is.null(read$value)) {
    thr_read_exclusions(NULL, NULL, samples)
  } else {
    read$value
  }
  remaining <- setdiff(samples, exclusions$samples)
  roi_paths <- thr_resolve_roi_paths(remaining, roi_folder)
  no_raw <- names(roi_paths)[is.na(roi_paths)]
  frame <- setdiff(remaining, no_raw)
  chosen <- thr_preflight_step(function() {
    thr_preflight_fallback(index_path, model_id, fallback_mode, fallback_json,
                           frame, h5_folder)
  })
  problems <- c(
    thr_preflight_frame_problems(length(samples), length(frame), h5_folder,
                                 roi_folder),
    read$problem,
    # Without a frame there are no files whose thresholds could agree
    if (length(frame) > 0) chosen$problem
  )
  thr_preflight_result(samples, exclusions, no_raw, frame, chosen$value,
                       problems, h5_folder, roi_folder)
}

#' Problems with the frame a pre-flight would freeze
#'
#' @param n_samples Usable samples of the model in the class-score folder.
#' @param n_frame Samples left after the exclusions.
#' @param h5_folder Class-score folder.
#' @param roi_folder Raw data folder.
#' @return Character vector, empty when the frame is fine.
#' @noRd
thr_preflight_frame_problems <- function(n_samples, n_frame, h5_folder,
                                         roi_folder) {
  if (n_samples == 0) {
    return(paste0("The model has no usable class-score files under ",
                  h5_folder, ". Build the index on this folder first."))
  }
  if (!thr_is_single_string(roi_folder) || !dir.exists(roi_folder)) {
    return(paste("The raw data folder does not exist. Enter the folder with",
                 "the raw IFCB data under Folders and press Apply; without",
                 "it no image can be shown."))
  }
  if (n_frame == 0) {
    return(paste("Nothing would be left to review: every sample of the",
                 "model is excluded or has no raw data."))
  }
  character()
}

#' Assemble the result of a pre-flight
#'
#' @param samples Samples the frame starts from.
#' @param exclusions List from \code{thr_read_exclusions()}.
#' @param no_raw Samples without raw data.
#' @param frame Samples left in the frame.
#' @param chosen List from \code{thr_preflight_fallback()}, or \code{NULL}.
#' @param problems Character vector of what prevents a freeze.
#' @param h5_folder Class-score folder.
#' @param roi_folder Raw data folder.
#' @return The list \code{thr_setup_preflight()} returns.
#' @noRd
thr_preflight_result <- function(samples, exclusions, no_raw, frame, chosen,
                                 problems, h5_folder, roi_folder) {
  absent <- if (is.null(chosen)) character() else chosen$absent
  problems <- as.character(problems)
  list(
    n_samples = length(samples),
    exclusions = exclusions$sources,
    invalid_lines = exclusions$invalid,
    n_excluded = exclusions$n_in_frame,
    n_no_raw = length(no_raw),
    n_frame = length(frame),
    fallback_source = if (is.null(chosen)) NA_character_ else chosen$source,
    n_fallback = if (is.null(chosen)) 0L else sum(!is.na(chosen$fallback)),
    fallback_absent = absent,
    problems = problems,
    notices = as.character(thr_preflight_notices(exclusions, length(no_raw),
                                                 roi_folder, absent)),
    can_freeze = length(problems) == 0,
    freeze_args = list(
      excluded_samples = exclusions$samples,
      no_raw_samples = no_raw,
      fallback = chosen$fallback,
      fallback_source = if (is.null(chosen)) "none" else chosen$source,
      h5_folder = h5_folder,
      roi_folder = roi_folder
    )
  )
}
