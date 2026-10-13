# Review page of the threshold review app (run_threshold_app())
#
# Loading a page (draw, find the raw files, cut out the images) and turning
# what the browser sends back on "Confirm page" into verdicts. Both are plain
# functions so that the Shiny module only has to pass inputs in and show what
# comes back.

#' Samples of a frozen frame
#'
#' @param db_path Path to the verdict SQLite file.
#' @param model_id Model identifier.
#' @return Character vector of sample names, sorted; empty when the model has
#'   no frozen frame.
#' @noRd
thr_frame_samples_db <- function(db_path, model_id) {
  thr_check_model_id(model_id)
  con <- thr_connect_verdicts(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  samples <- DBI::dbGetQuery(
    con, "SELECT sample_name FROM frame_files WHERE model_id = ?",
    params = list(model_id)
  )$sample_name
  sort(as.character(samples), method = "radix")
}

#' Raw files of every sample of a frozen frame
#'
#' Resolved once for the whole frame rather than page by page: when the raw
#' data is not in the standard folder layout, finding it takes a scan of the
#' raw data folder, which must not be repeated for every page. Only a file
#' that was found is remembered, though. A sample without one is looked up
#' again on every call, so that the review carries on by itself once raw
#' data that was away is back.
#'
#' @param db_path Path to the verdict SQLite file.
#' @param model_id Model identifier.
#' @param roi_folder Raw data folder.
#' @param known What an earlier call returned for the same frame and folder,
#'   or \code{NULL}.
#' @return Named character vector, see \code{thr_resolve_roi_paths()}.
#' @noRd
thr_frame_roi_paths <- function(db_path, model_id, roi_folder, known = NULL) {
  thr_refresh_roi_paths(known, thr_frame_samples_db(db_path, model_id),
                        roi_folder)
}

#' Samples of a page whose raw file was not found
#'
#' @param page Data frame of a loaded page, or \code{NULL}.
#' @return Character vector of distinct sample names.
#' @noRd
thr_page_lost_samples <- function(page) {
  if (is.null(page) || is.null(page$no_image_reason)) {
    return(character())
  }
  lost <- page$no_image_reason %in% THR_NO_IMAGE_RAW
  unique(as.character(page$sample_name[lost]))
}

#' Forget the raw files a page could not use
#'
#' A path that was found earlier is worth nothing once its file has gone: the
#' sample is marked as not found, so that the next page looks it up again.
#'
#' @param roi_paths Named character vector from \code{thr_frame_roi_paths()}.
#' @param page Data frame of the page loaded with those paths.
#' @return \code{roi_paths}, with \code{NA} for the samples whose draws came
#'   back without an image because their raw file was not found.
#' @noRd
thr_forget_lost_roi_paths <- function(roi_paths, page) {
  roi_paths[names(roi_paths) %in% thr_page_lost_samples(page)] <- NA_character_
  roi_paths
}

#' Tell the reviewer that raw data of a page was not found
#'
#' @param page Data frame of a loaded page, or \code{NULL}.
#' @param roi_folder Raw data folder the images were looked for in.
#' @return \code{NULL} when no draw of the page lacks its raw file; otherwise
#'   one string naming the folder and the number of images concerned.
#' @noRd
thr_missing_raw_notice <- function(page, roi_folder) {
  lost <- thr_page_lost_samples(page)
  if (length(lost) == 0) {
    return(NULL)
  }
  n_missing <- sum(page$no_image_reason %in% THR_NO_IMAGE_RAW)
  folder <- if (thr_is_single_string(roi_folder)) roi_folder else "(none set)"
  what <- if (thr_is_single_string(roi_folder) && dir.exists(roi_folder)) {
    sprintf(paste("the raw data (.roi file) of %d sample(s), such as %s, was",
                  "not found in the raw data folder %s."),
            length(lost), lost[1], folder)
  } else {
    sprintf(paste("the raw data folder %s was not found. Is the disk mounted,",
                  "or has the folder been moved or renamed?"), folder)
  }
  paste(
    sprintf("%d of the %d image(s) on this page cannot be shown:", n_missing,
            nrow(page)),
    what,
    "Nothing is recorded for these images: they stay pending and are shown",
    "once their raw data is back. Press \"Load page\" to try again."
  )
}

#' Would confirming a page settle any of its draws?
#'
#' A page can be confirmed when it has an image to give a verdict on, or a
#' draw whose ROI the raw file could not give, which a confirmation retires.
#' A page that holds nothing but draws whose raw data was not found cannot:
#' nothing on it was reviewed and nothing on it is to be recorded.
#'
#' @param page Data frame of a loaded page, or \code{NULL}.
#' @return \code{TRUE} or \code{FALSE}.
#' @noRd
thr_page_can_confirm <- function(page) {
  if (is.null(page) || nrow(page) == 0) {
    return(FALSE)
  }
  any(page$extracted | thr_draw_is_unavailable(page))
}

#' What is left of a class when a page comes back without draws
#'
#' @param db_path Path to the verdict SQLite file.
#' @param index_path Path to the index SQLite file.
#' @param model_id Model identifier of a frozen frame.
#' @param class_name One class label of the model.
#' @return A list with \code{n_predicted} (predictions of the class in the
#'   frame), \code{n_undrawn} (those never drawn), \code{n_pending} (drawn
#'   and not yet resolved) and \code{n_changed_files} (class-score files of
#'   the frame, of any class, that the index no longer holds as they were
#'   frozen, which is why a prediction cannot be drawn).
#' @noRd
thr_class_remaining <- function(db_path, index_path, model_id, class_name) {
  overview <- thr_class_overview(db_path, model_id)
  class_row <- overview[overview$class_name %in% enc2utf8(class_name), ,
                        drop = FALSE]
  if (!thr_is_single_string(class_name) || nrow(class_row) != 1) {
    stop("`class_name` is not a class of this model.", call. = FALSE)
  }
  status <- thr_frame_status(db_path, index_path, model_id)
  list(
    n_predicted = class_row$n_predicted,
    n_undrawn = class_row$n_predicted - class_row$n_drawn,
    n_pending = class_row$n_pending,
    n_changed_files = status$n_changed_files
  )
}

#' Remove the images of earlier pages
#'
#' Only folders named after an IFCB sample are removed, which is all the
#' extractor ever writes: whatever else is in the folder is left alone.
#'
#' @param image_folder The session's image folder.
#' @return The number of sample folders removed.
#' @noRd
thr_clear_image_folder <- function(image_folder) {
  if (!thr_is_single_string(image_folder) || !dir.exists(image_folder)) {
    return(0L)
  }
  folders <- list.dirs(image_folder, full.names = FALSE, recursive = FALSE)
  folders <- folders[grepl(VALID_SAMPLE_NAME_PATTERN, folders)]
  unlink(file.path(image_folder, folders), recursive = TRUE)
  length(folders)
}

#' Load the next review page of a class
#'
#' Draws the page (pending draws first, then new ones, written to the verdict
#' database before anything is shown), cuts the images out of the raw files
#' and gives the page a fresh token. The images of earlier pages are removed
#' first, so the image folder never holds more than one page.
#'
#' @param db_path Path to the verdict SQLite file.
#' @param index_path Path to the index SQLite file.
#' @param model_id Model identifier.
#' @param class_name Class label to review.
#' @param page_size Number of images on a page.
#' @param roi_paths Named character vector with the raw file of each sample,
#'   from \code{thr_frame_roi_paths()}.
#' @param image_folder The session's image folder.
#' @param extractor Image extractor, see \code{thr_extract_page_images()}.
#' @param seed \code{NULL}, or a number that makes the draw reproducible.
#' @return A list with \code{page} (the data frame of
#'   \code{thr_next_page()} plus \code{png_file}, \code{extracted},
#'   \code{no_image_reason}, \code{width} and \code{height}; zero rows when
#'   no draw of the class is pending and none can be drawn), \code{token} and
#'   \code{class_name}.
#' @noRd
thr_load_review_page <- function(db_path, index_path, model_id, class_name,
                                 page_size, roi_paths, image_folder,
                                 extractor = iRfcb::ifcb_extract_pngs,
                                 seed = NULL) {
  page <- thr_next_page(db_path, index_path, model_id, class_name,
                        page_size = page_size, seed = seed)
  thr_clear_image_folder(image_folder)
  page <- thr_extract_page_images(page, roi_paths, image_folder, extractor)
  size <- thr_png_dimensions(file.path(image_folder, page$png_file))
  page$width <- ifelse(page$extracted, size$width, NA_integer_)
  page$height <- ifelse(page$extracted, size$height, NA_integer_)
  list(page = page, token = thr_new_page_token(), class_name = class_name)
}

#' Result of a confirmation that recorded nothing
#'
#' @param status \code{"stale"} or \code{"error"}.
#' @param message What to tell the reviewer.
#' @return A list shaped like the result of \code{thr_confirm_page()}.
#' @noRd
thr_confirm_nothing <- function(status, message) {
  list(status = status, message = message, n_confirmed = 0L, n_wrong = 0L,
       n_unavailable = 0L, n_pending = 0L, n_no_raw = 0L, n_load_failed = 0L)
}

#' Tell the reviewer what a confirmation recorded
#'
#' @param n_confirmed Verdicts written.
#' @param n_wrong How many of the page's verdicts were "wrong".
#' @param n_unavailable Draws marked unavailable: their raw file was read and
#'   did not give the ROI.
#' @param n_pending Draws left pending, for whatever reason.
#' @param n_elsewhere Draws the page resolved that had already been resolved,
#'   by an earlier submit or another session.
#' @param n_no_raw How many of the pending draws are pending because their
#'   raw data was not found.
#' @param n_load_failed How many of the pending draws are pending because the
#'   browser failed to load their image.
#' @return One string.
#' @noRd
thr_confirm_message <- function(n_confirmed, n_wrong, n_unavailable, n_pending,
                                n_elsewhere = 0L, n_no_raw = 0L,
                                n_load_failed = 0L) {
  n_not_viewed <- n_pending - n_no_raw - n_load_failed
  paste(c(
    sprintf("Recorded %d verdict(s), %d marked wrong.", n_confirmed, n_wrong),
    if (n_unavailable > 0) sprintf(
      paste("%d image(s) could not be read from their raw file and are left",
            "out of the estimates."),
      n_unavailable
    ),
    if (n_no_raw > 0) sprintf(
      paste("%d image(s) could not be shown because their raw data was not",
            "found: they are still pending and will come back."),
      n_no_raw
    ),
    if (n_load_failed > 0) sprintf(
      "%d image(s) failed to load and will come back on a later page.",
      n_load_failed
    ),
    if (n_not_viewed > 0) sprintf(
      "%d image(s) had not been viewed and will come back on a later page.",
      n_not_viewed
    ),
    if (n_elsewhere > 0) sprintf(
      "%d image(s) had already been resolved and were not changed.",
      n_elsewhere
    )
  ), collapse = " ")
}

#' Result of a confirmation that was saved
#'
#' @param resolved A list from \code{thr_resolve_page_verdicts()}.
#' @param saved A list from \code{thr_save_verdicts()}.
#' @return A list shaped like the result of \code{thr_confirm_page()}.
#' @noRd
thr_confirm_saved <- function(resolved, saved) {
  n_resolved <- nrow(resolved$verdicts) + length(resolved$unavailable)
  counts <- list(
    n_confirmed = saved$n_confirmed,
    n_wrong = as.integer(sum(resolved$verdicts$verdict == 0)),
    n_unavailable = saved$n_unavailable,
    n_pending = length(resolved$still_pending),
    n_no_raw = length(resolved$no_raw),
    n_load_failed = length(resolved$load_failed)
  )
  message <- thr_confirm_message(
    counts$n_confirmed, counts$n_wrong, counts$n_unavailable, counts$n_pending,
    n_elsewhere = n_resolved - saved$n_confirmed - saved$n_unavailable,
    n_no_raw = counts$n_no_raw, n_load_failed = counts$n_load_failed
  )
  c(list(status = "saved", message = message), counts)
}

#' Record the confirmation of a review page
#'
#' Turns what the browser sent into verdicts with
#' \code{thr_resolve_page_verdicts()} and stores them with
#' \code{thr_save_verdicts()}. Nothing is written when the confirmation
#' belongs to a page that is no longer the current one, when the browser and
#' the server disagree about what was shown, or when the page has no draw a
#' confirmation could settle (see \code{thr_page_can_confirm()}); the result
#' then says why.
#'
#' @param db_path Path to the verdict SQLite file.
#' @param page Data frame of the page the server last rendered, or
#'   \code{NULL} when none is loaded.
#' @param payload What the browser sent: a list with \code{token},
#'   \code{loaded}, \code{failed} and \code{wrong}.
#' @param expected_token Token of the page the server last rendered.
#' @param annotator Name of the reviewer.
#' @return A list with \code{status} (\code{"saved"}, \code{"stale"} or
#'   \code{"error"}), \code{message} for the reviewer, \code{n_confirmed},
#'   \code{n_wrong}, \code{n_unavailable}, \code{n_pending} (every draw of
#'   the page left pending) and, among those, \code{n_no_raw} (raw data not
#'   found) and \code{n_load_failed} (image failed in the browser).
#' @noRd
thr_confirm_page <- function(db_path, page, payload, expected_token,
                             annotator) {
  if (is.null(page) || !is.list(payload) || is.null(names(payload))) {
    return(thr_confirm_nothing("stale", paste(
      "No page is waiting for this confirmation. Nothing was recorded;",
      "load a page and confirm it."
    )))
  }
  tryCatch({
    resolved <- thr_resolve_page_verdicts(
      page, payload$loaded, payload$failed, payload$wrong, payload$token,
      expected_token
    )
    if (nrow(page) > 0 && !thr_page_can_confirm(page)) {
      stop("Nothing was recorded: none of the ", nrow(page), " image(s) on ",
           "this page could be shown, because their raw data was not found. ",
           "They stay pending. Make the raw data folder available and press ",
           "\"Load page\" again.", call. = FALSE)
    }
    saved <- thr_save_verdicts(db_path, resolved, annotator, expected_token)
    thr_confirm_saved(resolved, saved)
  },
  thr_stale_page = function(e) thr_confirm_nothing("stale", conditionMessage(e)),
  error = function(e) {
    message <- conditionMessage(e)
    if (!grepl("Nothing was recorded", message, fixed = TRUE)) {
      message <- paste("Nothing was recorded.", message)
    }
    thr_confirm_nothing("error", message)
  })
}

#' Tell the reviewer what an undo did
#'
#' @param n_restored Draws returned to pending.
#' @param class_name Class of the page that was undone.
#' @return One string.
#' @noRd
thr_undo_message <- function(n_restored, class_name) {
  if (n_restored == 0) {
    return("Nothing to undo: the last page has no recorded images left.")
  }
  sprintf(
    paste("Undid the last page of %s: %d image(s) are pending again and are",
          "shown first."),
    class_name, n_restored
  )
}
