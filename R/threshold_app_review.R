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
#' raw data folder, which must not be repeated for every page.
#'
#' @param db_path Path to the verdict SQLite file.
#' @param model_id Model identifier.
#' @param roi_folder Raw data folder.
#' @return Named character vector, see \code{thr_resolve_roi_paths()}.
#' @noRd
thr_frame_roi_paths <- function(db_path, model_id, roi_folder) {
  thr_resolve_roi_paths(thr_frame_samples_db(db_path, model_id), roi_folder)
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
#'   \code{width} and \code{height}; zero rows when the class is exhausted),
#'   \code{token} and \code{class_name}.
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
       n_unavailable = 0L, n_pending = 0L)
}

#' Tell the reviewer what a confirmation recorded
#'
#' @param n_confirmed Verdicts written.
#' @param n_wrong How many of the page's verdicts were "wrong".
#' @param n_unavailable Draws marked unavailable.
#' @param n_pending Draws left pending.
#' @param n_elsewhere Draws the page resolved that had already been resolved,
#'   by an earlier submit or another session.
#' @return One string.
#' @noRd
thr_confirm_message <- function(n_confirmed, n_wrong, n_unavailable, n_pending,
                                n_elsewhere = 0L) {
  paste(c(
    sprintf("Recorded %d verdict(s), %d marked wrong.", n_confirmed, n_wrong),
    if (n_unavailable > 0) sprintf(
      "%d image(s) were unavailable and are left out of the estimates.",
      n_unavailable
    ),
    if (n_pending > 0) sprintf(
      "%d image(s) had not been viewed and will come back on a later page.",
      n_pending
    ),
    if (n_elsewhere > 0) sprintf(
      "%d image(s) had already been resolved and were not changed.",
      n_elsewhere
    )
  ), collapse = " ")
}

#' Record the confirmation of a review page
#'
#' Turns what the browser sent into verdicts with
#' \code{thr_resolve_page_verdicts()} and stores them with
#' \code{thr_save_verdicts()}. Nothing is written when the confirmation
#' belongs to a page that is no longer the current one, or when the browser
#' and the server disagree about what was shown; the result then says why.
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
#'   \code{n_wrong}, \code{n_unavailable} and \code{n_pending}.
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
    saved <- thr_save_verdicts(db_path, resolved, annotator, expected_token)
    n_resolved <- nrow(resolved$verdicts) + length(resolved$unavailable)
    n_elsewhere <- n_resolved - saved$n_confirmed - saved$n_unavailable
    n_wrong <- sum(resolved$verdicts$verdict == 0)
    list(
      status = "saved",
      message = thr_confirm_message(saved$n_confirmed, n_wrong,
                                    saved$n_unavailable,
                                    length(resolved$still_pending),
                                    n_elsewhere),
      n_confirmed = saved$n_confirmed, n_wrong = as.integer(n_wrong),
      n_unavailable = saved$n_unavailable,
      n_pending = length(resolved$still_pending)
    )
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
