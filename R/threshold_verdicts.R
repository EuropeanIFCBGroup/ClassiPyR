# Verdicts of the threshold review app
#
# A reviewer marks the images on a page that are not the predicted class and
# confirms the page. What the browser sends back is turned into verdicts here,
# in one pure function, so the rules for what counts as a verdict live in a
# single place and can be tested without a browser or a database.

# Recorded when no annotator name was given, as in the main app
THR_UNKNOWN_ANNOTATOR <- "Unknown"

# Counter that keeps page tokens of one R session apart
thr_token_state <- new.env(parent = emptyenv())

#' Make a token identifying one rendered page
#'
#' The token is not built from R's random number generator: after a
#' \code{set.seed()} the tokens of two sessions would repeat, and undoing a
#' page could then undo somebody else's.
#'
#' @return One string of 32 hexadecimal characters, different on every call.
#' @noRd
thr_new_page_token <- function() {
  count <- get0("count", envir = thr_token_state, inherits = FALSE,
                ifnotfound = 0) + 1
  assign("count", count, envir = thr_token_state)
  thr_string_md5(paste(
    format(Sys.time(), "%Y%m%d%H%M%OS6"), Sys.getpid(), count,
    basename(tempfile("")), sep = "|"
  ))
}

#' Signal that a confirmation belongs to a page that is no longer current
#'
#' @return Does not return; signals an error of class \code{thr_stale_page}.
#' @noRd
thr_stop_stale_page <- function() {
  stop(structure(
    class = c("thr_stale_page", "error", "condition"),
    list(message = paste("This confirmation belongs to a page that is no",
                         "longer the current one. Nothing was recorded;",
                         "review the page now shown and confirm again."),
         call = NULL)
  ))
}

#' Coerce draw ids reported by the browser
#'
#' JavaScript arrays arrive as numeric or character vectors or as lists,
#' depending on their content, so nothing about their type is taken on trust.
#'
#' @param ids Draw ids as received.
#' @param arg Name of the argument, for the error message.
#' @return Integer vector of distinct draw ids.
#' @noRd
thr_as_draw_ids <- function(ids, arg) {
  fail <- function() {
    stop("`", arg, "` must hold the draw ids of images on the page (whole ",
         "numbers). The page may be out of date; reload it.", call. = FALSE)
  }
  if (length(ids) == 0) {
    return(integer())
  }
  if (is.list(ids)) {
    if (!all(lengths(ids) == 1) || any(vapply(ids, is.list, logical(1)))) {
      fail()
    }
    ids <- unlist(ids, use.names = FALSE)
  }
  if (is.character(ids)) {
    if (anyNA(ids) || !all(grepl("^[0-9]{1,9}$", ids))) {
      fail()
    }
    ids <- as.numeric(ids)
  }
  valid <- is.numeric(ids) && !anyNA(ids) && all(is.finite(ids)) &&
    all(ids >= 1 & ids == round(ids) & ids <= .Machine$integer.max)
  if (!valid) {
    fail()
  }
  unique(as.integer(ids))
}

#' Check the page a confirmation refers to
#'
#' @param page Data frame with \code{draw_id}, \code{extracted} and, when
#'   some draw has no image, \code{no_image_reason}.
#' @return Integer vector of the page's draw ids.
#' @noRd
thr_check_page <- function(page) {
  thr_check_columns(page, c("draw_id", "extracted"), "page")
  ids <- page$draw_id
  valid_ids <- is.numeric(ids) && !anyNA(ids) && all(ids == round(ids)) &&
    anyDuplicated(ids) == 0
  if (!valid_ids) {
    stop("`page$draw_id` must hold distinct whole numbers without missing ",
         "values.", call. = FALSE)
  }
  if (!is.logical(page$extracted) || anyNA(page$extracted)) {
    stop("`page$extracted` must be TRUE or FALSE for every draw: whether its ",
         "image was extracted on the server.", call. = FALSE)
  }
  thr_check_no_image_reason(page)
  as.integer(ids)
}

#' Stop unless every draw without an image says why it has none
#'
#' What becomes of such a draw depends on the reason, so a page that does not
#' give one is refused rather than resolved on a guess: a raw data folder
#' that is away for a moment must never be taken for a ROI without an image.
#'
#' @param page Data frame with \code{extracted} (logical, checked).
#' @return \code{NULL}, invisibly.
#' @noRd
thr_check_no_image_reason <- function(page) {
  missing_image <- !page$extracted
  if (!any(missing_image)) {
    return(invisible(NULL))
  }
  reason <- page$no_image_reason[missing_image]
  if (is.null(reason) || !all(reason %in% THR_NO_IMAGE_REASONS)) {
    stop("`page$no_image_reason` must say why each draw without an image has ",
         "none: one of ", paste(THR_NO_IMAGE_REASONS, collapse = ", "),
         " (see thr_extract_page_images()).", call. = FALSE)
  }
  invisible(NULL)
}

#' Which draws without an image can never have one?
#'
#' Only these are retired. The raw file was there and was read, and the ROI
#' could not be made from it (or the draw has no usable sample name or ROI
#' number), so showing the draw again would change nothing. A draw whose raw
#' file was not found is not among them: the folder may be back tomorrow.
#'
#' @param page Data frame of a checked page.
#' @return Logical vector aligned with the rows of \code{page}.
#' @noRd
thr_draw_is_unavailable <- function(page) {
  if (is.null(page$no_image_reason)) {
    return(rep(FALSE, nrow(page)))
  }
  !page$extracted &
    page$no_image_reason %in% c(THR_NO_IMAGE_EXTRACT, THR_NO_IMAGE_INVALID)
}

#' Turn a page confirmation into verdicts
#'
#' The single place the confirmation rules live. A draw gets a verdict only
#' when its image was extracted on the server and the browser reported it as
#' loaded and not as failed: 0 when it was marked wrong, 1 otherwise.
#'
#' A draw is unavailable, which is final, only when its image can never be
#' made: the raw file was read and did not give the ROI (see
#' \code{thr_draw_is_unavailable()}). Everything that may be different next
#' time stays pending and comes back on a later page: a draw whose raw file
#' was not found, an image the browser failed to load, and an image that was
#' extracted but reported neither way.
#'
#' Nothing is resolved when the confirmation is not for the current page, or
#' when an image is marked wrong that is not on the page or cannot have a
#' verdict: both mean the browser and the server disagree about what was
#' shown.
#'
#' @param page Data frame of the page as it was rendered: \code{draw_id},
#'   \code{extracted} (logical) and \code{no_image_reason} (why a draw that
#'   was not extracted has no image, see \code{thr_extract_page_images()};
#'   may be left out when every draw was extracted).
#' @param loaded,failed Draw ids the browser reported as loaded and as
#'   failed. Ids that are not on the page are ignored.
#' @param wrong Draw ids the reviewer marked as not being the class.
#' @param token Page token sent with the confirmation.
#' @param expected_token Token of the page the server last rendered.
#' @return A list with \code{verdicts} (data frame with \code{draw_id} and
#'   \code{verdict}, 1 correct and 0 wrong), \code{unavailable} and
#'   \code{still_pending} (integer vectors of draw ids). Every draw of the
#'   page is in exactly one of the three. \code{no_raw} and
#'   \code{load_failed} name the draws among \code{still_pending} that are
#'   pending because their raw file was not found, or because the browser
#'   failed to load their image. A token mismatch signals an error of class
#'   \code{thr_stale_page}.
#' @noRd
thr_resolve_page_verdicts <- function(page, loaded, failed, wrong, token,
                                      expected_token) {
  current <- thr_is_single_string(token) &&
    thr_is_single_string(expected_token) && identical(token, expected_token)
  if (!current) {
    thr_stop_stale_page()
  }
  page_ids <- thr_check_page(page)
  loaded <- thr_as_draw_ids(loaded, "loaded")
  failed <- thr_as_draw_ids(failed, "failed")
  wrong <- thr_as_draw_ids(wrong, "wrong")

  # An image reported both ways was not reliably seen
  did_fail <- page$extracted & page_ids %in% failed
  has_verdict <- page$extracted & page_ids %in% loaded & !did_fail
  unavailable <- thr_draw_is_unavailable(page)
  thr_check_wrong_marks(wrong, page_ids, has_verdict)

  verdict_ids <- page_ids[has_verdict]
  list(
    verdicts = data.frame(draw_id = verdict_ids,
                          verdict = as.integer(!(verdict_ids %in% wrong))),
    unavailable = page_ids[unavailable],
    still_pending = page_ids[!has_verdict & !unavailable],
    no_raw = page_ids[!page$extracted & !unavailable],
    load_failed = page_ids[did_fail]
  )
}

#' Stop when an image is marked wrong that cannot have a verdict
#'
#' @param wrong Draw ids marked wrong.
#' @param page_ids Draw ids of the page.
#' @param has_verdict Logical vector aligned with \code{page_ids}.
#' @return \code{NULL}, invisibly.
#' @noRd
thr_check_wrong_marks <- function(wrong, page_ids, has_verdict) {
  off_page <- setdiff(wrong, page_ids)
  if (length(off_page) > 0) {
    stop(length(off_page), " image(s) marked wrong are not on the page ",
         "(draw ", paste(utils::head(off_page, 5), collapse = ", "),
         "). Nothing was recorded; reload the page.", call. = FALSE)
  }
  unseen <- setdiff(wrong, page_ids[has_verdict])
  if (length(unseen) > 0) {
    stop(length(unseen), " image(s) marked wrong cannot have a verdict ",
         "because their image was not extracted or did not load (draw ",
         paste(utils::head(unseen, 5), collapse = ", "),
         "). Nothing was recorded; reload the page.", call. = FALSE)
  }
  invisible(NULL)
}

#' Check resolved verdicts before they are saved
#'
#' @param resolved A list from \code{thr_resolve_page_verdicts()}.
#' @return A list with \code{draw_id}, \code{verdict} and \code{unavailable}
#'   as integer vectors.
#' @noRd
thr_check_resolved <- function(resolved) {
  shaped <- is.list(resolved) && !is.data.frame(resolved) &&
    all(c("verdicts", "unavailable") %in% names(resolved))
  if (!shaped) {
    stop("`resolved` must be the list returned by ",
         "thr_resolve_page_verdicts().", call. = FALSE)
  }
  thr_check_columns(resolved$verdicts, c("draw_id", "verdict"),
                    "resolved$verdicts")
  verdict <- resolved$verdicts$verdict
  if (!is.numeric(verdict) || anyNA(verdict) || !all(verdict %in% c(0, 1))) {
    stop("`resolved$verdicts$verdict` must be 1 (correct) or 0 (wrong) for ",
         "every draw.", call. = FALSE)
  }
  draw_id <- thr_as_draw_ids(resolved$verdicts$draw_id, "resolved$verdicts$draw_id")
  unavailable <- thr_as_draw_ids(resolved$unavailable, "resolved$unavailable")
  if (length(draw_id) != length(verdict)) {
    stop("`resolved$verdicts` lists a draw more than once.", call. = FALSE)
  }
  if (length(intersect(draw_id, unavailable)) > 0) {
    stop("`resolved` lists a draw both with a verdict and as unavailable.",
         call. = FALSE)
  }
  list(draw_id = draw_id, verdict = as.integer(verdict),
       unavailable = unavailable)
}

#' Clean an annotator name
#'
#' @param annotator Annotator name, possibly empty or \code{NULL}.
#' @return The trimmed name, or \code{"Unknown"} when none was given.
#' @noRd
thr_clean_annotator <- function(annotator) {
  if (is.null(annotator) || (length(annotator) == 1 && is.na(annotator))) {
    return(THR_UNKNOWN_ANNOTATOR)
  }
  if (!is.character(annotator) || length(annotator) != 1) {
    stop("`annotator` must be a single name.", call. = FALSE)
  }
  annotator <- trimws(enc2utf8(annotator))
  if (nzchar(annotator)) annotator else THR_UNKNOWN_ANNOTATOR
}

#' Stop unless a page token is usable
#'
#' @param page_token Page token.
#' @return \code{NULL}, invisibly.
#' @noRd
thr_check_page_token <- function(page_token) {
  if (!thr_is_single_string(page_token)) {
    stop("`page_token` must be the token of the confirmed page, from ",
         "thr_new_page_token().", call. = FALSE)
  }
  invisible(NULL)
}

#' Record the verdicts of a confirmed page
#'
#' One transaction, and only draws that are still pending are changed: a page
#' submitted twice is written once, and a verdict is never overwritten.
#'
#' @param db_path Path to the verdict SQLite file.
#' @param resolved A list from \code{thr_resolve_page_verdicts()}.
#' @param annotator Name of the reviewer; \code{"Unknown"} is recorded when
#'   it is empty.
#' @param page_token Token of the confirmed page, which \code{thr_undo_page()}
#'   takes to undo it.
#' @return A list with \code{n_confirmed} and \code{n_unavailable}: the draws
#'   this call changed.
#' @noRd
thr_save_verdicts <- function(db_path, resolved, annotator, page_token) {
  resolved <- thr_check_resolved(resolved)
  thr_check_page_token(page_token)
  stamp <- list(annotator = thr_clean_annotator(annotator),
                confirmed_at = thr_now(), page_token = page_token)
  con <- thr_connect_verdicts(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  thr_write_transaction(con, function() {
    list(
      n_confirmed = thr_mark_confirmed(con, resolved$draw_id, resolved$verdict,
                                       stamp),
      n_unavailable = thr_mark_unavailable(con, resolved$unavailable, stamp)
    )
  })
}

#' Store verdicts on pending draws
#'
#' @param con A DBI connection to the verdict SQLite file.
#' @param draw_id Integer vector of draw ids.
#' @param verdict Integer vector of verdicts, aligned with \code{draw_id}.
#' @param stamp List with \code{annotator}, \code{confirmed_at} and
#'   \code{page_token}.
#' @return Number of draws changed.
#' @noRd
thr_mark_confirmed <- function(con, draw_id, verdict, stamp) {
  n_draws <- length(draw_id)
  if (n_draws == 0) {
    return(0L)
  }
  as.integer(DBI::dbExecute(con, "
    UPDATE draws SET status = 'confirmed', verdict = ?, annotator = ?,
                     confirmed_at = ?, page_token = ?
    WHERE draw_id = ? AND status = 'pending'",
    params = list(verdict, rep(stamp$annotator, n_draws),
                  rep(stamp$confirmed_at, n_draws),
                  rep(stamp$page_token, n_draws), draw_id)))
}

#' Mark pending draws as unavailable
#'
#' @inheritParams thr_mark_confirmed
#' @return Number of draws changed.
#' @noRd
thr_mark_unavailable <- function(con, draw_id, stamp) {
  n_draws <- length(draw_id)
  if (n_draws == 0) {
    return(0L)
  }
  as.integer(DBI::dbExecute(con, "
    UPDATE draws SET status = 'unavailable', verdict = NULL, annotator = ?,
                     confirmed_at = ?, page_token = ?
    WHERE draw_id = ? AND status = 'pending'",
    params = list(rep(stamp$annotator, n_draws),
                  rep(stamp$confirmed_at, n_draws),
                  rep(stamp$page_token, n_draws), draw_id)))
}

#' Undo a confirmed page
#'
#' Every draw the page resolved, whether to a verdict or to unavailable, goes
#' back to pending and loses its verdict, annotator, time and token, so the
#' next page for the class shows the same draws again.
#'
#' @param db_path Path to the verdict SQLite file.
#' @param page_token Token the page was saved with.
#' @return Number of draws returned to pending; 0 when no page has that token.
#' @noRd
thr_undo_page <- function(db_path, page_token) {
  thr_check_page_token(page_token)
  con <- thr_connect_verdicts(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  thr_write_transaction(con, function() {
    as.integer(DBI::dbExecute(con, "
      UPDATE draws SET status = 'pending', verdict = NULL, annotator = NULL,
                       confirmed_at = NULL, page_token = NULL
      WHERE page_token = ? AND status IN ('confirmed', 'unavailable')",
      params = list(page_token)))
  })
}
