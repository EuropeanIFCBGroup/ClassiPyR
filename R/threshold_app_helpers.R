# Display helpers of the threshold review app (run_threshold_app())
#
# Everything the app shows that takes more than wiring an input to an output
# is built here, as plain functions, so it can be tested without a browser:
# how a score is printed, what the pickers offer, the per-bin coverage of a
# class and the gallery of a review page.

# Seed of the Monte Carlo intervals in the app. The Results page and the
# export must show the same numbers, and with the "lower bound" criterion the
# chosen threshold itself depends on the interval: a fresh random draw at
# export time could write a threshold the reviewer never saw.
THR_APP_SEED <- 20260101L

# Page sizes offered in the review, and the one selected at start
THR_APP_PAGE_SIZES <- c(50L, 100L, 200L, 500L)
THR_APP_DEFAULT_PAGE_SIZE <- 100L

# Most decimals a score percentage is printed with
THR_SCORE_MAX_DECIMALS <- 8L

#' Cut a number to a fixed number of decimals, without rounding
#'
#' @param x Numeric vector of non-negative numbers.
#' @param decimals Integer vector of decimals to keep (1 to 8), aligned with
#'   \code{x}.
#' @return Character vector; zeros beyond the first decimal are dropped.
#' @noRd
thr_truncate_decimals <- function(x, decimals) {
  # Two digits more than are ever shown: they absorb the representation error
  # of x (99.95 is stored as 99.9499...) without rounding a shown digit up
  full <- sprintf("%.10f", x)
  cut <- substr(full, 1, nchar(full) - (10L - decimals))
  cut <- sub("(\\.[0-9]*[1-9])0+$", "\\1", cut)
  sub("\\.0+$", ".0", cut)
}

#' Format a score as a percentage
#'
#' Winning scores crowd just below 1, where the score bins are finest (99.9%
#' and 99.99% are bin edges), so the number of decimals grows as the score
#' nears 1: two significant digits of the distance to 100% are kept, and at
#' least one decimal. Digits are cut off, never rounded, so a score is never
#' shown as higher than it is and never appears to sit in the next bin up.
#'
#' @param score Numeric vector of scores between 0 and 1.
#' @return Character vector such as \code{"87.3\%"}, \code{"99.95\%"} or
#'   \code{"99.999\%"}; \code{"100\%"} only for a score of 1, and \code{""}
#'   for a missing score.
#' @noRd
thr_format_score <- function(score) {
  score <- as.numeric(score)
  shown <- rep("", length(score))
  known <- !is.na(score)
  percent <- pmin(pmax(score[known], 0), 1) * 100
  remainder <- 100 - percent
  decimals <- ifelse(remainder > 0,
                     ceiling(-log10(pmax(remainder, 1e-300))) + 1, 1)
  decimals <- as.integer(pmin(pmax(decimals, 1), THR_SCORE_MAX_DECIMALS))
  text <- thr_truncate_decimals(percent, decimals)
  # Closer to 1 than the decimals can show
  overflow <- percent < 100 & suppressWarnings(as.numeric(text)) >= 100
  text[overflow] <- paste0("99.", strrep("9", THR_SCORE_MAX_DECIMALS))
  text[percent >= 100] <- "100"
  shown[known] <- paste0(text, "%")
  shown
}

#' Labels of the score bins
#'
#' @param edges Lower edges of the score bins.
#' @return Character vector with one label per bin, such as
#'   \code{"99.9\% to 99.99\%"}; the last bin runs to 100\%.
#' @noRd
thr_bin_labels <- function(edges) {
  edges <- thr_validate_edges(edges)
  paste(thr_format_score(edges), "to", thr_format_score(c(edges[-1], 1)))
}

#' Choices of the class picker
#'
#' The picker is keyed by the position of a class in the model, never by its
#' label: labels hold slashes, quotes and non-ASCII letters, and must not be
#' used as an identifier anywhere in the page.
#'
#' @param overview Data frame from \code{thr_class_overview()}.
#' @return Named character vector: the names are what is shown,
#'   \code{"<class> (<verdicts>/<predicted>)"}, and the values are the class
#'   positions as text.
#' @noRd
thr_class_choices <- function(overview) {
  thr_check_columns(overview, c("class_name", "n_predicted", "n_verdicts"),
                    "overview")
  labels <- sprintf("%s (%d/%d)", overview$class_name,
                    as.integer(overview$n_verdicts),
                    as.integer(overview$n_predicted))
  stats::setNames(as.character(seq_len(nrow(overview))), labels)
}

#' The class a picker value stands for
#'
#' @param choice Value sent by the class picker.
#' @param class_labels The model's class labels.
#' @return The class label, or \code{NULL} when the value is not the position
#'   of a class.
#' @noRd
thr_class_from_choice <- function(choice, class_labels) {
  if (length(choice) != 1 || is.na(choice)) {
    return(NULL)
  }
  choice <- as.character(choice)
  if (!grepl("^[0-9]{1,9}$", choice)) {
    return(NULL)
  }
  position <- as.integer(choice)
  if (position < 1 || position > length(class_labels)) {
    return(NULL)
  }
  class_labels[position]
}

#' Choices of the model picker
#'
#' @param models Data frame from \code{thr_index_models()}.
#' @return Named character vector: the names describe each model and the
#'   values are the model identifiers.
#' @noRd
thr_model_choices <- function(models) {
  thr_check_columns(models, c("model_id", "classifier_name", "n_files",
                              "n_rois"), "models")
  name <- ifelse(is.na(models$classifier_name) | !nzchar(models$classifier_name),
                 "unnamed model", models$classifier_name)
  labels <- sprintf("%s (%d files, %d ROIs, id %s)", name,
                    as.integer(models$n_files), as.integer(models$n_rois),
                    substr(models$model_id, 1, 8))
  stats::setNames(as.character(models$model_id), labels)
}

#' Review progress of one class per score bin, for display
#'
#' @param counts Data frame from \code{thr_stratum_counts()} for one class.
#' @param edges Lower edges of the score bins the frame was frozen with.
#' @param min_stratum_n Verdicts a bin needs before a threshold at or below
#'   it can be fitted (the whole bin, when it holds fewer predictions).
#' @return Data frame with one row per bin that holds predictions and the
#'   columns \code{Score bin}, \code{Predictions}, \code{Verdicts},
#'   \code{Correct}, \code{Pending}, \code{Unavailable} and
#'   \code{Enough verdicts}.
#' @noRd
thr_coverage_table <- function(counts, edges = THR_SCORE_EDGES,
                               min_stratum_n = THR_MIN_STRATUM_VERDICTS) {
  thr_check_columns(counts, c("bin", "n_population", "n_verdicts", "n_correct",
                              "n_pending", "n_unavailable"), "counts")
  counts <- counts[order(counts$bin), , drop = FALSE]
  needed <- pmin(counts$n_population, min_stratum_n)
  enough <- ifelse(counts$n_verdicts >= needed, "yes",
                   sprintf("no (%d of %d)", as.integer(counts$n_verdicts),
                           as.integer(needed)))
  table <- data.frame(
    bin = thr_bin_labels(edges)[counts$bin],
    Predictions = as.integer(counts$n_population),
    Verdicts = as.integer(counts$n_verdicts),
    Correct = as.integer(counts$n_correct),
    Pending = as.integer(counts$n_pending),
    Unavailable = as.integer(counts$n_unavailable),
    enough = as.character(enough),
    stringsAsFactors = FALSE
  )
  names(table)[c(1, 7)] <- c("Score bin", "Enough verdicts")
  rownames(table) <- NULL
  table
}

#' Can a draw's image be shown?
#'
#' @param row One row of a page.
#' @return \code{TRUE} when the image was extracted and has a file name.
#' @noRd
thr_card_has_image <- function(row) {
  isTRUE(row$extracted) && !is.null(row$png_file) && !is.na(row$png_file)
}

#' One positive whole number of a page row, or NULL
#'
#' @param value A value that may be absent or missing.
#' @return The number, or \code{NULL}, which leaves an HTML attribute out.
#' @noRd
thr_card_pixels <- function(value) {
  if (is.null(value) || length(value) != 1 || is.na(value) || value < 1) {
    return(NULL)
  }
  as.integer(value)
}

#' Gallery card of one draw
#'
#' The card is keyed by its draw id. A draw whose image was extracted gets a
#' lazily loaded image; any other draw gets a greyed card without one, which
#' the browser code will not let the reviewer mark.
#'
#' @param row One row of a page: \code{draw_id}, \code{sample_name},
#'   \code{roi_number}, \code{score}, \code{png_file}, \code{extracted} and,
#'   optionally, \code{width} and \code{height} of the image in pixels.
#' @param resource_prefix Resource path the session's images are served
#'   under.
#' @return A \code{div} tag. Everything taken from \code{row} is escaped.
#' @noRd
thr_gallery_card <- function(row, resource_prefix) {
  has_image <- thr_card_has_image(row)
  picture <- if (has_image) {
    shiny::tags$img(
      src = paste0(resource_prefix, "/", row$png_file), loading = "lazy",
      alt = "", draggable = "false",
      width = thr_card_pixels(row$width), height = thr_card_pixels(row$height)
    )
  }
  shiny::tags$div(
    class = if (has_image) "thr-card" else "thr-card unavailable",
    `data-draw` = as.integer(row$draw_id),
    picture,
    # Shown by the style sheet for a draw without an image, and for an image
    # the browser failed to load
    shiny::tags$div(class = "thr-placeholder", "Image unavailable"),
    shiny::tags$div(
      class = "thr-caption",
      shiny::tags$span(class = "thr-score", thr_format_score(row$score)),
      shiny::tags$br(),
      as.character(row$sample_name),
      shiny::tags$br(),
      paste("ROI", row$roi_number)
    )
  )
}

#' Gallery of a review page
#'
#' @param page Data frame of the page as loaded by
#'   \code{thr_load_review_page()}, or \code{NULL} when none is loaded.
#' @param resource_prefix Resource path the session's images are served
#'   under.
#' @param token Token of the page. The browser sends it back with the
#'   confirmation, so that a confirmation of a page that has been replaced
#'   is recognised.
#' @return A tag: the container \code{#thr-gallery} holding one card per
#'   draw, highest score first, or a notice when there is nothing to show.
#' @noRd
thr_gallery_ui <- function(page, resource_prefix, token) {
  if (is.null(page)) {
    return(shiny::tags$div(
      class = "alert alert-info",
      "No page loaded. Choose a class and press \"Load page\"."
    ))
  }
  thr_check_columns(page, c("draw_id", "sample_name", "roi_number", "score",
                            "png_file", "extracted"), "page")
  if (nrow(page) == 0) {
    return(shiny::tags$div(
      id = "thr-gallery", class = "thr-gallery", `data-token` = token,
      shiny::tags$div(
        class = "alert alert-success",
        paste("Nothing left to review for this class: every prediction of it",
              "that can be drawn has been drawn and resolved.")
      )
    ))
  }
  page <- page[order(-page$score, page$draw_id), , drop = FALSE]
  cards <- lapply(seq_len(nrow(page)), function(i) {
    thr_gallery_card(page[i, , drop = FALSE], resource_prefix)
  })
  shiny::tags$div(id = "thr-gallery", class = "thr-gallery",
                  `data-token` = token, cards)
}

#' One line describing a loaded page
#'
#' @param page Data frame of the page, or \code{NULL}.
#' @param class_name Class the page was loaded for.
#' @return One string; empty when no page is loaded.
#' @noRd
thr_page_caption <- function(page, class_name) {
  if (is.null(page)) {
    return("")
  }
  n_missing <- sum(!page$extracted)
  n_seen <- if (is.null(page$is_new)) 0L else sum(!page$is_new)
  parts <- c(
    sprintf("%d images predicted as %s", nrow(page), class_name),
    if (n_missing > 0) sprintf("%d without an image", n_missing),
    if (n_seen > 0) sprintf("%d shown before and not yet resolved", n_seen)
  )
  paste0(paste(parts, collapse = "; "), ".")
}

#' Run a step of the app, showing an error instead of ending the session
#'
#' An error inside a Shiny observer closes the session, and with it the page
#' a reviewer is working on. Every step that reads or writes files or the
#' databases therefore runs through this function.
#'
#' @param expr Expression to evaluate.
#' @param default Value returned when \code{expr} fails.
#' @param notify Function called with the error message, the notification
#'   type and its duration.
#' @return The value of \code{expr}, or \code{default} after an error.
#' @noRd
thr_app_try <- function(expr, default = NULL,
                        notify = shiny::showNotification) {
  tryCatch(expr, error = function(e) {
    notify(conditionMessage(e), type = "error", duration = NULL)
    default
  })
}
