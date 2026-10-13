# Evidence status, threshold choice and per-class resolution of the threshold
# review app (run_threshold_app()).
#
# The choice is deliberately conservative: a threshold is only fitted where
# the review supports it, and every other class keeps the threshold the model
# already has. The `reason` strings built here are shown to the reviewer.

# Slack when comparing an estimate with the target, so that a precision that
# is exactly on target is not lost to the rounding of a weighted sum.
THR_TARGET_TOLERANCE <- 1e-9

#' Format a share as a percentage for a message
#'
#' @param x Numeric share between 0 and 1.
#' @return Character, such as \code{"87.5\%"}.
#' @noRd
thr_format_share <- function(x) {
  ifelse(is.na(x), "unknown", sprintf("%.1f%%", 100 * x))
}

#' Format a threshold for a message
#'
#' @param x Numeric threshold.
#' @return Character with up to six significant digits, such as
#'   \code{"0.9999"}.
#' @noRd
thr_format_threshold <- function(x) {
  sprintf("%.6g", x)
}

#' Check a target precision
#'
#' @param target Target precision.
#' @return \code{NULL}, invisibly; called for its errors.
#' @noRd
thr_check_target <- function(target) {
  ok <- is.numeric(target) && length(target) == 1 && !is.na(target) &&
    target > 0 && target <= 1
  if (!ok) {
    stop("`target` must be a single precision above 0 and at most 1, such ",
         "as 0.9.", call. = FALSE)
  }
  invisible(NULL)
}

#' Count the predictions of a class that can be reviewed
#'
#' @param strata_counts Data frame of one class with the column
#'   \code{n_population} and optionally \code{n_unavailable}, the predictions
#'   whose image could not be shown (taken as 0 when absent).
#' @return The number of predictions that have or can still get a verdict.
#' @noRd
thr_n_reviewable <- function(strata_counts) {
  n_unavailable <- if ("n_unavailable" %in% names(strata_counts)) {
    sum(strata_counts$n_unavailable)
  } else {
    0
  }
  sum(strata_counts$n_population) - n_unavailable
}

#' Classify how much review evidence a class has
#'
#' A class needs \code{min_class_n} verdicts, or a verdict on every prediction
#' that can be reviewed when those are fewer: an image that could not be shown
#' is never drawn again, so it must not keep the class waiting.
#'
#' @param strata_counts Data frame of one class with the columns
#'   \code{n_population} and \code{n_verdicts} (zero rows for a class the
#'   model never predicted), and optionally \code{n_unavailable} (taken as 0
#'   when absent).
#' @param min_class_n Predictions and verdicts a class needs before a
#'   threshold is fitted.
#' @return One of \code{"too_few_predictions"} (fewer than \code{min_class_n}
#'   predictions, so no amount of review would be enough),
#'   \code{"not_reviewed"}, \code{"under_reviewed"} or \code{"sufficient"}.
#' @noRd
thr_evidence_status <- function(strata_counts,
                                min_class_n = THR_MIN_CLASS_VERDICTS) {
  thr_check_columns(strata_counts, c("n_population", "n_verdicts"),
                    "strata_counts")
  if (!is.numeric(min_class_n) || length(min_class_n) != 1 ||
      is.na(min_class_n) || min_class_n < 1) {
    stop("`min_class_n` must be a single number of 1 or more.", call. = FALSE)
  }
  n_predicted <- sum(strata_counts$n_population)
  n_verdicts <- sum(strata_counts$n_verdicts)
  if (n_predicted < min_class_n) {
    return("too_few_predictions")
  }
  if (n_verdicts == 0) {
    return("not_reviewed")
  }
  if (n_verdicts < min(min_class_n, thr_n_reviewable(strata_counts))) {
    return("under_reviewed")
  }
  "sufficient"
}

#' Check a precision curve and order it by threshold
#'
#' @param curve Data frame from \code{\link{estimate_precision_curve}}.
#' @return The curve ordered by increasing threshold.
#' @noRd
thr_check_curve <- function(curve) {
  thr_check_columns(
    curve,
    c("threshold", "n_population_kept", "kept_share", "precision", "lower",
      "upper", "supported"),
    "curve"
  )
  if (nrow(curve) == 0) {
    stop("`curve` has no rows. Pass the result of ",
         "estimate_precision_curve().", call. = FALSE)
  }
  if (!is.logical(curve$supported) || anyNA(curve$supported) ||
      anyNA(curve$threshold) || anyNA(curve$n_population_kept)) {
    stop("`curve` must have a TRUE/FALSE `supported` column and no missing ",
         "`threshold` or `n_population_kept` values.", call. = FALSE)
  }
  curve <- curve[order(curve$threshold), , drop = FALSE]
  rownames(curve) <- NULL
  curve
}

#' Build the result of a threshold choice
#'
#' @param status Status of the choice.
#' @param reason Explanation shown to the reviewer.
#' @param row One-row data frame of the chosen edge, or \code{NULL} when no
#'   threshold was chosen.
#' @return List with \code{threshold}, \code{status}, \code{reason},
#'   \code{precision}, \code{lower}, \code{upper} and \code{kept_share}.
#' @noRd
thr_choice_result <- function(status, reason, row = NULL) {
  value <- function(column) {
    if (is.null(row)) NA_real_ else as.numeric(row[[column]])
  }
  list(
    threshold = value("threshold"),
    status = status,
    reason = reason,
    precision = value("precision"),
    lower = value("lower"),
    upper = value("upper"),
    kept_share = value("kept_share")
  )
}

#' Explain a fitted threshold
#'
#' @param curve Checked precision curve.
#' @param metric The curve column the target is applied to.
#' @param candidates Row numbers of the supported edges that keep
#'   predictions, in increasing order.
#' @param meets Whether each candidate meets the target.
#' @param first Position in \code{candidates} of the chosen edge.
#' @param target Target precision.
#' @param label How the metric is named in a sentence.
#' @return A single string.
#' @noRd
thr_fitted_reason <- function(curve, metric, candidates, meets, first, target,
                              label) {
  chosen <- candidates[first]
  if (chosen == 1) {
    head <- sprintf(
      "No cut needed: %s is %s over all predictions, which meets the target of %s.",
      label, thr_format_share(metric[chosen]), thr_format_share(target)
    )
  } else {
    head <- sprintf(
      paste("Lowest threshold that meets the target of %s: %s is %s for",
            "scores of %s and above, which keeps %s of the predictions."),
      thr_format_share(target), label, thr_format_share(metric[chosen]),
      thr_format_threshold(curve$threshold[chosen]),
      thr_format_share(curve$kept_share[chosen])
    )
  }
  if (first > 1) {
    below <- candidates[first - 1]
    tail <- sprintf(" At %s it is %s.",
                    thr_format_threshold(curve$threshold[below]),
                    thr_format_share(metric[below]))
    if (any(meets[seq_len(first - 1)])) {
      tail <- paste0(tail, " Some lower thresholds meet the target again, ",
                     "but it is missed in between, so they are not used.")
    }
    return(paste0(head, tail))
  }
  if (chosen > 1) {
    return(paste0(
      head, " Lower thresholds could not be assessed because score bins ",
      "below ", thr_format_threshold(curve$threshold[chosen]),
      " have too few verdicts; reviewing more images of this class may ",
      "allow a lower threshold."
    ))
  }
  head
}

#' Edges of a precision curve that a threshold can be chosen from
#'
#' An edge has to be supported and keep predictions. One that keeps fewer
#' than \code{min_kept} is left out as well, so that a handful of predictions
#' at the very top of the score range neither blocks every threshold below
#' them nor becomes the threshold. When no edge keeps that many, the class is
#' small and all of its supported edges are used.
#'
#' @param curve Checked precision curve.
#' @param min_kept Predictions an edge has to keep.
#' @return Row numbers of the candidate edges, in increasing order.
#' @noRd
thr_choice_candidates <- function(curve, min_kept) {
  if (!is.numeric(min_kept) || length(min_kept) != 1 || is.na(min_kept) ||
      min_kept < 1) {
    stop("`min_kept` must be a single number of 1 or more.", call. = FALSE)
  }
  assessable <- curve$supported & curve$n_population_kept > 0
  weighty <- assessable & curve$n_population_kept >= min_kept
  which(if (any(weighty)) weighty else assessable)
}

#' Choose a threshold for one class from its precision curve
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Picks the lowest candidate threshold that meets a target precision, using
#' only the thresholds the review evidence supports.
#'
#' @details
#' Only edges that are `supported` and keep at least `min_kept` predictions
#' are considered. The precision at an edge that keeps only a handful of
#' predictions rests on those few images, so such an edge is neither chosen
#' nor allowed to rule out the thresholds below it; when no edge of the class
#' keeps `min_kept` predictions, every supported edge that keeps any is
#' considered instead.
#'
#' The chosen threshold is the lowest of these such that it **and every
#' higher one** meets the target. A dip below the target therefore blocks all
#' thresholds below it, even ones where the precision recovers. The precision
#' at an edge already covers everything that edge keeps, so the lower
#' threshold would meet the target too; it is passed over as a precaution,
#' because a curve that falls and rises again as the score increases is a
#' sign that the scores of the class do not order its predictions well.
#'
#' The result is one of:
#'
#' * `"fitted"`: a threshold was chosen. `reason` says what precision it
#'   reaches and what stopped a lower one: the precision at the next edge
#'   down, or too few verdicts in the score bins below. A threshold of 0
#'   means no cut is needed.
#' * `"target_unreachable"`: the highest edge that can be assessed misses
#'   the target. `reason` says how close it gets.
#' * `"under_reviewed"`: no edge can be assessed yet, because a score bin at
#'   or above every candidate has too few verdicts (or the class has no
#'   predictions).
#'
#' This function looks only at the curve. Whether the class has enough
#' verdicts overall to be fitted at all is decided separately.
#'
#' @param curve Data frame from [estimate_precision_curve()].
#' @param target Target precision, above 0 and at most 1.
#' @param criterion What has to meet the target: `"estimate"`, the estimated
#'   precision (the default), or `"lower"`, the lower bound of its interval,
#'   which is stricter.
#' @param min_kept Predictions an edge has to keep to be considered. Use 1
#'   to consider every supported edge that keeps a prediction.
#' @return A list with:
#'   \describe{
#'     \item{threshold}{The chosen threshold, or `NA` when none was chosen.}
#'     \item{status}{`"fitted"`, `"target_unreachable"` or
#'       `"under_reviewed"`.}
#'     \item{reason}{A sentence explaining the outcome, written for the
#'       reviewer.}
#'     \item{precision, lower, upper, kept_share}{The curve values at the
#'       chosen threshold, or `NA`.}
#'   }
#' @export
#' @seealso [estimate_precision_curve()]
#' @examples
#' counts <- data.frame(
#'   bin = 1:3,
#'   n_population = c(100, 50, 50),
#'   n_verdicts = c(20, 20, 20),
#'   n_correct = c(10, 16, 19)
#' )
#' curve <- estimate_precision_curve(counts, edges = c(0, 0.5, 0.9), seed = 1)
#'
#' choice <- choose_class_threshold(curve, target = 0.85)
#' choice$threshold
#' choice$reason
#'
#' # Requiring the lower bound to meet the target is stricter
#' choose_class_threshold(curve, target = 0.85, criterion = "lower")$status
#' @md
choose_class_threshold <- function(curve,
                                   target = THR_DEFAULT_TARGET_PRECISION,
                                   criterion = c("estimate", "lower"),
                                   min_kept = THR_MIN_STRATUM_VERDICTS) {
  criterion <- match.arg(criterion)
  thr_check_target(target)
  curve <- thr_check_curve(curve)
  metric <- if (criterion == "estimate") curve$precision else curve$lower
  label <- if (criterion == "estimate") {
    "estimated precision"
  } else {
    "the lower bound of precision"
  }

  candidates <- thr_choice_candidates(curve, min_kept)
  if (length(candidates) == 0) {
    return(thr_choice_result("under_reviewed", thr_unassessed_reason(curve)))
  }
  # A supported edge without an estimate should not exist; if one turns up it
  # is treated as missing the target, never as passing.
  meets <- !is.na(metric[candidates]) &
    metric[candidates] >= target - THR_TARGET_TOLERANCE
  top <- candidates[length(candidates)]
  if (!meets[length(meets)]) {
    reason <- sprintf(
      paste("Target not reached: at %s, the highest threshold that can be",
            "assessed, %s is %s, below the target of %s."),
      thr_format_threshold(curve$threshold[top]), label,
      thr_format_share(metric[top]), thr_format_share(target)
    )
    return(thr_choice_result("target_unreachable", reason))
  }
  first <- max(c(0L, which(!meets))) + 1L
  reason <- thr_fitted_reason(curve, metric, candidates, meets, first, target,
                              label)
  thr_choice_result("fitted", reason, curve[candidates[first], , drop = FALSE])
}

#' Explain why no threshold of a curve can be assessed
#'
#' Asking for more review is sound for a curve of
#' \code{\link{estimate_precision_curve}}: a score bin is only short of
#' verdicts while it holds images that can still be reviewed, and a bin in
#' which nothing can be reviewed does not hold back the edges below it. The
#' one exception is a class in which no image at all can be reviewed, which
#' the curve cannot tell from a class nobody has reviewed;
#' \code{\link{thr_fit_class}} has the counts and words that case itself.
#'
#' @param curve Checked precision curve.
#' @return A single string.
#' @noRd
thr_unassessed_reason <- function(curve) {
  if (all(curve$n_population_kept == 0)) {
    return("This class has no predictions in the review set, so there is nothing to fit.")
  }
  paste("No threshold can be assessed yet: every candidate threshold still",
        "has a score bin at or above it with too few verdicts. Review more",
        "images of this class.")
}

#' Explain why a class has too little evidence to be fitted
#'
#' @param evidence Status from \code{\link{thr_evidence_status}}, other than
#'   \code{"sufficient"}.
#' @param counts Stratum counts of the class.
#' @param min_class_n Predictions and verdicts a class needs to be fitted.
#' @return A single string. It asks for more review only when images are
#'   left that can be reviewed.
#' @noRd
thr_evidence_reason <- function(evidence, counts, min_class_n) {
  n_predicted <- sum(counts$n_population)
  n_reviewable <- thr_n_reviewable(counts)
  if (evidence == "too_few_predictions") {
    if (n_predicted == 0) {
      return("No predictions of this class in the review set.")
    }
    return(sprintf(paste("Only %d predictions of this class in the review",
                         "set; at least %d are needed to fit a threshold."),
                   n_predicted, min_class_n))
  }
  if (evidence == "not_reviewed") {
    if (n_reviewable > 0) {
      return("No images of this class have been reviewed yet.")
    }
    return(sprintf(paste("None of the %d predictions of this class can be",
                         "reviewed, because their images could not be shown."),
                   n_predicted))
  }
  if (n_reviewable >= min_class_n) {
    return(sprintf(
      "%d verdicts so far; at least %d are needed before a threshold is fitted.",
      sum(counts$n_verdicts), min_class_n
    ))
  }
  sprintf(paste("%d verdicts so far; all %d predictions whose image can be",
                "shown need one before a threshold is fitted."),
          sum(counts$n_verdicts), n_reviewable)
}

#' Fit one class: evidence check, then threshold choice
#'
#' @param counts Stratum counts of the class, with or without the column
#'   \code{n_unavailable}.
#' @param curve Its precision curve.
#' @inheritParams thr_resolve_thresholds
#' @return List with \code{status}, \code{reason} and \code{threshold}
#'   (\code{NA} unless the status is \code{"fitted"}).
#' @noRd
thr_fit_class <- function(counts, curve, target, criterion, min_class_n) {
  evidence <- thr_evidence_status(counts, min_class_n)
  if (evidence == "sufficient") {
    choice <- choose_class_threshold(curve, target, criterion)
    return(choice[c("status", "reason", "threshold")])
  }
  list(status = evidence,
       reason = thr_evidence_reason(evidence, counts, min_class_n),
       threshold = NA_real_)
}

#' Decide the final threshold of one class
#'
#' An override wins over everything; otherwise a fitted threshold is used;
#' otherwise the model's current threshold is kept, or 0 when it has none
#' (under \code{score >= threshold} that keeps every prediction, exactly like
#' having no threshold).
#'
#' @param fit List from \code{\link{thr_fit_class}}.
#' @param fallback The model's current threshold, or \code{NA}.
#' @param override Manual threshold, or \code{NA}.
#' @param edges The candidate thresholds of the precision curve.
#' @return List with \code{threshold}, \code{source}, \code{status} and
#'   \code{reason}.
#' @noRd
thr_final_threshold <- function(fit, fallback, override, edges) {
  threshold <- if (!is.na(override)) {
    override
  } else if (fit$status == "fitted") {
    fit$threshold
  } else if (is.na(fallback)) {
    0
  } else {
    fallback
  }
  # The estimate exists only at the edges, so the reviewer is told when the
  # numbers shown belong to the edge below a threshold that is not one.
  edge <- edges[findInterval(threshold, edges)]
  note <- if (edge == threshold) "" else sprintf(
    " Precision and share kept are those at %s, the nearest candidate threshold below it.",
    thr_format_threshold(edge)
  )
  if (!is.na(override)) {
    reason <- sprintf("Threshold set manually to %s.%s Without the override: %s",
                      thr_format_threshold(override), note, fit$reason)
    return(list(threshold = threshold, source = "manual", status = "manual",
                reason = reason))
  }
  if (fit$status == "fitted") {
    return(list(threshold = threshold, source = "fitted", status = "fitted",
                reason = fit$reason))
  }
  kept <- if (is.na(fallback)) {
    paste("The model has no threshold for this class, so 0 is written and",
          "every prediction is kept.")
  } else {
    sprintf("Keeping the model's current threshold of %s.",
            thr_format_threshold(fallback))
  }
  list(threshold = threshold, source = "fallback", status = fit$status,
       reason = paste0(fit$reason, " ", kept, note))
}

#' Build the resolved row of one class
#'
#' @param class_name Class label.
#' @param counts Stratum counts of the class.
#' @param curve Its precision curve.
#' @param fit List from \code{\link{thr_fit_class}}.
#' @param fallback The model's current threshold, or \code{NA}.
#' @param override Manual threshold, or \code{NA}.
#' @return One-row data frame; see \code{\link{thr_resolve_thresholds}}.
#' @noRd
thr_resolved_row <- function(class_name, counts, curve, fit, fallback,
                             override) {
  final <- thr_final_threshold(fit, fallback, override, curve$threshold)
  # A value between two edges is described by the edge at or below it.
  at <- curve[findInterval(final$threshold, curve$threshold), , drop = FALSE]
  data.frame(
    class_name = class_name,
    threshold = final$threshold,
    source = final$source,
    status = final$status,
    reason = final$reason,
    n_predicted = as.integer(sum(counts$n_population)),
    n_verdicts = as.integer(sum(counts$n_verdicts)),
    n_correct = as.integer(sum(counts$n_correct)),
    precision = at$precision,
    lower = at$lower,
    upper = at$upper,
    kept_share = at$kept_share,
    fitted_threshold = fit$threshold,
    previous_threshold = fallback,
    stringsAsFactors = FALSE
  )
}

#' Check and align the inputs of thr_resolve_thresholds()
#'
#' @inheritParams thr_resolve_thresholds
#' @return List with \code{strata} (class names as character), \code{fallback}
#'   and \code{override} (both numeric, aligned with \code{class_labels},
#'   \code{NA} where there is none).
#' @noRd
thr_check_resolve_inputs <- function(strata_counts, class_labels, fallback,
                                     overrides) {
  if (!is.character(class_labels) || length(class_labels) == 0 ||
      anyNA(class_labels)) {
    stop("`class_labels` must be a character vector with the model's class ",
         "labels.", call. = FALSE)
  }
  if (anyDuplicated(class_labels) > 0) {
    stop("`class_labels` lists a class more than once: ",
         paste(unique(class_labels[duplicated(class_labels)]), collapse = ", "),
         ".", call. = FALSE)
  }
  thr_check_columns(
    strata_counts,
    c("class_name", "bin", "n_population", "n_verdicts", "n_correct"),
    "strata_counts"
  )
  strata <- strata_counts
  strata$class_name <- as.character(strata$class_name)
  thr_check_known_classes(strata$class_name, class_labels, "strata_counts")

  ok <- length(fallback) == length(class_labels) &&
    (is.numeric(fallback) || all(is.na(fallback)))
  if (!ok) {
    stop("`fallback` must be a numeric vector with one value per class in ",
         "`class_labels` (NA where the model has no threshold).",
         call. = FALSE)
  }
  fallback <- as.numeric(fallback)
  fallback[is.na(fallback)] <- NA_real_
  if (any(fallback < 0 | fallback > 1, na.rm = TRUE)) {
    stop("`fallback` thresholds must be between 0 and 1.", call. = FALSE)
  }
  list(strata = strata, fallback = fallback,
       override = thr_align_overrides(overrides, class_labels))
}

#' Stop when class names are not among the model's labels
#'
#' @param class_name Class names found in an input.
#' @param class_labels The model's class labels.
#' @param arg Name of the argument, for the error message.
#' @return \code{NULL}, invisibly; called for its errors.
#' @noRd
thr_check_known_classes <- function(class_name, class_labels, arg) {
  unknown <- setdiff(class_name, class_labels)
  if (length(unknown) > 0) {
    stop("`", arg, "` has ", length(unknown), " class(es) that are not in ",
         "`class_labels`: ", paste(utils::head(unknown, 5), collapse = ", "),
         ". Class names must match the model's labels exactly.",
         call. = FALSE)
  }
  invisible(NULL)
}

#' Align manual overrides with the class labels
#'
#' @param overrides \code{NULL} or a data frame with \code{class_name} and
#'   \code{threshold}.
#' @param class_labels The model's class labels.
#' @return Numeric vector aligned with \code{class_labels}, \code{NA} where a
#'   class has no override.
#' @noRd
thr_align_overrides <- function(overrides, class_labels) {
  aligned <- rep(NA_real_, length(class_labels))
  if (is.null(overrides)) {
    return(aligned)
  }
  thr_check_columns(overrides, c("class_name", "threshold"), "overrides")
  class_name <- as.character(overrides$class_name)
  threshold <- overrides$threshold
  if (nrow(overrides) == 0) {
    return(aligned)
  }
  thr_check_known_classes(class_name, class_labels, "overrides")
  if (anyDuplicated(class_name) > 0) {
    stop("`overrides` lists a class more than once: ",
         paste(unique(class_name[duplicated(class_name)]), collapse = ", "),
         ".", call. = FALSE)
  }
  if (!is.numeric(threshold) || any(!is.finite(threshold)) ||
      any(threshold < 0 | threshold > 1)) {
    stop("`overrides$threshold` must hold numbers between 0 and 1. Remove ",
         "the row of a class to drop its override.", call. = FALSE)
  }
  aligned[match(class_name, class_labels)] <- as.numeric(threshold)
  aligned
}

#' Resolve the threshold of every class of a model
#'
#' Fits each class where the review supports it and falls back to the model's
#' current threshold everywhere else, so that the result always holds a
#' usable threshold for every class.
#'
#' @param strata_counts Data frame for all classes with the columns
#'   \code{class_name}, \code{bin}, \code{n_population}, \code{n_verdicts}
#'   and \code{n_correct}, and optionally \code{n_unavailable} (predictions
#'   whose image could not be shown, which can never be reviewed; taken as 0
#'   when absent). Classes without rows have no predictions.
#' @param class_labels The model's class labels, in model order.
#' @param fallback Numeric vector aligned with \code{class_labels}: the
#'   model's current thresholds, \code{NA} where it has none.
#' @param overrides \code{NULL}, or a data frame with \code{class_name} and
#'   \code{threshold} holding manual thresholds between 0 and 1.
#' @param min_class_n Predictions and verdicts a class needs to be fitted.
#' @inheritParams estimate_precision_curve
#' @inheritParams choose_class_threshold
#' @return Data frame with one row per class in \code{class_labels}, in that
#'   order: \code{class_name}; \code{threshold} (final, never \code{NA});
#'   \code{source} (\code{"fitted"}, \code{"manual"} or \code{"fallback"});
#'   \code{status} (\code{"fitted"}, \code{"manual"}, \code{"not_reviewed"},
#'   \code{"under_reviewed"}, \code{"too_few_predictions"} or
#'   \code{"target_unreachable"}); \code{reason}; \code{n_predicted},
#'   \code{n_verdicts}, \code{n_correct}; \code{precision}, \code{lower},
#'   \code{upper} and \code{kept_share} at the final threshold (for a value
#'   between two edges, those of the edge at or below it);
#'   \code{fitted_threshold} (what the review chose, \code{NA} when nothing);
#'   and \code{previous_threshold} (the fallback, \code{NA} when the model
#'   had none).
#' @noRd
thr_resolve_thresholds <- function(strata_counts, class_labels, fallback,
                                   overrides = NULL, edges = THR_SCORE_EDGES,
                                   target = THR_DEFAULT_TARGET_PRECISION,
                                   criterion = "estimate",
                                   min_class_n = THR_MIN_CLASS_VERDICTS,
                                   min_stratum_n = THR_MIN_STRATUM_VERDICTS,
                                   n_sim = 2000, conf_level = 0.95,
                                   seed = NULL) {
  criterion <- match.arg(criterion, c("estimate", "lower"))
  thr_check_target(target)
  inputs <- thr_check_resolve_inputs(strata_counts, class_labels, fallback,
                                     overrides)
  rows <- lapply(seq_along(class_labels), function(i) {
    counts <- inputs$strata[inputs$strata$class_name == class_labels[i], ,
                            drop = FALSE]
    # The same seed for every class: a class's interval then does not move
    # when verdicts come in for another class.
    curve <- estimate_precision_curve(counts, edges, min_stratum_n, n_sim,
                                      conf_level, seed)
    fit <- thr_fit_class(counts, curve, target, criterion, min_class_n)
    thr_resolved_row(class_labels[i], counts, curve, fit, inputs$fallback[i],
                     inputs$override[i])
  })
  resolved <- do.call(rbind, rows)
  rownames(resolved) <- NULL
  resolved
}
