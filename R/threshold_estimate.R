# Score binning and the stratified precision estimate of the threshold review
# app (run_threshold_app()).
#
# A predicted class is stratified by the score bin of its winning score. Within
# each stratum a simple random sample is reviewed, so the precision of the
# class above a candidate threshold is the population-weighted mean of the
# stratum precisions. Everything here is pure: counts in, estimates out.

#' Require columns in a data frame
#'
#' @param x Object to check.
#' @param columns Required column names.
#' @param arg Name of the argument, for the error message.
#' @return \code{NULL}, invisibly; called for its errors.
#' @noRd
thr_check_columns <- function(x, columns, arg) {
  if (!is.data.frame(x)) {
    stop("`", arg, "` must be a data frame with the columns ",
         paste(columns, collapse = ", "), ".", call. = FALSE)
  }
  missing_columns <- setdiff(columns, names(x))
  if (length(missing_columns) > 0) {
    stop("`", arg, "` is missing the column(s): ",
         paste(missing_columns, collapse = ", "), ".", call. = FALSE)
  }
  invisible(NULL)
}

#' Validate score bin edges
#'
#' @param edges Lower edges of the score bins.
#' @return The edges as a plain numeric vector.
#' @noRd
thr_validate_edges <- function(edges) {
  if (!is.numeric(edges) || length(edges) == 0 || anyNA(edges)) {
    stop("`edges` must be a numeric vector of score bin edges without ",
         "missing values.", call. = FALSE)
  }
  if (edges[1] != 0) {
    stop("`edges` must start at 0 so that every score falls in a bin, but ",
         "the first edge is ", format(edges[1]), ".", call. = FALSE)
  }
  if (is.unsorted(edges, strictly = TRUE)) {
    stop("`edges` must be strictly increasing: sort them and remove ",
         "duplicates.", call. = FALSE)
  }
  if (edges[length(edges)] > 1) {
    stop("`edges` must not go above 1, the highest possible score, but the ",
         "last edge is ", format(edges[length(edges)]), ".", call. = FALSE)
  }
  as.numeric(edges)
}

#' Assign scores to score bins
#'
#' Bin \code{b} is \code{[edges[b], edges[b + 1])} and the last bin includes 1,
#' so a threshold at \code{edges[b]} applied as \code{score >= threshold} keeps
#' exactly the bins \code{b} and up.
#'
#' @param score Numeric vector of winning scores between 0 and 1.
#' @param edges Lower edges of the score bins, increasing and starting at 0.
#' @return Integer vector of bin numbers, \code{1} to \code{length(edges)}.
#' @noRd
thr_score_bin <- function(score, edges = THR_SCORE_EDGES) {
  edges <- thr_validate_edges(edges)
  if (!is.numeric(score)) {
    stop("`score` must be numeric, not ", class(score)[1], ".", call. = FALSE)
  }
  if (anyNA(score)) {
    stop("`score` contains missing values; every ROI needs a winning score.",
         call. = FALSE)
  }
  # Scores are float32 softmax outputs, so a winning score can land a hair
  # outside [0, 1]. Anything further out is not a probability at all.
  tolerance <- 1e-6
  if (any(score < -tolerance | score > 1 + tolerance)) {
    stop("`score` must be between 0 and 1, but ranges from ",
         format(min(score)), " to ", format(max(score)),
         ". Pass the winning class score of each ROI.", call. = FALSE)
  }
  findInterval(pmin(pmax(score, 0), 1), edges)
}

#' Sum a vector from each position to its end
#'
#' Turns per-bin quantities into "at or above this edge" quantities.
#'
#' @param x Numeric vector ordered by bin.
#' @return Vector of the same length; element \code{b} is \code{sum(x[b:n])}.
#' @noRd
thr_sum_from <- function(x) {
  rev(cumsum(rev(x)))
}

#' Check one count column of a stratum table
#'
#' @param strata_counts Data frame of stratum counts.
#' @param column Name of the column to check.
#' @return The column as a numeric vector.
#' @noRd
thr_count_column <- function(strata_counts, column) {
  values <- strata_counts[[column]]
  ok <- is.numeric(values) && !anyNA(values) &&
    all(values >= 0 & values == floor(values))
  if (!ok) {
    stop("`strata_counts$", column, "` must hold whole numbers of 0 or more ",
         "without missing values.", call. = FALSE)
  }
  as.numeric(values)
}

#' Lay the stratum counts of one class out by bin
#'
#' Validates the counts and expands them to one entry per bin, so that bins
#' absent from the input become empty strata.
#'
#' @param strata_counts Data frame with columns \code{bin},
#'   \code{n_population}, \code{n_verdicts} and \code{n_correct}, and
#'   optionally \code{n_unavailable} (taken as 0 when absent).
#' @param n_bins Number of score bins.
#' @return List of four numeric vectors of length \code{n_bins}:
#'   \code{n_population}, \code{n_verdicts}, \code{n_correct} and
#'   \code{n_unavailable}.
#' @noRd
thr_strata_by_bin <- function(strata_counts, n_bins) {
  required <- c("bin", "n_population", "n_verdicts", "n_correct")
  thr_check_columns(strata_counts, required, "strata_counts")
  if (length(unique(strata_counts[["class_name"]])) > 1) {
    stop("`strata_counts` holds more than one class. Pass the rows of one ",
         "class at a time.", call. = FALSE)
  }
  counts <- lapply(stats::setNames(required, required), thr_count_column,
                   strata_counts = strata_counts)
  counts$n_unavailable <- if ("n_unavailable" %in% names(strata_counts)) {
    thr_count_column(strata_counts, "n_unavailable")
  } else {
    numeric(nrow(strata_counts))
  }
  thr_check_stratum_counts(counts, n_bins)

  by_bin <- function(values) {
    replace(numeric(n_bins), counts$bin, values)
  }
  lapply(counts[names(counts) != "bin"], by_bin)
}

#' Check that stratum counts are consistent with each other
#'
#' @param counts List with numeric vectors \code{bin}, \code{n_population},
#'   \code{n_verdicts}, \code{n_correct} and \code{n_unavailable}.
#' @param n_bins Number of score bins.
#' @return \code{NULL}, invisibly; called for its errors.
#' @noRd
thr_check_stratum_counts <- function(counts, n_bins) {
  if (any(counts$bin < 1 | counts$bin > n_bins)) {
    stop("`strata_counts$bin` must be between 1 and ", n_bins,
         ", the number of score bin edges. Were the counts made with other ",
         "edges?", call. = FALSE)
  }
  if (anyDuplicated(counts$bin) > 0) {
    stop("`strata_counts` lists a bin more than once. It needs one row per ",
         "score bin of a single class.", call. = FALSE)
  }
  if (any(counts$n_verdicts > counts$n_population)) {
    stop("`strata_counts$n_verdicts` is larger than `n_population` in at ",
         "least one bin: a stratum cannot have more verdicts than ",
         "predictions.", call. = FALSE)
  }
  if (any(counts$n_correct > counts$n_verdicts)) {
    stop("`strata_counts$n_correct` is larger than `n_verdicts` in at least ",
         "one bin.", call. = FALSE)
  }
  if (any(counts$n_verdicts + counts$n_unavailable > counts$n_population)) {
    stop("`strata_counts$n_unavailable` and `n_verdicts` add up to more than ",
         "`n_population` in at least one bin: a prediction whose image is ",
         "unavailable cannot have a verdict.", call. = FALSE)
  }
  invisible(NULL)
}

#' Leave the strata in which nothing can be reviewed out of the estimate
#'
#' A non-empty stratum whose images are all unavailable has no verdicts and
#' can never get any. For the estimate it is turned into an empty stratum, so
#' that it neither blocks the edges at or below it nor enters the precision
#' or its interval. A stratum in which only some images are unavailable is
#' left as it is: its verdicts stand in for the whole stratum.
#'
#' @param strata List from \code{\link{thr_strata_by_bin}}.
#' @return The same list with \code{n_population} and \code{n_unavailable}
#'   set to 0 in the strata that hold predictions but none that can be
#'   reviewed.
#' @noRd
thr_drop_unreviewable <- function(strata) {
  unreviewable <- strata$n_population > 0 &
    strata$n_unavailable == strata$n_population
  utils::modifyList(strata, list(
    n_population = ifelse(unreviewable, 0, strata$n_population),
    n_unavailable = ifelse(unreviewable, 0, strata$n_unavailable)
  ))
}

#' Check the settings of the precision curve
#'
#' @inheritParams estimate_precision_curve
#' @return \code{NULL}, invisibly; called for its errors.
#' @noRd
thr_check_curve_settings <- function(min_stratum_n, n_sim, conf_level, seed) {
  is_number <- function(x) {
    is.numeric(x) && length(x) == 1 && is.finite(x)
  }
  if (!is_number(min_stratum_n) || min_stratum_n < 1) {
    stop("`min_stratum_n` must be a single number of 1 or more.",
         call. = FALSE)
  }
  if (!is_number(n_sim) || n_sim < 1 || n_sim != floor(n_sim)) {
    stop("`n_sim` must be a single whole number of 1 or more.", call. = FALSE)
  }
  if (!is_number(conf_level) || conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be a single number between 0 and 1, such as ",
         "0.95.", call. = FALSE)
  }
  if (!is.null(seed) && !is_number(seed)) {
    stop("`seed` must be NULL or a single number.", call. = FALSE)
  }
  invisible(NULL)
}

#' Run a simulation under a seed without disturbing the caller's RNG
#'
#' With a seed, the global random number state is saved, the seed is set, and
#' the state is put back afterwards (or removed again when there was none), so
#' a seeded estimate never changes what the user's own random draws return.
#'
#' @param seed \code{NULL} to use the global random number stream as it is,
#'   or a single number.
#' @param simulate Function of no arguments that draws the random numbers.
#' @return The value of \code{simulate()}.
#' @noRd
thr_sim_with_seed <- function(seed, simulate) {
  if (is.null(seed)) {
    return(simulate())
  }
  global <- globalenv()
  had_state <- exists(".Random.seed", envir = global, inherits = FALSE)
  old_state <- if (had_state) get(".Random.seed", envir = global)
  on.exit({
    if (had_state) {
      assign(".Random.seed", old_state, envir = global)
    } else if (exists(".Random.seed", envir = global, inherits = FALSE)) {
      rm(".Random.seed", envir = global)
    }
  }, add = TRUE)
  set.seed(seed)
  simulate()
}

#' Simulate the uncertainty interval of the precision at or above each edge
#'
#' The verdicts of a stratum are known, so only its unreviewed remainder is
#' uncertain. That remainder's precision is drawn from the Jeffreys posterior
#' \code{Beta(correct + 0.5, wrong + 0.5)}, and the number of correct
#' predictions among the remainder is then drawn from a binomial with that
#' precision, because the remainder is a finite number of predictions whose
#' count of correct ones varies around its expectation. The interval therefore
#' narrows as a stratum is reviewed and closes when it is reviewed in full.
#' The same draws are combined for every edge, so the intervals along the
#' curve are consistent with each other.
#'
#' @param strata List from \code{\link{thr_strata_by_bin}}.
#' @param n_sim Number of simulation draws.
#' @param conf_level Confidence level of the interval.
#' @return Matrix with one row per bin and columns \code{lower}, \code{upper}.
#'   Rows of edges that keep nothing are \code{NaN}; the caller masks them.
#' @noRd
thr_simulate_interval <- function(strata, n_sim, conf_level) {
  n_bins <- length(strata$n_population)
  probs <- c((1 - conf_level) / 2, 1 - (1 - conf_level) / 2)
  kept <- thr_sum_from(strata$n_population)
  reviewed <- strata$n_verdicts > 0
  sampled <- reviewed & strata$n_verdicts < strata$n_population

  interval <- matrix(NA_real_, nrow = n_bins, ncol = 2,
                     dimnames = list(NULL, c("lower", "upper")))
  weighted_sum <- numeric(n_sim)
  for (b in rev(seq_len(n_bins))) {
    if (sampled[b]) {
      wrong <- strata$n_verdicts[b] - strata$n_correct[b]
      draws <- stats::rbeta(n_sim, strata$n_correct[b] + 0.5, wrong + 0.5)
      unreviewed <- strata$n_population[b] - strata$n_verdicts[b]
      weighted_sum <- weighted_sum + strata$n_correct[b] +
        stats::rbinom(n_sim, unreviewed, draws)
    } else if (reviewed[b]) {
      weighted_sum <- weighted_sum + strata$n_correct[b]
    }
    interval[b, ] <- stats::quantile(weighted_sum / kept[b], probs,
                                     names = FALSE, na.rm = TRUE)
  }
  interval
}

#' Estimate precision above each candidate threshold for one class
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Turns the review verdicts of one predicted class into a precision curve:
#' for each candidate threshold, the estimated share of the predictions
#' scoring at or above it that are correct.
#'
#' @details
#' The predictions of a class are stratified by score bin, and a random sample
#' of each stratum is reviewed. Precision at or above an edge is the mean of
#' the stratum precisions weighted by stratum **population**, not by the number
#' of verdicts, so over-sampling a small bin does not bias the estimate:
#' \deqn{\hat{p} = \frac{\sum_h N_h \, y_h / n_h}{\sum_h N_h}}{
#'   p = sum(N_h * y_h / n_h) / sum(N_h)}
#' over the non-empty strata \eqn{h} at or above the edge, with \eqn{N_h}
#' predictions, \eqn{n_h} verdicts and \eqn{y_h} correct verdicts.
#'
#' A prediction whose image could not be shown (`n_unavailable`) can never be
#' reviewed. In a stratum where some images are unavailable, the verdicts on
#' the others stand in for the whole stratum, which keeps its full weight
#' \eqn{N_h}. A stratum in which **no** image can be reviewed carries no
#' information and can never get any, so it is left out of the estimate and
#' its interval altogether, numerator and denominator, instead of blocking
#' the curve: the precision at an edge then describes the strata that could
#' be reviewed. `n_population_kept` and `kept_share` still count every
#' prediction.
#'
#' The interval is simulated. The reviewed predictions of a stratum are known;
#' the precision of its unreviewed remainder is drawn from
#' `Beta(y_h + 0.5, n_h - y_h + 0.5)`, the number of correct predictions among
#' the \eqn{N_h - n_h} unreviewed ones is drawn from a binomial with that
#' precision, and the strata are added up. The interval therefore covers both
#' the uncertainty about the precision of a stratum and the chance variation
#' in a finite remainder. It narrows as more of a stratum is reviewed, and a
#' stratum reviewed in full (`n_verdicts == n_population`) contributes no
#' uncertainty. The bounds are widened where needed to include the point
#' estimate, which happens when every verdict of a sampled stratum agrees.
#'
#' An edge has no estimate (`NA`) as soon as one stratum at or above it has
#' images that can be reviewed but no verdicts, or when it keeps no prediction
#' that can be reviewed. It is `supported` when every stratum at or above it
#' has at least `min(n_population - n_unavailable, min_stratum_n)` verdicts,
#' that is `min_stratum_n` of them or all that can be given. An edge that
#' keeps nothing is trivially supported; an edge that keeps only predictions
#' that cannot be reviewed is not supported, but does not hold back the edges
#' below it.
#'
#' @param strata_counts Data frame for **one** class with one row per score
#'   bin and the columns `bin` (1 to `length(edges)`), `n_population`
#'   (predictions in the bin), `n_verdicts` (reviewed predictions) and
#'   `n_correct` (reviewed predictions judged correct). Bins that are absent
#'   or have `n_population` 0 are empty strata. An optional column
#'   `n_unavailable` gives the predictions of the bin that can never be
#'   reviewed because their image could not be shown; it is taken as 0 when
#'   absent, and `n_verdicts + n_unavailable` cannot exceed `n_population`.
#'   Other columns are ignored.
#' @param edges Lower edges of the score bins, increasing and starting at 0.
#'   Bin `b` is `[edges[b], edges[b + 1])` and the last bin includes 1. These
#'   are also the candidate thresholds.
#' @param min_stratum_n Verdicts a stratum needs before the edges at or below
#'   it count as supported (every prediction of the stratum that can be
#'   reviewed, when those are fewer).
#' @param n_sim Number of simulation draws for the interval.
#' @param conf_level Confidence level of the interval.
#' @param seed `NULL`, or a single number that makes the interval
#'   reproducible. The session's random number stream is left exactly as it
#'   was.
#' @return A data frame with one row per edge and the columns:
#'   \describe{
#'     \item{threshold}{The edge, a candidate threshold.}
#'     \item{bin}{The lowest bin kept by that threshold.}
#'     \item{n_population_kept}{Predictions scoring at or above the edge.}
#'     \item{kept_share}{Their share of all predictions of the class (exact;
#'       `NA` when the class has no predictions).}
#'     \item{n_verdicts}{Verdicts in the bins at or above the edge.}
#'     \item{precision}{Estimated precision at or above the edge, or `NA`.
#'       Strata in which nothing can be reviewed are left out of it.}
#'     \item{lower, upper}{Bounds of the interval, or `NA`.}
#'     \item{supported}{Whether the evidence at or above the edge is enough
#'       to act on.}
#'   }
#' @export
#' @seealso [choose_class_threshold()] to pick a threshold from the curve.
#' @examples
#' # Three score bins: [0, 0.5), [0.5, 0.9) and [0.9, 1]
#' counts <- data.frame(
#'   bin = 1:3,
#'   n_population = c(100, 50, 50),
#'   n_verdicts = c(10, 10, 20),
#'   n_correct = c(5, 8, 19)
#' )
#'
#' curve <- estimate_precision_curve(counts, edges = c(0, 0.5, 0.9), seed = 1)
#' curve
#'
#' # Weighted by population: (100 * 0.5 + 50 * 0.8 + 50 * 0.95) / 200
#' curve$precision[1]
#' @md
estimate_precision_curve <- function(strata_counts, edges = THR_SCORE_EDGES,
                                     min_stratum_n = THR_MIN_STRATUM_VERDICTS,
                                     n_sim = 2000, conf_level = 0.95,
                                     seed = NULL) {
  edges <- thr_validate_edges(edges)
  thr_check_curve_settings(min_stratum_n, n_sim, conf_level, seed)
  strata <- thr_strata_by_bin(strata_counts, length(edges))
  # What the estimate is built from; `strata` keeps the exact counts.
  assessed <- thr_drop_unreviewable(strata)

  population <- assessed$n_population
  unreviewed <- population > 0 & assessed$n_verdicts == 0
  too_few <- assessed$n_verdicts <
    pmin(population - assessed$n_unavailable, min_stratum_n)

  kept <- thr_sum_from(strata$n_population)
  assessed_kept <- thr_sum_from(population)
  estimable <- assessed_kept > 0 & thr_sum_from(unreviewed) == 0
  # N * y / n, multiplied first so that a stratum reviewed in full contributes
  # exactly its number of correct verdicts.
  expected_correct <- ifelse(assessed$n_verdicts > 0,
                             population * assessed$n_correct /
                               assessed$n_verdicts,
                             0)
  precision <- ifelse(estimable, thr_sum_from(expected_correct) / assessed_kept,
                      NA_real_)
  interval <- thr_sim_with_seed(seed, function() {
    thr_simulate_interval(assessed, n_sim, conf_level)
  })
  total <- sum(strata$n_population)

  data.frame(
    threshold = edges,
    bin = seq_along(edges),
    n_population_kept = as.integer(kept),
    kept_share = if (total > 0) kept / total else NA_real_,
    n_verdicts = as.integer(thr_sum_from(strata$n_verdicts)),
    precision = precision,
    # The posterior of a stratum whose verdicts all agree sits wholly on one
    # side of the estimate, so the bounds are stretched to include it.
    lower = ifelse(estimable, pmin(interval[, "lower"], precision), NA_real_),
    upper = ifelse(estimable, pmax(interval[, "upper"], precision), NA_real_),
    # Nothing short of verdicts at or above the edge, and something to judge
    # it by unless it keeps nothing at all.
    supported = thr_sum_from(unreviewed | too_few) == 0 &
      (assessed_kept > 0 | kept == 0),
    stringsAsFactors = FALSE
  )
}
