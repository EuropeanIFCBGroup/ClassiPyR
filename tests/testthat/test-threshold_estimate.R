# Tests for score binning and the stratified precision curve

library(testthat)

# =============================================================================
# thr_score_bin
# =============================================================================

test_that("thr_score_bin puts boundary scores in left-closed bins", {
  edges <- c(0, 0.1, 0.5, 0.9999)

  expect_identical(thr_score_bin(0, edges), 1L)
  expect_identical(thr_score_bin(0.0999999, edges), 1L)
  expect_identical(thr_score_bin(0.1, edges), 2L)
  expect_identical(thr_score_bin(0.5, edges), 3L)
  expect_identical(thr_score_bin(0.9998999, edges), 3L)
  expect_identical(thr_score_bin(0.9999, edges), 4L)
  expect_identical(thr_score_bin(1, edges), 4L)
})

test_that("thr_score_bin uses the package edges by default", {
  k <- length(THR_SCORE_EDGES)

  expect_identical(thr_score_bin(c(0, 0.1, 0.9999, 1)), c(1L, 2L, k, k))
  # Every edge opens its own bin
  expect_identical(thr_score_bin(THR_SCORE_EDGES), seq_len(k))
})

test_that("a threshold at an edge keeps exactly the bins from that edge up", {
  set.seed(42)
  score <- c(runif(500), THR_SCORE_EDGES, 1)
  bin <- thr_score_bin(score)

  for (b in seq_along(THR_SCORE_EDGES)) {
    # The rule ifcb-classify applies: keep when score >= threshold
    expect_identical(score >= THR_SCORE_EDGES[b], bin >= b)
  }
})

test_that("thr_score_bin handles vectors and empty input", {
  expect_identical(thr_score_bin(c(0.05, 0.95, 0.5), strata_small_edges()),
                   c(1L, 3L, 2L))
  expect_identical(thr_score_bin(numeric(0)), integer(0))
})

test_that("thr_score_bin clamps a tiny floating point overshoot", {
  edges <- strata_small_edges()

  expect_identical(thr_score_bin(1 + 1e-7, edges), 3L)
  expect_identical(thr_score_bin(-1e-9, edges), 1L)
})

test_that("thr_score_bin rejects scores that are not winning probabilities", {
  expect_error(thr_score_bin(1.01), "between 0 and 1")
  expect_error(thr_score_bin(-0.5), "between 0 and 1")
  expect_error(thr_score_bin(c(0.5, NA)), "missing")
  expect_error(thr_score_bin("0.5"), "numeric")
})

test_that("thr_score_bin rejects unusable edges", {
  expect_error(thr_score_bin(0.5, c(0.1, 0.5)), "start at 0")
  expect_error(thr_score_bin(0.5, c(0, 0.5, 0.2)), "increasing")
  expect_error(thr_score_bin(0.5, c(0, 0.5, 0.5)), "increasing")
  expect_error(thr_score_bin(0.5, c(0, 0.5, 1.5)), "1")
  expect_error(thr_score_bin(0.5, c(0, NA)), "edges")
  expect_error(thr_score_bin(0.5, numeric(0)), "edges")
})

# =============================================================================
# estimate_precision_curve: point estimate
# =============================================================================

test_that("the curve has one row per edge with the documented columns", {
  curve <- estimate_precision_curve(strata_worked_example(),
                                    edges = strata_small_edges(), seed = 1)

  expect_s3_class(curve, "data.frame")
  expect_named(curve, c("threshold", "bin", "n_population_kept", "kept_share",
                        "n_verdicts", "precision", "lower", "upper",
                        "supported"))
  expect_identical(curve$threshold, strata_small_edges())
  expect_identical(curve$bin, 1:3)
  expect_type(curve$supported, "logical")
})

test_that("precision is weighted by stratum population, not by verdict count", {
  curve <- estimate_precision_curve(strata_worked_example(),
                                    edges = strata_small_edges(), seed = 1)

  # Worked out by hand in helper-threshold-strata.R
  expect_equal(curve$precision, c(0.6875, 0.875, 0.95))
  # Pooled verdicts would give 0.8 and 0.9 for the first two edges
  expect_false(isTRUE(all.equal(curve$precision[1], 32 / 40)))
  expect_false(isTRUE(all.equal(curve$precision[2], 27 / 30)))

  expect_identical(curve$n_population_kept, c(200L, 100L, 50L))
  expect_equal(curve$kept_share, c(1, 0.5, 0.25))
  expect_identical(curve$n_verdicts, c(40L, 30L, 20L))
})

test_that("row order and extra columns of the input do not matter", {
  counts <- strata_worked_example()
  shuffled <- counts[c(3, 1, 2), ]
  shuffled$n_pending <- 7L
  shuffled$class_name <- "Debris"

  expect_identical(
    estimate_precision_curve(shuffled, edges = strata_small_edges(), seed = 1),
    estimate_precision_curve(counts, edges = strata_small_edges(), seed = 1)
  )
})

test_that("absent bins and bins with no population are empty strata", {
  edges <- c(0, 0.25, 0.5, 0.75)
  with_gap <- strata_one_class(bin = c(1, 2, 4), n_population = c(40, 0, 60),
                               n_verdicts = c(10, 0, 20), n_correct = c(5, 0, 18))
  without_row <- with_gap[with_gap$bin != 2, ]

  curve <- estimate_precision_curve(with_gap, edges = edges, seed = 1)

  expect_identical(
    curve, estimate_precision_curve(without_row, edges = edges, seed = 1)
  )
  # Edges 2, 3 and 4 keep the same 60 predictions
  expect_identical(curve$n_population_kept, c(100L, 60L, 60L, 60L))
  expect_equal(curve$precision, c((40 * 0.5 + 60 * 0.9) / 100, 0.9, 0.9, 0.9))
  expect_true(all(curve$supported))
})

# =============================================================================
# estimate_precision_curve: interval
# =============================================================================

test_that("the interval brackets the estimate", {
  curve <- estimate_precision_curve(strata_worked_example(),
                                    edges = strata_small_edges(), seed = 1)

  expect_true(all(curve$lower <= curve$precision))
  expect_true(all(curve$upper >= curve$precision))
  expect_true(all(curve$lower >= 0 & curve$upper <= 1))
  expect_true(all(curve$upper - curve$lower > 0))
})

test_that("fully reviewed strata are exact and contribute no uncertainty", {
  census <- strata_one_class(bin = 1:3, n_population = c(8, 5, 4),
                             n_verdicts = c(8, 5, 4), n_correct = c(4, 4, 4))

  curve <- estimate_precision_curve(census, edges = strata_small_edges(),
                                    seed = 1)

  expect_equal(curve$precision, c(12 / 17, 8 / 9, 1))
  expect_identical(curve$lower, curve$precision)
  expect_identical(curve$upper, curve$precision)
})

test_that("a census stratum adds nothing to the width of a mixed estimate", {
  edges <- strata_small_edges()
  sampled_only <- strata_one_class(bin = 3, n_population = 100,
                                   n_verdicts = 20, n_correct = 15)
  with_census <- rbind(
    strata_one_class(bin = 2, n_population = 100, n_verdicts = 100,
                     n_correct = 60),
    sampled_only
  )

  top <- estimate_precision_curve(sampled_only, edges = edges, seed = 1)
  mixed <- estimate_precision_curve(with_census, edges = edges, seed = 1)

  # Edge 2 averages the sampled stratum with an exactly known one of the same
  # size, so its interval is exactly half as wide as the sampled stratum's.
  expect_equal(mixed$upper[2] - mixed$lower[2],
               (top$upper[3] - top$lower[3]) / 2)
  expect_equal(mixed$precision[2], (0.6 + 0.75) / 2)
})

test_that("only the unreviewed part of a stratum is uncertain", {
  edges <- strata_small_edges()
  curve_for <- function(n_population) {
    counts <- strata_one_class(bin = 3, n_population = n_population,
                               n_verdicts = 20, n_correct = 15)
    estimate_precision_curve(counts, edges = edges, seed = 1)
  }
  width <- function(curve) curve$upper[3] - curve$lower[3]

  nearly_done <- curve_for(21)
  half_done <- curve_for(40)
  barely_started <- curve_for(2000)

  # Same verdicts, so the same point estimate, but the more of the stratum
  # they cover the less there is left to be wrong about.
  expect_equal(nearly_done$precision[3], 0.75)
  expect_equal(barely_started$precision[3], 0.75)
  expect_lt(width(nearly_done), width(half_done))
  expect_lt(width(half_done), width(barely_started))

  # One unreviewed prediction out of 21 can move the precision by at most
  # 1/21, whichever way it turns out.
  expect_gte(nearly_done$lower[3], 15 / 21)
  expect_lte(nearly_done$upper[3], 16 / 21)
})

test_that("an all-correct stratum that is only partly reviewed stays uncertain", {
  partial <- strata_one_class(bin = 3, n_population = 500, n_verdicts = 20,
                              n_correct = 20)

  curve <- estimate_precision_curve(partial, edges = strata_small_edges(),
                                    seed = 1)

  expect_identical(curve$precision[3], 1)
  expect_lt(curve$lower[3], 1)
  expect_gt(curve$lower[3], 0.8)
  expect_identical(curve$upper[3], 1)
})

test_that("an all-wrong stratum that is only partly reviewed stays uncertain", {
  partial <- strata_one_class(bin = 3, n_population = 500, n_verdicts = 20,
                              n_correct = 0)

  curve <- estimate_precision_curve(partial, edges = strata_small_edges(),
                                    seed = 1)

  expect_identical(curve$precision[3], 0)
  expect_identical(curve$lower[3], 0)
  expect_gt(curve$upper[3], 0)
})

test_that("the interval narrows as more verdicts come in", {
  width_at <- function(n) {
    counts <- strata_one_class(bin = 3, n_population = 10000, n_verdicts = n,
                               n_correct = round(0.8 * n))
    curve <- estimate_precision_curve(counts, edges = strata_small_edges(),
                                      seed = 1)
    curve$upper[3] - curve$lower[3]
  }

  widths <- vapply(c(10, 40, 160, 640), width_at, numeric(1))

  expect_true(all(diff(widths) < 0))
})

test_that("a wider confidence level gives a wider interval", {
  width_at <- function(conf_level) {
    curve <- estimate_precision_curve(strata_worked_example(),
                                      edges = strata_small_edges(),
                                      conf_level = conf_level, seed = 1)
    curve$upper - curve$lower
  }

  expect_true(all(width_at(0.99) > width_at(0.8)))
})

# =============================================================================
# estimate_precision_curve: missing evidence and support
# =============================================================================

test_that("an unreviewed stratum makes every edge at or below it NA", {
  counts <- strata_one_class(bin = 1:3, n_population = c(100, 50, 50),
                             n_verdicts = c(10, 0, 20), n_correct = c(5, 0, 19))

  curve <- estimate_precision_curve(counts, edges = strata_small_edges(),
                                    seed = 1)

  expect_identical(is.na(curve$precision), c(TRUE, TRUE, FALSE))
  expect_identical(is.na(curve$lower), c(TRUE, TRUE, FALSE))
  expect_identical(is.na(curve$upper), c(TRUE, TRUE, FALSE))
  expect_identical(curve$supported, c(FALSE, FALSE, TRUE))
  # What is known exactly is still reported
  expect_identical(curve$n_population_kept, c(200L, 100L, 50L))
  expect_equal(curve$kept_share, c(1, 0.5, 0.25))
  expect_identical(curve$n_verdicts, c(30L, 20L, 20L))
})

test_that("support needs min_stratum_n verdicts in every stratum at or above", {
  counts <- strata_one_class(bin = 1:3, n_population = c(100, 50, 50),
                             n_verdicts = c(10, 9, 10), n_correct = c(5, 8, 10))

  curve <- estimate_precision_curve(counts, edges = strata_small_edges(),
                                    min_stratum_n = 10, seed = 1)

  # Bin 2 is one verdict short: it blocks its own edge and the one below,
  # but the estimate itself is still reported there.
  expect_identical(curve$supported, c(FALSE, FALSE, TRUE))
  expect_false(anyNA(curve$precision))

  relaxed <- estimate_precision_curve(counts, edges = strata_small_edges(),
                                      min_stratum_n = 9, seed = 1)
  expect_identical(relaxed$supported, c(TRUE, TRUE, TRUE))
})

test_that("a small stratum reviewed in full is supported", {
  counts <- strata_one_class(bin = 1:3, n_population = c(100, 3, 50),
                             n_verdicts = c(10, 3, 10), n_correct = c(9, 2, 10))

  curve <- estimate_precision_curve(counts, edges = strata_small_edges(),
                                    min_stratum_n = 10, seed = 1)

  expect_identical(curve$supported, c(TRUE, TRUE, TRUE))

  one_short <- counts
  one_short$n_verdicts[2] <- 2L
  one_short$n_correct[2] <- 2L
  curve <- estimate_precision_curve(one_short, edges = strata_small_edges(),
                                    min_stratum_n = 10, seed = 1)
  expect_identical(curve$supported, c(FALSE, FALSE, TRUE))
})

test_that("supported edges never have a missing estimate unless nothing is kept", {
  counts <- strata_one_class(bin = 1:2, n_population = c(100, 50),
                             n_verdicts = c(10, 0), n_correct = c(5, 0))

  curve <- estimate_precision_curve(counts, edges = strata_small_edges(),
                                    min_stratum_n = 1, seed = 1)

  # Edge 3 keeps nothing: there is nothing to estimate and nothing to block it
  expect_identical(curve$n_population_kept, c(150L, 50L, 0L))
  expect_identical(curve$supported, c(FALSE, FALSE, TRUE))
  expect_true(all(is.na(curve$precision)))
  expect_equal(curve$kept_share, c(1, 1 / 3, 0))
})

test_that("a class with no predictions gives an all-NA curve", {
  empty <- strata_one_class(integer(0), integer(0), integer(0))

  curve <- estimate_precision_curve(empty, edges = strata_small_edges(),
                                    seed = 1)

  expect_identical(nrow(curve), 3L)
  expect_identical(curve$n_population_kept, c(0L, 0L, 0L))
  expect_identical(curve$n_verdicts, c(0L, 0L, 0L))
  expect_true(all(is.na(curve$precision)))
  expect_true(all(is.na(curve$lower)))
  expect_true(all(is.na(curve$upper)))
  # NA rather than NaN, so it is written as JSON null
  expect_identical(curve$kept_share, rep(NA_real_, 3))
})

# =============================================================================
# estimate_precision_curve: seed
# =============================================================================

test_that("a seed makes the interval reproducible", {
  args <- list(strata_worked_example(), edges = strata_small_edges())

  first <- do.call(estimate_precision_curve, c(args, seed = 123))
  set.seed(999)
  second <- do.call(estimate_precision_curve, c(args, seed = 123))
  other <- do.call(estimate_precision_curve, c(args, seed = 124))

  expect_identical(first, second)
  expect_false(identical(first$lower, other$lower))
  # The point estimate never depends on the seed
  expect_identical(first$precision, other$precision)
})

test_that("a seed leaves the global random number stream untouched", {
  old_seed <- get0(".Random.seed", envir = globalenv(), inherits = FALSE)
  on.exit({
    if (is.null(old_seed)) {
      suppressWarnings(rm(".Random.seed", envir = globalenv()))
    } else {
      assign(".Random.seed", old_seed, envir = globalenv())
    }
  }, add = TRUE)

  set.seed(2024)
  state_before <- get(".Random.seed", envir = globalenv())
  expected_next <- runif(3)

  set.seed(2024)
  estimate_precision_curve(strata_worked_example(),
                           edges = strata_small_edges(), seed = 1)

  expect_identical(get(".Random.seed", envir = globalenv()), state_before)
  expect_identical(runif(3), expected_next)
})

test_that("a seed does not create a global random state that was not there", {
  old_seed <- get0(".Random.seed", envir = globalenv(), inherits = FALSE)
  on.exit({
    if (!is.null(old_seed)) {
      assign(".Random.seed", old_seed, envir = globalenv())
    }
  }, add = TRUE)

  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    rm(".Random.seed", envir = globalenv())
  }
  estimate_precision_curve(strata_worked_example(),
                           edges = strata_small_edges(), seed = 1)

  expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
})

test_that("without a seed the global stream is used and differs between calls", {
  old_seed <- get0(".Random.seed", envir = globalenv(), inherits = FALSE)
  on.exit({
    if (is.null(old_seed)) {
      suppressWarnings(rm(".Random.seed", envir = globalenv()))
    } else {
      assign(".Random.seed", old_seed, envir = globalenv())
    }
  }, add = TRUE)

  set.seed(7)
  first <- estimate_precision_curve(strata_worked_example(),
                                    edges = strata_small_edges())
  second <- estimate_precision_curve(strata_worked_example(),
                                     edges = strata_small_edges())
  set.seed(7)
  replay <- estimate_precision_curve(strata_worked_example(),
                                     edges = strata_small_edges())

  expect_false(identical(first$lower, second$lower))
  expect_identical(first, replay)
})

# =============================================================================
# estimate_precision_curve: input validation
# =============================================================================

test_that("estimate_precision_curve explains malformed stratum counts", {
  edges <- strata_small_edges()
  good <- strata_worked_example()

  expect_error(estimate_precision_curve(list(bin = 1), edges = edges),
               "data frame")
  expect_error(estimate_precision_curve(good[, c("bin", "n_population")],
                                        edges = edges),
               "n_verdicts")

  out_of_range <- good
  out_of_range$bin[3] <- 4L
  expect_error(estimate_precision_curve(out_of_range, edges = edges),
               "between 1 and 3")

  duplicated_bin <- good
  duplicated_bin$bin[2] <- 1L
  expect_error(estimate_precision_curve(duplicated_bin, edges = edges),
               "more than once")

  too_many_correct <- good
  too_many_correct$n_correct[1] <- 11L
  expect_error(estimate_precision_curve(too_many_correct, edges = edges),
               "n_correct")

  too_many_verdicts <- good
  too_many_verdicts$n_verdicts[2] <- 51L
  expect_error(estimate_precision_curve(too_many_verdicts, edges = edges),
               "n_verdicts")

  with_na <- good
  with_na$n_population[1] <- NA
  expect_error(estimate_precision_curve(with_na, edges = edges),
               "whole numbers")
})

test_that("estimate_precision_curve refuses the counts of several classes", {
  two_classes <- rbind(
    strata_for_class("Debris", 1:3, c(100, 50, 50)),
    strata_for_class("Skeletonema_spp", 1:3, c(100, 50, 50))
  )

  expect_error(
    estimate_precision_curve(two_classes, edges = strata_small_edges()),
    "one class"
  )
})

test_that("estimate_precision_curve validates its settings", {
  counts <- strata_worked_example()
  edges <- strata_small_edges()

  expect_error(estimate_precision_curve(counts, edges, min_stratum_n = 0),
               "min_stratum_n")
  expect_error(estimate_precision_curve(counts, edges, n_sim = 0), "n_sim")
  expect_error(estimate_precision_curve(counts, edges, conf_level = 1),
               "conf_level")
  expect_error(estimate_precision_curve(counts, edges, conf_level = 0),
               "conf_level")
  expect_error(estimate_precision_curve(counts, edges, seed = "a"), "seed")
  expect_error(estimate_precision_curve(counts, edges, seed = c(1, 2)), "seed")
})
