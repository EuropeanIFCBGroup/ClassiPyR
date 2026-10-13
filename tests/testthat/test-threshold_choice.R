# Tests for evidence status, threshold choice and per-class resolution

library(testthat)

# =============================================================================
# thr_evidence_status
# =============================================================================

test_that("thr_evidence_status reports too few predictions first", {
  none <- strata_one_class(integer(0), integer(0), integer(0))
  small <- strata_one_class(bin = 1:2, n_population = c(20, 29),
                            n_verdicts = c(20, 29))

  expect_identical(thr_evidence_status(none), "too_few_predictions")
  # Even a class reviewed in full is too small to fit
  expect_identical(thr_evidence_status(small, min_class_n = 50),
                   "too_few_predictions")
})

test_that("thr_evidence_status separates unreviewed, under-reviewed and sufficient", {
  counts <- function(n_verdicts) {
    strata_one_class(bin = 1:2, n_population = c(100, 100),
                     n_verdicts = n_verdicts)
  }

  expect_identical(thr_evidence_status(counts(c(0, 0)), 50), "not_reviewed")
  expect_identical(thr_evidence_status(counts(c(1, 0)), 50), "under_reviewed")
  expect_identical(thr_evidence_status(counts(c(25, 24)), 50), "under_reviewed")
  expect_identical(thr_evidence_status(counts(c(25, 25)), 50), "sufficient")
  expect_identical(thr_evidence_status(counts(c(90, 90)), 50), "sufficient")
})

test_that("thr_evidence_status uses the package minimum by default", {
  at_minimum <- strata_one_class(bin = 1, n_population = 500,
                                 n_verdicts = THR_MIN_CLASS_VERDICTS)
  below <- strata_one_class(bin = 1, n_population = 500,
                            n_verdicts = THR_MIN_CLASS_VERDICTS - 1L)
  exactly_enough_predictions <- strata_one_class(
    bin = 1, n_population = THR_MIN_CLASS_VERDICTS
  )

  expect_identical(thr_evidence_status(at_minimum), "sufficient")
  expect_identical(thr_evidence_status(below), "under_reviewed")
  expect_identical(thr_evidence_status(exactly_enough_predictions),
                   "not_reviewed")
})

test_that("thr_evidence_status validates its input", {
  expect_error(thr_evidence_status(data.frame(bin = 1)), "n_population")
  expect_error(thr_evidence_status(strata_one_class(1, 10), min_class_n = -1),
               "min_class_n")
})

# =============================================================================
# choose_class_threshold
# =============================================================================

test_that("a monotone curve gives the lowest edge that meets the target", {
  curve <- strata_curve(threshold = c(0, 0.5, 0.7, 0.9),
                        precision = c(0.6, 0.8, 0.92, 0.97))

  choice <- choose_class_threshold(curve, target = 0.9)

  expect_named(choice, c("threshold", "status", "reason", "precision",
                         "lower", "upper", "kept_share"))
  expect_identical(choice$threshold, 0.7)
  expect_identical(choice$status, "fitted")
  expect_identical(choice$precision, 0.92)
  expect_identical(choice$lower, curve$lower[3])
  expect_identical(choice$upper, curve$upper[3])
  expect_identical(choice$kept_share, curve$kept_share[3])
  # The reason names the edge below that fails
  expect_match(choice$reason, "0.5", fixed = TRUE)
  expect_match(choice$reason, "80.0%", fixed = TRUE)
})

test_that("a dip below the target blocks every edge below it", {
  dips_once <- strata_curve(threshold = c(0, 0.5, 0.7, 0.9),
                            precision = c(0.95, 0.8, 0.93, 0.97))
  dips_high <- strata_curve(threshold = c(0, 0.5, 0.7, 0.9),
                            precision = c(0.6, 0.95, 0.85, 0.97))

  low <- choose_class_threshold(dips_once, target = 0.9)
  high <- choose_class_threshold(dips_high, target = 0.9)

  # Edge 0 meets the target but 0.5 above it does not
  expect_identical(low$threshold, 0.7)
  expect_identical(high$threshold, 0.9)
  expect_identical(high$status, "fitted")
  expect_match(low$reason, "in between")
  expect_match(high$reason, "in between")
})

test_that("an unreachable target gives no threshold and says how close it got", {
  curve <- strata_curve(threshold = c(0, 0.5, 0.9),
                        precision = c(0.5, 0.7, 0.85))

  choice <- choose_class_threshold(curve, target = 0.9)

  expect_identical(choice$threshold, NA_real_)
  expect_identical(choice$status, "target_unreachable")
  expect_identical(choice$precision, NA_real_)
  expect_identical(choice$kept_share, NA_real_)
  expect_match(choice$reason, "85.0%", fixed = TRUE)
  expect_match(choice$reason, "90.0%", fixed = TRUE)
})

test_that("a passing edge below a failing top edge is not chosen", {
  curve <- strata_curve(threshold = c(0, 0.5, 0.9),
                        precision = c(0.95, 0.95, 0.85))

  expect_identical(choose_class_threshold(curve, 0.9)$status,
                   "target_unreachable")
})

test_that("a curve with nothing supported is under-reviewed", {
  curve <- strata_curve(threshold = c(0, 0.5, 0.9),
                        precision = c(0.95, 0.96, 0.97), supported = FALSE)

  choice <- choose_class_threshold(curve, target = 0.9)

  expect_identical(choice$threshold, NA_real_)
  expect_identical(choice$status, "under_reviewed")
  expect_match(choice$reason, "verdicts")
})

test_that("a curve of a class without predictions is under-reviewed", {
  empty <- strata_one_class(integer(0), integer(0), integer(0))
  curve <- estimate_precision_curve(empty, edges = strata_small_edges())

  choice <- choose_class_threshold(curve, target = 0.9)

  expect_identical(choice$status, "under_reviewed")
  expect_identical(choice$threshold, NA_real_)
  expect_match(choice$reason, "no predictions")
})

test_that("a curve that meets the target everywhere needs no cut", {
  curve <- strata_curve(threshold = c(0, 0.5, 0.9),
                        precision = c(0.93, 0.96, 0.99))

  choice <- choose_class_threshold(curve, target = 0.9)

  expect_identical(choice$threshold, 0)
  expect_identical(choice$status, "fitted")
  expect_identical(choice$kept_share, 1)
  expect_match(choice$reason, "No cut needed")
})

test_that("the lower-bound criterion is stricter than the estimate", {
  curve <- strata_curve(threshold = c(0, 0.5, 0.7, 0.9),
                        precision = c(0.6, 0.91, 0.93, 0.97),
                        lower = c(0.5, 0.85, 0.89, 0.92))

  by_estimate <- choose_class_threshold(curve, 0.9, criterion = "estimate")
  by_lower <- choose_class_threshold(curve, 0.9, criterion = "lower")

  expect_identical(by_estimate$threshold, 0.5)
  expect_identical(by_lower$threshold, 0.9)
  expect_gt(by_lower$threshold, by_estimate$threshold)
  expect_match(by_lower$reason, "lower bound")
  # The default is the point estimate
  expect_identical(choose_class_threshold(curve, 0.9), by_estimate)
})

test_that("the lower-bound criterion can make a target unreachable", {
  curve <- strata_curve(threshold = c(0, 0.5, 0.9),
                        precision = c(0.91, 0.93, 0.97),
                        lower = c(0.7, 0.8, 0.88))

  expect_identical(choose_class_threshold(curve, 0.9, "estimate")$status,
                   "fitted")
  expect_identical(choose_class_threshold(curve, 0.9, "lower")$status,
                   "target_unreachable")
})

test_that("supported edges that keep nothing are skipped, not failures", {
  curve <- strata_curve(threshold = c(0, 0.5, 0.9),
                        precision = c(0.95, 0.96, NA),
                        kept_share = c(1, 0.4, 0))

  choice <- choose_class_threshold(curve, target = 0.9)

  expect_identical(choice$threshold, 0)
  expect_identical(choice$status, "fitted")
})

test_that("unsupported lower edges stop the search and are named as the limit", {
  curve <- strata_curve(threshold = c(0, 0.5, 0.7, 0.9),
                        precision = c(0.99, 0.99, 0.92, 0.97),
                        supported = c(FALSE, FALSE, TRUE, TRUE))

  choice <- choose_class_threshold(curve, target = 0.9)

  expect_identical(choice$threshold, 0.7)
  expect_identical(choice$status, "fitted")
  expect_match(choice$reason, "too few verdicts")
})

test_that("a precision exactly at the target meets it", {
  exact <- strata_curve(threshold = c(0, 0.5), precision = c(0.5, 0.9))
  # 0.9 as it comes out of a weighted sum
  rounded <- strata_curve(threshold = c(0, 0.5),
                          precision = c(0.5, 0.9 - 1e-15))
  short <- strata_curve(threshold = c(0, 0.5), precision = c(0.5, 0.8999))

  expect_identical(choose_class_threshold(exact, 0.9)$threshold, 0.5)
  expect_identical(choose_class_threshold(rounded, 0.9)$threshold, 0.5)
  expect_identical(choose_class_threshold(short, 0.9)$status,
                   "target_unreachable")
})

test_that("a supported edge with a missing estimate counts as failing", {
  curve <- strata_curve(threshold = c(0, 0.5, 0.9),
                        precision = c(0.95, NA, 0.97))

  expect_identical(choose_class_threshold(curve, 0.9)$threshold, 0.9)
})

test_that("choose_class_threshold does not depend on the row order", {
  curve <- strata_curve(threshold = c(0, 0.5, 0.7, 0.9),
                        precision = c(0.6, 0.8, 0.92, 0.97))

  expect_identical(choose_class_threshold(curve[c(3, 1, 4, 2), ], 0.9),
                   choose_class_threshold(curve, 0.9))
})

test_that("choose_class_threshold works on an estimated curve", {
  curve <- estimate_precision_curve(strata_worked_example(),
                                    edges = strata_small_edges(), seed = 1)

  # Precision is 0.6875, 0.875 and 0.95 at the three edges
  expect_identical(choose_class_threshold(curve, 0.85)$threshold, 0.5)
  expect_identical(choose_class_threshold(curve, 0.9)$threshold, 0.9)
  expect_identical(choose_class_threshold(curve, 0.6)$threshold, 0)
  expect_identical(choose_class_threshold(curve, 0.99)$status,
                   "target_unreachable")
})

test_that("choose_class_threshold validates its arguments", {
  curve <- strata_curve(threshold = c(0, 0.5), precision = c(0.5, 0.95))

  expect_error(choose_class_threshold(curve, target = 1.2), "target")
  expect_error(choose_class_threshold(curve, target = 0), "target")
  expect_error(choose_class_threshold(curve, target = NA_real_), "target")
  expect_error(choose_class_threshold(curve, criterion = "upper"))
  expect_error(choose_class_threshold(curve[, c("threshold", "precision")]),
               "supported")
  expect_error(choose_class_threshold(list(threshold = 1)), "data frame")
  expect_error(choose_class_threshold(curve[0, ]), "no rows")
  unknown_support <- curve
  unknown_support$supported[1] <- NA
  expect_error(choose_class_threshold(unknown_support), "supported")
})

# =============================================================================
# thr_resolve_thresholds
# =============================================================================

# One class per outcome, on the three wide bins of strata_small_edges().
resolve_fixture <- function() {
  population <- c(100, 50, 50)
  strata <- rbind(
    # 0.5 / 0.8 / 0.95 per bin: only the top edge reaches 0.9
    strata_for_class("fitted", 1:3, population, c(20, 20, 20), c(10, 16, 19)),
    strata_for_class("not_reviewed", 1:3, population),
    strata_for_class("under_reviewed", 1:3, population, c(10, 10, 10)),
    # Reviewed in full, but only 30 predictions
    strata_for_class("too_few", 1:3, c(10, 10, 10), c(10, 10, 10)),
    strata_for_class("unreachable", 1:3, population, c(20, 20, 20),
                     c(10, 10, 10)),
    # Enough verdicts overall, but the top bin is short of min_stratum_n
    strata_for_class("unsupported", 1:3, population, c(50, 10, 5)),
    strata_for_class("manual", 1:3, population, c(20, 20, 20), c(10, 16, 19))
  )
  labels <- c("fitted", "not_reviewed", "under_reviewed", "too_few",
              "unreachable", "no_rows", "unsupported", "manual")
  list(
    strata = strata,
    labels = labels,
    fallback = c(0.3, 0.42, NA, 0.6, 0.7, NA, NaN, 0.2),
    overrides = data.frame(class_name = "manual", threshold = 0.85,
                           stringsAsFactors = FALSE)
  )
}

resolve_fixture_result <- function(...) {
  fixture <- resolve_fixture()
  thr_resolve_thresholds(fixture$strata, fixture$labels, fixture$fallback,
                         overrides = fixture$overrides,
                         edges = strata_small_edges(), target = 0.9,
                         min_class_n = 50, min_stratum_n = 10, seed = 1, ...)
}

test_that("thr_resolve_thresholds returns one row per class in label order", {
  fixture <- resolve_fixture()

  resolved <- resolve_fixture_result()

  expect_s3_class(resolved, "data.frame")
  expect_named(resolved, c("class_name", "threshold", "source", "status",
                           "reason", "n_predicted", "n_verdicts", "n_correct",
                           "precision", "lower", "upper", "kept_share",
                           "fitted_threshold", "previous_threshold"))
  expect_identical(resolved$class_name, fixture$labels)
  expect_identical(rownames(resolved), as.character(seq_along(fixture$labels)))
  expect_type(resolved$threshold, "double")
  expect_type(resolved$n_predicted, "integer")
  expect_type(resolved$n_verdicts, "integer")
  expect_type(resolved$n_correct, "integer")
  expect_type(resolved$reason, "character")
})

test_that("every class gets a finite threshold between 0 and 1", {
  resolved <- resolve_fixture_result()

  expect_true(all(is.finite(resolved$threshold)))
  expect_true(all(resolved$threshold >= 0 & resolved$threshold <= 1))
  expect_true(all(nzchar(resolved$reason)))
})

test_that("each status is reachable and maps to the right source", {
  resolved <- resolve_fixture_result()

  expect_identical(
    resolved$status,
    c("fitted", "not_reviewed", "under_reviewed", "too_few_predictions",
      "target_unreachable", "too_few_predictions", "under_reviewed", "manual")
  )
  expect_identical(
    resolved$source,
    c("fitted", "fallback", "fallback", "fallback", "fallback", "fallback",
      "fallback", "manual")
  )
})

test_that("fitted, manual and fallback thresholds are written as specified", {
  resolved <- resolve_fixture_result()

  # fitted edge; fallback values; 0 where the model has no threshold; override
  expect_identical(resolved$threshold,
                   c(0.9, 0.42, 0, 0.6, 0.7, 0, 0, 0.85))
  expect_identical(resolved$previous_threshold,
                   c(0.3, 0.42, NA, 0.6, 0.7, NA, NA, 0.2))
  expect_identical(resolved$fitted_threshold,
                   c(0.9, NA, NA, NA, NA, NA, NA, 0.9))
})

test_that("counts are summed per class", {
  resolved <- resolve_fixture_result()

  expect_identical(resolved$n_predicted,
                   c(200L, 200L, 200L, 30L, 200L, 0L, 200L, 200L))
  expect_identical(resolved$n_verdicts,
                   c(60L, 0L, 30L, 30L, 60L, 0L, 65L, 60L))
  expect_identical(resolved$n_correct,
                   c(45L, 0L, 30L, 30L, 30L, 0L, 65L, 45L))
})

test_that("the reported precision describes the final threshold", {
  resolved <- resolve_fixture_result()
  by_class <- function(class_name, column) {
    resolved[[column]][resolved$class_name == class_name]
  }

  # Fitted at the 0.9 edge
  expect_equal(by_class("fitted", "precision"), 0.95)
  expect_equal(by_class("fitted", "kept_share"), 0.25)
  expect_lt(by_class("fitted", "lower"), 0.95)

  # Fallback 0.7 is not an edge: the estimate of the 0.5 edge below it is used
  expect_equal(by_class("unreachable", "precision"), 0.5)
  expect_equal(by_class("unreachable", "kept_share"), 0.5)

  # Fallback 0.42 sits in the first bin; no verdicts, but the share is exact
  expect_identical(by_class("not_reviewed", "precision"), NA_real_)
  expect_equal(by_class("not_reviewed", "kept_share"), 1)

  expect_identical(by_class("no_rows", "precision"), NA_real_)
  expect_identical(by_class("no_rows", "kept_share"), NA_real_)
})

test_that("a manual override wins and reports the nearest edge at or below it", {
  resolved <- resolve_fixture_result()
  manual <- resolved[resolved$class_name == "manual", ]

  expect_identical(manual$threshold, 0.85)
  expect_identical(manual$source, "manual")
  expect_identical(manual$status, "manual")
  # 0.85 lies in the [0.5, 0.9) bin: (50 * 0.8 + 50 * 0.95) / 100
  expect_equal(manual$precision, 0.875)
  expect_equal(manual$kept_share, 0.5)
  # What the review itself would have chosen is kept alongside
  expect_identical(manual$fitted_threshold, 0.9)
  # The reason says which edge the reported numbers belong to
  expect_match(
    manual$reason,
    "Threshold set manually to 0.85. Precision and share kept are those at 0.5,",
    fixed = TRUE
  )
  expect_match(manual$reason, "Without the override: Lowest threshold",
               fixed = TRUE)
})

test_that("an override at an edge needs no note about the nearest edge", {
  fixture <- resolve_fixture()
  overrides <- data.frame(class_name = "manual", threshold = 0.5,
                          stringsAsFactors = FALSE)

  resolved <- thr_resolve_thresholds(
    fixture$strata, fixture$labels, fixture$fallback, overrides = overrides,
    edges = strata_small_edges(), min_class_n = 50, min_stratum_n = 10,
    seed = 1
  )

  expect_no_match(resolved$reason[8], "nearest candidate")
  expect_match(resolved$reason[8], "Threshold set manually to 0.5. Without",
               fixed = TRUE)
})

test_that("reasons are complete, single-spaced sentences", {
  resolved <- resolve_fixture_result()
  choice_reasons <- c(
    choose_class_threshold(strata_curve(c(0, 0.5, 0.7, 0.9),
                                        c(0.95, 0.8, 0.93, 0.97)), 0.9)$reason,
    choose_class_threshold(strata_curve(c(0, 0.5), c(0.95, 0.99)), 0.9)$reason,
    choose_class_threshold(strata_curve(c(0, 0.9999), c(0.5, 0.6)), 0.9)$reason,
    choose_class_threshold(strata_curve(c(0, 0.5), c(0.9, 0.9), FALSE))$reason
  )

  for (reason in c(resolved$reason, choice_reasons)) {
    expect_no_match(reason, "  ", fixed = TRUE)
    expect_match(reason, "[.]$")
    expect_no_match(reason, "NA", fixed = TRUE)
  }
  expect_match(choice_reasons[3], "at 0.9999,", fixed = TRUE)
})

test_that("an override beats every other status, at any value in [0, 1]", {
  fixture <- resolve_fixture()
  overrides <- data.frame(
    class_name = c("no_rows", "not_reviewed", "unreachable", "fitted"),
    threshold = c(1, 0, 0.123456789, 0.5),
    stringsAsFactors = FALSE
  )

  resolved <- thr_resolve_thresholds(
    fixture$strata, fixture$labels, fixture$fallback, overrides = overrides,
    edges = strata_small_edges(), min_class_n = 50, min_stratum_n = 10,
    seed = 1
  )
  picked <- resolved[match(overrides$class_name, resolved$class_name), ]

  expect_identical(picked$threshold, overrides$threshold)
  expect_identical(picked$source, rep("manual", 4))
  expect_identical(picked$status, rep("manual", 4))
  # An override at an edge reports that edge
  expect_equal(picked$precision[4], 0.875)
  # Classes without an override are untouched
  expect_identical(resolved$source[resolved$class_name == "manual"], "fitted")
})

test_that("without overrides the manual class is simply fitted", {
  fixture <- resolve_fixture()

  for (overrides in list(NULL, fixture$overrides[0, ])) {
    resolved <- thr_resolve_thresholds(
      fixture$strata, fixture$labels, fixture$fallback, overrides = overrides,
      edges = strata_small_edges(), min_class_n = 50, min_stratum_n = 10,
      seed = 1
    )
    expect_identical(resolved$status[8], "fitted")
    expect_identical(resolved$threshold[8], 0.9)
  }
})

test_that("fallback reasons say which value is kept", {
  resolved <- resolve_fixture_result()
  reason_of <- function(class_name) {
    resolved$reason[resolved$class_name == class_name]
  }

  expect_match(reason_of("not_reviewed"), "0.42", fixed = TRUE)
  expect_match(reason_of("not_reviewed"), "reviewed")
  expect_match(reason_of("under_reviewed"), "30")
  expect_match(reason_of("under_reviewed"), "no threshold")
  expect_match(reason_of("too_few"), "30")
  expect_match(reason_of("no_rows"), "No predictions")
  expect_match(reason_of("unreachable"), "0.7", fixed = TRUE)
  expect_match(reason_of("unsupported"), "verdicts")
})

test_that("the criterion and target are passed on to the choice", {
  by_estimate <- resolve_fixture_result()
  by_lower <- resolve_fixture_result(criterion = "lower")

  expect_identical(by_estimate$status[1], "fitted")
  # 19 of 20 correct cannot put the lower bound at 0.9
  expect_identical(by_lower$status[1], "target_unreachable")
  expect_identical(by_lower$threshold[1], 0.3)
  expect_identical(by_lower$source[1], "fallback")

  fixture <- resolve_fixture()
  relaxed <- thr_resolve_thresholds(
    fixture$strata, fixture$labels, fixture$fallback,
    edges = strata_small_edges(), target = 0.85, min_class_n = 50,
    min_stratum_n = 10, seed = 1
  )
  expect_identical(relaxed$threshold[1], 0.5)
})

test_that("the result does not depend on the row order of the counts", {
  fixture <- resolve_fixture()
  shuffled <- fixture$strata[rev(seq_len(nrow(fixture$strata))), ]
  shuffled$class_name <- factor(shuffled$class_name)

  resolved <- thr_resolve_thresholds(
    shuffled, fixture$labels, fixture$fallback, overrides = fixture$overrides,
    edges = strata_small_edges(), min_class_n = 50, min_stratum_n = 10,
    seed = 1
  )

  expect_identical(resolved, resolve_fixture_result())
})

test_that("a seed makes the result reproducible and independent per class", {
  fixture <- resolve_fixture()
  alone <- thr_resolve_thresholds(
    fixture$strata[fixture$strata$class_name == "fitted", ], "fitted", 0.3,
    edges = strata_small_edges(), min_class_n = 50, min_stratum_n = 10,
    seed = 1
  )

  expect_identical(resolve_fixture_result(), resolve_fixture_result())
  # A class's interval does not move when other classes are added
  expect_identical(alone$lower, resolve_fixture_result()$lower[1])
})

test_that("awkward class labels pass through untouched", {
  labels <- strata_awkward_labels()
  strata <- strata_for_class(labels[4], 1:3, c(100, 50, 50), c(20, 20, 20),
                             c(10, 16, 19))

  resolved <- thr_resolve_thresholds(strata, labels, rep(NA_real_, 5),
                                     edges = strata_small_edges(), seed = 1)

  expect_identical(resolved$class_name, labels)
  expect_identical(resolved$status,
                   replace(rep("too_few_predictions", 5), 4, "fitted"))
  expect_identical(resolved$threshold, c(0, 0, 0, 0.9, 0))
})

test_that("thr_resolve_thresholds works with the package defaults", {
  n_bins <- length(THR_SCORE_EDGES)
  strata <- strata_for_class("Debris", seq_len(n_bins), rep(100, n_bins),
                             rep(10, n_bins), c(rep(5, n_bins - 2), 10, 10))

  resolved <- thr_resolve_thresholds(strata, c("Debris", "Other"),
                                     c(0.5, 0.5), seed = 1)

  expect_identical(resolved$status, c("fitted", "too_few_predictions"))
  expect_identical(resolved$threshold,
                   c(THR_SCORE_EDGES[n_bins - 1], 0.5))
})

test_that("thr_resolve_thresholds rejects inputs that do not line up", {
  fixture <- resolve_fixture()
  resolve <- function(strata = fixture$strata, labels = fixture$labels,
                      fallback = fixture$fallback, overrides = NULL) {
    thr_resolve_thresholds(strata, labels, fallback, overrides = overrides,
                           edges = strata_small_edges())
  }
  override <- function(class_name, threshold) {
    data.frame(class_name = class_name, threshold = threshold,
               stringsAsFactors = FALSE)
  }

  expect_error(resolve(labels = fixture$labels[-1], fallback = fixture$fallback[-1]),
               "not in `class_labels`")
  expect_error(resolve(fallback = c(0.5, 0.5)), "one value per class")
  expect_error(resolve(fallback = replace(fixture$fallback, 1, 1.5)),
               "between 0 and 1")
  expect_error(resolve(labels = c(fixture$labels, "fitted"),
                       fallback = c(fixture$fallback, 0.1)), "more than once")
  expect_error(resolve(labels = character(0), fallback = numeric(0)),
               "class_labels")
  expect_error(resolve(strata = fixture$strata[, -1]), "class_name")
  expect_error(resolve(overrides = override("unknown", 0.5)),
               "not in `class_labels`")
  expect_error(resolve(overrides = override("fitted", 1.5)), "between 0 and 1")
  expect_error(resolve(overrides = override("fitted", NA_real_)),
               "between 0 and 1")
  expect_error(resolve(overrides = override(c("fitted", "fitted"), c(0.1, 0.2))),
               "more than once")
  expect_error(resolve(overrides = data.frame(class = "fitted", value = 0.5)),
               "class_name")
})
