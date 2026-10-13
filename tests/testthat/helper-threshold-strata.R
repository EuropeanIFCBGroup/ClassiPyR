# Shared test helpers for the threshold statistics and export layer
# (test-threshold_estimate.R, test-threshold_choice.R, test-threshold_export.R).
# Every helper is prefixed `strata_` so it cannot collide with the helpers of
# the other threshold test files.

# Stratum counts for one class, in the shape estimate_precision_curve() takes.
strata_one_class <- function(bin, n_population, n_verdicts = 0L,
                             n_correct = n_verdicts) {
  data.frame(
    bin = as.integer(bin),
    n_population = as.integer(n_population),
    n_verdicts = as.integer(n_verdicts),
    n_correct = as.integer(n_correct),
    stringsAsFactors = FALSE
  )
}

# The same with a class_name column, in the shape thr_resolve_thresholds() takes.
strata_for_class <- function(class_name, bin, n_population, n_verdicts = 0L,
                             n_correct = n_verdicts) {
  counts <- strata_one_class(bin, n_population, n_verdicts, n_correct)
  cbind(
    data.frame(class_name = rep(class_name, nrow(counts)),
               stringsAsFactors = FALSE),
    counts
  )
}

# Three wide bins keep the hand calculations in the tests readable.
strata_small_edges <- function() c(0, 0.5, 0.9)

# Worked example used across the tests (edges 0, 0.5, 0.9):
#   bin 1: N = 100, 10 verdicts,  5 correct -> 0.50
#   bin 2: N =  50, 10 verdicts,  8 correct -> 0.80
#   bin 3: N =  50, 20 verdicts, 19 correct -> 0.95
# Population-weighted precision at or above each edge:
#   0   : (100 * 0.50 + 50 * 0.80 + 50 * 0.95) / 200 = 0.6875
#   0.5 : (50 * 0.80 + 50 * 0.95) / 100              = 0.875
#   0.9 : 0.95
# Pooling the verdicts instead would give 32/40 = 0.8, 27/30 = 0.9 and 0.95.
strata_worked_example <- function() {
  strata_one_class(
    bin = 1:3,
    n_population = c(100, 50, 50),
    n_verdicts = c(10, 10, 20),
    n_correct = c(5, 8, 19)
  )
}

# A precision curve written out by hand, for the choice tests. `lower` and
# `upper` default to a fixed band around the estimate.
strata_curve <- function(threshold, precision, supported = TRUE,
                         kept_share = rev(seq_along(threshold)) / length(threshold),
                         lower = precision - 0.05, upper = pmin(precision + 0.05, 1),
                         n_total = 1000) {
  n <- length(threshold)
  data.frame(
    threshold = threshold,
    bin = seq_len(n),
    n_population_kept = round(kept_share * n_total),
    kept_share = kept_share,
    n_verdicts = rev(seq_len(n)) * 20L,
    precision = precision,
    lower = lower,
    upper = upper,
    supported = rep_len(supported, n),
    stringsAsFactors = FALSE
  )
}

# The five deliberately awkward labels of the h5 fixtures (slash, `_001`
# suffix, non-ASCII), read from the fixture description itself.
strata_awkward_labels <- function() {
  expected <- jsonlite::fromJSON(
    testthat::test_path("test_data", "threshold_review", "expected.json"),
    simplifyVector = FALSE
  )
  enc2utf8(unlist(expected$class_labels))
}

strata_model_fixture <- function(class_labels = strata_awkward_labels()) {
  list(
    model_id = "0123456789abcdef0123456789abcdef",
    classifier_name = "fixture_A",
    checkpoint_sha256 = paste(rep("ab", 32), collapse = ""),
    transform = "dataset_squarepad",
    class_labels = class_labels
  )
}

# Resolved thresholds as thr_resolve_thresholds() returns them, written out by
# hand so the export tests do not depend on the estimator. One row per class
# of strata_awkward_labels(), covering every source and awkward numbers.
strata_resolved_fixture <- function(class_labels = strata_awkward_labels()) {
  stopifnot(length(class_labels) == 5)
  data.frame(
    class_name = class_labels,
    threshold = c(0.1 + 0.2, 0.9999, 1 / 3, 0, 1),
    source = c("fitted", "fitted", "manual", "fallback", "fallback"),
    status = c("fitted", "fitted", "manual", "not_reviewed",
               "target_unreachable"),
    reason = c("Fitted.", "Fitted, with a \"quote\" and a comma.",
               "Set manually.", "Not reviewed.", "Target not reached."),
    n_predicted = c(400L, 250L, 120L, 60L, 0L),
    n_verdicts = c(120L, 80L, 60L, 0L, 0L),
    n_correct = c(110L, 79L, 41L, 0L, 0L),
    precision = c(0.93, 2 / 3, 0.7, NA, NA),
    lower = c(0.88, 0.61, 0.6, NA, NA),
    upper = c(0.97, 0.72, 0.8, NA, NA),
    kept_share = c(0.75, 0.5, 1, NA, NA),
    fitted_threshold = c(0.1 + 0.2, 0.9999, 0.5, NA, NA),
    previous_threshold = c(0.3, 0.05, 0.9, NA, 1),
    stringsAsFactors = FALSE
  )
}

strata_export_fixture <- function(class_labels = strata_awkward_labels()) {
  build_thresholds_export(
    strata_resolved_fixture(class_labels),
    strata_model_fixture(class_labels),
    target = 0.9,
    fallback_source = "h5",
    generated_by = "ClassiPyR test"
  )
}

# Bit pattern of a double, for comparisons that must not go through any
# decimal formatting or parsing.
strata_double_bits <- function(x) {
  vapply(x, function(value) {
    paste(as.character(writeBin(value, raw(), endian = "big")), collapse = "")
  }, character(1), USE.NAMES = FALSE)
}
