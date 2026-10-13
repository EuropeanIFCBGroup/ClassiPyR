# Tests for threshold resolution when some images can never be reviewed

library(testthat)

# Three classes whose reviews are all held back by images that could not be
# shown: one image of a small top bin, one image of a class of exactly 50,
# and every image of a class.
resolve_unavailable_fixture <- function() {
  strata <- rbind(
    strata_for_class("small_bin", 1:3, c(100, 100, 5), c(100, 100, 4)),
    strata_for_class("exactly_fifty", 1:3, c(20, 20, 10), c(20, 20, 9)),
    strata_for_class("short", 1:3, c(20, 20, 10), c(20, 10, 0)),
    strata_for_class("nothing", 1:3, c(30, 20, 10))
  )
  strata$n_unavailable <- c(0L, 0L, 1L,  0L, 0L, 1L,  0L, 0L, 2L,
                            30L, 20L, 10L)
  strata
}

resolve_unavailable <- function(strata = resolve_unavailable_fixture()) {
  labels <- c("small_bin", "exactly_fifty", "short", "nothing")
  resolved <- thr_resolve_thresholds(strata, labels, fallback = rep(0.3, 4),
                                     edges = strata_small_edges(),
                                     target = 0.9, min_class_n = 50,
                                     min_stratum_n = 10, seed = 1)
  stats::setNames(split(resolved, seq_len(nrow(resolved))), labels)
}

test_that("images that cannot be reviewed do not stop a class being fitted", {
  resolved <- resolve_unavailable()

  expect_identical(resolved$small_bin$status, "fitted")
  expect_identical(resolved$small_bin$threshold, 0)
  expect_identical(resolved$exactly_fifty$status, "fitted")
  expect_identical(resolved$exactly_fifty$threshold, 0)
  expect_identical(resolved$exactly_fifty$n_verdicts, 49L)
  expect_identical(resolved$exactly_fifty$n_predicted, 50L)

  # The same counts without the column: both wait for a review that cannot
  # happen
  strata <- resolve_unavailable_fixture()
  blind <- resolve_unavailable(strata[names(strata) != "n_unavailable"])
  expect_identical(blind$small_bin$status, "under_reviewed")
  expect_identical(blind$exactly_fifty$status, "under_reviewed")
})

test_that("the verdicts asked for are those that can still be given", {
  resolved <- resolve_unavailable()

  # 50 predictions, 2 unavailable, 30 verdicts: 48 is all that can be asked
  expect_identical(resolved$short$status, "under_reviewed")
  expect_match(resolved$short$reason, "30 verdicts so far", fixed = TRUE)
  expect_match(resolved$short$reason, "48", fixed = TRUE)
  expect_no_match(resolved$short$reason, "at least 50", fixed = TRUE)

  # With the full minimum in reach the wording does not change
  strata <- resolve_unavailable_fixture()
  strata$n_unavailable <- 0L
  expect_match(resolve_unavailable(strata)$short$reason, "at least 50",
               fixed = TRUE)
})

test_that("a class in which nothing can be reviewed is not asked for more", {
  resolved <- resolve_unavailable()

  expect_identical(resolved$nothing$status, "not_reviewed")
  expect_identical(resolved$nothing$source, "fallback")
  expect_identical(resolved$nothing$threshold, 0.3)
  expect_match(resolved$nothing$reason, "could not be shown", fixed = TRUE)
  expect_match(resolved$nothing$reason, "60", fixed = TRUE)
  expect_no_match(resolved$nothing$reason, "yet", fixed = TRUE)
  expect_no_match(resolved$nothing$reason, "[Rr]eview(ing)? more")
  for (reason in vapply(resolved, function(row) row$reason, character(1))) {
    expect_no_match(reason, "  ", fixed = TRUE)
    expect_match(reason, "[.]$")
  }
})

test_that("no class is asked for more review once nothing is left to review", {
  # Every image that can be shown has a verdict, in every bin, whatever the
  # mix of unavailable images.
  population <- c(60, 12, 9, 4)
  unavailable <- list(c(0, 0, 0, 0), c(0, 0, 0, 4), c(0, 12, 9, 4),
                      c(59, 11, 8, 3), c(60, 0, 0, 0), c(10, 3, 9, 1),
                      c(60, 12, 9, 4))
  for (n_unavailable in unavailable) {
    strata <- strata_for_class("done", 1:4, population,
                               population - n_unavailable)
    strata$n_unavailable <- as.integer(n_unavailable)

    resolved <- thr_resolve_thresholds(strata, "done", fallback = 0.3,
                                       edges = c(0, 0.5, 0.9, 0.99),
                                       seed = 1)

    expect_no_match(resolved$reason, "[Rr]eview(ing)? more|so far|yet")
  }
})

# ---- from the verdict database -----------------------------------------------

test_that("unavailable draws reach the estimate from the verdict database", {
  class_name <- "Skeletonema_spp"
  space <- thr_test_app_frozen_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  page <- thr_next_page(space$db_path, space$index_path, space$model_id,
                        class_name, page_size = 1000)
  # The raw files hold no image for one draw of a bin of 3 and for the only
  # draw of a bin of 1; everything else is confirmed as correct.
  population <- table(page$bin)
  small <- as.integer(names(population)[population == 3][1])
  single <- max(as.integer(names(population)[population == 1]))
  no_image <- c(page$draw_id[page$bin == small][1],
                page$draw_id[page$bin == single])
  page$extracted <- !(page$draw_id %in% no_image)
  page$no_image_reason <- ifelse(page$extracted, NA_character_,
                                 "not_extractable")
  token <- thr_new_page_token()
  verdicts <- thr_resolve_page_verdicts(
    page, loaded = setdiff(page$draw_id, no_image), failed = integer(),
    wrong = integer(), token = token, expected_token = token
  )
  thr_save_verdicts(space$db_path, verdicts, "tester", token)

  resolved <- thr_resolve_thresholds_db(space$db_path, space$model_id,
                                        target = 0.9, seed = 1)
  row <- resolved[resolved$class_name == class_name, ]
  curve <- thr_class_curve(space$db_path, space$model_id, class_name)

  expect_identical(row$n_verdicts, nrow(page) - 2L)
  expect_identical(row$status, "fitted")
  expect_identical(row$threshold, 0)
  expect_identical(row$precision, 1)
  # Every edge is supported except the one that keeps nothing but the image
  # that cannot be shown
  expect_identical(curve$bin[curve$n_population_kept > 0 & !curve$supported],
                   single)
  expect_identical(curve$n_population_kept[1], nrow(page))
})
