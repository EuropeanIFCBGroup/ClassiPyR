# Tests for the stratified draws of the threshold review app

library(testthat)

thr_test_alloc_strata <- function(bin, n_population, n_drawn = 0L, n_have = n_drawn) {
  data.frame(bin = as.integer(bin), n_population = as.integer(n_population),
             n_drawn = as.integer(n_drawn), n_have = as.integer(n_have))
}

thr_test_unit_key <- function(page) {
  paste(page$sample_name, page$roi_number)
}

# Every ROI the index holds for a class of a model, as "sample roi" keys
thr_test_class_units <- function(index_path, model_id, class_idx, samples = NULL) {
  units <- thr_test_query(
    index_path,
    "SELECT f.sample_name, r.roi_number, r.score FROM rois r
     JOIN files f ON f.file_id = r.file_id
     WHERE f.model_id = ? AND f.status = 'ok' AND r.class_idx = ?",
    params = list(model_id, class_idx)
  )
  if (!is.null(samples)) {
    units <- units[units$sample_name %in% samples, , drop = FALSE]
  }
  units
}

# Draw and confirm pages until the class runs dry; returns every page stacked
thr_test_drain_class <- function(space, model_id, class_name, page_size = 30) {
  pages <- list()
  repeat {
    page <- thr_next_page(space$db_path, space$index_path, model_id, class_name,
                          page_size = page_size)
    if (nrow(page) == 0) break
    pages[[length(pages) + 1]] <- page
    thr_test_confirm_page(space$db_path, page)
    if (length(pages) > 200) stop("the class never ran dry")
  }
  do.call(rbind, pages)
}

thr_test_frozen_synthetic <- function(n_samples = 6, n_rois = 200, ...) {
  space <- thr_test_db_space()
  model_id <- thr_test_synthetic_index(space, n_samples = n_samples, n_rois = n_rois)
  thr_freeze_frame(space$db_path, space$index_path, model_id, ...)
  c(space, list(model_id = model_id))
}

# --- thr_allocate_draw -------------------------------------------------------

test_that("allocation fills the least covered strata first", {
  strata <- thr_test_alloc_strata(1:3, c(100, 100, 100), n_drawn = c(0, 5, 10))

  # Bin 1 is raised to 5, then bins 1 and 2 take turns, the lower bin first
  expect_identical(thr_allocate_draw(strata, 12), c(9L, 3L, 0L))
  expect_identical(thr_allocate_draw(strata, 5), c(5L, 0L, 0L))
  expect_identical(thr_allocate_draw(strata, 6), c(6L, 0L, 0L))
  expect_identical(thr_allocate_draw(strata, 7), c(6L, 1L, 0L))
  # All level at 10 after 15, then one each
  expect_identical(thr_allocate_draw(strata, 18), c(11L, 6L, 1L))
})

test_that("allocation spreads a first page evenly over the bins", {
  strata <- thr_test_alloc_strata(1:4, rep(1000, 4))

  expect_identical(thr_allocate_draw(strata, 100), rep(25L, 4))
  expect_identical(thr_allocate_draw(strata, 6), c(2L, 2L, 1L, 1L))
})

test_that("a small stratum is taken whole", {
  strata <- thr_test_alloc_strata(1:3, c(3, 100, 1))

  expect_identical(thr_allocate_draw(strata, 20), c(3L, 16L, 1L))
})

test_that("exhausted strata get nothing", {
  strata <- thr_test_alloc_strata(1:2, c(5, 50), n_drawn = c(5, 10))

  expect_identical(thr_allocate_draw(strata, 4), c(0L, 4L))
})

test_that("unavailable draws use up the stratum but not its coverage", {
  # Bin 1: 4 drawn, 3 of them unavailable, so it counts as covered by 1
  strata <- thr_test_alloc_strata(1:2, c(10, 10), n_drawn = c(4, 2),
                                  n_have = c(1, 2))

  expect_identical(thr_allocate_draw(strata, 3), c(2L, 1L))
  # Only 6 ROIs of bin 1 are still undrawn
  expect_identical(thr_allocate_draw(strata, 100), c(6L, 8L))
})

test_that("a request larger than the supply returns what is left", {
  strata <- thr_test_alloc_strata(1:2, c(2, 3), n_drawn = c(1, 0))

  allocation <- thr_allocate_draw(strata, 100)

  expect_identical(allocation, c(1L, 3L))
  expect_identical(thr_allocate_draw(thr_test_alloc_strata(1:2, c(2, 3),
                                                           n_drawn = c(2, 3)), 10),
                   c(0L, 0L))
})

test_that("allocation is aligned with the rows, whatever their order", {
  strata <- thr_test_alloc_strata(c(3, 1, 2), c(10, 10, 10))

  expect_identical(thr_allocate_draw(strata, 1), c(0L, 1L, 0L))
  expect_identical(thr_allocate_draw(strata, 5), c(1L, 2L, 2L))
})

test_that("allocation handles empty requests and empty input", {
  strata <- thr_test_alloc_strata(1:2, c(10, 10))

  expect_identical(thr_allocate_draw(strata, 0), c(0L, 0L))
  expect_identical(thr_allocate_draw(strata[0, ], 10), integer())
})

test_that("thr_allocate_draw validates its arguments", {
  strata <- thr_test_alloc_strata(1:2, c(10, 10))

  expect_error(thr_allocate_draw(strata[c("bin", "n_population")], 5), "n_drawn")
  expect_error(thr_allocate_draw(list(bin = 1), 5), "strata")
  expect_error(thr_allocate_draw(strata, -1), "n_request")
  expect_error(thr_allocate_draw(strata, 2.5), "n_request")
  expect_error(thr_allocate_draw(strata, NA), "n_request")
  expect_error(thr_allocate_draw(strata, c(1, 2)), "n_request")
  bad <- strata
  bad$n_drawn[1] <- NA
  expect_error(thr_allocate_draw(bad, 5), "n_drawn")
  bad <- strata
  bad$n_population[2] <- -3L
  expect_error(thr_allocate_draw(bad, 5), "n_population")
  bad <- strata
  bad$bin <- c(1L, 1L)
  expect_error(thr_allocate_draw(bad, 5), "bin")
})

# --- thr_draw_units ----------------------------------------------------------

test_that("units are drawn without replacement from the candidates", {
  candidates <- data.frame(sample_name = rep(c("a", "b"), each = 50),
                           roi_number = rep(1:50, 2), stringsAsFactors = FALSE)

  drawn <- thr_draw_units(candidates, 30)

  expect_equal(nrow(drawn), 30)
  expect_identical(names(drawn), names(candidates))
  expect_equal(anyDuplicated(thr_test_unit_key(drawn)), 0)
  expect_true(all(thr_test_unit_key(drawn) %in% thr_test_unit_key(candidates)))
})

test_that("asking for more units than there are returns them all, once", {
  candidates <- data.frame(roi_number = c(42L, 7L, 9L))

  expect_setequal(thr_draw_units(candidates, 10)$roi_number, c(42L, 7L, 9L))
  expect_equal(nrow(thr_draw_units(candidates, 0)), 0)
  expect_identical(names(thr_draw_units(candidates, 0)), "roi_number")
  # One candidate with a large number must come back as itself
  expect_identical(thr_draw_units(candidates[1, , drop = FALSE], 1)$roi_number, 42L)
  expect_equal(nrow(thr_draw_units(candidates[0, , drop = FALSE], 5)), 0)
})

test_that("a seed reproduces the draw and leaves the global RNG alone", {
  candidates <- data.frame(roi_number = 1:1000)
  set.seed(99)
  state <- .Random.seed

  first <- thr_draw_units(candidates, 20, seed = 11)
  second <- thr_draw_units(candidates, 20, seed = 11)
  other <- thr_draw_units(candidates, 20, seed = 12)

  expect_identical(first, second)
  expect_false(identical(first$roi_number, other$roi_number))
  expect_identical(.Random.seed, state)
})

test_that("a seeded draw does not create a global RNG state", {
  global <- globalenv()
  had_state <- exists(".Random.seed", envir = global, inherits = FALSE)
  old_state <- if (had_state) get(".Random.seed", envir = global)
  on.exit(if (had_state) assign(".Random.seed", old_state, envir = global),
          add = TRUE)
  if (had_state) rm(".Random.seed", envir = global)

  thr_draw_units(data.frame(roi_number = 1:100), 5, seed = 3)

  expect_false(exists(".Random.seed", envir = global, inherits = FALSE))
})

test_that("without a seed the draw uses the session's random stream", {
  candidates <- data.frame(roi_number = 1:1000)

  set.seed(5)
  first <- thr_draw_units(candidates, 20)
  after_first <- .Random.seed
  second <- thr_draw_units(candidates, 20)
  set.seed(5)
  replay <- thr_draw_units(candidates, 20)

  expect_false(identical(first$roi_number, second$roi_number))
  expect_identical(first, replay)
  set.seed(5)
  expect_false(identical(.Random.seed, after_first))
})

test_that("thr_draw_units validates its arguments", {
  candidates <- data.frame(roi_number = 1:10)

  expect_error(thr_draw_units(1:10, 3), "candidates")
  expect_error(thr_draw_units(candidates, -1), "`n`")
  expect_error(thr_draw_units(candidates, 1.5), "`n`")
  expect_error(thr_draw_units(candidates, NA), "`n`")
  expect_error(thr_draw_units(candidates, 3, seed = "a"), "seed")
  expect_error(thr_draw_units(candidates, 3, seed = c(1, 2)), "seed")
  expect_error(thr_draw_units(candidates, 3, seed = NA_real_), "seed")
})

# --- thr_next_page: basics ---------------------------------------------------

test_that("a page needs a frozen frame and a class of the model", {
  space <- thr_test_db_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  thr_test_fixture_index(space)
  page <- function(model_id = thr_test_model_a_id, class_name = "Debris", ...) {
    thr_next_page(space$db_path, space$index_path, model_id, class_name, ...)
  }

  expect_error(page(), "No verdict database")
  expect_false(file.exists(space$db_path))
  thr_freeze_frame(space$db_path, space$index_path, thr_test_model_a_id)

  expect_error(page(model_id = thr_test_model_b_id), "no frozen sampling frame")
  expect_error(page(class_name = "Debris_typo"), "not a class of this model")
  expect_error(page(class_name = c("Debris", "Skeletonema_spp")), "class_name")
  expect_error(page(page_size = 0), "page_size")
  expect_error(page(page_size = 2.5), "page_size")
  expect_error(page(page_size = NA), "page_size")
  expect_error(page(seed = "x"), "seed")
  expect_equal(nrow(thr_test_table(space$db_path, "draws")), 0)
})

test_that("the first page draws the class and stores the draws as pending", {
  space <- thr_test_frozen_fixture()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)

  page <- thr_next_page(space$db_path, space$index_path, thr_test_model_a_id,
                        "Skeletonema_spp")

  expect_identical(names(page), c("draw_id", "class_name", "bin", "sample_name",
                                  "roi_number", "score", "is_new"))
  # 18 ROIs of the class are in the frame, fewer than a page
  expect_equal(nrow(page), 18)
  expect_true(all(page$is_new))
  expect_true(all(page$class_name == "Skeletonema_spp"))
  expect_false(is.unsorted(rev(page$score)))
  expect_type(page$draw_id, "integer")
  expect_type(page$bin, "integer")
  expect_type(page$roi_number, "integer")
  expect_identical(page$bin, thr_score_bin(page$score, THR_SCORE_EDGES))

  units <- thr_test_class_units(space$index_path, thr_test_model_a_id, 1L)
  expect_setequal(thr_test_unit_key(page), thr_test_unit_key(units))
  at <- match(thr_test_unit_key(page), thr_test_unit_key(units))
  expect_identical(page$score, units$score[at])

  draws <- thr_test_table(space$db_path, "draws")
  expect_equal(nrow(draws), 18)
  expect_true(all(draws$status == "pending"))
  expect_true(all(is.na(draws$verdict)))
  expect_true(all(is.na(draws$page_token)))
  expect_true(all(draws$model_id == thr_test_model_a_id))
  expect_false(anyNA(as.POSIXct(draws$drawn_at, format = "%Y-%m-%dT%H:%M:%SZ",
                                tz = "UTC")))
})

test_that("class names with awkward characters are drawn and stored as they are", {
  space <- thr_test_frozen_fixture()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)

  for (class_name in c("Snowella/Woronichinia", "Pauliella_taeniata_001",
                       "Öresund_ciliate")) {
    page <- thr_next_page(space$db_path, space$index_path, thr_test_model_a_id,
                          class_name)
    expect_gt(nrow(page), 0)
    expect_true(all(page$class_name == class_name))
  }
  stored <- enc2utf8(unique(thr_test_table(space$db_path, "draws")$class_name))
  expect_setequal(stored, c("Snowella/Woronichinia", "Pauliella_taeniata_001",
                            "Öresund_ciliate"))
  overview <- thr_class_overview(space$db_path, thr_test_model_a_id)
  expect_identical(overview$n_drawn, c(0L, 7L, 16L, 12L, 0L))
})

test_that("two calls without confirming return the same draws", {
  space <- thr_test_frozen_synthetic()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)

  first <- thr_next_page(space$db_path, space$index_path, space$model_id,
                         "Debris", page_size = 40)
  second <- thr_next_page(space$db_path, space$index_path, space$model_id,
                          "Debris", page_size = 40)

  expect_equal(nrow(first), 40)
  expect_true(all(first$is_new))
  expect_false(any(second$is_new))
  expect_identical(second[names(second) != "is_new"],
                   first[names(first) != "is_new"])
  expect_equal(nrow(thr_test_table(space$db_path, "draws")), 40)
})

test_that("pending draws come first and the page is topped up", {
  space <- thr_test_frozen_synthetic()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  page <- function(page_size) {
    thr_next_page(space$db_path, space$index_path, space$model_id, "Debris",
                  page_size = page_size)
  }

  first <- page(10)
  larger <- page(25)

  expect_equal(nrow(larger), 25)
  expect_setequal(larger$draw_id[!larger$is_new], first$draw_id)
  expect_equal(sum(larger$is_new), 15)

  # A smaller page shows the oldest pending draws and draws nothing new
  smaller <- page(5)
  expect_equal(nrow(smaller), 5)
  expect_false(any(smaller$is_new))
  expect_setequal(smaller$draw_id, sort(larger$draw_id)[1:5])
  expect_equal(nrow(thr_test_table(space$db_path, "draws")), 25)
})

test_that("a page only draws the class that was asked for", {
  space <- thr_test_frozen_synthetic()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)

  debris <- thr_next_page(space$db_path, space$index_path, space$model_id,
                          "Debris", page_size = 10)
  other <- thr_next_page(space$db_path, space$index_path, space$model_id,
                         "Skeletonema_spp", page_size = 10)

  expect_true(all(other$is_new))
  expect_length(intersect(other$draw_id, debris$draw_id), 0)
  again <- thr_next_page(space$db_path, space$index_path, space$model_id,
                         "Debris", page_size = 10)
  expect_setequal(again$draw_id, debris$draw_id)
})

# --- thr_next_page: without replacement, within the frame --------------------

test_that("draws never repeat a unit and never leave the frame", {
  space <- thr_test_frozen_synthetic(
    excluded_samples = "D20230201T000000_IFCB134",
    no_raw_samples = "D20230501T000000_IFCB134"
  )
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  frame_samples <- thr_test_table(space$db_path, "frame_files")$sample_name
  expect_length(frame_samples, 4)

  drawn <- thr_test_drain_class(space, space$model_id, "Debris", page_size = 30)

  expect_equal(anyDuplicated(thr_test_unit_key(drawn)), 0)
  expect_equal(anyDuplicated(drawn$draw_id), 0)
  expect_true(all(drawn$sample_name %in% frame_samples))
  # Exactly the class's ROIs in the frame: every one drawn, nothing else
  units <- thr_test_class_units(space$index_path, space$model_id, 5L,
                                samples = frame_samples)
  expect_setequal(thr_test_unit_key(drawn), thr_test_unit_key(units))
  overview <- thr_class_overview(space$db_path, space$model_id)
  expect_identical(overview$n_drawn[5], overview$n_predicted[5])
  expect_identical(overview$n_verdicts[5], overview$n_predicted[5])
})

test_that("an exhausted class returns zero rows", {
  space <- thr_test_frozen_fixture()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  page <- thr_next_page(space$db_path, space$index_path, thr_test_model_a_id,
                        "Debris")
  thr_test_confirm_page(space$db_path, page)

  empty <- thr_next_page(space$db_path, space$index_path, thr_test_model_a_id,
                         "Debris")

  expect_equal(nrow(empty), 0)
  expect_identical(names(empty), names(page))
  expect_identical(vapply(empty, typeof, character(1)),
                   vapply(page, typeof, character(1)))
})

test_that("a class the model never predicted returns zero rows", {
  space <- thr_test_db_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  thr_test_write_fake(space$h5_folder, thr_test_a_samples[1])
  build_threshold_index(space$h5_folder, space$index_path,
                        reader = thr_test_fake_reader())
  model_id <- thr_index_models(space$index_path)$model_id
  thr_freeze_frame(space$db_path, space$index_path, model_id)

  page <- thr_next_page(space$db_path, space$index_path, model_id, "Debris")

  expect_equal(nrow(page), 0)
  expect_equal(nrow(thr_test_table(space$db_path, "draws")), 0)
})

test_that("unavailable draws are not drawn again and are made up for", {
  space <- thr_test_frozen_synthetic()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  first <- thr_next_page(space$db_path, space$index_path, space$model_id,
                         "Debris", page_size = 20)
  thr_test_execute(space$db_path,
                   "UPDATE draws SET status = 'unavailable' WHERE draw_id = ?",
                   params = list(first$draw_id[1:8]))

  second <- thr_next_page(space$db_path, space$index_path, space$model_id,
                          "Debris", page_size = 20)

  expect_equal(nrow(second), 20)
  expect_equal(sum(second$is_new), 8)
  expect_length(intersect(second$draw_id, first$draw_id[1:8]), 0)
  expect_length(intersect(thr_test_unit_key(second[second$is_new, ]),
                          thr_test_unit_key(first)), 0)
})

test_that("two models that scored the same sample are drawn independently", {
  space <- thr_test_frozen_fixture()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  thr_freeze_frame(space$db_path, space$index_path, thr_test_model_b_id)

  page_a <- thr_next_page(space$db_path, space$index_path, thr_test_model_a_id,
                          "Debris")
  page_b <- thr_next_page(space$db_path, space$index_path, thr_test_model_b_id,
                          "Debris")

  expect_equal(nrow(page_a), 12)
  units_b <- thr_test_class_units(space$index_path, thr_test_model_b_id, 5L)
  expect_setequal(thr_test_unit_key(page_b), thr_test_unit_key(units_b))
  expect_identical(page_b$score,
                   units_b$score[match(thr_test_unit_key(page_b),
                                       thr_test_unit_key(units_b))])
})

# --- thr_next_page: stratification -------------------------------------------

test_that("a page is spread over the score bins and small bins are taken whole", {
  space <- thr_test_frozen_synthetic()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)

  page <- thr_next_page(space$db_path, space$index_path, space$model_id,
                        "Debris", page_size = 60)

  counts <- thr_stratum_counts(space$db_path, space$model_id, "Debris")
  expect_equal(sum(counts$n_pending), 60)
  expect_identical(as.integer(table(factor(page$bin, levels = counts$bin))),
                   counts$n_pending)
  whole <- counts$n_pending == counts$n_population
  # Strata that were not taken whole are level to within one draw
  expect_lte(diff(range(counts$n_pending[!whole])), 1)
  expect_true(all(counts$n_pending[whole] <= min(counts$n_pending[!whole]) + 1))
  expect_true(any(whole))
})

test_that("pending draws count as coverage when a page is topped up", {
  space <- thr_test_frozen_synthetic()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  n_strata <- nrow(thr_stratum_counts(space$db_path, space$model_id, "Debris"))
  expect_gt(n_strata, 6)

  # Three draws, one in each of the three lowest bins, left unconfirmed
  thr_next_page(space$db_path, space$index_path, space$model_id, "Debris",
                page_size = 3)
  thr_next_page(space$db_path, space$index_path, space$model_id, "Debris",
                page_size = n_strata)

  # The top-up went to the bins that had nothing yet
  counts <- thr_stratum_counts(space$db_path, space$model_id, "Debris")
  expect_identical(counts$n_pending, rep(1L, n_strata))
})

test_that("later pages top up the least covered bins", {
  space <- thr_test_frozen_synthetic()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  first <- thr_next_page(space$db_path, space$index_path, space$model_id,
                         "Debris", page_size = 60)
  # Lose every draw of one bin: its coverage drops to zero
  lost_bin <- as.integer(names(which.max(table(first$bin))))
  thr_test_confirm_page(space$db_path, first[first$bin != lost_bin, ])
  thr_test_execute(space$db_path,
                   "UPDATE draws SET status = 'unavailable' WHERE draw_id = ?",
                   params = list(first$draw_id[first$bin == lost_bin]))
  second <- thr_next_page(space$db_path, space$index_path, space$model_id,
                          "Debris", page_size = 2)

  # Every other bin is covered by more than two verdicts, so both go here
  expect_equal(nrow(second), 2)
  expect_true(all(second$bin == lost_bin))
  expect_true(all(second$is_new))
})

# --- thr_next_page: seeds ----------------------------------------------------

test_that("a seed reproduces a page and does not disturb the global RNG", {
  first_space <- thr_test_frozen_synthetic()
  second_space <- thr_test_frozen_synthetic()
  third_space <- thr_test_frozen_synthetic()
  on.exit(unlink(c(first_space$root, second_space$root, third_space$root),
                 recursive = TRUE), add = TRUE)
  page <- function(space, seed) {
    thr_next_page(space$db_path, space$index_path, space$model_id, "Debris",
                  page_size = 40, seed = seed)
  }
  set.seed(2024)
  state <- .Random.seed

  first <- page(first_space, 7)
  second <- page(second_space, 7)
  third <- page(third_space, 8)

  expect_identical(.Random.seed, state)
  columns <- c("bin", "sample_name", "roi_number", "score")
  expect_identical(first[columns], second[columns])
  expect_false(setequal(thr_test_unit_key(first), thr_test_unit_key(third)))
})

test_that("without a seed the draw is random", {
  first_space <- thr_test_frozen_synthetic()
  second_space <- thr_test_frozen_synthetic()
  on.exit(unlink(c(first_space$root, second_space$root), recursive = TRUE),
          add = TRUE)
  page <- function(space) {
    thr_next_page(space$db_path, space$index_path, space$model_id, "Debris",
                  page_size = 40)
  }

  expect_false(setequal(thr_test_unit_key(page(first_space)),
                        thr_test_unit_key(page(second_space))))
})

# --- thr_next_page: frame files that changed ---------------------------------

test_that("a frame file that was re-scored is no longer drawn from", {
  space <- thr_test_frozen_synthetic()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  changed_sample <- "D20230301T000000_IFCB134"
  # Same model, same sample, other content: every ROI is now Debris
  thr_test_write_fake(
    file.path(space$h5_folder, "synthetic"), changed_sample,
    thr_test_summary(roi_numbers = 1:300, class_idx = rep(5L, 300),
                     top_score = thr_test_spread_scores(300))
  )
  build_threshold_index(space$h5_folder, space$index_path,
                        reader = thr_test_fake_reader())
  status <- thr_frame_status(space$db_path, space$index_path, space$model_id)
  expect_identical(status$n_changed_files, 1L)

  drawn <- thr_test_drain_class(space, space$model_id, "Debris", page_size = 50)

  expect_false(changed_sample %in% drawn$sample_name)
  intact <- setdiff(thr_test_table(space$db_path, "frame_files")$sample_name,
                    changed_sample)
  units <- thr_test_class_units(space$index_path, space$model_id, 5L,
                                samples = intact)
  # Every reachable ROI is still drawn: the changed file does not starve pages
  expect_setequal(thr_test_unit_key(drawn), thr_test_unit_key(units))
  counts <- thr_stratum_counts(space$db_path, space$model_id, "Debris")
  expect_true(all(counts$n_verdicts <= counts$n_population))
})

test_that("pages are filled from other bins when a changed file leaves one short", {
  space <- thr_test_frozen_synthetic()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  unlink(file.path(space$h5_folder, "synthetic",
                   "D20230301T000000_IFCB134_class.h5"))
  build_threshold_index(space$h5_folder, space$index_path,
                        reader = thr_test_fake_reader())
  counts <- thr_stratum_counts(space$db_path, space$model_id, "Debris")
  reachable <- nrow(thr_test_class_units(space$index_path, space$model_id, 5L))
  expect_lt(reachable, sum(counts$n_population))

  # Small bins cannot supply what the frame promised; the page is still full
  page <- thr_next_page(space$db_path, space$index_path, space$model_id,
                        "Debris", page_size = 100)
  expect_equal(nrow(page), 100)

  # Asking for everything the frame promised returns what can be reached
  everything <- thr_next_page(space$db_path, space$index_path, space$model_id,
                              "Debris", page_size = sum(counts$n_population))
  expect_equal(nrow(everything), reachable)
  expect_equal(sum(everything$is_new), reachable - 100)
})

# --- thr_next_page: atomicity ------------------------------------------------

test_that("a failure while drawing leaves no draws behind", {
  space <- thr_test_frozen_synthetic()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)

  # Fail at the third stratum, after two strata were already written
  local({
    calls <- 0
    local_mocked_bindings(thr_insert_draws = function(con, ...) {
      calls <<- calls + 1
      if (calls == 3) stop("disk full", call. = FALSE)
      DBI::dbExecute(con, "
        INSERT INTO draws (model_id, class_name, bin, sample_name, roi_number,
                           score, drawn_at)
        VALUES ('x', 'Debris', 1, 's', ?, 0.5, 'now')", params = list(calls))
    })
    expect_error(
      thr_next_page(space$db_path, space$index_path, space$model_id, "Debris"),
      "disk full"
    )
  })

  expect_equal(nrow(thr_test_table(space$db_path, "draws")), 0)
  page <- thr_next_page(space$db_path, space$index_path, space$model_id, "Debris")
  expect_equal(nrow(page), 100)
})

test_that("candidates that do not fall in the stratum's bin stop the draw", {
  space <- thr_test_frozen_synthetic()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)

  local({
    local_mocked_bindings(
      thr_score_bin = function(score, edges) rep(1L, length(score))
    )
    expect_error(
      thr_next_page(space$db_path, space$index_path, space$model_id, "Debris"),
      "Internal error"
    )
  })

  expect_equal(nrow(thr_test_table(space$db_path, "draws")), 0)
})
