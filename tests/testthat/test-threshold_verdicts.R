# Tests for resolving and recording the verdicts of a review page

library(testthat)

# A page as the verdict rules see it. Draws without an image say why:
# "not_extractable" (the raw file was read and the ROI could not be made)
# unless `reason` says otherwise.
thr_test_page <- function(draw_id = 1:6, extracted = TRUE,
                          reason = "not_extractable") {
  extracted <- rep_len(extracted, length(draw_id))
  data.frame(draw_id = as.integer(draw_id), extracted = extracted,
             no_image_reason = ifelse(extracted, NA_character_,
                                      rep_len(reason, length(draw_id))),
             stringsAsFactors = FALSE)
}

thr_test_resolve <- function(page = thr_test_page(), loaded = page$draw_id,
                             failed = integer(), wrong = integer(),
                             token = "tok", expected_token = "tok") {
  thr_resolve_page_verdicts(page, loaded, failed, wrong, token, expected_token)
}

# A frozen fixture frame with the 12 Debris ROIs of model A drawn as a page
thr_test_drawn_page <- function() {
  space <- thr_test_frozen_fixture()
  page <- thr_next_page(space$db_path, space$index_path, thr_test_model_a_id,
                        "Debris")
  page <- page[order(page$draw_id), ]
  page$extracted <- TRUE
  c(space, list(page = page))
}

# Mark rows of a drawn page as ROIs the raw file could not give an image for
thr_test_unextractable <- function(page, rows) {
  page$extracted[rows] <- FALSE
  page$no_image_reason <- NA_character_
  page$no_image_reason[rows] <- "not_extractable"
  page
}

thr_test_draws <- function(db_path) {
  thr_test_query(db_path, "SELECT * FROM draws ORDER BY draw_id")
}

# --- thr_new_page_token ------------------------------------------------------

test_that("page tokens are unique strings", {
  tokens <- vapply(1:300, function(i) thr_new_page_token(), character(1))

  expect_equal(anyDuplicated(tokens), 0)
  expect_true(all(nzchar(tokens)))
  expect_false(anyNA(tokens))
})

test_that("page tokens do not depend on or disturb the random number state", {
  set.seed(1)
  state <- .Random.seed
  first <- thr_new_page_token()
  expect_identical(.Random.seed, state)
  set.seed(1)
  second <- thr_new_page_token()

  expect_false(identical(first, second))
})

# --- thr_resolve_page_verdicts: the rules ------------------------------------

test_that("loaded images get a verdict: wrong when marked, correct otherwise", {
  resolved <- thr_test_resolve(wrong = c(2, 5))

  expect_identical(names(resolved), c("verdicts", "unavailable", "still_pending",
                                      "no_raw", "load_failed"))
  expect_identical(resolved$verdicts$draw_id, 1:6)
  expect_identical(resolved$verdicts$verdict, c(1L, 0L, 1L, 1L, 0L, 1L))
  expect_identical(resolved$unavailable, integer())
  expect_identical(resolved$still_pending, integer())
  expect_identical(resolved$no_raw, integer())
  expect_identical(resolved$load_failed, integer())
})

test_that("a page with nothing marked is all correct", {
  resolved <- thr_test_resolve()

  expect_identical(resolved$verdicts$verdict, rep(1L, 6))
})

test_that("a stale token is a classed condition and resolves nothing", {
  expect_error(thr_test_resolve(token = "old"), class = "thr_stale_page")
  expect_error(thr_test_resolve(token = NULL), class = "thr_stale_page")
  expect_error(thr_test_resolve(token = NA_character_), class = "thr_stale_page")
  expect_error(thr_test_resolve(token = c("tok", "tok")), class = "thr_stale_page")
  expect_error(thr_test_resolve(token = 12), class = "thr_stale_page")
  expect_error(thr_test_resolve(expected_token = NULL), class = "thr_stale_page")
  # Even a payload that is otherwise invalid is reported as stale first
  expect_error(thr_test_resolve(token = "old", wrong = 99), class = "thr_stale_page")

  caught <- tryCatch(thr_test_resolve(token = "old"),
                     thr_stale_page = function(condition) condition)
  expect_s3_class(caught, "error")
  expect_match(conditionMessage(caught), "page")
})

test_that("a wrong mark outside the page is an error", {
  expect_error(thr_test_resolve(wrong = c(2, 99)), "not on the page")
  failure <- tryCatch(thr_test_resolve(wrong = 99), error = function(e) e)
  expect_false(inherits(failure, "thr_stale_page"))
})

test_that("a wrong mark on an image without a verdict is an error", {
  # Not reported as loaded
  expect_error(thr_test_resolve(loaded = c(1, 3:6), wrong = 2),
               "cannot have a verdict")
  # Reported as failed
  expect_error(thr_test_resolve(failed = 2, wrong = 2), "cannot have a verdict")
  # Never extracted on the server
  page <- thr_test_page(extracted = c(TRUE, FALSE, TRUE, TRUE, TRUE, TRUE))
  expect_error(thr_test_resolve(page = page, wrong = 2), "cannot have a verdict")
})

test_that("an image that failed in the browser stays pending, also when reported as loaded too", {
  # A failed load is transient (a dropped connection, a slow disk): the draw
  # must come back on a later page instead of being retired for good
  resolved <- thr_test_resolve(loaded = 1:6, failed = c(3, 4), wrong = 1)

  expect_identical(resolved$verdicts$draw_id, c(1L, 2L, 5L, 6L))
  expect_identical(resolved$verdicts$verdict, c(0L, 1L, 1L, 1L))
  expect_identical(resolved$unavailable, integer())
  expect_identical(resolved$still_pending, c(3L, 4L))
  expect_identical(resolved$load_failed, c(3L, 4L))
  expect_identical(resolved$no_raw, integer())
})

test_that("images neither loaded nor failed stay pending", {
  resolved <- thr_test_resolve(loaded = c(1, 2), failed = 3)

  expect_identical(resolved$verdicts$draw_id, c(1L, 2L))
  expect_identical(resolved$unavailable, integer())
  expect_identical(resolved$still_pending, c(3L, 4L, 5L, 6L))
  expect_identical(resolved$load_failed, 3L)
})

test_that("a draw that was not extracted never gets a verdict", {
  page <- thr_test_page(extracted = c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE))

  # Whatever the browser says about it
  resolved <- thr_test_resolve(page = page, loaded = 1:6)

  expect_identical(resolved$verdicts$draw_id, c(1L, 2L, 5L, 6L))
  expect_identical(resolved$unavailable, c(3L, 4L))
  expect_identical(resolved$still_pending, integer())

  unreported <- thr_test_resolve(page = page, loaded = c(1, 2))
  expect_identical(unreported$unavailable, c(3L, 4L))
  expect_identical(unreported$still_pending, c(5L, 6L))
})

test_that("a draw whose raw data was not found stays pending", {
  # An unmounted disk or a renamed folder must not retire draws
  page <- thr_test_page(extracted = c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE),
                        reason = "no_raw")

  # Whatever the browser says about it
  for (failed in list(integer(), 3:4)) {
    resolved <- thr_test_resolve(page = page, loaded = 1:6, failed = failed)

    expect_identical(resolved$verdicts$draw_id, c(1L, 2L, 5L, 6L))
    expect_identical(resolved$unavailable, integer())
    expect_identical(resolved$still_pending, c(3L, 4L))
    expect_identical(resolved$no_raw, c(3L, 4L))
    expect_identical(resolved$load_failed, integer())
  }

  nothing <- thr_test_resolve(page = thr_test_page(extracted = FALSE,
                                                   reason = "no_raw"),
                              loaded = NULL)
  expect_equal(nrow(nothing$verdicts), 0)
  expect_identical(nothing$unavailable, integer())
  expect_identical(nothing$still_pending, 1:6)
})

test_that("only a ROI the raw file could not give is unavailable", {
  page <- thr_test_page(1:4, extracted = c(TRUE, FALSE, FALSE, FALSE))
  page$no_image_reason <- c(NA, "no_raw", "not_extractable", "invalid")

  resolved <- thr_test_resolve(page = page, loaded = 1)

  expect_identical(resolved$verdicts$draw_id, 1L)
  expect_identical(resolved$still_pending, 2L)
  # A draw without a valid sample name or ROI number can never have an image
  expect_identical(resolved$unavailable, c(3L, 4L))
})

test_that("every draw of the page ends up in exactly one group", {
  page <- thr_test_page(1:10, extracted = rep(c(TRUE, FALSE), c(8, 2)),
                        reason = rep(c("no_raw", "not_extractable"), c(9, 1)))

  resolved <- thr_test_resolve(page = page, loaded = c(1:5, 9), failed = c(5, 6),
                               wrong = c(1, 4))

  groups <- c(resolved$verdicts$draw_id, resolved$unavailable,
              resolved$still_pending)
  expect_setequal(groups, 1:10)
  expect_equal(anyDuplicated(groups), 0)
  expect_identical(resolved$verdicts$draw_id, 1:4)
  expect_identical(resolved$unavailable, 10L)
  expect_identical(resolved$still_pending, 5:9)
  # Why a draw is still pending, for those with a reason beyond "not viewed"
  expect_identical(resolved$load_failed, c(5L, 6L))
  expect_identical(resolved$no_raw, 9L)
})

test_that("an empty page resolves to nothing", {
  resolved <- thr_test_resolve(page = thr_test_page(integer()), loaded = NULL)

  expect_equal(nrow(resolved$verdicts), 0)
  expect_identical(names(resolved$verdicts), c("draw_id", "verdict"))
  expect_identical(resolved$unavailable, integer())
  expect_identical(resolved$still_pending, integer())
})

# --- thr_resolve_page_verdicts: what the browser sends -----------------------

test_that("ids are accepted in the shapes JavaScript payloads arrive in", {
  expected <- thr_test_resolve(wrong = c(2L, 5L))

  expect_identical(thr_test_resolve(loaded = as.numeric(1:6), wrong = c(2, 5)),
                   expected)
  expect_identical(thr_test_resolve(loaded = as.list(1:6), wrong = list(2, 5)),
                   expected)
  expect_identical(thr_test_resolve(loaded = as.character(1:6),
                                    wrong = c("2", "5")), expected)
  expect_identical(thr_test_resolve(loaded = list("1", "2", "3", "4", "5", "6"),
                                    wrong = list("2", "5")), expected)
  # An empty JavaScript array arrives as an empty list or NULL
  expect_identical(thr_test_resolve(failed = list(), wrong = NULL),
                   thr_test_resolve())
  # Repeated ids count once
  expect_identical(thr_test_resolve(loaded = c(1:6, 1:6), wrong = c(2, 5, 5, 2)),
                   expected)
})

test_that("ids that are not whole numbers are refused", {
  expect_error(thr_test_resolve(wrong = "abc"), "`wrong`")
  expect_error(thr_test_resolve(wrong = 2.5), "`wrong`")
  expect_error(thr_test_resolve(wrong = NA), "`wrong`")
  expect_error(thr_test_resolve(wrong = c(2, NA)), "`wrong`")
  expect_error(thr_test_resolve(wrong = TRUE), "`wrong`")
  expect_error(thr_test_resolve(wrong = -2), "`wrong`")
  expect_error(thr_test_resolve(wrong = "2; DROP TABLE draws"), "`wrong`")
  expect_error(thr_test_resolve(wrong = list(2, NULL)), "`wrong`")
  expect_error(thr_test_resolve(wrong = list(2, c(3, 4))), "`wrong`")
  expect_error(thr_test_resolve(wrong = 1e12), "`wrong`")
  expect_error(thr_test_resolve(loaded = c(1, Inf)), "`loaded`")
  expect_error(thr_test_resolve(failed = "1.0"), "`failed`")
  expect_error(thr_test_resolve(loaded = list(a = list(1))), "`loaded`")
})

test_that("loaded or failed ids that are not on the page are ignored", {
  resolved <- thr_test_resolve(loaded = c(1:6, 77), failed = c(3, 88))

  expect_identical(resolved$verdicts$draw_id, c(1L, 2L, 4L, 5L, 6L))
  expect_identical(resolved$unavailable, integer())
  expect_identical(resolved$still_pending, 3L)
})

test_that("the page itself is validated", {
  expect_error(thr_test_resolve(page = list(draw_id = 1)), "page")
  expect_error(thr_test_resolve(page = data.frame(draw_id = 1:3)), "extracted")
  expect_error(thr_test_resolve(page = thr_test_page(c(1, 1, 2))), "draw_id")
  bad <- thr_test_page()
  bad$extracted[2] <- NA
  expect_error(thr_test_resolve(page = bad), "extracted")
  bad <- thr_test_page()
  bad$extracted <- "yes"
  expect_error(thr_test_resolve(page = bad), "extracted")
  bad <- thr_test_page()
  bad$draw_id <- bad$draw_id + 0.5
  expect_error(thr_test_resolve(page = bad), "draw_id")
})

test_that("a draw without an image must say why", {
  # Without the reason a missing raw folder could not be told from a ROI
  # that has no image, and the draw would be retired on a guess
  no_reason <- data.frame(draw_id = 1:3, extracted = c(TRUE, FALSE, TRUE))
  expect_error(thr_test_resolve(page = no_reason), "no_image_reason")
  bad <- thr_test_page(extracted = c(TRUE, FALSE, TRUE, TRUE, TRUE, TRUE))
  bad$no_image_reason[2] <- NA
  expect_error(thr_test_resolve(page = bad), "no_image_reason")
  bad$no_image_reason[2] <- "who knows"
  expect_error(thr_test_resolve(page = bad), "no_image_reason")
  # A page on which every image was extracted needs no reasons
  all_there <- data.frame(draw_id = 1:3, extracted = TRUE)
  expect_identical(thr_test_resolve(page = all_there)$verdicts$draw_id, 1:3)
})

# --- thr_save_verdicts -------------------------------------------------------

test_that("saving records verdicts, the annotator, the time and the page token", {
  drawn <- thr_test_drawn_page()
  on.exit(unlink(drawn$root, recursive = TRUE), add = TRUE)
  ids <- drawn$page$draw_id
  expect_length(ids, 12)
  resolved <- thr_resolve_page_verdicts(
    thr_test_unextractable(drawn$page, 9:10), loaded = ids[1:8],
    failed = NULL, wrong = ids[c(2, 7)],
    token = "page-1", expected_token = "page-1"
  )

  result <- thr_save_verdicts(drawn$db_path, resolved, "Anna", "page-1")

  expect_identical(result, list(n_confirmed = 8L, n_unavailable = 2L))
  draws <- thr_test_draws(drawn$db_path)
  confirmed <- draws[draws$draw_id %in% ids[1:8], ]
  expect_true(all(confirmed$status == "confirmed"))
  expect_identical(confirmed$verdict, c(1L, 0L, 1L, 1L, 1L, 1L, 0L, 1L))
  expect_true(all(confirmed$annotator == "Anna"))
  expect_true(all(confirmed$page_token == "page-1"))
  confirmed_at <- as.POSIXct(confirmed$confirmed_at, format = "%Y-%m-%dT%H:%M:%SZ",
                             tz = "UTC")
  expect_false(anyNA(confirmed_at))
  expect_true(all(abs(as.numeric(difftime(Sys.time(), confirmed_at,
                                          units = "secs"))) < 120))

  unavailable <- draws[draws$draw_id %in% ids[9:10], ]
  expect_true(all(unavailable$status == "unavailable"))
  expect_true(all(is.na(unavailable$verdict)))
  expect_true(all(unavailable$annotator == "Anna"))
  expect_true(all(unavailable$page_token == "page-1"))
  expect_false(anyNA(unavailable$confirmed_at))

  pending <- draws[draws$draw_id %in% ids[11:12], ]
  expect_true(all(pending$status == "pending"))
  expect_true(all(is.na(pending$verdict)))
  expect_true(all(is.na(pending$annotator)))
  expect_true(all(is.na(pending$confirmed_at)))
  expect_true(all(is.na(pending$page_token)))
})

test_that("a double submit writes once", {
  drawn <- thr_test_drawn_page()
  on.exit(unlink(drawn$root, recursive = TRUE), add = TRUE)
  ids <- drawn$page$draw_id
  resolved <- thr_resolve_page_verdicts(thr_test_unextractable(drawn$page, 12),
                                        loaded = ids, failed = NULL,
                                        wrong = ids[1:3], token = "t",
                                        expected_token = "t")
  thr_save_verdicts(drawn$db_path, resolved, "Anna", "page-1")
  before <- thr_test_draws(drawn$db_path)

  again <- thr_save_verdicts(drawn$db_path, resolved, "Anna", "page-1")
  # Nor does a different answer for the same draws overwrite the first
  other <- thr_resolve_page_verdicts(drawn$page, loaded = ids, failed = integer(),
                                     wrong = ids[4:12], token = "t",
                                     expected_token = "t")
  changed <- thr_save_verdicts(drawn$db_path, other, "Berit", "page-2")

  expect_identical(again, list(n_confirmed = 0L, n_unavailable = 0L))
  expect_identical(changed, list(n_confirmed = 0L, n_unavailable = 0L))
  expect_identical(thr_test_draws(drawn$db_path), before)
})

test_that("draws left pending can be resolved by a later page", {
  drawn <- thr_test_drawn_page()
  on.exit(unlink(drawn$root, recursive = TRUE), add = TRUE)
  ids <- drawn$page$draw_id
  first <- thr_resolve_page_verdicts(drawn$page, loaded = ids[1:5], failed = NULL,
                                     wrong = NULL, token = "t", expected_token = "t")
  thr_save_verdicts(drawn$db_path, first, "Anna", "page-1")

  page <- thr_next_page(drawn$db_path, drawn$index_path, thr_test_model_a_id,
                        "Debris")
  expect_setequal(page$draw_id, ids[6:12])
  expect_false(any(page$is_new))
  page$extracted <- TRUE
  second <- thr_resolve_page_verdicts(page, loaded = page$draw_id, failed = NULL,
                                      wrong = ids[6], token = "u",
                                      expected_token = "u")
  result <- thr_save_verdicts(drawn$db_path, second, "Anna", "page-2")

  expect_identical(result, list(n_confirmed = 7L, n_unavailable = 0L))
  draws <- thr_test_draws(drawn$db_path)
  expect_true(all(draws$status == "confirmed"))
  expect_identical(draws$page_token, rep(c("page-1", "page-2"), c(5, 7)))
  expect_equal(sum(draws$verdict), 11)
})

test_that("a missing annotator name is recorded as Unknown", {
  drawn <- thr_test_drawn_page()
  on.exit(unlink(drawn$root, recursive = TRUE), add = TRUE)
  ids <- drawn$page$draw_id
  resolve <- function(rows) {
    thr_resolve_page_verdicts(drawn$page[rows, ], loaded = ids[rows],
                              failed = NULL, wrong = NULL, token = "t",
                              expected_token = "t")
  }

  thr_save_verdicts(drawn$db_path, resolve(1:2), "", "p1")
  thr_save_verdicts(drawn$db_path, resolve(3:4), NULL, "p2")
  thr_save_verdicts(drawn$db_path, resolve(5:6), "   ", "p3")
  thr_save_verdicts(drawn$db_path, resolve(7:8), "  Anna ", "p4")

  draws <- thr_test_draws(drawn$db_path)
  expect_identical(draws$annotator[1:8], rep(c("Unknown", "Anna"), c(6, 2)))
  expect_error(thr_save_verdicts(drawn$db_path, resolve(9), c("a", "b"), "p5"),
               "annotator")
  expect_error(thr_save_verdicts(drawn$db_path, resolve(9), 7, "p5"), "annotator")
})

test_that("saving is all or nothing", {
  drawn <- thr_test_drawn_page()
  on.exit(unlink(drawn$root, recursive = TRUE), add = TRUE)
  ids <- drawn$page$draw_id
  resolved <- thr_resolve_page_verdicts(thr_test_unextractable(drawn$page, 12),
                                        loaded = ids, failed = NULL,
                                        wrong = NULL, token = "t",
                                        expected_token = "t")
  before <- thr_test_draws(drawn$db_path)

  local({
    local_mocked_bindings(
      thr_mark_unavailable = function(...) stop("disk full", call. = FALSE)
    )
    expect_error(thr_save_verdicts(drawn$db_path, resolved, "Anna", "page-1"),
                 "disk full")
  })

  expect_identical(thr_test_draws(drawn$db_path), before)
  expect_identical(thr_save_verdicts(drawn$db_path, resolved, "Anna", "page-1"),
                   list(n_confirmed = 11L, n_unavailable = 1L))
})

test_that("thr_save_verdicts validates its arguments", {
  drawn <- thr_test_drawn_page()
  on.exit(unlink(drawn$root, recursive = TRUE), add = TRUE)
  ids <- drawn$page$draw_id
  good <- list(verdicts = data.frame(draw_id = ids[1:2], verdict = c(1L, 0L)),
               unavailable = ids[3], still_pending = ids[4:12])
  save <- function(resolved = good, annotator = "Anna", page_token = "p") {
    thr_save_verdicts(drawn$db_path, resolved, annotator, page_token)
  }

  expect_error(save(resolved = list(verdicts = good$verdicts)), "resolved")
  expect_error(save(resolved = "nothing"), "resolved")
  bad <- good
  bad$verdicts$verdict <- c(1L, 2L)
  expect_error(save(resolved = bad), "verdict")
  bad <- good
  bad$verdicts$verdict <- c(1L, NA)
  expect_error(save(resolved = bad), "verdict")
  bad <- good
  bad$unavailable <- ids[1]
  expect_error(save(resolved = bad), "both")
  bad <- good
  bad$verdicts <- data.frame(draw_id = ids[1:2])
  expect_error(save(resolved = bad), "verdict")
  expect_error(save(page_token = NULL), "page_token")
  expect_error(save(page_token = ""), "page_token")
  expect_error(save(page_token = c("a", "b")), "page_token")
  expect_true(all(thr_test_draws(drawn$db_path)$status == "pending"))

  missing_db <- file.path(drawn$root, "nowhere", "verdicts.sqlite")
  expect_error(thr_save_verdicts(missing_db, good, "Anna", "p"),
               "No verdict database")
  expect_false(file.exists(missing_db))
})

test_that("a page on which every image failed in the browser retires nothing", {
  drawn <- thr_test_drawn_page()
  on.exit(unlink(drawn$root, recursive = TRUE), add = TRUE)
  ids <- drawn$page$draw_id
  resolved <- thr_resolve_page_verdicts(drawn$page, loaded = NULL, failed = ids,
                                        wrong = NULL, token = "t",
                                        expected_token = "t")

  expect_identical(thr_save_verdicts(drawn$db_path, resolved, "Anna", "page-1"),
                   list(n_confirmed = 0L, n_unavailable = 0L))
  expect_true(all(thr_test_draws(drawn$db_path)$status == "pending"))
  # The same draws are offered again
  page <- thr_next_page(drawn$db_path, drawn$index_path, thr_test_model_a_id,
                        "Debris")
  expect_setequal(page$draw_id, ids)
})

test_that("a page of ROIs the raw files cannot give records no verdict", {
  drawn <- thr_test_drawn_page()
  on.exit(unlink(drawn$root, recursive = TRUE), add = TRUE)
  ids <- drawn$page$draw_id
  resolved <- thr_resolve_page_verdicts(thr_test_unextractable(drawn$page, 1:12),
                                        loaded = NULL, failed = NULL,
                                        wrong = NULL, token = "t",
                                        expected_token = "t")

  expect_identical(thr_save_verdicts(drawn$db_path, resolved, "Anna", "page-1"),
                   list(n_confirmed = 0L, n_unavailable = 12L))
  draws <- thr_test_draws(drawn$db_path)
  expect_true(all(draws$status == "unavailable"))
  expect_true(all(is.na(draws$verdict)))
  # The class has nothing left to offer: its ROIs were all drawn
  expect_equal(nrow(thr_next_page(drawn$db_path, drawn$index_path,
                                  thr_test_model_a_id, "Debris")), 0)
})

test_that("a draw listed twice with a verdict is refused", {
  drawn <- thr_test_drawn_page()
  on.exit(unlink(drawn$root, recursive = TRUE), add = TRUE)
  ids <- drawn$page$draw_id
  twice <- list(verdicts = data.frame(draw_id = ids[c(1, 1)], verdict = c(1L, 0L)),
                unavailable = integer(), still_pending = integer())

  expect_error(thr_save_verdicts(drawn$db_path, twice, "Anna", "p"),
               "more than once")
  expect_true(all(thr_test_draws(drawn$db_path)$status == "pending"))
})

test_that("ids that are not in the database change nothing", {
  drawn <- thr_test_drawn_page()
  on.exit(unlink(drawn$root, recursive = TRUE), add = TRUE)
  resolved <- list(verdicts = data.frame(draw_id = 9001:9002, verdict = c(1L, 1L)),
                   unavailable = 9003L, still_pending = integer())

  expect_identical(thr_save_verdicts(drawn$db_path, resolved, "Anna", "p"),
                   list(n_confirmed = 0L, n_unavailable = 0L))
  expect_equal(nrow(thr_test_draws(drawn$db_path)), 12)
})

# --- thr_undo_page -----------------------------------------------------------

test_that("undo returns a page's confirmed and unavailable draws to pending", {
  drawn <- thr_test_drawn_page()
  on.exit(unlink(drawn$root, recursive = TRUE), add = TRUE)
  ids <- drawn$page$draw_id
  before <- thr_test_draws(drawn$db_path)
  resolved <- thr_resolve_page_verdicts(thr_test_unextractable(drawn$page, 9:10),
                                        loaded = ids[1:8], failed = NULL,
                                        wrong = ids[1:2],
                                        token = "t", expected_token = "t")
  thr_save_verdicts(drawn$db_path, resolved, "Anna", "page-1")

  n_undone <- thr_undo_page(drawn$db_path, "page-1")

  expect_identical(n_undone, 10L)
  expect_identical(thr_test_draws(drawn$db_path), before)
  # The same draws are offered again
  page <- thr_next_page(drawn$db_path, drawn$index_path, thr_test_model_a_id,
                        "Debris")
  expect_setequal(page$draw_id, ids)
  expect_false(any(page$is_new))
  # Undoing twice does nothing more
  expect_identical(thr_undo_page(drawn$db_path, "page-1"), 0L)
})

test_that("undo only touches the page with that token", {
  drawn <- thr_test_drawn_page()
  on.exit(unlink(drawn$root, recursive = TRUE), add = TRUE)
  ids <- drawn$page$draw_id
  resolve <- function(rows, wrong = NULL) {
    thr_resolve_page_verdicts(drawn$page[rows, ], loaded = ids[rows],
                              failed = NULL, wrong = wrong, token = "t",
                              expected_token = "t")
  }
  thr_save_verdicts(drawn$db_path, resolve(1:6, wrong = ids[1]), "Anna", "page-1")
  thr_save_verdicts(drawn$db_path, resolve(7:12), "Anna", "page-2")

  expect_identical(thr_undo_page(drawn$db_path, "page-2"), 6L)

  draws <- thr_test_draws(drawn$db_path)
  expect_identical(draws$status, rep(c("confirmed", "pending"), each = 6))
  expect_identical(draws$verdict[1:6], c(0L, 1L, 1L, 1L, 1L, 1L))
  expect_identical(thr_undo_page(drawn$db_path, "no-such-page"), 0L)
})

test_that("thr_undo_page validates its arguments", {
  drawn <- thr_test_drawn_page()
  on.exit(unlink(drawn$root, recursive = TRUE), add = TRUE)

  expect_error(thr_undo_page(drawn$db_path, NULL), "page_token")
  expect_error(thr_undo_page(drawn$db_path, NA_character_), "page_token")
  expect_error(thr_undo_page(drawn$db_path, c("a", "b")), "page_token")
  expect_error(thr_undo_page(file.path(drawn$root, "none.sqlite"), "p"),
               "No verdict database")
})
