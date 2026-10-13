# Tests for the Review page helpers of the threshold review app: loading a
# page with its images and turning a browser confirmation into verdicts.

thr_test_app_class <- "Skeletonema_spp"

# Load a page of the synthetic model with the fake extractor
thr_test_app_load <- function(space, page_size = 20, roi_paths = NULL,
                              fake = thr_test_app_fake_extractor(),
                              class_name = thr_test_app_class) {
  if (is.null(roi_paths)) {
    roi_paths <- thr_frame_roi_paths(space$db_path, space$model_id,
                                     space$roi_folder)
  }
  thr_load_review_page(space$db_path, space$index_path, space$model_id,
                       class_name, page_size, roi_paths, space$image_folder,
                       extractor = fake$extractor)
}

# What a browser that loaded every image on the page would send
thr_test_app_payload <- function(loaded_page, wrong = integer(),
                                 loaded = NULL, failed = integer()) {
  page <- loaded_page$page
  if (is.null(loaded)) {
    loaded <- page$draw_id[page$extracted]
  }
  list(token = loaded_page$token, loaded = loaded, failed = failed,
       wrong = wrong, nonce = 1)
}

thr_test_app_draws <- function(space) {
  thr_test_query(space$db_path, "SELECT * FROM draws ORDER BY draw_id")
}

# ---- frame samples and raw paths ---------------------------------------------

test_that("thr_frame_samples_db lists the samples of a frozen frame", {
  space <- thr_test_app_frozen_space(
    excluded_samples = thr_test_app_samples()[1]
  )
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)

  samples <- thr_frame_samples_db(space$db_path, space$model_id)

  expect_equal(samples, thr_test_app_samples()[-1])
  expect_equal(thr_frame_samples_db(space$db_path, "no-such-model"),
               character())
})

test_that("thr_frame_roi_paths resolves every frame sample once", {
  space <- thr_test_app_frozen_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)

  paths <- thr_frame_roi_paths(space$db_path, space$model_id,
                               space$roi_folder)

  expect_named(paths, thr_test_app_samples())
  expect_true(all(file.exists(paths)))
  nowhere <- thr_frame_roi_paths(space$db_path, space$model_id,
                                 file.path(space$root, "moved"))
  expect_true(all(is.na(nowhere)))
  expect_named(nowhere, thr_test_app_samples())
})

# ---- thr_clear_image_folder --------------------------------------------------

test_that("thr_clear_image_folder removes sample folders and nothing else", {
  folder <- tempfile("thr_images_")
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  sample_dir <- file.path(folder, thr_test_app_raw_sample)
  dir.create(sample_dir, recursive = TRUE)
  file.create(file.path(sample_dir, "x.png"))
  dir.create(file.path(folder, "something_else"))
  file.create(file.path(folder, "notes.txt"))

  removed <- thr_clear_image_folder(folder)

  expect_equal(removed, 1L)
  expect_false(dir.exists(sample_dir))
  expect_true(dir.exists(file.path(folder, "something_else")))
  expect_true(file.exists(file.path(folder, "notes.txt")))
  expect_equal(thr_clear_image_folder(file.path(folder, "absent")), 0L)
  expect_equal(thr_clear_image_folder(NULL), 0L)
})

# ---- thr_load_review_page ----------------------------------------------------

test_that("thr_load_review_page draws a page and extracts its images", {
  space <- thr_test_app_frozen_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)

  loaded <- thr_test_app_load(space, page_size = 20)

  page <- loaded$page
  expect_equal(nrow(page), 20)
  expect_true(all(c("draw_id", "sample_name", "roi_number", "score", "bin",
                    "is_new", "png_file", "extracted", "width", "height") %in%
                    names(page)))
  expect_true(all(page$extracted))
  expect_true(all(file.exists(file.path(space$image_folder, page$png_file))))
  expect_equal(unique(page$width), 160L)
  expect_equal(unique(page$height), 74L)
  expect_false(is.unsorted(rev(page$score)))
  expect_equal(loaded$class_name, thr_test_app_class)
  expect_match(loaded$token, "^[0-9a-f]{32}$")
  # The draws were written before anything was shown
  draws <- thr_test_app_draws(space)
  expect_setequal(draws$draw_id, page$draw_id)
  expect_equal(unique(draws$status), "pending")
})

test_that("thr_load_review_page gives every page its own token", {
  space <- thr_test_app_frozen_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)

  first <- thr_test_app_load(space, page_size = 5)
  second <- thr_test_app_load(space, page_size = 5)

  expect_false(first$token == second$token)
  # Nothing was resolved, so the same draws come back
  expect_setequal(second$page$draw_id, first$page$draw_id)
  expect_false(any(second$page$is_new))
})

test_that("thr_load_review_page marks draws whose raw file is unknown", {
  space <- thr_test_app_frozen_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  roi_paths <- thr_frame_roi_paths(space$db_path, space$model_id,
                                   space$roi_folder)
  lost <- thr_test_app_samples()[1:3]
  roi_paths[lost] <- NA

  loaded <- thr_test_app_load(space, page_size = 60, roi_paths = roi_paths)

  page <- loaded$page
  expect_true(any(page$sample_name %in% lost))
  expect_equal(page$extracted, !(page$sample_name %in% lost))
  expect_true(all(is.na(page$width[!page$extracted])))
  expect_equal(unique(page$width[page$extracted]), 160L)
})

test_that("thr_load_review_page keeps only the current page's images", {
  space <- thr_test_app_frozen_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)

  first <- thr_test_app_load(space, page_size = 10)
  thr_confirm_page(space$db_path, first$page, thr_test_app_payload(first),
                   first$token, "tester")
  second <- thr_test_app_load(space, page_size = 10)

  on_disk <- list.files(space$image_folder, recursive = TRUE)
  expect_setequal(on_disk, second$page$png_file)
  expect_length(intersect(first$page$draw_id, second$page$draw_id), 0)
})

test_that("thr_load_review_page returns an empty page for an exhausted class", {
  space <- thr_test_app_frozen_space(n_samples = 2, n_rois = 10)
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  fake <- thr_test_app_fake_extractor()

  everything <- thr_test_app_load(space, page_size = 500, fake = fake)
  thr_confirm_page(space$db_path, everything$page,
                   thr_test_app_payload(everything), everything$token, "t")
  nothing <- thr_test_app_load(space, page_size = 500, fake = fake)

  expect_equal(nrow(nothing$page), 0)
  expect_true(all(c("png_file", "extracted", "width", "height") %in%
                    names(nothing$page)))
  expect_match(nothing$token, "^[0-9a-f]{32}$")
  expect_false(nothing$token == everything$token)
})

test_that("thr_load_review_page passes on the errors of a bad request", {
  space <- thr_test_app_frozen_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)

  expect_error(thr_test_app_load(space, class_name = "No such class"),
               "not a class of this model")
  expect_error(thr_test_app_load(space, page_size = 0), "page_size")
  expect_equal(nrow(thr_test_app_draws(space)), 0)
})

# ---- thr_confirm_page --------------------------------------------------------

test_that("thr_confirm_page records the verdicts of a valid confirmation", {
  space <- thr_test_app_frozen_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  loaded <- thr_test_app_load(space, page_size = 20)
  wrong <- loaded$page$draw_id[c(2, 5, 9)]

  result <- thr_confirm_page(space$db_path, loaded$page,
                             thr_test_app_payload(loaded, wrong = wrong),
                             loaded$token, " Anna ")

  expect_equal(result$status, "saved")
  expect_equal(result$n_confirmed, 20L)
  expect_equal(result$n_wrong, 3L)
  expect_equal(result$n_unavailable, 0L)
  expect_equal(result$n_pending, 0L)
  expect_match(result$message, "Recorded 20 verdict", fixed = TRUE)
  expect_match(result$message, "3 marked wrong", fixed = TRUE)
  draws <- thr_test_app_draws(space)
  expect_equal(unique(draws$status), "confirmed")
  expect_equal(draws$verdict, as.integer(!(draws$draw_id %in% wrong)))
  expect_equal(unique(draws$annotator), "Anna")
  expect_equal(unique(draws$page_token), loaded$token)
})

test_that("thr_confirm_page accepts ids the way a browser sends them", {
  space <- thr_test_app_frozen_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  loaded <- thr_test_app_load(space, page_size = 4)
  ids <- loaded$page$draw_id
  # jsonlite hands an empty array over as list() and a mixed one as a list
  payload <- list(token = loaded$token, loaded = as.list(as.numeric(ids)),
                  failed = list(), wrong = as.numeric(ids[1]), nonce = 0.5)

  result <- thr_confirm_page(space$db_path, loaded$page, payload,
                             loaded$token, NULL)

  expect_equal(result$status, "saved")
  expect_equal(result$n_confirmed, 4L)
  expect_equal(result$n_wrong, 1L)
  expect_equal(unique(thr_test_app_draws(space)$annotator), "Unknown")
})

test_that("thr_confirm_page writes nothing for a stale token", {
  space <- thr_test_app_frozen_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  old <- thr_test_app_load(space, page_size = 10)
  current <- thr_test_app_load(space, page_size = 10)

  result <- thr_confirm_page(space$db_path, current$page,
                             thr_test_app_payload(old), current$token, "t")

  expect_equal(result$status, "stale")
  expect_match(result$message, "Nothing was recorded", fixed = TRUE)
  expect_equal(result$n_confirmed, 0L)
  expect_equal(unique(thr_test_app_draws(space)$status), "pending")
})

test_that("thr_confirm_page writes nothing when a wrong mark has no image", {
  space <- thr_test_app_frozen_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  loaded <- thr_test_app_load(space, page_size = 10)
  ids <- loaded$page$draw_id

  # Marked wrong, but the browser never reported the image as loaded
  result <- thr_confirm_page(
    space$db_path, loaded$page,
    thr_test_app_payload(loaded, loaded = ids[-1], wrong = ids[1]),
    loaded$token, "t"
  )

  expect_equal(result$status, "error")
  expect_match(result$message, "Nothing was recorded", fixed = TRUE)
  expect_equal(unique(thr_test_app_draws(space)$status), "pending")
})

test_that("thr_confirm_page separates unavailable and unloaded images", {
  space <- thr_test_app_frozen_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  roi_paths <- thr_frame_roi_paths(space$db_path, space$model_id,
                                   space$roi_folder)
  roi_paths[thr_test_app_samples()[1]] <- NA
  loaded <- thr_test_app_load(space, page_size = 60, roi_paths = roi_paths)
  page <- loaded$page
  shown <- page$draw_id[page$extracted]
  n_missing <- sum(!page$extracted)
  expect_gt(n_missing, 0)
  failed <- shown[1:2]
  not_loaded <- shown[3:7]
  seen <- setdiff(shown, c(failed, not_loaded))

  result <- thr_confirm_page(
    space$db_path, page,
    thr_test_app_payload(loaded, loaded = seen, failed = failed,
                         wrong = seen[1]),
    loaded$token, "t"
  )

  expect_equal(result$status, "saved")
  expect_equal(result$n_confirmed, length(seen))
  expect_equal(result$n_unavailable, n_missing + 2L)
  expect_equal(result$n_pending, 5L)
  expect_match(result$message, sprintf("%d image(s) were unavailable",
                                       n_missing + 2L), fixed = TRUE)
  expect_match(result$message, "5 image(s) had not been viewed", fixed = TRUE)
  expect_match(result$message, "come back", fixed = TRUE)
  draws <- thr_test_app_draws(space)
  status <- stats::setNames(draws$status, draws$draw_id)
  expect_equal(unique(status[as.character(seen)]), "confirmed")
  expect_equal(unique(status[as.character(not_loaded)]), "pending")
  expect_equal(unique(status[as.character(failed)]), "unavailable")
  expect_equal(unique(status[as.character(page$draw_id[!page$extracted])]),
               "unavailable")
})

test_that("thr_confirm_page says when a page was already resolved", {
  space <- thr_test_app_frozen_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  loaded <- thr_test_app_load(space, page_size = 10)
  payload <- thr_test_app_payload(loaded, wrong = loaded$page$draw_id[1])
  thr_confirm_page(space$db_path, loaded$page, payload, loaded$token, "a")
  before <- thr_test_app_draws(space)

  # The same confirmation arriving twice, e.g. from a second browser tab
  again <- thr_confirm_page(space$db_path, loaded$page, payload,
                            loaded$token, "b")

  expect_equal(again$status, "saved")
  expect_equal(again$n_confirmed, 0L)
  expect_match(again$message, "already", fixed = TRUE)
  expect_equal(thr_test_app_draws(space), before)
})

test_that("thr_confirm_page refuses a confirmation it cannot read", {
  space <- thr_test_app_frozen_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  loaded <- thr_test_app_load(space, page_size = 5)

  no_page <- thr_confirm_page(space$db_path, NULL,
                              thr_test_app_payload(loaded), loaded$token, "t")
  expect_equal(no_page$status, "stale")

  for (payload in list(NULL, "token", list(), 1:3)) {
    result <- thr_confirm_page(space$db_path, loaded$page, payload,
                               loaded$token, "t")
    expect_true(result$status %in% c("stale", "error"))
    expect_equal(result$n_confirmed, 0L)
  }
  garbage <- thr_confirm_page(
    space$db_path, loaded$page,
    list(token = loaded$token, loaded = "DROP TABLE draws", failed = list(),
         wrong = list()),
    loaded$token, "t"
  )
  expect_equal(garbage$status, "error")
  expect_equal(unique(thr_test_app_draws(space)$status), "pending")
})

# ---- thr_undo_message --------------------------------------------------------

test_that("thr_undo_message says what an undo did", {
  expect_match(thr_undo_message(12L, "Debris"), "12 image", fixed = TRUE)
  expect_match(thr_undo_message(12L, "Debris"), "Debris", fixed = TRUE)
  expect_match(thr_undo_message(0L, "Debris"), "Nothing to undo", fixed = TRUE)
})
