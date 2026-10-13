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

test_that("thr_frame_roi_paths finds raw data that was away when first asked", {
  space <- thr_test_app_frozen_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  away <- file.path(space$root, "raw_away")
  file.rename(space$roi_folder, away)
  nowhere <- thr_frame_roi_paths(space$db_path, space$model_id,
                                 space$roi_folder)
  expect_true(all(is.na(nowhere)))

  # The folder is back: what was not found is looked up again
  file.rename(away, space$roi_folder)
  back <- thr_frame_roi_paths(space$db_path, space$model_id, space$roi_folder,
                              known = nowhere)

  expect_named(back, thr_test_app_samples())
  expect_true(all(file.exists(back)))
  # What was found is not looked up a second time
  local_mocked_bindings(
    thr_resolve_roi_paths = function(...) stop("must not look up again")
  )
  expect_equal(thr_frame_roi_paths(space$db_path, space$model_id,
                                   space$roi_folder, known = back), back)
})

test_that("thr_forget_lost_roi_paths drops the paths a page could not use", {
  samples <- thr_test_app_samples(3)
  paths <- stats::setNames(file.path("/raw", paste0(samples, ".roi")), samples)
  page <- thr_test_app_page(sample_name = samples, roi_number = 2:7)
  page$extracted <- rep(c(TRUE, FALSE, FALSE), 2)
  page$no_image_reason <- rep(c(NA, "no_raw", "not_extractable"), 2)

  kept <- thr_forget_lost_roi_paths(paths, page)

  expect_named(kept, samples)
  # Only the sample whose raw file was not found is looked up again
  expect_equal(is.na(unname(kept)), c(FALSE, TRUE, FALSE))
  expect_equal(kept[[3]], paths[[3]])
  expect_equal(thr_forget_lost_roi_paths(paths, page[0, ]), paths)
  # A sample the paths do not hold is not added
  expect_named(thr_forget_lost_roi_paths(paths[1], page), samples[1])
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
                    "is_new", "png_file", "extracted", "no_image_reason",
                    "width", "height") %in% names(page)))
  expect_true(all(page$extracted))
  expect_true(all(is.na(page$no_image_reason)))
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
  expect_equal(unique(page$no_image_reason[!page$extracted]), "no_raw")
  expect_true(all(is.na(page$width[!page$extracted])))
  expect_equal(unique(page$width[page$extracted]), 160L)
})

# ---- thr_missing_raw_notice and thr_page_can_confirm -------------------------

test_that("thr_missing_raw_notice says which folder and how many images", {
  space <- thr_test_app_frozen_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  roi_paths <- thr_frame_roi_paths(space$db_path, space$model_id,
                                   space$roi_folder)
  complete <- thr_test_app_load(space, page_size = 40, roi_paths = roi_paths)
  expect_null(thr_missing_raw_notice(complete$page, space$roi_folder))
  expect_null(thr_missing_raw_notice(NULL, space$roi_folder))
  expect_null(thr_missing_raw_notice(complete$page[0, ], space$roi_folder))

  # Raw files of two samples are gone while the folder is there
  lost <- thr_test_app_samples()[1:2]
  unlink(roi_paths[lost])
  partly <- thr_test_app_load(space, page_size = 40, roi_paths = roi_paths)
  n_lost <- sum(partly$page$sample_name %in% lost)
  expect_gt(n_lost, 0)
  notice <- thr_missing_raw_notice(partly$page, space$roi_folder)
  expect_match(notice, sprintf("%d of the 40 image(s)", n_lost), fixed = TRUE)
  expect_match(notice, space$roi_folder, fixed = TRUE)
  expect_match(notice, "2 sample(s)", fixed = TRUE)
  expect_match(notice, "stay pending", fixed = TRUE)

  # The folder itself is away
  file.rename(space$roi_folder, file.path(space$root, "raw_away"))
  nothing <- thr_test_app_load(space, page_size = 40, roi_paths = roi_paths)
  notice <- thr_missing_raw_notice(nothing$page, space$roi_folder)
  expect_match(notice, "40 of the 40 image(s)", fixed = TRUE)
  expect_match(notice, space$roi_folder, fixed = TRUE)
  expect_match(notice, "was not found", fixed = TRUE)
  expect_match(notice, "Load page", fixed = TRUE)
})

test_that("thr_missing_raw_notice is silent about ROIs that have no image", {
  page <- thr_test_app_page(roi_number = 1:3)
  page$extracted <- c(FALSE, TRUE, TRUE)
  page$no_image_reason <- c("not_extractable", NA, NA)

  expect_null(thr_missing_raw_notice(page, thr_test_app_raw_folder()))
})

test_that("thr_page_can_confirm needs a draw that a confirmation would settle", {
  page <- thr_test_app_page(roi_number = 1:3)
  page$extracted <- c(FALSE, TRUE, FALSE)
  page$no_image_reason <- c("no_raw", NA, "no_raw")
  expect_true(thr_page_can_confirm(page))

  # Nothing but images whose raw data was not found: nothing was reviewed
  page$extracted[2] <- FALSE
  page$no_image_reason[2] <- "no_raw"
  expect_false(thr_page_can_confirm(page))
  # ROIs the raw file cannot give must be confirmable, or they would come
  # back on every page and the class could never be finished
  page$no_image_reason[3] <- "not_extractable"
  expect_true(thr_page_can_confirm(page))

  expect_false(thr_page_can_confirm(NULL))
  expect_false(thr_page_can_confirm(page[0, ]))
  expect_true(thr_page_can_confirm(data.frame(draw_id = 1L, extracted = TRUE)))
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

test_that("thr_class_remaining tells a finished class from one that cannot be drawn", {
  space <- thr_test_app_frozen_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  remaining <- function() {
    thr_class_remaining(space$db_path, space$index_path, space$model_id,
                        thr_test_app_class)
  }
  expect_equal(remaining(), list(n_predicted = 240L, n_undrawn = 240L,
                                 n_pending = 0L, n_changed_files = 0L))

  # The class-score file of one sample is removed after the freeze
  lost <- thr_test_app_samples()[3]
  unlink(file.path(space$h5_folder, paste0(lost, "_class.h5")))
  build_threshold_index(space$h5_folder, space$index_path,
                        reader = thr_test_fake_reader())
  fake <- thr_test_app_fake_extractor()
  everything <- thr_test_app_load(space, page_size = 500, fake = fake)
  expect_equal(nrow(everything$page), 200)
  expect_false(lost %in% everything$page$sample_name)
  expect_equal(remaining()$n_pending, 200L)
  thr_confirm_page(space$db_path, everything$page,
                   thr_test_app_payload(everything), everything$token, "t")
  nothing <- thr_test_app_load(space, page_size = 500, fake = fake)

  # An empty page, and 40 predictions nobody has seen
  expect_equal(nrow(nothing$page), 0)
  expect_equal(remaining(), list(n_predicted = 240L, n_undrawn = 40L,
                                 n_pending = 0L, n_changed_files = 1L))
  expect_error(
    thr_class_remaining(space$db_path, space$index_path, space$model_id,
                        "No such class"),
    "not a class of this model"
  )
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
  expect_named(result, c("status", "message", "n_confirmed", "n_wrong",
                         "n_unavailable", "n_pending", "n_no_raw",
                         "n_load_failed"))
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

test_that("thr_confirm_page separates retired, pending and unloaded images", {
  space <- thr_test_app_frozen_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  roi_paths <- thr_frame_roi_paths(space$db_path, space$model_id,
                                   space$roi_folder)
  lost <- thr_test_app_samples()[1]
  roi_paths[lost] <- NA
  # Two ROIs that the raw file of a sample that is there cannot give
  preview <- thr_test_app_load(space, page_size = 60, roi_paths = roi_paths)$page
  refuse <- preview$roi_number[preview$sample_name != lost][1:2]
  loaded <- thr_test_app_load(space, page_size = 60, roi_paths = roi_paths,
                              fake = thr_test_app_fake_extractor(refuse = refuse))
  page <- loaded$page
  expect_setequal(page$draw_id, preview$draw_id)
  shown <- page$draw_id[page$extracted]
  no_raw <- page$draw_id[page$no_image_reason %in% "no_raw"]
  unextractable <- page$draw_id[page$no_image_reason %in% "not_extractable"]
  expect_gt(length(no_raw), 0)
  expect_gte(length(unextractable), 2)
  expect_setequal(page$sample_name[page$draw_id %in% no_raw], lost)
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
  # Only what the raw file could not give is retired
  expect_equal(result$n_unavailable, length(unextractable))
  expect_equal(result$n_no_raw, length(no_raw))
  expect_equal(result$n_load_failed, 2L)
  expect_equal(result$n_pending, length(no_raw) + 2L + 5L)
  expect_match(result$message,
               sprintf("%d image(s) could not be read from their raw file",
                       length(unextractable)), fixed = TRUE)
  expect_match(result$message,
               sprintf("%d image(s) could not be shown because their raw data",
                       length(no_raw)), fixed = TRUE)
  expect_match(result$message, "2 image(s) failed to load", fixed = TRUE)
  expect_match(result$message, "5 image(s) had not been viewed", fixed = TRUE)
  expect_match(result$message, "come back", fixed = TRUE)
  draws <- thr_test_app_draws(space)
  status <- stats::setNames(draws$status, draws$draw_id)
  expect_equal(unique(status[as.character(seen)]), "confirmed")
  expect_equal(unique(status[as.character(unextractable)]), "unavailable")
  for (pending in list(not_loaded, failed, no_raw)) {
    expect_equal(unique(status[as.character(pending)]), "pending")
  }
  # What stayed pending is shown again, first
  again <- thr_test_app_load(space, page_size = 60)$page
  expect_true(all(c(not_loaded, failed, no_raw) %in% again$draw_id))
  expect_true(all(again$extracted))
})

test_that("thr_confirm_page retires nothing while the raw data is away", {
  space <- thr_test_app_frozen_space()
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  overview <- function() {
    all_classes <- thr_class_overview(space$db_path, space$model_id)
    all_classes[all_classes$class_name == thr_test_app_class, ]
  }
  expect_equal(overview()$n_predicted, 240L)
  away <- file.path(space$root, "raw_away")
  file.rename(space$roi_folder, away)
  fake <- thr_test_app_fake_extractor()
  first_ids <- NULL

  # Three times "Load page" and "Confirm page" on an empty gallery
  for (round in 1:3) {
    roi_paths <- thr_frame_roi_paths(space$db_path, space$model_id,
                                     space$roi_folder)
    loaded <- thr_test_app_load(space, page_size = 100, roi_paths = roi_paths,
                                fake = fake)
    expect_equal(nrow(loaded$page), 100)
    expect_false(any(loaded$page$extracted))
    first_ids <- first_ids %||% loaded$page$draw_id
    # The same draws every time: none of them was used up
    expect_setequal(loaded$page$draw_id, first_ids)
    result <- thr_confirm_page(space$db_path, loaded$page,
                               thr_test_app_payload(loaded), loaded$token, "x")

    expect_equal(result$status, "error")
    expect_match(result$message, "Nothing was recorded", fixed = TRUE)
    expect_match(result$message, "raw data", fixed = TRUE)
    expect_equal(result$n_unavailable, 0L)
  }
  expect_equal(fake$n_calls(), 0)
  expect_equal(overview()$n_unavailable, 0L)
  expect_equal(overview()$n_pending, 100L)
  expect_equal(unique(thr_test_app_draws(space)$status), "pending")
  expect_true(all(is.na(thr_test_app_draws(space)$page_token)))

  # The folder is back: the very same draws are reviewed
  file.rename(away, space$roi_folder)
  loaded <- thr_test_app_load(space, page_size = 100, fake = fake)
  expect_setequal(loaded$page$draw_id, first_ids)
  expect_true(all(loaded$page$extracted))
  result <- thr_confirm_page(space$db_path, loaded$page,
                             thr_test_app_payload(loaded), loaded$token, "x")
  expect_equal(result$status, "saved")
  expect_equal(result$n_confirmed, 100L)
  expect_equal(overview()$n_verdicts, 100L)
})

test_that("thr_confirm_page can retire a page of ROIs that have no image", {
  space <- thr_test_app_frozen_space(n_samples = 2, n_rois = 10)
  on.exit(unlink(space$root, recursive = TRUE), add = TRUE)
  # Every ROI of the class is refused by the extractor, as a ROI of zero
  # size is by iRfcb
  loaded <- thr_test_app_load(space, page_size = 500,
                              fake = thr_test_app_fake_extractor(refuse = 1:10))
  n_draws <- nrow(loaded$page)
  expect_gt(n_draws, 0)
  expect_equal(unique(loaded$page$no_image_reason), "not_extractable")

  result <- thr_confirm_page(space$db_path, loaded$page,
                             thr_test_app_payload(loaded), loaded$token, "t")

  expect_equal(result$status, "saved")
  expect_equal(result$n_confirmed, 0L)
  expect_equal(result$n_unavailable, n_draws)
  expect_equal(unique(thr_test_app_draws(space)$status), "unavailable")
})

test_that("thr_confirm_message says what became of every image", {
  plain <- thr_confirm_message(20L, 3L, 0L, 0L)
  expect_equal(plain, "Recorded 20 verdict(s), 3 marked wrong.")

  full <- thr_confirm_message(n_confirmed = 10L, n_wrong = 1L,
                              n_unavailable = 2L, n_pending = 12L,
                              n_elsewhere = 6L, n_no_raw = 4L,
                              n_load_failed = 3L)
  expect_match(full, "Recorded 10 verdict(s), 1 marked wrong.", fixed = TRUE)
  expect_match(full, "2 image(s) could not be read from their raw file and are left out",
               fixed = TRUE)
  expect_match(full, "4 image(s) could not be shown because their raw data was not found",
               fixed = TRUE)
  expect_match(full, "3 image(s) failed to load", fixed = TRUE)
  # 12 pending, of which 4 and 3 for a reason of their own
  expect_match(full, "5 image(s) had not been viewed", fixed = TRUE)
  expect_match(full, "6 image(s) had already been resolved", fixed = TRUE)
  # Nothing that stays pending is called unavailable
  expect_no_match(thr_confirm_message(0L, 0L, 0L, 7L, n_no_raw = 4L,
                                      n_load_failed = 3L),
                  "left out|not been viewed")
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
    # Every result has the same shape
    expect_named(result, c("status", "message", "n_confirmed", "n_wrong",
                           "n_unavailable", "n_pending", "n_no_raw",
                           "n_load_failed"))
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
