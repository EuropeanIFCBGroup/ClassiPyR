# Tests for finding raw data and extracting the images of a review page

# ---- thr_resolve_roi_paths ---------------------------------------------------

test_that("thr_resolve_roi_paths finds samples in the standard layout", {
  paths <- thr_resolve_roi_paths(thr_test_app_raw_sample,
                                 thr_test_app_raw_folder())

  expect_named(paths, thr_test_app_raw_sample)
  expect_true(file.exists(paths[[1]]))
  expect_equal(basename(paths[[1]]), paste0(thr_test_app_raw_sample, ".roi"))
})

test_that("thr_resolve_roi_paths gives NA for samples without raw data", {
  samples <- c(thr_test_app_raw_sample, "D20240101T000000_IFCB134")

  paths <- thr_resolve_roi_paths(samples, thr_test_app_raw_folder())

  expect_named(paths, samples)
  expect_false(is.na(paths[[1]]))
  expect_true(is.na(paths[[2]]))
  expect_type(paths, "character")
})

test_that("thr_resolve_roi_paths never errors on invalid sample names", {
  samples <- c("../../etc/passwd", "not a sample", "", NA,
               thr_test_app_raw_sample)

  paths <- thr_resolve_roi_paths(samples, thr_test_app_raw_folder())

  expect_length(paths, 5)
  expect_true(all(is.na(paths[1:4])))
  expect_false(is.na(paths[[5]]))
})

test_that("thr_resolve_roi_paths falls back to one recursive scan", {
  roi_folder <- tempfile("thr_raw_")
  on.exit(unlink(roi_folder, recursive = TRUE), add = TRUE)
  odd <- "D20230101T120000_IFCB134"
  flat <- "D20230102T120000_IFCB134"
  dir.create(file.path(roi_folder, "cruise", "leg1"), recursive = TRUE)
  file.create(file.path(roi_folder, "cruise", "leg1", paste0(odd, ".roi")))
  file.create(file.path(roi_folder, paste0(flat, ".roi")))
  n_scans <- 0
  local_mocked_bindings(
    thr_scan_roi_files = function(roi_folder) {
      n_scans <<- n_scans + 1
      list.files(roi_folder, "\\.roi$", recursive = TRUE, full.names = TRUE)
    }
  )

  paths <- thr_resolve_roi_paths(
    c(odd, flat, "D20230103T120000_IFCB134"), roi_folder
  )

  expect_equal(basename(paths[[odd]]), paste0(odd, ".roi"))
  expect_true(file.exists(paths[[odd]]))
  expect_true(file.exists(paths[[flat]]))
  expect_true(is.na(paths[["D20230103T120000_IFCB134"]]))
  expect_equal(n_scans, 1)
})

test_that("thr_resolve_roi_paths does not scan when every sample is found", {
  local_mocked_bindings(
    thr_scan_roi_files = function(roi_folder) stop("must not scan")
  )

  paths <- thr_resolve_roi_paths(thr_test_app_raw_sample,
                                 thr_test_app_raw_folder())

  expect_false(is.na(paths[[1]]))
})

test_that("thr_resolve_roi_paths handles a missing folder and no samples", {
  missing <- thr_resolve_roi_paths(thr_test_app_raw_sample,
                                   file.path(tempdir(), "no_such_raw_folder"))
  expect_true(is.na(missing[[1]]))
  expect_true(is.na(thr_resolve_roi_paths(thr_test_app_raw_sample, NA)[[1]]))
  expect_true(is.na(thr_resolve_roi_paths(thr_test_app_raw_sample, "")[[1]]))

  none <- thr_resolve_roi_paths(character(), thr_test_app_raw_folder())
  expect_length(none, 0)
  expect_type(none, "character")
})

test_that("thr_resolve_roi_paths returns each sample once", {
  paths <- thr_resolve_roi_paths(rep(thr_test_app_raw_sample, 3),
                                 thr_test_app_raw_folder())

  expect_named(paths, thr_test_app_raw_sample)
})

# ---- thr_refresh_roi_paths ---------------------------------------------------

test_that("thr_refresh_roi_paths looks up again only what did not resolve", {
  roi_folder <- tempfile("thr_raw_")
  on.exit(unlink(roi_folder, recursive = TRUE), add = TRUE)
  samples <- sprintf("D2023010%dT120000_IFCB134", 1:3)
  thr_test_app_raw_layout(roi_folder, samples[1])
  first <- thr_refresh_roi_paths(NULL, samples, roi_folder)
  expect_named(first, samples)
  expect_equal(is.na(unname(first)), c(FALSE, TRUE, TRUE))

  # The raw data of the second sample arrives
  thr_test_app_raw_layout(roi_folder, samples[2])
  n_scans <- 0
  local_mocked_bindings(
    thr_scan_roi_files = function(roi_folder) {
      n_scans <<- n_scans + 1
      list.files(roi_folder, "\\.roi$", recursive = TRUE, full.names = TRUE)
    }
  )
  second <- thr_refresh_roi_paths(first, samples, roi_folder)

  expect_named(second, samples)
  expect_equal(second[[1]], first[[1]])
  expect_true(file.exists(second[[2]]))
  expect_true(is.na(second[[3]]))
  expect_equal(n_scans, 1)

  # Nothing is looked up once every sample has its file
  thr_test_app_raw_layout(roi_folder, samples[3])
  third <- thr_refresh_roi_paths(second, samples, roi_folder)
  expect_true(all(file.exists(third)))
  n_scans <- 0
  expect_equal(thr_refresh_roi_paths(third, samples, roi_folder), third)
  expect_equal(n_scans, 0)
})

test_that("thr_refresh_roi_paths recovers when the raw folder comes back", {
  roi_folder <- tempfile("thr_raw_")
  away <- paste0(roi_folder, "_away")
  on.exit(unlink(c(roi_folder, away), recursive = TRUE), add = TRUE)
  samples <- sprintf("D2023010%dT120000_IFCB134", 1:2)
  thr_test_app_raw_layout(away, samples)

  # Resolved while the folder was away: nothing is found
  nowhere <- thr_refresh_roi_paths(NULL, samples, roi_folder)
  expect_true(all(is.na(nowhere)))
  expect_named(nowhere, samples)

  file.rename(away, roi_folder)
  back <- thr_refresh_roi_paths(nowhere, samples, roi_folder)
  expect_true(all(file.exists(back)))

  # Paths of samples that are not asked for are dropped, new ones are added
  other <- thr_refresh_roi_paths(back, c(samples[2], "not a sample"), roi_folder)
  expect_named(other, c(samples[2], "not a sample"))
  expect_equal(other[[1]], back[[2]])
  expect_true(is.na(other[[2]]))
  expect_length(thr_refresh_roi_paths(back, character(), roi_folder), 0)
})

# ---- thr_extract_page_images -------------------------------------------------

test_that("thr_extract_page_images extracts the real bin's ROIs", {
  out_folder <- tempfile("thr_png_")
  on.exit(unlink(out_folder, recursive = TRUE), add = TRUE)
  page <- thr_test_app_page(roi_number = 2:8)
  roi_paths <- thr_resolve_roi_paths(page$sample_name,
                                     thr_test_app_raw_folder())

  result <- thr_extract_page_images(page, roi_paths, out_folder)

  expect_equal(result[names(page)], page)
  expect_equal(
    result$png_file,
    sprintf("%s/%s_%05d.png", thr_test_app_raw_sample, thr_test_app_raw_sample,
            2:8)
  )
  expect_true(all(result$extracted))
  expect_true(all(is.na(result$no_image_reason)))
  expect_type(result$no_image_reason, "character")
  expect_true(all(file.exists(file.path(out_folder, result$png_file))))
  # Nothing but the requested images is written
  expect_setequal(list.files(out_folder, recursive = TRUE), result$png_file)
})

test_that("thr_extract_page_images marks a ROI without an image", {
  out_folder <- tempfile("thr_png_")
  on.exit(unlink(out_folder, recursive = TRUE), add = TRUE)
  # ROI 1 of the bin has zero size and ROI 99 does not exist; asking iRfcb for
  # an ROI that does not exist fails the whole call, so the rest of the
  # sample has to be rescued one ROI at a time.
  page <- thr_test_app_page(roi_number = c(1L, 2L, 99L, 3L))
  roi_paths <- thr_resolve_roi_paths(page$sample_name,
                                     thr_test_app_raw_folder())

  result <- thr_extract_page_images(page, roi_paths, out_folder)

  expect_equal(result$extracted, c(FALSE, TRUE, FALSE, TRUE))
  expect_equal(result$draw_id, page$draw_id)
  # The raw file was read and did not give these ROIs: that is final
  expect_equal(result$no_image_reason,
               c("not_extractable", NA, "not_extractable", NA))
})

test_that("thr_extract_page_images handles a missing ROI file", {
  out_folder <- tempfile("thr_png_")
  on.exit(unlink(out_folder, recursive = TRUE), add = TRUE)
  fake <- thr_test_app_fake_extractor()
  gone <- "D20240101T000000_IFCB134"
  page <- thr_test_app_page(
    sample_name = c(thr_test_app_raw_sample, gone, "D20240102T000000_IFCB134"),
    roi_number = 2:4
  )
  roi_paths <- c(
    stats::setNames(thr_test_app_raw_roi(), thr_test_app_raw_sample),
    stats::setNames(NA_character_, gone)
  )

  result <- thr_extract_page_images(page, roi_paths, out_folder, fake$extractor)

  # No path, and a sample the path vector does not mention at all
  expect_equal(result$extracted, c(TRUE, FALSE, FALSE))
  expect_equal(fake$n_calls(), 1)
  expect_false(anyNA(result$png_file))
  # Nothing was tried, so nothing is known about these ROIs
  expect_equal(result$no_image_reason, c(NA, "no_raw", "no_raw"))

  # A path to a file that has since been removed
  roi_paths[[gone]] <- file.path(tempdir(), paste0(gone, ".roi"))
  result <- thr_extract_page_images(page, roi_paths, out_folder, fake$extractor)
  expect_equal(result$extracted, c(TRUE, FALSE, FALSE))
  expect_equal(result$no_image_reason, c(NA, "no_raw", "no_raw"))
  expect_equal(fake$n_calls(), 1)
})

test_that("thr_extract_page_images tells a missing raw folder from a bad ROI", {
  out_folder <- tempfile("thr_png_")
  roi_folder <- tempfile("thr_raw_")
  on.exit(unlink(c(out_folder, roi_folder), recursive = TRUE), add = TRUE)
  samples <- c("D20230101T120000_IFCB134", "D20230102T120000_IFCB134")
  thr_test_app_raw_layout(roi_folder, samples)
  fake <- thr_test_app_fake_extractor()
  page <- thr_test_app_page(sample_name = samples, roi_number = 2:5)
  roi_paths <- thr_resolve_roi_paths(samples, roi_folder)

  # The whole raw data folder goes away after its files were looked up
  away <- paste0(roi_folder, "_away")
  file.rename(roi_folder, away)
  on.exit(unlink(away, recursive = TRUE), add = TRUE)
  result <- thr_extract_page_images(page, roi_paths, out_folder, fake$extractor)

  expect_false(any(result$extracted))
  expect_equal(unique(result$no_image_reason), "no_raw")
  expect_equal(fake$n_calls(), 0)

  # It comes back: the same page gets its images
  file.rename(away, roi_folder)
  result <- thr_extract_page_images(page, roi_paths, out_folder, fake$extractor)
  expect_true(all(result$extracted))
  expect_true(all(is.na(result$no_image_reason)))
})

test_that("thr_extract_page_images does not blame a ROI when the raw file vanishes", {
  out_folder <- tempfile("thr_png_")
  roi_folder <- tempfile("thr_raw_")
  on.exit(unlink(c(out_folder, roi_folder), recursive = TRUE), add = TRUE)
  sample_name <- "D20230101T120000_IFCB134"
  thr_test_app_raw_layout(roi_folder, sample_name)
  roi_paths <- thr_resolve_roi_paths(sample_name, roi_folder)
  # The disk goes away while the file is being read
  vanishing <- function(roi_file, ...) {
    unlink(roi_file)
    stop("cannot open file", call. = FALSE)
  }
  page <- thr_test_app_page(sample_name = sample_name, roi_number = 2:3)

  result <- thr_extract_page_images(page, roi_paths, out_folder, vanishing)

  expect_false(any(result$extracted))
  expect_equal(unique(result$no_image_reason), "no_raw")
})

test_that("thr_extract_page_images survives an extractor that throws", {
  out_folder <- tempfile("thr_png_")
  roi_folder <- tempfile("thr_raw_")
  on.exit(unlink(c(out_folder, roi_folder), recursive = TRUE), add = TRUE)
  bad <- "D20230101T120000_IFCB134"
  good <- "D20230102T120000_IFCB134"
  thr_test_app_raw_layout(roi_folder, c(bad, good))
  fake <- thr_test_app_fake_extractor(fail = bad)
  page <- thr_test_app_page(sample_name = c(bad, bad, good),
                            roi_number = c(2L, 3L, 2L))
  roi_paths <- thr_resolve_roi_paths(c(bad, good), roi_folder)

  expect_no_error(
    result <- thr_extract_page_images(page, roi_paths, out_folder,
                                      fake$extractor)
  )

  expect_equal(result$extracted, c(FALSE, FALSE, TRUE))
  # The raw file is there and reading it failed
  expect_equal(result$no_image_reason,
               c("not_extractable", "not_extractable", NA))
  # One call for the sample, then one per ROI to rescue what can be rescued
  failing <- vapply(fake$calls(), function(call) call$sample_name == bad,
                    logical(1))
  expect_equal(sum(failing), 3)
  expect_equal(sum(!failing), 1)
})

test_that("thr_extract_page_images rescues the ROIs a failed call left out", {
  out_folder <- tempfile("thr_png_")
  on.exit(unlink(out_folder, recursive = TRUE), add = TRUE)
  fake <- thr_test_app_fake_extractor(refuse = 99L)
  page <- thr_test_app_page(roi_number = c(2L, 99L, 3L))
  roi_paths <- stats::setNames(thr_test_app_raw_roi(), thr_test_app_raw_sample)

  result <- thr_extract_page_images(page, roi_paths, out_folder, fake$extractor)

  expect_equal(result$extracted, c(TRUE, FALSE, TRUE))
  expect_equal(result$no_image_reason, c(NA, "not_extractable", NA))
  expect_equal(fake$n_calls(), 4)
})

test_that("thr_extract_page_images calls the extractor once per sample", {
  out_folder <- tempfile("thr_png_")
  roi_folder <- tempfile("thr_raw_")
  on.exit(unlink(c(out_folder, roi_folder), recursive = TRUE), add = TRUE)
  samples <- c("D20230101T120000_IFCB134", "D20230102T120000_IFCB134")
  thr_test_app_raw_layout(roi_folder, samples)
  fake <- thr_test_app_fake_extractor()
  page <- thr_test_app_page(sample_name = samples,
                            roi_number = c(2L, 2L, 3L, 3L, 4L, 4L))
  roi_paths <- thr_resolve_roi_paths(samples, roi_folder)

  result <- thr_extract_page_images(page, roi_paths, out_folder, fake$extractor)

  expect_true(all(result$extracted))
  expect_equal(fake$n_calls(), 2)
  expect_equal(lapply(fake$calls(), `[[`, "rois"), list(2:4, 2:4))
  expect_equal(vapply(fake$calls(), `[[`, "", "sample_name"), samples)
})

test_that("thr_extract_page_images does not extract an image twice", {
  out_folder <- tempfile("thr_png_")
  on.exit(unlink(out_folder, recursive = TRUE), add = TRUE)
  fake <- thr_test_app_fake_extractor()
  roi_paths <- stats::setNames(thr_test_app_raw_roi(), thr_test_app_raw_sample)

  first <- thr_extract_page_images(thr_test_app_page(roi_number = 2:3),
                                   roi_paths, out_folder, fake$extractor)
  expect_equal(fake$n_calls(), 1)

  # Same page again: nothing to do
  again <- thr_extract_page_images(thr_test_app_page(roi_number = 2:3),
                                   roi_paths, out_folder, fake$extractor)
  expect_equal(fake$n_calls(), 1)
  expect_equal(again, first)

  # An image that is already there needs no raw file
  cached <- thr_extract_page_images(thr_test_app_page(roi_number = 2:3),
                                    character(), out_folder, fake$extractor)
  expect_true(all(cached$extracted))
  expect_true(all(is.na(cached$no_image_reason)))

  # A page that overlaps: only the new ROI is asked for
  more <- thr_extract_page_images(thr_test_app_page(roi_number = 2:4),
                                  roi_paths, out_folder, fake$extractor)
  expect_equal(fake$n_calls(), 2)
  expect_equal(fake$calls()[[2]]$rois, 4L)
  expect_true(all(more$extracted))
})

test_that("thr_extract_page_images handles an empty page", {
  out_folder <- tempfile("thr_png_")
  on.exit(unlink(out_folder, recursive = TRUE), add = TRUE)
  fake <- thr_test_app_fake_extractor()
  page <- thr_test_app_page()[0, ]

  result <- thr_extract_page_images(page, character(), out_folder,
                                    fake$extractor)

  expect_equal(nrow(result), 0)
  expect_type(result$png_file, "character")
  expect_type(result$extracted, "logical")
  expect_type(result$no_image_reason, "character")
  expect_equal(fake$n_calls(), 0)
})

test_that("thr_extract_page_images never builds a path from a bad name", {
  out_folder <- tempfile("thr_png_")
  on.exit(unlink(out_folder, recursive = TRUE), add = TRUE)
  fake <- thr_test_app_fake_extractor()
  page <- thr_test_app_page(sample_name = "../escape", roi_number = 2L)
  roi_paths <- stats::setNames(thr_test_app_raw_roi(), "../escape")

  result <- thr_extract_page_images(page, roi_paths, out_folder, fake$extractor)

  expect_false(result$extracted)
  expect_true(is.na(result$png_file))
  expect_equal(result$no_image_reason, "invalid")
  expect_equal(fake$n_calls(), 0)
})

test_that("thr_extract_page_images validates its arguments", {
  out_folder <- tempfile("thr_png_")
  on.exit(unlink(out_folder, recursive = TRUE), add = TRUE)
  page <- thr_test_app_page()

  expect_error(
    thr_extract_page_images(page[c("draw_id", "sample_name")], character(),
                            out_folder),
    "roi_number"
  )
  expect_error(thr_extract_page_images(page, character(), NA), "out_folder")
  expect_error(
    thr_extract_page_images(page, character(), out_folder, extractor = "x"),
    "extractor"
  )
})

# ---- thr_png_dimensions ------------------------------------------------------

test_that("thr_png_dimensions reads width and height from the header", {
  dims <- thr_png_dimensions(thr_test_app_png())

  expect_equal(dims$width, 160L)
  expect_equal(dims$height, 74L)
})

test_that("thr_png_dimensions gives NA for anything that is not a PNG", {
  not_png <- tempfile(fileext = ".png")
  on.exit(unlink(not_png), add = TRUE)
  writeLines("this is not an image", not_png)
  short <- tempfile(fileext = ".png")
  on.exit(unlink(short), add = TRUE)
  writeBin(as.raw(c(0x89, 0x50)), short)

  dims <- thr_png_dimensions(c(thr_test_app_png(), not_png, short,
                               file.path(tempdir(), "absent.png"), NA))

  expect_equal(dims$width, c(160L, NA, NA, NA, NA))
  expect_equal(dims$height, c(74L, NA, NA, NA, NA))
  expect_equal(nrow(thr_png_dimensions(character())), 0)
})
