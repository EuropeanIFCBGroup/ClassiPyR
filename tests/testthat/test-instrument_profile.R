test_that("normalize_instrument_type accepts valid values case-insensitively", {
  expect_equal(normalize_instrument_type("IFCB"), "IFCB")
  expect_equal(normalize_instrument_type("ifcb"), "IFCB")
  expect_equal(normalize_instrument_type("generic"), "generic")
  expect_equal(normalize_instrument_type("Generic"), "generic")
})

test_that("normalize_instrument_type falls back to IFCB for invalid input", {
  expect_equal(normalize_instrument_type(NULL), "IFCB")
  expect_equal(normalize_instrument_type(NA), "IFCB")
  expect_equal(normalize_instrument_type(""), "IFCB")
  expect_equal(normalize_instrument_type("microscope"), "IFCB")
  expect_equal(normalize_instrument_type(c("a", "b")), "IFCB")
})

test_that("parse_image_extensions handles strings and vectors", {
  expect_equal(parse_image_extensions("png, jpg, .JPEG"), c("png", "jpg", "jpeg"))
  expect_equal(parse_image_extensions(c("tif", "tiff")), c("tif", "tiff"))
  expect_equal(parse_image_extensions(".PNG"), "png")
  expect_equal(parse_image_extensions("png png jpg"), c("png", "jpg"))
})

test_that("parse_image_extensions falls back to defaults when empty", {
  expect_equal(parse_image_extensions(NULL), DEFAULT_IMAGE_EXTENSIONS)
  expect_equal(parse_image_extensions(""), DEFAULT_IMAGE_EXTENSIONS)
  expect_equal(parse_image_extensions(character(0)), DEFAULT_IMAGE_EXTENSIONS)
})

test_that("image_file_pattern matches the right files case-insensitively", {
  pat <- image_file_pattern(c("png", "jpg"))
  expect_true(grepl(pat, "cell.png", ignore.case = TRUE))
  expect_true(grepl(pat, "cell.PNG", ignore.case = TRUE))
  expect_true(grepl(pat, "photo.jpg", ignore.case = TRUE))
  expect_false(grepl(pat, "notes.txt", ignore.case = TRUE))
  expect_false(grepl(pat, "cell.png.bak", ignore.case = TRUE))
})

test_that("assign_roi_numbers parses IFCB suffix", {
  files <- c("D20230101T120000_IFCB134_00001.png",
             "D20230101T120000_IFCB134_00042.png")
  expect_equal(assign_roi_numbers(files, "IFCB"), c(1L, 42L))
  # default is IFCB
  expect_equal(assign_roi_numbers(files), c(1L, 42L))
})

test_that("assign_roi_numbers assigns stable order for generic", {
  files <- c("beta.jpg", "alpha.jpg", "gamma.png")
  roi <- assign_roi_numbers(files, "generic")
  # alpha -> 1, beta -> 2, gamma -> 3, in original vector order
  expect_equal(roi, c(2L, 1L, 3L))

  # Stability: same set in a different input order yields the same mapping
  files2 <- c("gamma.png", "alpha.jpg", "beta.jpg")
  roi2 <- assign_roi_numbers(files2, "generic")
  names(roi) <- files
  names(roi2) <- files2
  expect_equal(roi[["alpha.jpg"]], roi2[["alpha.jpg"]])
  expect_equal(roi[["beta.jpg"]], roi2[["beta.jpg"]])
  expect_equal(roi[["gamma.png"]], roi2[["gamma.png"]])
})

test_that("assign_roi_numbers handles empty input", {
  expect_equal(assign_roi_numbers(character(0), "generic"), integer(0))
  expect_equal(assign_roi_numbers(character(0), "IFCB"), integer(0))
})
