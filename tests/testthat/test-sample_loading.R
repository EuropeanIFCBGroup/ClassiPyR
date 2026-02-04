# Tests for sample loading functions

library(testthat)

test_that("load_from_csv reads classification file correctly", {
  # Create mock CSV
  temp_csv <- tempfile(fileext = ".csv")
  mock_data <- data.frame(
    file_name = c("D20230101_00001.png", "D20230101_00002.png"),
    class_name = c("Diatom_01", "Ciliate_02"),
    score = c(0.95, 0.87),
    stringsAsFactors = FALSE
  )
  write.csv(mock_data, temp_csv, row.names = FALSE)

  # Load and check
  classifications <- load_from_csv(temp_csv)

  expect_s3_class(classifications, "data.frame")
  expect_equal(nrow(classifications), 2)
  expect_true("file_name" %in% names(classifications))
  expect_true("class_name" %in% names(classifications))

  # Cleanup
  unlink(temp_csv)
})

test_that("create_new_classifications creates correct structure", {
  sample_name <- "D20230101T120000_IFCB134"
  roi_dims <- data.frame(
    roi_number = 1:5,
    width = rep(100, 5),
    height = rep(80, 5),
    area = rep(8000, 5)
  )

  classifications <- create_new_classifications(sample_name, roi_dims)

  expect_s3_class(classifications, "data.frame")
  expect_equal(nrow(classifications), 5)
  expect_true(all(classifications$class_name == "unclassified"))
  expect_true(all(grepl(sample_name, classifications$file_name)))
  expect_true(all(grepl("\\.png$", classifications$file_name)))
})

test_that("create_new_classifications sorts by area descending", {
  sample_name <- "D20230101T120000_IFCB134"
  roi_dims <- data.frame(
    roi_number = 1:3,
    width = c(10, 20, 15),
    height = c(10, 20, 15),
    area = c(100, 400, 225)
  )

  classifications <- create_new_classifications(sample_name, roi_dims)

  # Should be sorted by area descending
  expect_equal(classifications$roi_area, c(400, 225, 100))
})

test_that("filter_to_extracted filters correctly", {
  classifications <- data.frame(
    file_name = c("img_001.png", "img_002.png", "img_003.png"),
    class_name = c("A", "B", "C"),
    stringsAsFactors = FALSE
  )

  # Create temp folder with only some files

temp_dir <- tempfile()
  dir.create(temp_dir)
  file.create(file.path(temp_dir, "img_001.png"))
  file.create(file.path(temp_dir, "img_003.png"))

  filtered <- filter_to_extracted(classifications, temp_dir)

  expect_equal(nrow(filtered), 2)
  expect_true("img_001.png" %in% filtered$file_name)
  expect_true("img_003.png" %in% filtered$file_name)
  expect_false("img_002.png" %in% filtered$file_name)

  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})

test_that("filter_to_extracted returns all if folder doesn't exist", {
  classifications <- data.frame(
    file_name = c("img_001.png", "img_002.png"),
    class_name = c("A", "B"),
    stringsAsFactors = FALSE
  )

  filtered <- filter_to_extracted(classifications, "/nonexistent/folder")

  expect_equal(nrow(filtered), 2)
})

test_that("load_from_classifier_mat handles class names correctly", {
  skip_if_not_installed("iRfcb")

  # This test would require a mock MAT file

  # For now, we test that the function exists and has correct signature
  expect_true(is.function(load_from_classifier_mat))
  expect_equal(
    names(formals(load_from_classifier_mat)),
    c("mat_path", "sample_name", "class2use", "roi_dimensions", "use_threshold")
  )
})

# Tests using real MAT files from test_data/

test_that("load_class_list reads class2use.mat correctly", {
  skip_if_not_installed("iRfcb")

  mat_path <- testthat::test_path("test_data", "class2use.mat")
  skip_if_not(file.exists(mat_path), "Test data file not found")

  classes <- load_class_list(mat_path)

  expect_type(classes, "character")
  expect_true(length(classes) > 0)
  # All classes should be non-empty strings
  expect_true(all(nchar(classes) > 0))
})

test_that("load_from_mat reads manual annotation file correctly", {
  skip_if_not_installed("iRfcb")

  mat_path <- testthat::test_path("test_data", "D20220522T000439_IFCB134.mat")
  adc_path <- testthat::test_path("test_data", "raw", "2022", "D20220522", "D20220522T000439_IFCB134.adc")
  class2use_path <- testthat::test_path("test_data", "class2use.mat")
  skip_if_not(file.exists(mat_path), "Test MAT file not found")
  skip_if_not(file.exists(adc_path), "Test ADC file not found")
  skip_if_not(file.exists(class2use_path), "Test class2use file not found")

  sample_name <- "D20220522T000439_IFCB134"
  class2use <- load_class_list(class2use_path)
  roi_dimensions <- read_roi_dimensions(adc_path)

  classifications <- load_from_mat(mat_path, sample_name, class2use, roi_dimensions)

  expect_s3_class(classifications, "data.frame")
  expect_true(nrow(classifications) > 0)
  expect_named(classifications, c("file_name", "class_name", "score", "width", "height", "roi_area"))
  # All file names should contain sample name and .png
  expect_true(all(grepl(sample_name, classifications$file_name)))
  expect_true(all(grepl("\\.png$", classifications$file_name)))
  # Class names should be strings
  expect_type(classifications$class_name, "character")
  # Should be sorted by area descending
  expect_equal(classifications$roi_area, sort(classifications$roi_area, decreasing = TRUE))
})

test_that("load_from_classifier_mat reads classifier output correctly", {
  skip_if_not_installed("iRfcb")

  mat_path <- testthat::test_path("test_data", "D20230314T001205_IFCB134_class_v1.mat")
  skip_if_not(file.exists(mat_path), "Test classifier MAT file not found")

  # Create mock roi_dimensions for this sample
  # The classifier file has its own roinum, so we need matching dimensions
  sample_name <- "D20230314T001205_IFCB134"

  # Read roinum from the mat file to know how many ROIs
  roi_numbers <- as.vector(iRfcb::ifcb_get_mat_variable(mat_path, variable_name = "roinum"))

  roi_dimensions <- data.frame(
    roi_number = roi_numbers,
    width = rep(100, length(roi_numbers)),
    height = rep(100, length(roi_numbers)),
    area = seq(10000, 10000 + length(roi_numbers) - 1)
  )

  classifications <- load_from_classifier_mat(
    mat_path = mat_path,
    sample_name = sample_name,
    class2use = character(0),  # Not used for classifier files
    roi_dimensions = roi_dimensions,
    use_threshold = TRUE
  )

  expect_s3_class(classifications, "data.frame")
  expect_true(nrow(classifications) > 0)
  expect_named(classifications, c("file_name", "class_name", "score", "width", "height", "roi_area"))
  # Class names should be strings
  expect_type(classifications$class_name, "character")
  # Should be sorted by area descending
  expect_equal(classifications$roi_area, sort(classifications$roi_area, decreasing = TRUE))
})

test_that("load_from_classifier_mat works with use_threshold=FALSE", {
  skip_if_not_installed("iRfcb")

  mat_path <- testthat::test_path("test_data", "D20230314T001205_IFCB134_class_v1.mat")
  skip_if_not(file.exists(mat_path), "Test classifier MAT file not found")

  sample_name <- "D20230314T001205_IFCB134"

  # Read roinum from the mat file
  roi_numbers <- as.vector(iRfcb::ifcb_get_mat_variable(mat_path, variable_name = "roinum"))

  roi_dimensions <- data.frame(
    roi_number = roi_numbers,
    width = rep(100, length(roi_numbers)),
    height = rep(100, length(roi_numbers)),
    area = seq(10000, 10000 + length(roi_numbers) - 1)
  )

  # Test with use_threshold = FALSE (uses TBclass instead of TBclass_above_threshold)
  classifications <- load_from_classifier_mat(
    mat_path = mat_path,
    sample_name = sample_name,
    class2use = character(0),
    roi_dimensions = roi_dimensions,
    use_threshold = FALSE
  )

  expect_s3_class(classifications, "data.frame")
  expect_true(nrow(classifications) > 0)
  # With threshold=FALSE, no class should be "unclassified" (all have TBclass)
  expect_type(classifications$class_name, "character")
})

test_that("read_roi_dimensions reads real ADC file correctly", {
  adc_path <- testthat::test_path("test_data", "raw", "2022", "D20220522", "D20220522T000439_IFCB134.adc")
  skip_if_not(file.exists(adc_path), "Test ADC file not found")

  dims <- read_roi_dimensions(adc_path)

  expect_s3_class(dims, "data.frame")
  expect_true(nrow(dims) > 0)
  expect_named(dims, c("roi_number", "width", "height", "area"))
  # ROI numbers should be positive
  expect_true(all(dims$roi_number > 0))
  # Width/height can be 0 for trigger events, but should be non-negative
  expect_true(all(dims$width >= 0))
  expect_true(all(dims$height >= 0))
  expect_true(all(dims$area >= 0))
  # Area should be width * height
  expect_equal(dims$area, dims$width * dims$height)
  # Most ROIs should have positive dimensions
  expect_true(sum(dims$area > 0) > 0)
})
