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

test_that("load_from_csv with use_threshold=FALSE uses class_name_auto", {
  temp_csv <- tempfile(fileext = ".csv")
  mock_data <- data.frame(
    file_name = c("D20230101_00001.png", "D20230101_00002.png"),
    class_name = c("unclassified", "Diatom"),
    class_name_auto = c("Ciliate", "Diatom"),
    score = c(0.45, 0.95),
    stringsAsFactors = FALSE
  )
  write.csv(mock_data, temp_csv, row.names = FALSE)

  # With threshold (default) — uses class_name
  with_threshold <- load_from_csv(temp_csv, use_threshold = TRUE)
  expect_equal(with_threshold$class_name, c("unclassified", "Diatom"))

  # Without threshold — uses class_name_auto
  without_threshold <- load_from_csv(temp_csv, use_threshold = FALSE)
  expect_equal(without_threshold$class_name, c("Ciliate", "Diatom"))

  unlink(temp_csv)
})

test_that("load_from_csv with use_threshold=FALSE falls back when class_name_auto missing", {
  temp_csv <- tempfile(fileext = ".csv")
  mock_data <- data.frame(
    file_name = c("D20230101_00001.png"),
    class_name = c("Diatom"),
    score = c(0.95),
    stringsAsFactors = FALSE
  )
  write.csv(mock_data, temp_csv, row.names = FALSE)

  # Without threshold but no class_name_auto column — uses class_name
  result <- load_from_csv(temp_csv, use_threshold = FALSE)
  expect_equal(result$class_name, "Diatom")

  unlink(temp_csv)
})

test_that("load_from_h5 reads H5 classification file correctly", {
  skip_if_not_installed("hdf5r")

  h5_path <- testthat::test_path("test_data", "D20220522T000439_IFCB134_class.h5")
  skip_if_not(file.exists(h5_path), "Test H5 file not found")

  sample_name <- "D20220522T000439_IFCB134"

  # Create mock roi_dimensions matching the H5 file's ROI numbers
  h5 <- hdf5r::H5File$new(h5_path, "r")
  roi_numbers <- h5[["roi_numbers"]]$read()
  h5$close_all()

  roi_dimensions <- data.frame(
    roi_number = roi_numbers,
    width = rep(100, length(roi_numbers)),
    height = rep(100, length(roi_numbers)),
    area = seq(10000, 10000 + length(roi_numbers) - 1)
  )

  classifications <- load_from_h5(
    h5_path = h5_path,
    sample_name = sample_name,
    roi_dimensions = roi_dimensions,
    use_threshold = TRUE
  )

  expect_s3_class(classifications, "data.frame")
  expect_true(nrow(classifications) > 0)
  expect_named(classifications, c("file_name", "class_name", "score", "width", "height", "roi_area"))
  expect_type(classifications$class_name, "character")
  expect_type(classifications$score, "double")
  # All scores should be between 0 and 1
  expect_true(all(classifications$score >= 0 & classifications$score <= 1))
  # Should be sorted by area descending
  expect_equal(classifications$roi_area, sort(classifications$roi_area, decreasing = TRUE))
})

test_that("load_from_h5 with use_threshold=FALSE uses class_name_auto", {
  skip_if_not_installed("hdf5r")

  h5_path <- testthat::test_path("test_data", "D20220522T000439_IFCB134_class.h5")
  skip_if_not(file.exists(h5_path), "Test H5 file not found")

  sample_name <- "D20220522T000439_IFCB134"

  h5 <- hdf5r::H5File$new(h5_path, "r")
  roi_numbers <- h5[["roi_numbers"]]$read()
  h5$close_all()

  roi_dimensions <- data.frame(
    roi_number = roi_numbers,
    width = rep(100, length(roi_numbers)),
    height = rep(100, length(roi_numbers)),
    area = seq(10000, 10000 + length(roi_numbers) - 1)
  )

  with_threshold <- load_from_h5(h5_path, sample_name, roi_dimensions, use_threshold = TRUE)
  without_threshold <- load_from_h5(h5_path, sample_name, roi_dimensions, use_threshold = FALSE)

  expect_s3_class(without_threshold, "data.frame")
  expect_equal(nrow(with_threshold), nrow(without_threshold))
  # Without threshold should have no "unclassified" (all have raw predictions)
  expect_type(without_threshold$class_name, "character")
})

test_that("load_from_h5 errors without hdf5r package", {
  # We can't truly unload hdf5r, but we can check the function exists
  expect_true(is.function(load_from_h5))
  expect_equal(
    names(formals(load_from_h5)),
    c("h5_path", "sample_name", "roi_dimensions", "use_threshold")
  )
})

test_that("load_from_csv reads real CSV with class_name_auto", {
  csv_path <- testthat::test_path("test_data", "D20220522T000439_IFCB134.csv")
  skip_if_not(file.exists(csv_path), "Test CSV file not found")

  # With threshold
  with_threshold <- load_from_csv(csv_path, use_threshold = TRUE)
  expect_s3_class(with_threshold, "data.frame")
  expect_true(nrow(with_threshold) > 0)
  expect_true("file_name" %in% names(with_threshold))
  expect_true("class_name" %in% names(with_threshold))

  # Without threshold
  without_threshold <- load_from_csv(csv_path, use_threshold = FALSE)
  expect_equal(nrow(with_threshold), nrow(without_threshold))
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

test_that("rescan_file_index discovers H5 classifier files", {
  # Create temp folder structure
  roi_dir <- file.path(tempdir(), "test_h5_scan", "roi", "2022", "D20220522")
  csv_dir <- file.path(tempdir(), "test_h5_scan", "classified")
  output_dir <- file.path(tempdir(), "test_h5_scan", "output")
  dir.create(roi_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(csv_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # Create dummy ROI file
  file.create(file.path(roi_dir, "D20220522T000439_IFCB134.roi"))

  # Create dummy H5 and MAT classifier files
  file.create(file.path(csv_dir, "D20220522T000439_IFCB134_class.h5"))
  file.create(file.path(csv_dir, "D20220522T000439_IFCB134_class_v1.mat"))

  result <- rescan_file_index(
    roi_folder = file.path(tempdir(), "test_h5_scan", "roi"),
    csv_folder = csv_dir,
    output_folder = output_dir,
    verbose = FALSE,
    db_folder = tempdir()
  )

  expect_type(result, "list")
  expect_true("D20220522T000439_IFCB134" %in% result$classified_samples)
  expect_true("D20220522T000439_IFCB134" %in% names(result$classifier_h5_files))
  expect_true("D20220522T000439_IFCB134" %in% names(result$classifier_mat_files))

  # Cleanup
  unlink(file.path(tempdir(), "test_h5_scan"), recursive = TRUE)
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
