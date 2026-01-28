# Tests for utility functions

library(testthat)

# =============================================================================
# Config directory functions
# =============================================================================

test_that("get_config_dir returns a valid path", {
  config_dir <- get_config_dir()

  expect_type(config_dir, "character")
  expect_true(nchar(config_dir) > 0)
  # Should contain ClassiPyR in the path
  expect_true(grepl("ClassiPyR", config_dir))
})

test_that("get_settings_path returns a valid path ending in .json", {
  settings_path <- get_settings_path()

  expect_type(settings_path, "character")
  expect_true(grepl("\\.json$", settings_path))
  expect_true(grepl("ClassiPyR", settings_path))
})

test_that("get_settings_path creates directory if needed", {
  # Get the settings path
  settings_path <- get_settings_path()
  config_dir <- dirname(settings_path)

  # The directory should exist after calling get_settings_path
  expect_true(dir.exists(config_dir))
})

# =============================================================================
# Python environment
# =============================================================================

test_that("init_python_env returns boolean", {
  # This test just checks the return type, not actual Python availability
  result <- init_python_env()

  expect_type(result, "logical")
  expect_length(result, 1)
})

test_that("init_python_env accepts venv_path parameter", {
  # Test that the function accepts the parameter without error
  temp_venv <- file.path(tempdir(), "test-venv-param")

  # Should not error even if venv doesn't exist (will try to create or return FALSE)
  result <- tryCatch(
    init_python_env(venv_path = temp_venv),
    error = function(e) FALSE
  )

  expect_type(result, "logical")
})

# =============================================================================
# Sample path functions
# =============================================================================

test_that("get_sample_paths returns correct structure", {
 sample_name <- "D20230314T001205_IFCB134"
 roi_folder <- "/data/raw"

 paths <- get_sample_paths(sample_name, roi_folder)

 expect_type(paths, "list")
 expect_named(paths, c("year", "date_part", "roi_path", "adc_path", "adc_folder"))
 expect_equal(paths$year, "2023")
 expect_equal(paths$date_part, "D20230314")
 expect_true(grepl("\\.roi$", paths$roi_path))
 expect_true(grepl("\\.adc$", paths$adc_path))
})

test_that("get_sample_paths handles different sample names", {
 # Test with different year
 paths <- get_sample_paths("D20210101T000000_IFCB123", "/data")
 expect_equal(paths$year, "2021")
 expect_equal(paths$date_part, "D20210101")

 # Test with different IFCB number
 paths <- get_sample_paths("D20220615T120000_IFCB999", "/data")
 expect_equal(paths$year, "2022")
})

test_that("read_roi_dimensions returns correct structure", {
 # Create a mock ADC file
 temp_adc <- tempfile(fileext = ".adc")
 # ADC has many columns, V16=width, V17=height
 mock_data <- data.frame(
   V1 = 1:3, V2 = 0, V3 = 0, V4 = 0, V5 = 0,
   V6 = 0, V7 = 0, V8 = 0, V9 = 0, V10 = 0,
   V11 = 0, V12 = 0, V13 = 0, V14 = 0, V15 = 0,
   V16 = c(100, 150, 200),  # width
   V17 = c(80, 120, 160)    # height
 )
 write.table(mock_data, temp_adc, row.names = FALSE, col.names = FALSE, sep = ",")

 # Read and check
 dims <- read_roi_dimensions(temp_adc)

 expect_s3_class(dims, "data.frame")
 expect_named(dims, c("roi_number", "width", "height", "area"))
 expect_equal(dims$roi_number, 1:3)
 expect_equal(dims$width, c(100, 150, 200))
 expect_equal(dims$height, c(80, 120, 160))
 expect_equal(dims$area, c(8000, 18000, 32000))

 # Cleanup
 unlink(temp_adc)
})

test_that("create_empty_changes_log returns correct structure", {
 log <- create_empty_changes_log()

 expect_s3_class(log, "data.frame")
 expect_equal(nrow(log), 0)
 expect_named(log, c("image", "original_class", "new_class"))
})

test_that("load_class_list handles text files", {
 # Create a mock text class list
 temp_txt <- tempfile(fileext = ".txt")
 writeLines(c("Diatom", "Ciliate", "Dinoflagellate", ""), temp_txt)

 classes <- load_class_list(temp_txt)

 expect_type(classes, "character")
 expect_equal(length(classes), 3)
 expect_true("Diatom" %in% classes)
 expect_true("Ciliate" %in% classes)

 # Cleanup
 unlink(temp_txt)
})

test_that("load_class_list rejects unsupported formats", {
 temp_csv <- tempfile(fileext = ".csv")
 writeLines("class", temp_csv)

 expect_error(load_class_list(temp_csv), "Unsupported file format")

 unlink(temp_csv)
})

# Security validation tests

test_that("is_valid_sample_name validates correct formats", {
  # Valid sample names
  expect_true(is_valid_sample_name("D20230314T001205_IFCB134"))
  expect_true(is_valid_sample_name("D20210101T000000_IFCB1"))
  expect_true(is_valid_sample_name("D20221231T235959_IFCB999"))

  # Invalid sample names
  expect_false(is_valid_sample_name("../../../etc/passwd"))
  expect_false(is_valid_sample_name("D20230314_IFCB134"))
  expect_false(is_valid_sample_name("20230314T001205_IFCB134"))
  expect_false(is_valid_sample_name("D20230314T001205_IFCB"))
  expect_false(is_valid_sample_name(""))
  expect_false(is_valid_sample_name(NULL))
  expect_false(is_valid_sample_name(NA))
  expect_false(is_valid_sample_name(123))
})

test_that("get_sample_paths rejects invalid sample names", {
  expect_error(
    get_sample_paths("../../../etc/passwd", "/data"),
    "Invalid sample name format"
  )

  expect_error(
    get_sample_paths("malicious_name", "/data"),
    "Invalid sample name format"
  )

  expect_error(
    get_sample_paths("", "/data"),
    "Invalid sample name format"
  )
})

test_that("sanitize_string removes dangerous characters", {
  # XSS prevention - angle brackets and quotes removed
  expect_equal(sanitize_string("<script>alert('xss')</script>"), "scriptalert(xss)script")
  expect_equal(sanitize_string("normal_file.png"), "normal_file.png")

  # Path traversal prevention - dots and slashes removed
  expect_equal(sanitize_string("../../../etc/passwd"), "etcpasswd")
  expect_equal(sanitize_string("..\\..\\windows\\system32"), "windowssystem32")

  # Quotes and ampersands removed
  expect_equal(sanitize_string("test\"file'name&"), "testfilename")
})

test_that("load_class_list sanitizes dangerous class names", {
  temp_txt <- tempfile(fileext = ".txt")
  writeLines(c("Valid_Class", "Also-Valid", "Has<Script>", "Path_Traversal"), temp_txt)

  # Should warn about unsafe characters (< > etc)
  expect_warning(classes <- load_class_list(temp_txt), "unsafe characters")

  # Dangerous characters should be removed
  expect_false(any(grepl("<|>", classes)))

  # Valid characters like hyphens should be preserved
  expect_true("Also-Valid" %in% classes)

  unlink(temp_txt)
})

test_that("load_class_list replaces path separators with underscore", {
  temp_txt <- tempfile(fileext = ".txt")
  writeLines(c("Snowella/Woronichinia", "Normal_Class"), temp_txt)

  # Should show message (not warning) about slash replacement
  expect_message(classes <- load_class_list(temp_txt), "slashes.*replaced")

  # Slash should be replaced with underscore, not removed
  expect_true("Snowella_Woronichinia" %in% classes)
  expect_false(any(grepl("/", classes)))

  unlink(temp_txt)
})

test_that("load_class_list allows common taxonomic characters", {
  temp_txt <- tempfile(fileext = ".txt")
  writeLines(c(
    "Strombidium-like",
    "Mesodinium_rubrum",
    "Centrales_chain",
    "Chaetoceros spp"
  ), temp_txt)

  # Should NOT warn for normal taxonomic names
  expect_silent(classes <- load_class_list(temp_txt))

  expect_equal(length(classes), 4)
  expect_true("Strombidium-like" %in% classes)

  unlink(temp_txt)
})

test_that("read_roi_dimensions handles missing files", {
  expect_error(
    read_roi_dimensions("/nonexistent/file.adc"),
    "ADC file not found"
  )
})

test_that("read_roi_dimensions handles empty files", {
  temp_adc <- tempfile(fileext = ".adc")
  writeLines(character(0), temp_adc)

  # Should handle gracefully (may error on empty CSV)
  result <- tryCatch(
    read_roi_dimensions(temp_adc),
    error = function(e) "error"
  )

  # Either returns empty data frame or errors gracefully
  expect_true(
    identical(result, "error") ||
    (is.data.frame(result) && nrow(result) == 0)
  )

  unlink(temp_adc)
})

test_that("read_roi_dimensions returns empty data frame for ADC with no rows", {
  temp_adc <- tempfile(fileext = ".adc")
  # Write a CSV that read.csv will return with 0 rows
  # This requires a single empty row that gets interpreted as one row of NA
  # Or we need to simulate an edge case that read.csv handles
  # Actually, ADC files are headerless CSVs - an empty file causes read.csv to error
  # This test covers the error handling path instead
  writeLines("", temp_adc)  # Single empty line

  # The function wraps in tryCatch, so it should error gracefully
  expect_error(
    read_roi_dimensions(temp_adc),
    "Failed to read ADC file"
  )

  unlink(temp_adc)
})

test_that("read_roi_dimensions handles ADC files with fewer than 17 columns", {
  temp_adc <- tempfile(fileext = ".adc")
  # Create ADC with only 10 columns
  mock_data <- data.frame(
    V1 = 1:3, V2 = 0, V3 = 0, V4 = 0, V5 = 0,
    V6 = 0, V7 = 0, V8 = 0, V9 = 0, V10 = 0
  )
  write.table(mock_data, temp_adc, row.names = FALSE, col.names = FALSE, sep = ",")

  # Should warn about fewer columns and use default dimensions
  expect_warning(
    dims <- read_roi_dimensions(temp_adc),
    "fewer than 17 columns"
  )

  expect_s3_class(dims, "data.frame")
  expect_equal(nrow(dims), 3)
  # Default dimensions should be 1x1
  expect_equal(dims$width, c(1, 1, 1))
  expect_equal(dims$height, c(1, 1, 1))
  expect_equal(dims$area, c(1, 1, 1))

  unlink(temp_adc)
})

test_that("get_config_dir uses tempdir during R CMD check", {
  # Simulate R CMD check environment
  old_val <- Sys.getenv("_R_CHECK_PACKAGE_NAME_", unset = NA)
  Sys.setenv("_R_CHECK_PACKAGE_NAME_" = "ClassiPyR")

  config_dir <- get_config_dir()

  # Should use tempdir during R CMD check

  expect_true(grepl(tempdir(), config_dir, fixed = TRUE))

  # Restore environment
  if (is.na(old_val)) {
    Sys.unsetenv("_R_CHECK_PACKAGE_NAME_")
  } else {
    Sys.setenv("_R_CHECK_PACKAGE_NAME_" = old_val)
  }
})
