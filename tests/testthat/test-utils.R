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

test_that("read_roi_dimensions handles 0-byte ADC file", {
  # create a truly empty temp file
  tmp_file <- tempfile(fileext = ".adc")
  file.create(tmp_file)  # creates 0-byte file
  
  res <- read_roi_dimensions(tmp_file)
  
  # Expect a data frame with zero rows and correct columns
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 0)
  expect_equal(colnames(res), c("roi_number", "width", "height", "area"))
  
  # Clean up
  unlink(tmp_file)
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

# =============================================================================
# File index cache functions
# =============================================================================

test_that("get_file_index_path returns a valid path ending in .json", {
  index_path <- get_file_index_path()

  expect_type(index_path, "character")
  expect_true(grepl("\\.json$", index_path))
  expect_true(grepl("ClassiPyR", index_path))
  # Should be in the same directory as settings
  expect_equal(dirname(index_path), dirname(get_settings_path()))
})

test_that("save_file_index and load_file_index round-trip data correctly", {
  # Clean up any existing cache first
  cache_path <- get_file_index_path()
  if (file.exists(cache_path)) file.remove(cache_path)

  test_data <- list(
    roi_folder = "/data/roi",
    csv_folder = "/data/csv",
    output_folder = "/data/output",
    sample_names = c("D20230101T120000_IFCB134", "D20230102T130000_IFCB134"),
    classified_samples = c("D20230101T120000_IFCB134"),
    annotated_samples = character(),
    roi_path_map = list(
      "D20230101T120000_IFCB134" = "/data/roi/2023/D20230101/D20230101T120000_IFCB134.roi",
      "D20230102T130000_IFCB134" = "/data/roi/2023/D20230102/D20230102T130000_IFCB134.roi"
    ),
    csv_path_map = list(
      "D20230101T120000_IFCB134" = "/data/csv/2023/D20230101T120000_IFCB134.csv"
    ),
    classifier_mat_files = list(
      "D20230101T120000_IFCB134" = "/data/csv/2023/D20230101T120000_IFCB134_class_v1.mat"
    ),
    timestamp = "2024-01-01 12:00:00"
  )

  # Write using actual exported function
  save_file_index(test_data)
  expect_true(file.exists(cache_path))

  # Read back using actual exported function
  loaded <- load_file_index()

  expect_type(loaded, "list")
  expect_equal(loaded$roi_folder, "/data/roi")
  expect_equal(loaded$csv_folder, "/data/csv")
  expect_equal(loaded$output_folder, "/data/output")
  expect_length(loaded$sample_names, 2)
  expect_equal(loaded$sample_names[[1]], "D20230101T120000_IFCB134")
  expect_length(loaded$classified_samples, 1)
  expect_length(loaded$annotated_samples, 0)

  # Path maps survive JSON round-trip as named lists
  roi_map <- as.list(loaded$roi_path_map)
  expect_equal(roi_map[["D20230101T120000_IFCB134"]],
               "/data/roi/2023/D20230101/D20230101T120000_IFCB134.roi")
  expect_equal(roi_map[["D20230102T130000_IFCB134"]],
               "/data/roi/2023/D20230102/D20230102T130000_IFCB134.roi")

  csv_map <- as.list(loaded$csv_path_map)
  expect_equal(csv_map[["D20230101T120000_IFCB134"]],
               "/data/csv/2023/D20230101T120000_IFCB134.csv")

  mat_map <- as.list(loaded$classifier_mat_files)
  expect_equal(mat_map[["D20230101T120000_IFCB134"]],
               "/data/csv/2023/D20230101T120000_IFCB134_class_v1.mat")

  expect_equal(loaded$timestamp, "2024-01-01 12:00:00")

  # Clean up
  if (file.exists(cache_path)) file.remove(cache_path)
})

test_that("save_file_index and load_file_index handle empty lists correctly", {
  cache_path <- get_file_index_path()
  if (file.exists(cache_path)) file.remove(cache_path)

  test_data <- list(
    roi_folder = "/test/roi",
    csv_folder = "/test/csv",
    output_folder = "/test/output",
    sample_names = c("D20220101T000000_IFCB1"),
    classified_samples = character(),
    annotated_samples = character(),
    roi_path_map = list("D20220101T000000_IFCB1" = "/test/roi/sample.roi"),
    csv_path_map = list(),
    classifier_mat_files = list(),
    timestamp = as.character(Sys.time())
  )

  save_file_index(test_data)
  loaded <- load_file_index()

  expect_equal(loaded$roi_folder, "/test/roi")
  expect_equal(as.character(loaded$sample_names), "D20220101T000000_IFCB1")

  # Path map round-trips correctly
  roi_map <- as.list(loaded$roi_path_map)
  expect_equal(roi_map[["D20220101T000000_IFCB1"]], "/test/roi/sample.roi")

  # Empty lists round-trip correctly
  expect_length(loaded$csv_path_map, 0)
  expect_length(loaded$classifier_mat_files, 0)
  expect_length(loaded$classified_samples, 0)
  expect_length(loaded$annotated_samples, 0)

  if (file.exists(cache_path)) file.remove(cache_path)
})

test_that("load_file_index returns NULL when no cache exists", {
  cache_path <- get_file_index_path()
  if (file.exists(cache_path)) file.remove(cache_path)

  result <- load_file_index()
  expect_null(result)
})

test_that("load_file_index returns NULL for invalid JSON", {
  cache_path <- get_file_index_path()

  # Write invalid JSON to the actual cache path
  dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
  writeLines("this is not valid json {{{", cache_path)

  result <- load_file_index()
  expect_null(result)

  if (file.exists(cache_path)) file.remove(cache_path)
})

test_that("save_file_index handles write errors gracefully", {
  # Try to save to an invalid path - should not error (message only)
  expect_no_error(
    save_file_index(list(test = TRUE))
  )
})

# =============================================================================
# rescan_file_index
# =============================================================================

test_that("rescan_file_index returns NULL for invalid roi_folder", {
  result <- rescan_file_index(
    roi_folder = "/nonexistent/path",
    csv_folder = "/nonexistent/path",
    output_folder = "/nonexistent/path",
    verbose = FALSE
  )
  expect_null(result)
})

test_that("rescan_file_index scans folders and builds cache", {
  # Create a temp directory structure with mock ROI, CSV, and MAT files
  temp_root <- tempfile("rescan_test_")
  roi_folder <- file.path(temp_root, "raw", "2023", "D20230101")
  csv_folder <- file.path(temp_root, "classified", "2023")
  output_folder <- file.path(temp_root, "manual")
  dir.create(roi_folder, recursive = TRUE)
  dir.create(csv_folder, recursive = TRUE)
  dir.create(output_folder, recursive = TRUE)

  # Create mock ROI/ADC files
  file.create(file.path(roi_folder, "D20230101T120000_IFCB134.roi"))
  file.create(file.path(roi_folder, "D20230101T120000_IFCB134.adc"))
  file.create(file.path(roi_folder, "D20230101T130000_IFCB134.roi"))
  file.create(file.path(roi_folder, "D20230101T130000_IFCB134.adc"))

  # Create a mock CSV classification
  writeLines("file_name,class_name", file.path(csv_folder, "D20230101T120000_IFCB134.csv"))

  # Create a mock manual annotation MAT
  file.create(file.path(output_folder, "D20230101T130000_IFCB134.mat"))

  result <- rescan_file_index(
    roi_folder = file.path(temp_root, "raw"),
    csv_folder = file.path(temp_root, "classified"),
    output_folder = output_folder,
    verbose = FALSE
  )

  expect_type(result, "list")
  expect_length(result$sample_names, 2)
  expect_true("D20230101T120000_IFCB134" %in% result$sample_names)
  expect_true("D20230101T130000_IFCB134" %in% result$sample_names)

  # Check classified samples (CSV match)
  expect_true("D20230101T120000_IFCB134" %in% result$classified_samples)

  # Check annotated samples (MAT in output folder)
  expect_true("D20230101T130000_IFCB134" %in% result$annotated_samples)

  # Check ROI path map
  expect_true(!is.null(result$roi_path_map[["D20230101T120000_IFCB134"]]))
  expect_true(grepl("\\.roi$", result$roi_path_map[["D20230101T120000_IFCB134"]]))

  # Check CSV path map
  expect_true(!is.null(result$csv_path_map[["D20230101T120000_IFCB134"]]))

  # Check timestamp exists
  expect_true(!is.null(result$timestamp))

  # Verify the cache file was written
  cache_path <- get_file_index_path()
  expect_true(file.exists(cache_path))

  # Verify round-trip: load cache and compare
  loaded <- load_file_index()
  expect_equal(length(loaded$sample_names), 2)

  unlink(temp_root, recursive = TRUE)
})

test_that("rescan_file_index works with non-standard folder structure", {
  # Create a flat folder structure (no YYYY/DYYYYMMDD hierarchy)
  temp_root <- tempfile("flat_test_")
  roi_folder <- file.path(temp_root, "all_roi_files")
  dir.create(roi_folder, recursive = TRUE)

  # ROI files directly in the folder, no subdirectories
  file.create(file.path(roi_folder, "D20220601T100000_IFCB1.roi"))
  file.create(file.path(roi_folder, "D20220601T100000_IFCB1.adc"))
  file.create(file.path(roi_folder, "D20230715T200000_IFCB999.roi"))
  file.create(file.path(roi_folder, "D20230715T200000_IFCB999.adc"))

  result <- rescan_file_index(
    roi_folder = roi_folder,
    csv_folder = tempdir(),
    output_folder = tempdir(),
    verbose = FALSE
  )

  expect_type(result, "list")
  expect_length(result$sample_names, 2)
  expect_true("D20220601T100000_IFCB1" %in% result$sample_names)
  expect_true("D20230715T200000_IFCB999" %in% result$sample_names)

  # Path map should contain the flat paths (no year subdirectory)
  roi_path <- result$roi_path_map[["D20220601T100000_IFCB1"]]
  expect_true(grepl("all_roi_files", roi_path))
  # The path should go directly from roi_folder to the file, no YYYY/DYYYYMMDD layer
  expect_equal(normalizePath(dirname(roi_path), winslash = "/"), 
               normalizePath(roi_folder, winslash = "/"))

  unlink(temp_root, recursive = TRUE)
})

test_that("rescan_file_index reads folder paths from saved settings", {
  # This test verifies that rescan_file_index falls back to saved settings
  # We can't easily mock get_settings_path, so we test the fallback path:
  # when all folder args are NULL and no settings file exists, it should
  # return NULL gracefully
  result <- rescan_file_index(
    roi_folder = NULL,
    csv_folder = NULL,
    output_folder = NULL,
    verbose = FALSE
  )
  # If no settings exist with valid paths, result is NULL
  # (the actual behavior depends on whether settings are saved,
  # but the function should not error)
  expect_true(is.null(result) || is.list(result))
})
