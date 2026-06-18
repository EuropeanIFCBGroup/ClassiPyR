# Tests for PNG folder import functionality

library(testthat)

# Helper: create a temporary PNG class folder structure
create_test_png_folder <- function() {
  base <- tempfile("png_import_")
  dir.create(base)

  # Create class subfolders with _NNN suffixes (iRfcb convention)
  diatom_dir <- file.path(base, "Diatom_001")
  ciliate_dir <- file.path(base, "Ciliate_002")
  dir.create(diatom_dir)
  dir.create(ciliate_dir)

  # Create dummy PNG files (1x1 pixel PNGs)
  png_bytes <- as.raw(c(
    0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A,
    0x00, 0x00, 0x00, 0x0D, 0x49, 0x48, 0x44, 0x52,
    0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01,
    0x08, 0x02, 0x00, 0x00, 0x00, 0x90, 0x77, 0x53,
    0xDE, 0x00, 0x00, 0x00, 0x0C, 0x49, 0x44, 0x41,
    0x54, 0x08, 0xD7, 0x63, 0xF8, 0xCF, 0xC0, 0x00,
    0x00, 0x00, 0x02, 0x00, 0x01, 0xE2, 0x21, 0xBC,
    0x33, 0x00, 0x00, 0x00, 0x00, 0x49, 0x45, 0x4E,
    0x44, 0xAE, 0x42, 0x60, 0x82
  ))

  # Sample 1: two ROIs in Diatom, one in Ciliate
  writeBin(png_bytes, file.path(diatom_dir, "D20230101T120000_IFCB134_00001.png"))
  writeBin(png_bytes, file.path(diatom_dir, "D20230101T120000_IFCB134_00003.png"))
  writeBin(png_bytes, file.path(ciliate_dir, "D20230101T120000_IFCB134_00002.png"))

  # Sample 2: one ROI in Diatom
  writeBin(png_bytes, file.path(diatom_dir, "D20230202T080000_IFCB134_00001.png"))

  base
}

# ===========================================================================
# scan_png_class_folder tests
# ===========================================================================

test_that("scan_png_class_folder parses folder structure correctly", {
  png_folder <- create_test_png_folder()
  on.exit(unlink(png_folder, recursive = TRUE))

  result <- scan_png_class_folder(png_folder)

  expect_type(result, "list")
  expect_s3_class(result$annotations, "data.frame")
  expect_equal(nrow(result$annotations), 4)

  expect_true(all(c("sample_name", "roi_number", "file_name", "class_name")
                  %in% names(result$annotations)))

  # Check class names have _NNN stripped
  expect_equal(sort(result$classes_found), c("Ciliate", "Diatom"))

  # Check sample names
  expect_equal(sort(result$sample_names),
               c("D20230101T120000_IFCB134", "D20230202T080000_IFCB134"))
})

test_that("scan_png_class_folder strips trailing _NNN from folder names", {
  base <- tempfile("png_strip_")
  dir.create(base)
  on.exit(unlink(base, recursive = TRUE))

  # Create folders with various suffix patterns
  dir.create(file.path(base, "Mesodinium_rubrum_005"))
  dir.create(file.path(base, "Strombidium-like_008"))
  dir.create(file.path(base, "NoSuffix"))

  png_bytes <- as.raw(c(
    0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A,
    0x00, 0x00, 0x00, 0x0D, 0x49, 0x48, 0x44, 0x52,
    0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01,
    0x08, 0x02, 0x00, 0x00, 0x00, 0x90, 0x77, 0x53,
    0xDE, 0x00, 0x00, 0x00, 0x0C, 0x49, 0x44, 0x41,
    0x54, 0x08, 0xD7, 0x63, 0xF8, 0xCF, 0xC0, 0x00,
    0x00, 0x00, 0x02, 0x00, 0x01, 0xE2, 0x21, 0xBC,
    0x33, 0x00, 0x00, 0x00, 0x00, 0x49, 0x45, 0x4E,
    0x44, 0xAE, 0x42, 0x60, 0x82
  ))

  writeBin(png_bytes, file.path(base, "Mesodinium_rubrum_005", "D20230101T120000_IFCB134_00001.png"))
  writeBin(png_bytes, file.path(base, "Strombidium-like_008", "D20230101T120000_IFCB134_00002.png"))
  writeBin(png_bytes, file.path(base, "NoSuffix", "D20230101T120000_IFCB134_00003.png"))

  result <- scan_png_class_folder(base)
  expect_equal(sort(result$classes_found),
               c("Mesodinium_rubrum", "NoSuffix", "Strombidium-like"))
})

test_that("scan_png_class_folder handles empty folder", {
  empty <- tempfile("png_empty_")
  dir.create(empty)
  on.exit(unlink(empty, recursive = TRUE))

  result <- scan_png_class_folder(empty)
  expect_equal(nrow(result$annotations), 0)
  expect_equal(length(result$classes_found), 0)
  expect_equal(length(result$sample_names), 0)
})

test_that("scan_png_class_folder errors on nonexistent folder", {
  expect_error(scan_png_class_folder("/nonexistent/path"),
               "does not exist")
})

test_that("scan_png_class_folder warns about invalid filenames", {
  base <- tempfile("png_bad_")
  dir.create(base)
  dir.create(file.path(base, "Diatom_001"))
  on.exit(unlink(base, recursive = TRUE))

  png_bytes <- as.raw(c(
    0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A,
    0x00, 0x00, 0x00, 0x0D, 0x49, 0x48, 0x44, 0x52,
    0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01,
    0x08, 0x02, 0x00, 0x00, 0x00, 0x90, 0x77, 0x53,
    0xDE, 0x00, 0x00, 0x00, 0x0C, 0x49, 0x44, 0x41,
    0x54, 0x08, 0xD7, 0x63, 0xF8, 0xCF, 0xC0, 0x00,
    0x00, 0x00, 0x02, 0x00, 0x01, 0xE2, 0x21, 0xBC,
    0x33, 0x00, 0x00, 0x00, 0x00, 0x49, 0x45, 0x4E,
    0x44, 0xAE, 0x42, 0x60, 0x82
  ))

  writeBin(png_bytes, file.path(base, "Diatom_001", "badname.png"))
  writeBin(png_bytes, file.path(base, "Diatom_001", "D20230101T120000_IFCB134_00001.png"))

  expect_warning(
    result <- scan_png_class_folder(base),
    "unexpected name format"
  )
  expect_equal(nrow(result$annotations), 1)
})

test_that("scan_png_class_folder warns about duplicate ROIs", {
  base <- tempfile("png_dup_")
  dir.create(base)
  dir.create(file.path(base, "Diatom_001"))
  dir.create(file.path(base, "Ciliate_002"))
  on.exit(unlink(base, recursive = TRUE))

  png_bytes <- as.raw(c(
    0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A,
    0x00, 0x00, 0x00, 0x0D, 0x49, 0x48, 0x44, 0x52,
    0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01,
    0x08, 0x02, 0x00, 0x00, 0x00, 0x90, 0x77, 0x53,
    0xDE, 0x00, 0x00, 0x00, 0x0C, 0x49, 0x44, 0x41,
    0x54, 0x08, 0xD7, 0x63, 0xF8, 0xCF, 0xC0, 0x00,
    0x00, 0x00, 0x02, 0x00, 0x01, 0xE2, 0x21, 0xBC,
    0x33, 0x00, 0x00, 0x00, 0x00, 0x49, 0x45, 0x4E,
    0x44, 0xAE, 0x42, 0x60, 0x82
  ))

  # Same file in two class folders
  writeBin(png_bytes, file.path(base, "Diatom_001", "D20230101T120000_IFCB134_00001.png"))
  writeBin(png_bytes, file.path(base, "Ciliate_002", "D20230101T120000_IFCB134_00001.png"))

  expect_warning(
    result <- scan_png_class_folder(base),
    "Duplicate ROI"
  )
  # Only the first occurrence should be kept
  expect_equal(nrow(result$annotations), 1)
})

# ===========================================================================
# import_png_folder_to_db tests
# ===========================================================================

test_that("import_png_folder_to_db writes annotations to database", {
  png_folder <- create_test_png_folder()
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  db_path <- get_db_path(db_dir)
  on.exit({
    unlink(png_folder, recursive = TRUE)
    unlink(db_dir, recursive = TRUE)
  })

  class2use <- c("unclassified", "Diatom", "Ciliate")

  result <- import_png_folder_to_db(png_folder, db_path, class2use,
                                     annotator = "TestUser")

  expect_equal(result$success, 2L)  # 2 samples
  expect_equal(result$failed, 0L)

  # Verify database contents
  samples <- list_annotated_samples_db(db_path)
  expect_equal(sort(samples),
               c("D20230101T120000_IFCB134", "D20230202T080000_IFCB134"))
})

test_that("import_png_folder_to_db applies class mapping", {
  png_folder <- create_test_png_folder()
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  db_path <- get_db_path(db_dir)
  on.exit({
    unlink(png_folder, recursive = TRUE)
    unlink(db_dir, recursive = TRUE)
  })

  class2use <- c("unclassified", "Renamed_Diatom", "Ciliate")

  # Map "Diatom" -> "Renamed_Diatom"
  result <- import_png_folder_to_db(
    png_folder, db_path, class2use,
    class_mapping = c("Diatom" = "Renamed_Diatom"),
    annotator = "TestUser"
  )

  expect_equal(result$success, 2L)

  # Verify mapped class names in database
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  rows <- DBI::dbGetQuery(con,
    "SELECT DISTINCT class_name FROM annotations ORDER BY class_name")
  expect_true("Renamed_Diatom" %in% rows$class_name)
  expect_false("Diatom" %in% rows$class_name)
  expect_true("Ciliate" %in% rows$class_name)
})

test_that("import_png_folder_to_db overwrites existing samples", {
  png_folder <- create_test_png_folder()
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  db_path <- get_db_path(db_dir)
  on.exit({
    unlink(png_folder, recursive = TRUE)
    unlink(db_dir, recursive = TRUE)
  })

  class2use <- c("unclassified", "Diatom", "Ciliate")

  # First import
  result1 <- import_png_folder_to_db(png_folder, db_path, class2use,
                                      annotator = "User1")
  expect_equal(result1$success, 2L)

  # Second import (should overwrite)
  result2 <- import_png_folder_to_db(png_folder, db_path, class2use,
                                      annotator = "User2")
  expect_equal(result2$success, 2L)

  # Verify annotator was updated (overwritten)
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  rows <- DBI::dbGetQuery(con, "SELECT DISTINCT annotator FROM annotations")
  expect_equal(rows$annotator, "User2")
})

test_that("import_png_folder_to_db handles empty folder", {
  empty <- tempfile("png_empty_")
  dir.create(empty)
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  db_path <- get_db_path(db_dir)
  on.exit({
    unlink(empty, recursive = TRUE)
    unlink(db_dir, recursive = TRUE)
  })

  result <- import_png_folder_to_db(empty, db_path, c("unclassified"))
  expect_equal(result$success, 0L)
  expect_equal(result$failed, 0L)
})

# ===========================================================================
# scan_png_class_folder with example_data
# ===========================================================================

test_that("scan_png_class_folder parses example_data/png correctly", {

  example_png <- file.path(
    testthat::test_path(), "test_data", "example_png"
  )
  
  skip_if_not(dir.exists(example_png), "example_data/png not available")
  
  result <- scan_png_class_folder(example_png)

  expect_true(nrow(result$annotations) > 0)
  expect_true(length(result$classes_found) > 0)
  expect_true(length(result$sample_names) > 0)

  # Verify _NNN suffixes are stripped
  expect_false(any(grepl("_\\d{3}$", result$classes_found)))

  # Known classes from example data (without _NNN suffix)
  expect_true("Mesodinium_rubrum" %in% result$classes_found)
})

# ===========================================================================
# import_png_folder_with_unclassified tests
# ===========================================================================

test_that("import_png_folder_with_unclassified imports then backfills", {
  png_folder <- create_test_png_folder()
  db_dir <- tempfile("db_")
  roi_dir <- tempfile("roi_")
  dir.create(db_dir)
  db_path <- get_db_path(db_dir)
  on.exit({
    unlink(png_folder, recursive = TRUE)
    unlink(c(db_dir, roi_dir), recursive = TRUE)
  })

  class2use <- c("unclassified", "Diatom", "Ciliate")

  # Sample 1 has ROIs 1,2,3 imported; the .adc says it really has 5 ROIs
  write_mock_adc(roi_dir, "D20230101T120000_IFCB134", n_roi = 5)
  # Sample 2 has ROI 1 imported; the .adc says it really has 3 ROIs
  write_mock_adc(roi_dir, "D20230202T080000_IFCB134", n_roi = 3)

  result <- import_png_folder_with_unclassified(
    png_folder, db_path, class2use, roi_folder = roi_dir,
    annotator = "TestUser"
  )

  expect_equal(result$import$success, 2L)
  expect_equal(result$import$failed, 0L)
  # Sample 1: ROIs 4,5 missing; Sample 2: ROIs 2,3 missing
  expect_equal(result$filled$added, 4L)
  expect_equal(result$filled$samples, 2L)
  expect_equal(result$filled$skipped, 0L)

  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)

  s1 <- dbGetQuery(con,
    "SELECT roi_number, class_name, is_manual FROM annotations
     WHERE sample_name = ? ORDER BY roi_number",
    params = list("D20230101T120000_IFCB134"))
  expect_equal(s1$roi_number, 1:5)
  expect_equal(s1$class_name[4:5], c("unclassified", "unclassified"))
  expect_equal(s1$is_manual[4:5], c(0L, 0L))
})

test_that("import_png_folder_with_unclassified only backfills imported samples", {
  png_folder <- create_test_png_folder()
  db_dir <- tempfile("db_")
  roi_dir <- tempfile("roi_")
  dir.create(db_dir)
  db_path <- get_db_path(db_dir)
  on.exit({
    unlink(png_folder, recursive = TRUE)
    unlink(c(db_dir, roi_dir), recursive = TRUE)
  })

  class2use <- c("unclassified", "Diatom", "Ciliate")

  # A pre-existing sample from an earlier session, present in the DB but NOT
  # in the PNG folder being imported now.
  other_sample <- "D20221231T000000_IFCB134"
  save_annotations_db(db_path, other_sample,
    data.frame(file_name = paste0(other_sample, "_00001.png"),
               class_name = "Diatom", stringsAsFactors = FALSE),
    class2use, "Earlier")
  write_mock_adc(roi_dir, other_sample, n_roi = 9)

  # ADCs for the samples actually in the PNG folder
  write_mock_adc(roi_dir, "D20230101T120000_IFCB134", n_roi = 5)
  write_mock_adc(roi_dir, "D20230202T080000_IFCB134", n_roi = 3)

  import_png_folder_with_unclassified(
    png_folder, db_path, class2use, roi_folder = roi_dir
  )

  # The earlier sample must be untouched (still just its 1 imported ROI)
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)
  n_other <- dbGetQuery(con,
    "SELECT COUNT(*) AS n FROM annotations WHERE sample_name = ?",
    params = list(other_sample))$n
  expect_equal(n_other, 1L)
})

test_that("import_png_folder_with_unclassified skips backfill when fill = FALSE", {
  png_folder <- create_test_png_folder()
  db_dir <- tempfile("db_")
  roi_dir <- tempfile("roi_")
  dir.create(db_dir)
  db_path <- get_db_path(db_dir)
  on.exit({
    unlink(png_folder, recursive = TRUE)
    unlink(c(db_dir, roi_dir), recursive = TRUE)
  })

  class2use <- c("unclassified", "Diatom", "Ciliate")
  write_mock_adc(roi_dir, "D20230101T120000_IFCB134", n_roi = 5)

  result <- import_png_folder_with_unclassified(
    png_folder, db_path, class2use, roi_folder = roi_dir, fill = FALSE
  )

  expect_equal(result$import$success, 2L)
  expect_equal(result$filled$added, 0L)
  expect_equal(result$filled$samples, 0L)
})
