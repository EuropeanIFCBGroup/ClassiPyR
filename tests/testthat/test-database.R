# Tests for SQLite database backend

library(testthat)

test_that("get_db_path returns correct path", {
  expect_equal(
    get_db_path("/data/local_db"),
    file.path("/data/local_db", "annotations.sqlite")
  )
})

test_that("save_annotations_db creates database with correct schema", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  db_path <- get_db_path(db_dir)

  classifications <- data.frame(
    file_name = c("D20230101T120000_IFCB134_00001.png",
                  "D20230101T120000_IFCB134_00002.png"),
    class_name = c("Diatom", "Ciliate"),
    stringsAsFactors = FALSE
  )
  class2use <- c("unclassified", "Diatom", "Ciliate", "Dinoflagellate")

  result <- save_annotations_db(db_path, "D20230101T120000_IFCB134",
                                classifications, class2use, "TestUser")

  expect_true(result)
  expect_true(file.exists(db_path))

  # Verify schema
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))

  tables <- DBI::dbGetQuery(con, "SELECT name FROM sqlite_master WHERE type='table'")
  expect_true("annotations" %in% tables$name)
  expect_true("class_lists" %in% tables$name)

  # Verify annotations data
  annotations <- DBI::dbGetQuery(con, "SELECT * FROM annotations ORDER BY roi_number")
  expect_equal(nrow(annotations), 2)
  expect_equal(annotations$sample_name, rep("D20230101T120000_IFCB134", 2))
  expect_equal(annotations$roi_number, c(1L, 2L))
  expect_equal(annotations$class_name, c("Diatom", "Ciliate"))
  expect_equal(annotations$annotator, rep("TestUser", 2))

  # Verify class list data
  class_list <- DBI::dbGetQuery(con, "SELECT * FROM class_lists ORDER BY class_index")
  expect_equal(nrow(class_list), length(class2use))
  expect_equal(class_list$class_name, class2use)
  expect_equal(class_list$class_index, seq_along(class2use))

  unlink(db_dir, recursive = TRUE)
})

test_that("save_annotations_db returns FALSE for empty classifications", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  db_path <- get_db_path(db_dir)

  result <- save_annotations_db(db_path, "sample",
                                data.frame(file_name = character(),
                                           class_name = character()),
                                c("unclassified"), "TestUser")
  expect_false(result)

  result2 <- save_annotations_db(db_path, "sample", NULL, c("unclassified"), "TestUser")
  expect_false(result2)

  unlink(db_dir, recursive = TRUE)
})

test_that("save_annotations_db upserts (re-saving replaces data)", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  db_path <- get_db_path(db_dir)

  sample_name <- "D20230101T120000_IFCB134"
  class2use <- c("unclassified", "Diatom", "Ciliate")

  # First save
  classifications_v1 <- data.frame(
    file_name = paste0(sample_name, "_00001.png"),
    class_name = "Diatom",
    stringsAsFactors = FALSE
  )
  save_annotations_db(db_path, sample_name, classifications_v1, class2use, "User1")

  # Second save with different data
  classifications_v2 <- data.frame(
    file_name = paste0(sample_name, "_00001.png"),
    class_name = "Ciliate",
    stringsAsFactors = FALSE
  )
  save_annotations_db(db_path, sample_name, classifications_v2, class2use, "User2")

  # Verify only latest version exists
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))

  annotations <- DBI::dbGetQuery(con,
    "SELECT * FROM annotations WHERE sample_name = ?",
    params = list(sample_name))
  expect_equal(nrow(annotations), 1)
  expect_equal(annotations$class_name, "Ciliate")
  expect_equal(annotations$annotator, "User2")

  unlink(db_dir, recursive = TRUE)
})

test_that("load_annotations_db returns correct data frame format", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  db_path <- get_db_path(db_dir)

  sample_name <- "D20230101T120000_IFCB134"
  class2use <- c("unclassified", "Diatom", "Ciliate")

  classifications <- data.frame(
    file_name = c(paste0(sample_name, "_00001.png"),
                  paste0(sample_name, "_00002.png"),
                  paste0(sample_name, "_00003.png")),
    class_name = c("Diatom", "Ciliate", "Diatom"),
    stringsAsFactors = FALSE
  )
  save_annotations_db(db_path, sample_name, classifications, class2use, "TestUser")

  # Load with ROI dimensions
  roi_dims <- data.frame(
    roi_number = 1:3,
    width = c(100, 150, 80),
    height = c(80, 100, 60),
    area = c(8000, 15000, 4800)
  )

  result <- load_annotations_db(db_path, sample_name, roi_dims)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_true(all(c("file_name", "class_name", "score", "width", "height", "roi_area") %in% names(result)))

  # Should be sorted by area descending
  expect_equal(result$roi_area, c(15000, 8000, 4800))
  expect_equal(result$class_name, c("Ciliate", "Diatom", "Diatom"))

  unlink(db_dir, recursive = TRUE)
})

test_that("load_annotations_db returns NULL for missing sample", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  db_path <- get_db_path(db_dir)

  roi_dims <- data.frame(
    roi_number = 1:3, width = rep(100, 3),
    height = rep(80, 3), area = rep(8000, 3)
  )

  # Non-existent database
  result <- load_annotations_db(db_path, "nonexistent_sample", roi_dims)
  expect_null(result)

  # Existing database but missing sample
  save_annotations_db(db_path, "other_sample",
                      data.frame(file_name = "other_sample_00001.png",
                                 class_name = "Diatom",
                                 stringsAsFactors = FALSE),
                      c("Diatom"), "test")

  result2 <- load_annotations_db(db_path, "nonexistent_sample", roi_dims)
  expect_null(result2)

  unlink(db_dir, recursive = TRUE)
})

test_that("list_annotated_samples_db returns correct sample names", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  db_path <- get_db_path(db_dir)

  # Empty / non-existent database
  expect_equal(list_annotated_samples_db(db_path), character())

  # Add two samples
  class2use <- c("unclassified", "Diatom")
  save_annotations_db(db_path, "sample_A",
                      data.frame(file_name = "sample_A_00001.png",
                                 class_name = "Diatom",
                                 stringsAsFactors = FALSE),
                      class2use, "test")
  save_annotations_db(db_path, "sample_B",
                      data.frame(file_name = "sample_B_00001.png",
                                 class_name = "Diatom",
                                 stringsAsFactors = FALSE),
                      class2use, "test")

  samples <- list_annotated_samples_db(db_path)
  expect_equal(sort(samples), c("sample_A", "sample_B"))

  unlink(db_dir, recursive = TRUE)
})

test_that("round-trip: save then load returns identical data", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  db_path <- get_db_path(db_dir)

  sample_name <- "D20230101T120000_IFCB134"
  class2use <- c("unclassified", "Diatom", "Ciliate", "Dinoflagellate")

  classifications <- data.frame(
    file_name = sprintf("%s_%05d.png", sample_name, 1:5),
    class_name = c("Diatom", "Ciliate", "Dinoflagellate", "Diatom", "unclassified"),
    stringsAsFactors = FALSE
  )

  roi_dims <- data.frame(
    roi_number = 1:5,
    width = c(100, 150, 80, 200, 120),
    height = c(80, 100, 60, 150, 90),
    area = c(8000, 15000, 4800, 30000, 10800)
  )

  save_annotations_db(db_path, sample_name, classifications, class2use, "RoundTrip")

  loaded <- load_annotations_db(db_path, sample_name, roi_dims)

  # The loaded result is sorted by area descending
  expected <- classifications
  expected$score <- NA_real_
  expected$width <- roi_dims$width
  expected$height <- roi_dims$height
  expected$roi_area <- roi_dims$area
  expected <- expected[order(-expected$roi_area), ]
  rownames(expected) <- NULL
  rownames(loaded) <- NULL

  expect_equal(loaded$file_name, expected$file_name)
  expect_equal(loaded$class_name, expected$class_name)
  expect_equal(loaded$width, expected$width)
  expect_equal(loaded$height, expected$height)
  expect_equal(loaded$roi_area, expected$roi_area)

  unlink(db_dir, recursive = TRUE)
})

test_that("load_from_db delegates to load_annotations_db", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  db_path <- get_db_path(db_dir)

  sample_name <- "D20230101T120000_IFCB134"
  class2use <- c("unclassified", "Diatom")

  classifications <- data.frame(
    file_name = paste0(sample_name, "_00001.png"),
    class_name = "Diatom",
    stringsAsFactors = FALSE
  )

  roi_dims <- data.frame(
    roi_number = 1L, width = 100, height = 80, area = 8000
  )

  save_annotations_db(db_path, sample_name, classifications, class2use, "test")

  result <- load_from_db(db_path, sample_name, roi_dims)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$class_name, "Diatom")

  unlink(db_dir, recursive = TRUE)
})

test_that("update_annotator changes annotator for a single sample", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  db_path <- get_db_path(db_dir)

  sample_name <- "D20230101T120000_IFCB134"
  class2use <- c("unclassified", "Diatom")

  classifications <- data.frame(
    file_name = sprintf("%s_%05d.png", sample_name, 1:3),
    class_name = c("Diatom", "Diatom", "unclassified"),
    stringsAsFactors = FALSE
  )
  save_annotations_db(db_path, sample_name, classifications, class2use, "OldUser")

  counts <- update_annotator(db_path, sample_name, "NewUser")
  expect_equal(counts, c("D20230101T120000_IFCB134" = 3L))

  # Verify in DB
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  rows <- DBI::dbGetQuery(con,
    "SELECT DISTINCT annotator FROM annotations WHERE sample_name = ?",
    params = list(sample_name))
  expect_equal(rows$annotator, "NewUser")

  unlink(db_dir, recursive = TRUE)
})

test_that("update_annotator changes multiple samples", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  db_path <- get_db_path(db_dir)

  class2use <- c("unclassified", "Diatom")

  save_annotations_db(db_path, "sample_A",
                      data.frame(file_name = "sample_A_00001.png",
                                 class_name = "Diatom",
                                 stringsAsFactors = FALSE),
                      class2use, "User1")
  save_annotations_db(db_path, "sample_B",
                      data.frame(file_name = c("sample_B_00001.png", "sample_B_00002.png"),
                                 class_name = c("Diatom", "Diatom"),
                                 stringsAsFactors = FALSE),
                      class2use, "User2")

  counts <- update_annotator(db_path, c("sample_A", "sample_B"), "SharedUser")
  expect_equal(counts, c(sample_A = 1L, sample_B = 2L))

  # Verify both updated
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  rows <- DBI::dbGetQuery(con, "SELECT DISTINCT annotator FROM annotations")
  expect_equal(rows$annotator, "SharedUser")

  unlink(db_dir, recursive = TRUE)
})

test_that("update_annotator returns 0 for non-existent sample", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  db_path <- get_db_path(db_dir)

  save_annotations_db(db_path, "existing",
                      data.frame(file_name = "existing_00001.png",
                                 class_name = "Diatom",
                                 stringsAsFactors = FALSE),
                      c("Diatom"), "test")

  counts <- update_annotator(db_path, "nonexistent", "NewUser")
  expect_equal(counts, c(nonexistent = 0L))

  # Mix of existing and non-existing
  counts2 <- update_annotator(db_path, c("existing", "nonexistent"), "NewUser")
  expect_equal(counts2, c(existing = 1L, nonexistent = 0L))

  unlink(db_dir, recursive = TRUE)
})

test_that("update_annotator validates inputs", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  db_path <- get_db_path(db_dir)

  save_annotations_db(db_path, "sample",
                      data.frame(file_name = "sample_00001.png",
                                 class_name = "Diatom",
                                 stringsAsFactors = FALSE),
                      c("Diatom"), "test")

  # Missing database
  expect_error(update_annotator("/nonexistent/db.sqlite", "sample", "X"),
               "Database not found")

  # Invalid annotator
  expect_error(update_annotator(db_path, "sample", NA_character_),
               "annotator must be")
  expect_error(update_annotator(db_path, "sample", c("A", "B")),
               "annotator must be")

  # Empty sample_names returns empty vector
  counts <- update_annotator(db_path, character(0), "X")
  expect_length(counts, 0)

  unlink(db_dir, recursive = TRUE)
})

test_that("import_mat_to_db migrates data correctly", {
  skip_if_not_installed("iRfcb")
  skip_if_not(reticulate::py_available(), "Python not available")
  skip_if_not(reticulate::py_module_available("scipy"), "scipy not available")

  sample_name <- "D20220522T000439_IFCB134"

  # Check if there's a test annotation mat file
  output_test <- testthat::test_path("test_data", "manual")
  test_mat <- file.path(output_test, paste0(sample_name, ".mat"))
  skip_if_not(file.exists(test_mat), "No test MAT annotation file for migration test")

  db_dir <- tempfile("db_")
  dir.create(db_dir)
  db_path <- get_db_path(db_dir)

  result <- import_mat_to_db(test_mat, db_path, sample_name, "migrated")
  expect_true(result)

  # Verify data was imported
  samples <- list_annotated_samples_db(db_path)
  expect_true(sample_name %in% samples)

  unlink(db_dir, recursive = TRUE)
})

test_that("import_mat_to_db returns FALSE for missing file", {
  result <- import_mat_to_db(
    "/nonexistent/file.mat",
    tempfile(fileext = ".sqlite"),
    "sample", "test"
  )
  expect_false(result)
})

test_that("export_db_to_mat creates valid .mat file", {
  skip_if_not_installed("iRfcb")
  skip_if_not(reticulate::py_available(), "Python not available")
  skip_if_not(reticulate::py_module_available("scipy"), "scipy not available")

  db_dir <- tempfile("db_")
  dir.create(db_dir)
  db_path <- get_db_path(db_dir)

  sample_name <- "D20230101T120000_IFCB134"
  class2use <- c("unclassified", "Diatom", "Ciliate", "Dinoflagellate")

  classifications <- data.frame(
    file_name = sprintf("%s_%05d.png", sample_name, 1:4),
    class_name = c("Diatom", "Ciliate", "Diatom", "Dinoflagellate"),
    stringsAsFactors = FALSE
  )
  save_annotations_db(db_path, sample_name, classifications, class2use, "TestUser")

  mat_dir <- tempfile("mat_")
  dir.create(mat_dir)

  result <- export_db_to_mat(db_path, sample_name, mat_dir)
  expect_true(result)

  mat_path <- file.path(mat_dir, paste0(sample_name, ".mat"))
  expect_true(file.exists(mat_path))

  # Verify contents via ifcb_get_mat_variable
  classlist <- iRfcb::ifcb_get_mat_variable(mat_path, variable_name = "classlist")
  expect_equal(nrow(classlist), 4)
  # class indices: Diatom=2, Ciliate=3, Diatom=2, Dinoflagellate=4
  expect_equal(classlist[, 2], c(2, 3, 2, 4))

  unlink(c(db_dir, mat_dir), recursive = TRUE)
})

test_that("export_db_to_mat returns FALSE for missing sample", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  db_path <- get_db_path(db_dir)

  # Create DB with one sample
  save_annotations_db(db_path, "existing_sample",
                      data.frame(file_name = "existing_sample_00001.png",
                                 class_name = "Diatom",
                                 stringsAsFactors = FALSE),
                      c("unclassified", "Diatom"), "test")

  result <- export_db_to_mat(db_path, "nonexistent_sample", db_dir)
  expect_false(result)

  unlink(db_dir, recursive = TRUE)
})

test_that("export_db_to_mat returns FALSE for non-existent database", {
  result <- export_db_to_mat("/nonexistent/db.sqlite", "sample", tempdir())
  expect_false(result)
})

test_that("import_all_mat_to_db imports multiple files and returns correct counts", {
  skip_if_not_installed("iRfcb")
  skip_if_not(reticulate::py_available(), "Python not available")
  skip_if_not(reticulate::py_module_available("scipy"), "scipy not available")

  mat_dir <- tempfile("mat_")
  dir.create(mat_dir)
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  db_path <- get_db_path(db_dir)

  class2use <- c("unclassified", "Diatom", "Ciliate")

  # Create two .mat files using ifcb_create_manual_file
  iRfcb::ifcb_create_manual_file(
    roi_length = 3, class2use = class2use,
    output_file = file.path(mat_dir, "sample_A.mat"),
    classlist = c(2, 3, 2)
  )
  iRfcb::ifcb_create_manual_file(
    roi_length = 2, class2use = class2use,
    output_file = file.path(mat_dir, "sample_B.mat"),
    classlist = c(1, 3)
  )
  # Create a classifier file that should be excluded
  iRfcb::ifcb_create_manual_file(
    roi_length = 2, class2use = class2use,
    output_file = file.path(mat_dir, "sample_C_class_v1.mat"),
    classlist = c(1, 2)
  )

  result <- import_all_mat_to_db(mat_dir, db_path, "test")

  expect_equal(result$success, 2L)
  expect_equal(result$failed, 0L)
  expect_equal(result$skipped, 0L)

  # Verify both samples in DB
  samples <- list_annotated_samples_db(db_path)
  expect_true("sample_A" %in% samples)
  expect_true("sample_B" %in% samples)
  expect_false("sample_C_class_v1" %in% samples)

  # Re-import should skip existing
  result2 <- import_all_mat_to_db(mat_dir, db_path, "test")
  expect_equal(result2$success, 0L)
  expect_equal(result2$skipped, 2L)

  unlink(c(mat_dir, db_dir), recursive = TRUE)
})

test_that("export_all_db_to_mat exports multiple samples", {
  skip_if_not_installed("iRfcb")
  skip_if_not(reticulate::py_available(), "Python not available")
  skip_if_not(reticulate::py_module_available("scipy"), "scipy not available")

  db_dir <- tempfile("db_")
  dir.create(db_dir)
  db_path <- get_db_path(db_dir)

  class2use <- c("unclassified", "Diatom", "Ciliate")

  save_annotations_db(db_path, "sample_X",
                      data.frame(file_name = "sample_X_00001.png",
                                 class_name = "Diatom",
                                 stringsAsFactors = FALSE),
                      class2use, "test")
  save_annotations_db(db_path, "sample_Y",
                      data.frame(file_name = "sample_Y_00001.png",
                                 class_name = "Ciliate",
                                 stringsAsFactors = FALSE),
                      class2use, "test")

  mat_dir <- tempfile("mat_")
  dir.create(mat_dir)

  result <- export_all_db_to_mat(db_path, mat_dir)

  expect_equal(result$success, 2L)
  expect_equal(result$failed, 0L)
  expect_true(file.exists(file.path(mat_dir, "sample_X.mat")))
  expect_true(file.exists(file.path(mat_dir, "sample_Y.mat")))

  unlink(c(db_dir, mat_dir), recursive = TRUE)
})

test_that("round-trip: DB -> .mat -> DB produces matching data", {
  skip_if_not_installed("iRfcb")
  skip_if_not(reticulate::py_available(), "Python not available")
  skip_if_not(reticulate::py_module_available("scipy"), "scipy not available")

  db_dir <- tempfile("db_")
  dir.create(db_dir)
  db_path <- get_db_path(db_dir)

  sample_name <- "D20230101T120000_IFCB134"
  class2use <- c("unclassified", "Diatom", "Ciliate", "Dinoflagellate")

  original <- data.frame(
    file_name = sprintf("%s_%05d.png", sample_name, 1:5),
    class_name = c("Diatom", "Ciliate", "Dinoflagellate", "Diatom", "unclassified"),
    stringsAsFactors = FALSE
  )
  save_annotations_db(db_path, sample_name, original, class2use, "Original")

  # Export to .mat
  mat_dir <- tempfile("mat_")
  dir.create(mat_dir)
  export_db_to_mat(db_path, sample_name, mat_dir)

  # Import back to a fresh DB
  db_dir2 <- tempfile("db2_")
  dir.create(db_dir2)
  db_path2 <- get_db_path(db_dir2)

  mat_path <- file.path(mat_dir, paste0(sample_name, ".mat"))
  import_mat_to_db(mat_path, db_path2, sample_name, "reimported")

  # Compare: read both DBs and check class names match
  con1 <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con1), add = TRUE)
  rows1 <- DBI::dbGetQuery(con1,
    "SELECT roi_number, class_name FROM annotations WHERE sample_name = ? ORDER BY roi_number",
    params = list(sample_name))

  con2 <- DBI::dbConnect(RSQLite::SQLite(), db_path2)
  on.exit(DBI::dbDisconnect(con2), add = TRUE)
  rows2 <- DBI::dbGetQuery(con2,
    "SELECT roi_number, class_name FROM annotations WHERE sample_name = ? ORDER BY roi_number",
    params = list(sample_name))

  expect_equal(nrow(rows1), nrow(rows2))
  expect_equal(rows1$class_name, rows2$class_name)

  unlink(c(db_dir, db_dir2, mat_dir), recursive = TRUE)
})

test_that("export_db_to_png extracts images into class subfolders", {
  roi_path <- testthat::test_path("test_data", "raw", "2022", "D20220522",
                                   "D20220522T000439_IFCB134.roi")
  skip_if_not(file.exists(roi_path), "Test ROI file not found")

  db_dir <- tempfile("db_")
  dir.create(db_dir)
  db_path <- get_db_path(db_dir)

  sample_name <- "D20220522T000439_IFCB134"
  class2use <- c("unclassified", "Diatom", "Ciliate")

  # Save annotations for ROIs 2-5 (ROI 1 is empty in test data)
  classifications <- data.frame(
    file_name = sprintf("%s_%05d.png", sample_name, 2:5),
    class_name = c("Diatom", "Ciliate", "Diatom", "Ciliate"),
    stringsAsFactors = FALSE
  )
  save_annotations_db(db_path, sample_name, classifications, class2use, "test")

  png_dir <- tempfile("png_")
  dir.create(png_dir)

  result <- export_db_to_png(db_path, sample_name, roi_path, png_dir)
  expect_true(result)

  # Check class subfolders exist
  expect_true(dir.exists(file.path(png_dir, "Diatom")))
  expect_true(dir.exists(file.path(png_dir, "Ciliate")))

  # Check that PNG files were created in the right subfolders
  diatom_files <- list.files(file.path(png_dir, "Diatom"), pattern = "\\.png$")
  ciliate_files <- list.files(file.path(png_dir, "Ciliate"), pattern = "\\.png$")
  expect_equal(length(diatom_files), 2)
  expect_equal(length(ciliate_files), 2)

  unlink(c(db_dir, png_dir), recursive = TRUE)
})

test_that("export_db_to_png skip_class excludes specified classes", {
  roi_path <- testthat::test_path("test_data", "raw", "2022", "D20220522",
                                   "D20220522T000439_IFCB134.roi")
  skip_if_not(file.exists(roi_path), "Test ROI file not found")

  db_dir <- tempfile("db_")
  dir.create(db_dir)
  db_path <- get_db_path(db_dir)

  sample_name <- "D20220522T000439_IFCB134"
  class2use <- c("unclassified", "Diatom", "Ciliate")

  # ROIs 2-5: two Diatom, one Ciliate, one unclassified
  classifications <- data.frame(
    file_name = sprintf("%s_%05d.png", sample_name, 2:5),
    class_name = c("Diatom", "Ciliate", "Diatom", "unclassified"),
    stringsAsFactors = FALSE
  )
  save_annotations_db(db_path, sample_name, classifications, class2use, "test")

  png_dir <- tempfile("png_")
  dir.create(png_dir)

  result <- export_db_to_png(db_path, sample_name, roi_path, png_dir,
                             skip_class = "unclassified")
  expect_true(result)

  # unclassified subfolder should NOT exist
  expect_false(dir.exists(file.path(png_dir, "unclassified")))
  # Diatom and Ciliate should exist
  expect_true(dir.exists(file.path(png_dir, "Diatom")))
  expect_true(dir.exists(file.path(png_dir, "Ciliate")))

  diatom_files <- list.files(file.path(png_dir, "Diatom"), pattern = "\\.png$")
  ciliate_files <- list.files(file.path(png_dir, "Ciliate"), pattern = "\\.png$")
  expect_equal(length(diatom_files), 2)
  expect_equal(length(ciliate_files), 1)

  unlink(c(db_dir, png_dir), recursive = TRUE)
})

test_that("export_db_to_png skip_class with all ROIs skipped returns TRUE", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  db_path <- get_db_path(db_dir)

  sample_name <- "D20220522T000439_IFCB134"
  class2use <- c("unclassified", "Diatom")

  classifications <- data.frame(
    file_name = sprintf("%s_%05d.png", sample_name, 2:3),
    class_name = c("unclassified", "unclassified"),
    stringsAsFactors = FALSE
  )
  save_annotations_db(db_path, sample_name, classifications, class2use, "test")

  roi_path <- testthat::test_path("test_data", "raw", "2022", "D20220522",
                                   "D20220522T000439_IFCB134.roi")
  skip_if_not(file.exists(roi_path), "Test ROI file not found")

  png_dir <- tempfile("png_")
  dir.create(png_dir)

  result <- export_db_to_png(db_path, sample_name, roi_path, png_dir,
                             skip_class = "unclassified")
  expect_true(result)

  # No class subfolders should be created
  expect_equal(length(list.dirs(png_dir, recursive = FALSE)), 0)

  unlink(c(db_dir, png_dir), recursive = TRUE)
})

test_that("export_db_to_png returns FALSE for missing sample", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  db_path <- get_db_path(db_dir)

  # Create DB with one sample
  save_annotations_db(db_path, "existing_sample",
                      data.frame(file_name = "existing_sample_00001.png",
                                 class_name = "Diatom",
                                 stringsAsFactors = FALSE),
                      c("unclassified", "Diatom"), "test")

  roi_path <- testthat::test_path("test_data", "raw", "2022", "D20220522",
                                   "D20220522T000439_IFCB134.roi")
  skip_if_not(file.exists(roi_path), "Test ROI file not found")

  result <- export_db_to_png(db_path, "nonexistent_sample", roi_path, tempdir())
  expect_false(result)

  unlink(db_dir, recursive = TRUE)
})

test_that("export_db_to_png returns FALSE for missing ROI file", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  db_path <- get_db_path(db_dir)

  save_annotations_db(db_path, "sample_A",
                      data.frame(file_name = "sample_A_00001.png",
                                 class_name = "Diatom",
                                 stringsAsFactors = FALSE),
                      c("unclassified", "Diatom"), "test")

  result <- export_db_to_png(db_path, "sample_A", "/nonexistent/file.roi", tempdir())
  expect_false(result)

  unlink(db_dir, recursive = TRUE)
})

test_that("export_all_db_to_png exports multiple samples and skips missing ROIs", {
  roi_path <- testthat::test_path("test_data", "raw", "2022", "D20220522",
                                   "D20220522T000439_IFCB134.roi")
  skip_if_not(file.exists(roi_path), "Test ROI file not found")

  db_dir <- tempfile("db_")
  dir.create(db_dir)
  db_path <- get_db_path(db_dir)

  class2use <- c("unclassified", "Diatom")

  # sample_A has a valid ROI path (use ROI 2 since ROI 1 is empty in test data)
  save_annotations_db(db_path, "D20220522T000439_IFCB134",
                      data.frame(file_name = "D20220522T000439_IFCB134_00002.png",
                                 class_name = "Diatom",
                                 stringsAsFactors = FALSE),
                      class2use, "test")

  # sample_B has no ROI path (will be skipped)
  save_annotations_db(db_path, "sample_no_roi",
                      data.frame(file_name = "sample_no_roi_00001.png",
                                 class_name = "Diatom",
                                 stringsAsFactors = FALSE),
                      class2use, "test")

  png_dir <- tempfile("png_")
  dir.create(png_dir)

  roi_map <- list("D20220522T000439_IFCB134" = roi_path)

  result <- export_all_db_to_png(db_path, png_dir, roi_map)

  expect_equal(result$success, 1L)
  expect_equal(result$failed, 0L)
  expect_equal(result$skipped, 1L)
  expect_true(dir.exists(file.path(png_dir, "Diatom")))

  unlink(c(db_dir, png_dir), recursive = TRUE)
})

test_that("save_annotations_db stores is_manual flags", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  db_path <- get_db_path(db_dir)

  sample_name <- "D20230101T120000_IFCB134"
  class2use <- c("unclassified", "Diatom", "Ciliate")

  classifications <- data.frame(
    file_name = sprintf("%s_%05d.png", sample_name, 1:3),
    class_name = c("Diatom", "unclassified", "Ciliate"),
    stringsAsFactors = FALSE
  )

  result <- save_annotations_db(db_path, sample_name, classifications,
                                class2use, "TestUser",
                                is_manual = c(1L, 0L, 1L))
  expect_true(result)

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))

  rows <- DBI::dbGetQuery(con,
    "SELECT roi_number, class_name, is_manual FROM annotations WHERE sample_name = ? ORDER BY roi_number",
    params = list(sample_name))

  expect_equal(rows$is_manual, c(1L, 0L, 1L))
  expect_equal(rows$class_name, c("Diatom", "unclassified", "Ciliate"))

  unlink(db_dir, recursive = TRUE)
})

test_that("save_annotations_db defaults is_manual to 1", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  db_path <- get_db_path(db_dir)

  sample_name <- "D20230101T120000_IFCB134"

  classifications <- data.frame(
    file_name = sprintf("%s_%05d.png", sample_name, 1:2),
    class_name = c("Diatom", "Ciliate"),
    stringsAsFactors = FALSE
  )

  save_annotations_db(db_path, sample_name, classifications,
                      c("unclassified", "Diatom", "Ciliate"), "TestUser")

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))

  rows <- DBI::dbGetQuery(con,
    "SELECT is_manual FROM annotations WHERE sample_name = ?",
    params = list(sample_name))

  expect_true(all(rows$is_manual == 1L))

  unlink(db_dir, recursive = TRUE)
})

test_that("schema migration adds is_manual to existing DB", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  db_path <- get_db_path(db_dir)

  # Create a database with the OLD schema (no is_manual column)
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbExecute(con, "
    CREATE TABLE annotations (
      sample_name TEXT NOT NULL,
      roi_number  INTEGER NOT NULL,
      class_name  TEXT NOT NULL,
      annotator   TEXT,
      timestamp   TEXT DEFAULT (datetime('now')),
      PRIMARY KEY (sample_name, roi_number)
    )
  ")
  DBI::dbExecute(con, "
    CREATE TABLE class_lists (
      sample_name TEXT NOT NULL,
      class_index INTEGER NOT NULL,
      class_name  TEXT NOT NULL,
      PRIMARY KEY (sample_name, class_index)
    )
  ")

  # Insert a row without is_manual
  DBI::dbExecute(con,
    "INSERT INTO annotations (sample_name, roi_number, class_name, annotator) VALUES (?, ?, ?, ?)",
    params = list("sample_old", 1L, "Diatom", "test"))
  DBI::dbDisconnect(con)

  # Now run init_db_schema which should migrate
  con2 <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con2))
  init_db_schema(con2)

  cols <- DBI::dbGetQuery(con2, "PRAGMA table_info(annotations)")
  expect_true("is_manual" %in% cols$name)

  # Existing row should have default value 1
  row <- DBI::dbGetQuery(con2,
    "SELECT is_manual FROM annotations WHERE sample_name = 'sample_old'")
  expect_equal(row$is_manual, 1L)

  unlink(db_dir, recursive = TRUE)
})

test_that("import_mat_to_db reads class2use_manual from .mat", {
  skip_if_not_installed("iRfcb")
  skip_if_not(reticulate::py_available(), "Python not available")
  skip_if_not(reticulate::py_module_available("scipy"), "scipy not available")

  # Create a .mat file with a known class list
  mat_dir <- tempfile("mat_")
  dir.create(mat_dir)
  mat_path <- file.path(mat_dir, "test_sample.mat")
  class2use <- c("unclassified", "Diatom", "Ciliate")

  iRfcb::ifcb_create_manual_file(
    roi_length = 3, class2use = class2use,
    output_file = mat_path,
    classlist = c(2, 3, 1)
  )

  db_dir <- tempfile("db_")
  dir.create(db_dir)
  db_path <- get_db_path(db_dir)

  result <- import_mat_to_db(mat_path, db_path, "test_sample")
  expect_true(result)

  # Verify the class list stored in DB matches the .mat file's embedded list
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))

  cl <- DBI::dbGetQuery(con,
    "SELECT class_name FROM class_lists WHERE sample_name = 'test_sample' ORDER BY class_index")
  expect_equal(cl$class_name, class2use)

  # Verify class names mapped correctly
  ann <- DBI::dbGetQuery(con,
    "SELECT roi_number, class_name FROM annotations WHERE sample_name = 'test_sample' ORDER BY roi_number")
  expect_equal(ann$class_name, c("Diatom", "Ciliate", "unclassified"))

  unlink(c(mat_dir, db_dir), recursive = TRUE)
})

test_that("import_mat_to_db preserves NaN as is_manual=0", {
  skip_if_not_installed("iRfcb")
  skip_if_not(reticulate::py_available(), "Python not available")
  skip_if_not(reticulate::py_module_available("scipy"), "scipy not available")

  mat_dir <- tempfile("mat_")
  dir.create(mat_dir)
  mat_path <- file.path(mat_dir, "test_nan.mat")
  class2use <- c("unclassified", "Diatom", "Ciliate")

  # Create .mat with NaN entries (unreviewed ROIs)
  iRfcb::ifcb_create_manual_file(
    roi_length = 4, class2use = class2use,
    output_file = mat_path,
    classlist = c(2, NaN, 3, NaN)
  )

  db_dir <- tempfile("db_")
  dir.create(db_dir)
  db_path <- get_db_path(db_dir)

  result <- import_mat_to_db(mat_path, db_path, "test_nan")
  expect_true(result)

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))

  rows <- DBI::dbGetQuery(con,
    "SELECT roi_number, class_name, is_manual FROM annotations WHERE sample_name = 'test_nan' ORDER BY roi_number")

  expect_equal(rows$is_manual, c(1L, 0L, 1L, 0L))
  expect_equal(rows$class_name, c("Diatom", "unclassified", "Ciliate", "unclassified"))

  unlink(c(mat_dir, db_dir), recursive = TRUE)
})

test_that("export_db_to_mat restores NaN for is_manual=0 rows", {
  skip_if_not_installed("iRfcb")
  skip_if_not(reticulate::py_available(), "Python not available")
  skip_if_not(reticulate::py_module_available("scipy"), "scipy not available")

  db_dir <- tempfile("db_")
  dir.create(db_dir)
  db_path <- get_db_path(db_dir)

  sample_name <- "D20230101T120000_IFCB134"
  class2use <- c("unclassified", "Diatom", "Ciliate")

  classifications <- data.frame(
    file_name = sprintf("%s_%05d.png", sample_name, 1:4),
    class_name = c("Diatom", "unclassified", "Ciliate", "unclassified"),
    stringsAsFactors = FALSE
  )
  # ROIs 2 and 4 are unreviewed (NaN in .mat)
  save_annotations_db(db_path, sample_name, classifications, class2use,
                      "TestUser", is_manual = c(1L, 0L, 1L, 0L))

  mat_dir <- tempfile("mat_")
  dir.create(mat_dir)

  result <- export_db_to_mat(db_path, sample_name, mat_dir)
  expect_true(result)

  mat_path <- file.path(mat_dir, paste0(sample_name, ".mat"))
  classlist <- iRfcb::ifcb_get_mat_variable(mat_path, variable_name = "classlist")

  # Reviewed ROIs should have valid indices, unreviewed should be NaN
  expect_equal(classlist[1, 2], 2)   # Diatom
  expect_true(is.nan(classlist[2, 2]))  # unreviewed -> NaN
  expect_equal(classlist[3, 2], 3)   # Ciliate
  expect_true(is.nan(classlist[4, 2]))  # unreviewed -> NaN

  unlink(c(db_dir, mat_dir), recursive = TRUE)
})

test_that("full roundtrip: .mat -> SQLite -> .mat preserves NaN and class list", {
  skip_if_not_installed("iRfcb")
  skip_if_not(reticulate::py_available(), "Python not available")
  skip_if_not(reticulate::py_module_available("scipy"), "scipy not available")

  # Create original .mat with NaN entries
  mat_dir <- tempfile("mat_orig_")
  dir.create(mat_dir)
  original_mat <- file.path(mat_dir, "roundtrip_sample.mat")
  class2use <- c("unclassified", "Diatom", "Ciliate", "Dinoflagellate")

  original_classlist <- c(2, NaN, 3, 4, NaN)
  iRfcb::ifcb_create_manual_file(
    roi_length = 5, class2use = class2use,
    output_file = original_mat,
    classlist = original_classlist
  )

  # Import into SQLite
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  db_path <- get_db_path(db_dir)

  import_mat_to_db(original_mat, db_path, "roundtrip_sample")

  # Export back to .mat
  export_dir <- tempfile("mat_export_")
  dir.create(export_dir)
  export_db_to_mat(db_path, "roundtrip_sample", export_dir)

  # Read back and compare
  exported_mat <- file.path(export_dir, "roundtrip_sample.mat")
  exported_classlist <- iRfcb::ifcb_get_mat_variable(exported_mat,
                                                      variable_name = "classlist")
  exported_class2use <- as.character(
    iRfcb::ifcb_get_mat_variable(exported_mat,
                                  variable_name = "class2use_manual"))

  # Class list should match exactly
  expect_equal(exported_class2use, class2use)

  # Classlist indices should match: classified ROIs keep their index, NaN stays NaN
  for (i in seq_along(original_classlist)) {
    if (is.nan(original_classlist[i])) {
      expect_true(is.nan(exported_classlist[i, 2]),
                  info = paste("ROI", i, "should be NaN"))
    } else {
      expect_equal(exported_classlist[i, 2], original_classlist[i],
                   info = paste("ROI", i, "index mismatch"))
    }
  }

  unlink(c(mat_dir, db_dir, export_dir), recursive = TRUE)
})
