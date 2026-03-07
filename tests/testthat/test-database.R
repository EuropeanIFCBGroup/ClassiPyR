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
  on.exit(unlink(db_dir, recursive = TRUE), add = TRUE)
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
  on.exit(DBI::dbDisconnect(con), add = TRUE)

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
})

test_that("save_annotations_db returns FALSE for empty classifications", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  on.exit(unlink(db_dir, recursive = TRUE), add = TRUE)
  db_path <- get_db_path(db_dir)

  result <- save_annotations_db(db_path, "sample",
                                data.frame(file_name = character(),
                                           class_name = character()),
                                c("unclassified"), "TestUser")
  expect_false(result)

  result2 <- save_annotations_db(db_path, "sample", NULL, c("unclassified"), "TestUser")
  expect_false(result2)
})

test_that("save_annotations_db upserts (re-saving replaces data)", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  on.exit(unlink(db_dir, recursive = TRUE), add = TRUE)
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
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  annotations <- DBI::dbGetQuery(con,
    "SELECT * FROM annotations WHERE sample_name = ?",
    params = list(sample_name))
  expect_equal(nrow(annotations), 1)
  expect_equal(annotations$class_name, "Ciliate")
  expect_equal(annotations$annotator, "User2")
})

test_that("delete_annotations_db removes rows from both tables", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  on.exit(unlink(db_dir, recursive = TRUE), add = TRUE)
  db_path <- get_db_path(db_dir)

  sample_name <- "D20230101T120000_IFCB134"
  classifications <- data.frame(
    file_name = c("D20230101T120000_IFCB134_00001.png",
                  "D20230101T120000_IFCB134_00002.png"),
    class_name = c("Diatom", "Ciliate"),
    stringsAsFactors = FALSE
  )
  class2use <- c("unclassified", "Diatom", "Ciliate")

  save_annotations_db(db_path, sample_name, classifications, class2use, "TestUser")

  result <- delete_annotations_db(db_path, sample_name)
  expect_true(result)

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  annotations <- DBI::dbGetQuery(con,
    "SELECT * FROM annotations WHERE sample_name = ?",
    params = list(sample_name))
  expect_equal(nrow(annotations), 0)

  class_list <- DBI::dbGetQuery(con,
    "SELECT * FROM class_lists WHERE sample_name = ?",
    params = list(sample_name))
  expect_equal(nrow(class_list), 0)
})

test_that("delete_annotations_db does not affect other samples", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  on.exit(unlink(db_dir, recursive = TRUE), add = TRUE)
  db_path <- get_db_path(db_dir)

  class2use <- c("unclassified", "Diatom", "Ciliate")

  save_annotations_db(db_path, "sample_A",
    data.frame(file_name = "sample_A_00001.png", class_name = "Diatom",
               stringsAsFactors = FALSE),
    class2use, "TestUser")

  save_annotations_db(db_path, "sample_B",
    data.frame(file_name = "sample_B_00001.png", class_name = "Ciliate",
               stringsAsFactors = FALSE),
    class2use, "TestUser")

  delete_annotations_db(db_path, "sample_A")

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  remaining <- DBI::dbGetQuery(con, "SELECT DISTINCT sample_name FROM annotations")
  expect_equal(remaining$sample_name, "sample_B")

  remaining_cl <- DBI::dbGetQuery(con, "SELECT DISTINCT sample_name FROM class_lists")
  expect_equal(remaining_cl$sample_name, "sample_B")
})

test_that("delete_annotations_db returns FALSE for non-existent database", {
  expect_warning(
    result <- delete_annotations_db("/nonexistent/path/annotations.sqlite", "sample"),
    "does not exist"
  )
  expect_false(result)
})

test_that("delete_annotations_db returns TRUE for sample not in database", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  on.exit(unlink(db_dir, recursive = TRUE), add = TRUE)
  db_path <- get_db_path(db_dir)

  # Save one sample so the DB and tables exist
  save_annotations_db(db_path, "existing_sample",
    data.frame(file_name = "existing_sample_00001.png", class_name = "Diatom",
               stringsAsFactors = FALSE),
    c("unclassified", "Diatom"), "TestUser")

  # Deleting a non-existent sample succeeds (no-op)
  result <- delete_annotations_db(db_path, "nonexistent_sample")
  expect_true(result)
})

test_that("load_annotations_db returns correct data frame format", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  on.exit(unlink(db_dir, recursive = TRUE), add = TRUE)
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
})

test_that("load_annotations_db returns NULL for missing sample", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  on.exit(unlink(db_dir, recursive = TRUE), add = TRUE)
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
})

test_that("list_annotated_samples_db returns correct sample names", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  on.exit(unlink(db_dir, recursive = TRUE), add = TRUE)
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
})

test_that("round-trip: save then load returns identical data", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  on.exit(unlink(db_dir, recursive = TRUE), add = TRUE)
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
})

test_that("load_from_db delegates to load_annotations_db", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  on.exit(unlink(db_dir, recursive = TRUE), add = TRUE)
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
})

test_that("update_annotator changes annotator for a single sample", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  on.exit(unlink(db_dir, recursive = TRUE), add = TRUE)
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
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  rows <- DBI::dbGetQuery(con,
    "SELECT DISTINCT annotator FROM annotations WHERE sample_name = ?",
    params = list(sample_name))
  expect_equal(rows$annotator, "NewUser")
})

test_that("update_annotator changes multiple samples", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  on.exit(unlink(db_dir, recursive = TRUE), add = TRUE)
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
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  rows <- DBI::dbGetQuery(con, "SELECT DISTINCT annotator FROM annotations")
  expect_equal(rows$annotator, "SharedUser")
})

test_that("update_annotator returns 0 for non-existent sample", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  on.exit(unlink(db_dir, recursive = TRUE), add = TRUE)
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
})

test_that("update_annotator validates inputs", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  on.exit(unlink(db_dir, recursive = TRUE), add = TRUE)
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
})

test_that("import_mat_to_db migrates data correctly", {
  skip_if_not_installed("iRfcb")
  skip_if_not(reticulate::py_available(), "Python not available")
  skip_if_not(reticulate::py_module_available("scipy"), "scipy not available")

  sample_name <- "D20230101T120000_IFCB134"
  class2use <- c("unclassified", "Diatom", "Ciliate", "Dinoflagellate")

  # Create a MAT file via export_db_to_mat so the test is self-contained
  db_dir <- tempfile("db_export_")
  dir.create(db_dir)
  db_path <- get_db_path(db_dir)

  classifications <- data.frame(
    file_name = sprintf("%s_%05d.png", sample_name, 1:4),
    class_name = c("Diatom", "Ciliate", "Diatom", "Dinoflagellate"),
    stringsAsFactors = FALSE
  )
  save_annotations_db(db_path, sample_name, classifications, class2use, "exporter")

  mat_dir <- tempfile("mat_")
  dir.create(mat_dir)
  result <- export_db_to_mat(db_path, sample_name, mat_dir)
  expect_true(result)

  test_mat <- file.path(mat_dir, paste0(sample_name, ".mat"))
  expect_true(file.exists(test_mat))

  # Now import the MAT file into a fresh database
  db_dir2 <- tempfile("db_import_")
  dir.create(db_dir2)
  on.exit(unlink(c(db_dir, db_dir2, mat_dir), recursive = TRUE), add = TRUE)
  db_path2 <- get_db_path(db_dir2)

  result <- import_mat_to_db(test_mat, db_path2, sample_name, "migrated")
  expect_true(result)

  # Verify data was imported
  samples <- list_annotated_samples_db(db_path2)
  expect_true(sample_name %in% samples)
})

test_that("import_mat_to_db returns FALSE for missing file", {
  expect_warning(
    result <- import_mat_to_db(
      "/nonexistent/file.mat",
      tempfile(fileext = ".sqlite"),
      "sample", "test"
    ),
    "MAT file not found"
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
  on.exit(unlink(c(db_dir, mat_dir), recursive = TRUE), add = TRUE)

  result <- export_db_to_mat(db_path, sample_name, mat_dir)
  expect_true(result)

  mat_path <- file.path(mat_dir, paste0(sample_name, ".mat"))
  expect_true(file.exists(mat_path))

  # Verify contents via ifcb_get_mat_variable
  classlist <- iRfcb::ifcb_get_mat_variable(mat_path, variable_name = "classlist")
  expect_equal(nrow(classlist), 4)
  # class indices: Diatom=2, Ciliate=3, Diatom=2, Dinoflagellate=4
  expect_equal(classlist[, 2], c(2, 3, 2, 4))
})

test_that("export_db_to_mat returns FALSE for missing sample", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  on.exit(unlink(db_dir, recursive = TRUE), add = TRUE)
  db_path <- get_db_path(db_dir)

  # Create DB with one sample
  save_annotations_db(db_path, "existing_sample",
                      data.frame(file_name = "existing_sample_00001.png",
                                 class_name = "Diatom",
                                 stringsAsFactors = FALSE),
                      c("unclassified", "Diatom"), "test")

  expect_warning(
    result <- export_db_to_mat(db_path, "nonexistent_sample", db_dir),
    "No annotations found for sample"
  )
  expect_false(result)
})

test_that("export_db_to_mat returns FALSE for non-existent database", {
  expect_warning(
    result <- export_db_to_mat("/nonexistent/db.sqlite", "sample", tempdir()),
    "Database not found"
  )
  expect_false(result)
})

test_that("export_db_to_mat returns FALSE when class list is missing", {
  # Create a DB with annotations but manually remove the class_lists entries
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  on.exit(unlink(db_dir, recursive = TRUE), add = TRUE)
  db_path <- get_db_path(db_dir)

  sample_name <- "D20230101T120000_IFCB134"
  class2use <- c("unclassified", "Diatom")

  save_annotations_db(db_path, sample_name,
                      data.frame(file_name = paste0(sample_name, "_00001.png"),
                                 class_name = "Diatom",
                                 stringsAsFactors = FALSE),
                      class2use, "test")

  # Remove the class_lists entries so the function hits the "no class list" path

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbExecute(con, "DELETE FROM class_lists WHERE sample_name = ?",
                 params = list(sample_name))
  DBI::dbDisconnect(con)

  expect_warning(
    result <- export_db_to_mat(db_path, sample_name, tempdir()),
    "No class list found"
  )
  expect_false(result)
})

test_that("import_all_mat_to_db returns zero counts for empty folder", {
  mat_dir <- tempfile("mat_empty_")
  dir.create(mat_dir)
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  on.exit(unlink(c(mat_dir, db_dir), recursive = TRUE), add = TRUE)
  db_path <- get_db_path(db_dir)

  result <- import_all_mat_to_db(mat_dir, db_path)
  expect_equal(result$success, 0L)
  expect_equal(result$failed, 0L)
  expect_equal(result$skipped, 0L)
})

test_that("import_all_mat_to_db excludes _class and class2use files", {
  mat_dir <- tempfile("mat_filter_")
  dir.create(mat_dir)
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  db_path <- get_db_path(db_dir)

  # Create files that should be excluded
  file.create(file.path(mat_dir, "sample_A_class_v1.mat"))
  file.create(file.path(mat_dir, "class2use_manual.mat"))
  on.exit(unlink(c(mat_dir, db_dir), recursive = TRUE), add = TRUE)

  result <- import_all_mat_to_db(mat_dir, db_path)
  # All files are filtered out, so nothing to import
  expect_equal(result$success, 0L)
  expect_equal(result$failed, 0L)
  expect_equal(result$skipped, 0L)
})

test_that("export_all_db_to_mat returns zero counts for empty database", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  on.exit(unlink(db_dir, recursive = TRUE), add = TRUE)
  db_path <- get_db_path(db_dir)

  # Create empty database (no annotations)
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbDisconnect(con)

  result <- export_all_db_to_mat(db_path, tempdir())
  expect_equal(result$success, 0L)
  expect_equal(result$failed, 0L)
})

test_that("export_all_db_to_png returns zero counts for empty database", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  on.exit(unlink(db_dir, recursive = TRUE), add = TRUE)
  db_path <- get_db_path(db_dir)

  # Create empty database
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbDisconnect(con)

  result <- export_all_db_to_png(db_path, tempdir(), list())
  expect_equal(result$success, 0L)
  expect_equal(result$failed, 0L)
  expect_equal(result$skipped, 0L)
})

test_that("export_db_to_png returns FALSE for missing database", {
  expect_warning(
    result <- export_db_to_png("/nonexistent/db.sqlite", "sample",
                               "/nonexistent/file.roi", tempdir()),
    "Database not found"
  )
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
  on.exit(unlink(c(mat_dir, db_dir), recursive = TRUE), add = TRUE)
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
  on.exit(unlink(c(db_dir, mat_dir), recursive = TRUE), add = TRUE)

  result <- export_all_db_to_mat(db_path, mat_dir)

  expect_equal(result$success, 2L)
  expect_equal(result$failed, 0L)
  expect_true(file.exists(file.path(mat_dir, "sample_X.mat")))
  expect_true(file.exists(file.path(mat_dir, "sample_Y.mat")))
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
  on.exit(unlink(c(db_dir, db_dir2, mat_dir), recursive = TRUE), add = TRUE)
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
  on.exit(unlink(c(db_dir, png_dir), recursive = TRUE), add = TRUE)

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
  on.exit(unlink(c(db_dir, png_dir), recursive = TRUE), add = TRUE)

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

  # Function returns TRUE at skip_class filter before using the ROI file,
  # so we only need a file that exists
  roi_path <- tempfile(fileext = ".roi")
  file.create(roi_path)

  png_dir <- tempfile("png_")
  dir.create(png_dir)
  on.exit(unlink(c(db_dir, png_dir, roi_path), recursive = TRUE), add = TRUE)

  result <- export_db_to_png(db_path, sample_name, roi_path, png_dir,
                             skip_class = "unclassified")
  expect_true(result)

  # No class subfolders should be created
  expect_equal(length(list.dirs(png_dir, recursive = FALSE)), 0)
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

  # Need a real file for roi_path since the function checks existence first
  roi_path <- tempfile(fileext = ".roi")
  file.create(roi_path)
  on.exit(unlink(c(db_dir, roi_path), recursive = TRUE), add = TRUE)

  expect_warning(
    result <- export_db_to_png(db_path, "nonexistent_sample", roi_path, tempdir()),
    "No annotations found for sample"
  )
  expect_false(result)
})

test_that("export_db_to_png returns FALSE for missing ROI file", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  on.exit(unlink(db_dir, recursive = TRUE), add = TRUE)
  db_path <- get_db_path(db_dir)

  save_annotations_db(db_path, "sample_A",
                      data.frame(file_name = "sample_A_00001.png",
                                 class_name = "Diatom",
                                 stringsAsFactors = FALSE),
                      c("unclassified", "Diatom"), "test")

  expect_warning(
    result <- export_db_to_png(db_path, "sample_A", "/nonexistent/file.roi", tempdir()),
    "ROI file not found"
  )
  expect_false(result)
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
  on.exit(unlink(c(db_dir, png_dir), recursive = TRUE), add = TRUE)

  roi_map <- list("D20220522T000439_IFCB134" = roi_path)

  result <- export_all_db_to_png(db_path, png_dir, roi_map)

  expect_equal(result$success, 1L)
  expect_equal(result$failed, 0L)
  expect_equal(result$skipped, 1L)
  expect_true(dir.exists(file.path(png_dir, "Diatom")))
})

test_that("create_ecotaxa_inventory_txt writes inventory with required columns", {
  roi_path <- testthat::test_path("test_data", "raw", "2022", "D20220522",
                                   "D20220522T000439_IFCB134.roi")
  skip_if_not(file.exists(roi_path), "Test ROI file not found")

  db_dir <- tempfile("db_")
  dir.create(db_dir)
  db_path <- get_db_path(db_dir)

  sample_name <- "D20220522T000439_IFCB134"
  class2use <- c("unclassified", "Diatom")
  classifications <- data.frame(
    file_name = sprintf("%s_%05d.png", sample_name, 2),
    class_name = "Diatom",
    stringsAsFactors = FALSE
  )
  save_annotations_db(db_path, sample_name, classifications, class2use, "TestAnnotator")
  save_class_taxonomy_db(
    db_path,
    class_aphia_map = c("Diatom" = "12345"),
    scientific_name_map = c("Diatom" = "Bacillariophyceae")
  )

  png_dir <- tempfile("png_")
  dir.create(png_dir)
  on.exit(unlink(c(db_dir, png_dir), recursive = TRUE), add = TRUE)
  export_db_to_png(db_path, sample_name, roi_path, png_dir)

  written <- ClassiPyR:::create_ecotaxa_inventory_txt(png_dir, db_path)
  expect_equal(written, 1L)

  txt_path <- file.path(png_dir, "Diatom", "ecotaxa_Diatom.tsv")
  expect_true(file.exists(txt_path))

  inv <- utils::read.delim(txt_path, sep = "\t", stringsAsFactors = FALSE, check.names = FALSE)
  expect_equal(
    names(inv),
    c(
      "img_file_name", "object_id", "object_date", "object_time",
      "object_annotation_status", "object_annotation_person_name",
      "object_annotation_category", "object_aphiaid",
      "object_annotation_hierarchy", "object_roi_number"
    )
  )

  expect_equal(inv$img_file_name[1], "[t]")
  expect_equal(inv$img_file_name[2], "D20220522T000439_IFCB134_00002.png")
  expect_equal(inv$object_id[2], "D20220522T000439_IFCB134_00002")
  expect_equal(inv$object_date[2], "20220522")
  expect_equal(inv$object_time[2], "000439")
  expect_equal(inv$object_annotation_status[2], "validated")
  expect_equal(inv$object_annotation_person_name[2], "TestAnnotator")
  expect_equal(inv$object_annotation_category[2], "Diatom")
  expect_equal(inv$object_aphiaid[2], "12345")
  expect_equal(inv$object_annotation_hierarchy[2], "Bacillariophyceae")
  expect_equal(inv$object_roi_number[2], "2")
})

test_that("parse_ifcb_png_name handles legacy IFCB filename format", {
  parsed <- ClassiPyR:::parse_ifcb_png_name("IFCB134_2023_072_004021_00002.png")
  expect_equal(parsed$object_id, "IFCB134_2023_072_004021_00002")
  expect_equal(parsed$object_date, "20230313")
  expect_equal(parsed$object_time, "004021")
  expect_equal(parsed$object_roi_number, "2")
})

test_that("export_all_db_to_zip exports PNGs and calls ifcb_zip_pngs with txt", {
  roi_path <- testthat::test_path("test_data", "raw", "2022", "D20220522",
                                   "D20220522T000439_IFCB134.roi")
  skip_if_not(file.exists(roi_path), "Test ROI file not found")

  db_dir <- tempfile("db_")
  dir.create(db_dir)
  db_path <- get_db_path(db_dir)

  sample_name <- "D20220522T000439_IFCB134"
  class2use <- c("unclassified", "Diatom")
  classifications <- data.frame(
    file_name = sprintf("%s_%05d.png", sample_name, 2),
    class_name = "Diatom",
    stringsAsFactors = FALSE
  )
  save_annotations_db(db_path, sample_name, classifications, class2use, "test")

  roi_map <- list("D20220522T000439_IFCB134" = roi_path)
  zip_path <- file.path(tempdir(), paste0("classipyr_test_", as.integer(Sys.time()), ".zip"))
  if (file.exists(zip_path)) file.remove(zip_path)
  on.exit(unlink(c(db_dir, zip_path), recursive = TRUE), add = TRUE)

  got_include_txt <- NA
  got_readme <- NA_character_
  got_tsv <- character()
  local_mocked_bindings(
    ifcb_zip_pngs = function(png_folder, zip_filename, readme_file = NULL,
                             email_address = "", version = "",
                             print_progress = TRUE, include_txt = FALSE,
                             split_zip = FALSE, max_size = 500,
                             quiet = FALSE) {
      got_include_txt <<- include_txt
      got_readme <<- if (is.null(readme_file)) NA_character_ else readme_file
      got_tsv <<- list.files(png_folder, pattern = "^ecotaxa_.*\\.tsv$", recursive = TRUE)
      file.create(zip_filename)
      TRUE
    },
    .package = "ClassiPyR"
  )

  res <- export_all_db_to_zip(db_path, zip_path, roi_map)

  expect_equal(res$success, 1L)
  expect_equal(res$failed, 0L)
  expect_equal(res$skipped, 0L)
  expect_equal(res$inventory_files, 1L)
  expect_true(file.exists(zip_path))
  expect_true(isTRUE(got_include_txt))
  expect_true(nzchar(got_readme))
  expect_true(any(grepl("^Diatom/ecotaxa_Diatom\\.tsv$", got_tsv)))
})

test_that("save_annotations_db stores is_manual flags", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  on.exit(unlink(db_dir, recursive = TRUE), add = TRUE)
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
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  rows <- DBI::dbGetQuery(con,
    "SELECT roi_number, class_name, is_manual FROM annotations WHERE sample_name = ? ORDER BY roi_number",
    params = list(sample_name))

  expect_equal(rows$is_manual, c(1L, 0L, 1L))
  expect_equal(rows$class_name, c("Diatom", "unclassified", "Ciliate"))
})

test_that("save_annotations_db defaults is_manual to 1", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  on.exit(unlink(db_dir, recursive = TRUE), add = TRUE)
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
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  rows <- DBI::dbGetQuery(con,
    "SELECT is_manual FROM annotations WHERE sample_name = ?",
    params = list(sample_name))

  expect_true(all(rows$is_manual == 1L))
})

test_that("schema migration adds is_manual to existing DB", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  on.exit(unlink(db_dir, recursive = TRUE), add = TRUE)
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
  on.exit(unlink(c(mat_dir, db_dir), recursive = TRUE), add = TRUE)
  db_path <- get_db_path(db_dir)

  result <- import_mat_to_db(mat_path, db_path, "test_sample")
  expect_true(result)

  # Verify the class list stored in DB matches the .mat file's embedded list
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  cl <- DBI::dbGetQuery(con,
    "SELECT class_name FROM class_lists WHERE sample_name = 'test_sample' ORDER BY class_index")
  expect_equal(cl$class_name, class2use)

  # Verify class names mapped correctly
  ann <- DBI::dbGetQuery(con,
    "SELECT roi_number, class_name FROM annotations WHERE sample_name = 'test_sample' ORDER BY roi_number")
  expect_equal(ann$class_name, c("Diatom", "Ciliate", "unclassified"))
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
  on.exit(unlink(c(mat_dir, db_dir), recursive = TRUE), add = TRUE)
  db_path <- get_db_path(db_dir)

  result <- import_mat_to_db(mat_path, db_path, "test_nan")
  expect_true(result)

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  rows <- DBI::dbGetQuery(con,
    "SELECT roi_number, class_name, is_manual FROM annotations WHERE sample_name = 'test_nan' ORDER BY roi_number")

  expect_equal(rows$is_manual, c(1L, 0L, 1L, 0L))
  expect_equal(rows$class_name, c("Diatom", "unclassified", "Ciliate", "unclassified"))
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
  on.exit(unlink(c(db_dir, mat_dir), recursive = TRUE), add = TRUE)

  result <- export_db_to_mat(db_path, sample_name, mat_dir)
  expect_true(result)

  mat_path <- file.path(mat_dir, paste0(sample_name, ".mat"))
  classlist <- iRfcb::ifcb_get_mat_variable(mat_path, variable_name = "classlist")

  # Reviewed ROIs should have valid indices, unreviewed should be NaN
  expect_equal(classlist[1, 2], 2)   # Diatom
  expect_true(is.nan(classlist[2, 2]))  # unreviewed -> NaN
  expect_equal(classlist[3, 2], 3)   # Ciliate
  expect_true(is.nan(classlist[4, 2]))  # unreviewed -> NaN
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
  on.exit(unlink(c(mat_dir, db_dir, export_dir), recursive = TRUE), add = TRUE)
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
})

# ============================================================================
# Class Review Mode database functions
# ============================================================================

test_that("list_classes_db returns correct class counts", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  on.exit(unlink(db_dir, recursive = TRUE), add = TRUE)
  db_path <- get_db_path(db_dir)

  # Non-existent database returns empty data frame
  result <- list_classes_db(db_path)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_true(all(c("class_name", "count") %in% names(result)))

  # Add annotations across two samples
  class2use <- c("unclassified", "Diatom", "Ciliate")

  save_annotations_db(db_path, "sample_A",
                      data.frame(file_name = sprintf("sample_A_%05d.png", 1:3),
                                 class_name = c("Diatom", "Diatom", "Ciliate"),
                                 stringsAsFactors = FALSE),
                      class2use, "test")
  save_annotations_db(db_path, "sample_B",
                      data.frame(file_name = sprintf("sample_B_%05d.png", 1:2),
                                 class_name = c("Ciliate", "Diatom"),
                                 stringsAsFactors = FALSE),
                      class2use, "test")

  result <- list_classes_db(db_path)
  expect_equal(nrow(result), 2)  # Ciliate and Diatom
  expect_equal(result$class_name, c("Ciliate", "Diatom"))  # alphabetical
  expect_equal(result$count, c(2L, 3L))
})

test_that("load_class_annotations_db returns correct file_names", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  on.exit(unlink(db_dir, recursive = TRUE), add = TRUE)
  db_path <- get_db_path(db_dir)

  class2use <- c("unclassified", "Diatom", "Ciliate")

  save_annotations_db(db_path, "sample_A",
                      data.frame(file_name = sprintf("sample_A_%05d.png", 1:3),
                                 class_name = c("Diatom", "Ciliate", "Diatom"),
                                 stringsAsFactors = FALSE),
                      class2use, "test")
  save_annotations_db(db_path, "sample_B",
                      data.frame(file_name = sprintf("sample_B_%05d.png", 1:2),
                                 class_name = c("Diatom", "Ciliate"),
                                 stringsAsFactors = FALSE),
                      class2use, "test")

  # Load all Diatom annotations
  result <- load_class_annotations_db(db_path, "Diatom")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_true(all(c("sample_name", "roi_number", "class_name", "file_name") %in% names(result)))

  # Check file_name format
  expect_equal(result$file_name,
               c("sample_A_00001.png", "sample_A_00003.png", "sample_B_00001.png"))
  expect_equal(result$sample_name, c("sample_A", "sample_A", "sample_B"))

  # Non-existent class returns NULL
  result2 <- load_class_annotations_db(db_path, "Nonexistent")
  expect_null(result2)

  # Non-existent database returns NULL
  result3 <- load_class_annotations_db("/nonexistent/db.sqlite", "Diatom")
  expect_null(result3)
})

test_that("save_class_review_changes_db updates only targeted rows", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  on.exit(unlink(db_dir, recursive = TRUE), add = TRUE)
  db_path <- get_db_path(db_dir)

  class2use <- c("unclassified", "Diatom", "Ciliate", "Dinoflagellate")

  # Create two samples
  save_annotations_db(db_path, "sample_A",
                      data.frame(file_name = sprintf("sample_A_%05d.png", 1:3),
                                 class_name = c("Diatom", "Diatom", "Ciliate"),
                                 stringsAsFactors = FALSE),
                      class2use, "OrigUser")
  save_annotations_db(db_path, "sample_B",
                      data.frame(file_name = sprintf("sample_B_%05d.png", 1:2),
                                 class_name = c("Diatom", "Ciliate"),
                                 stringsAsFactors = FALSE),
                      class2use, "OrigUser")

  # Reclassify: sample_A ROI 1 from Diatom to Dinoflagellate,
  #             sample_B ROI 2 from Ciliate to Diatom
  changes <- data.frame(
    sample_name = c("sample_A", "sample_B"),
    roi_number = c(1L, 2L),
    new_class_name = c("Dinoflagellate", "Diatom"),
    stringsAsFactors = FALSE
  )

  updated <- save_class_review_changes_db(db_path, changes, "Reviewer")
  expect_equal(updated, 2L)

  # Verify only changed rows were updated
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # sample_A ROI 1 should be Dinoflagellate with Reviewer annotator
  row_a1 <- DBI::dbGetQuery(con,
    "SELECT class_name, annotator, is_manual FROM annotations WHERE sample_name = 'sample_A' AND roi_number = 1")
  expect_equal(row_a1$class_name, "Dinoflagellate")
  expect_equal(row_a1$annotator, "Reviewer")
  expect_equal(row_a1$is_manual, 1L)

  # sample_A ROI 2 should be unchanged (still Diatom, OrigUser)
  row_a2 <- DBI::dbGetQuery(con,
    "SELECT class_name, annotator FROM annotations WHERE sample_name = 'sample_A' AND roi_number = 2")
  expect_equal(row_a2$class_name, "Diatom")
  expect_equal(row_a2$annotator, "OrigUser")

  # sample_B ROI 2 should be Diatom with Reviewer annotator
  row_b2 <- DBI::dbGetQuery(con,
    "SELECT class_name, annotator FROM annotations WHERE sample_name = 'sample_B' AND roi_number = 2")
  expect_equal(row_b2$class_name, "Diatom")
  expect_equal(row_b2$annotator, "Reviewer")
})

test_that("list_classes_db filters by year, month, instrument", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  on.exit(unlink(db_dir, recursive = TRUE), add = TRUE)
  db_path <- get_db_path(db_dir)

  class2use <- c("Diatom", "Ciliate")

  # Two samples: different year/month/instrument
  save_annotations_db(db_path, "D20230615T120000_IFCB134",
                      data.frame(file_name = sprintf("D20230615T120000_IFCB134_%05d.png", 1:2),
                                 class_name = c("Diatom", "Ciliate"),
                                 stringsAsFactors = FALSE),
                      class2use, "test")
  save_annotations_db(db_path, "D20240815T120000_IFCB135",
                      data.frame(file_name = sprintf("D20240815T120000_IFCB135_%05d.png", 1:3),
                                 class_name = c("Diatom", "Diatom", "Ciliate"),
                                 stringsAsFactors = FALSE),
                      class2use, "test")

  # No filter — all 5 annotations
  all <- list_classes_db(db_path)
  expect_equal(sum(all$count), 5L)

  # Filter by year
  y2023 <- list_classes_db(db_path, year = "2023")
  expect_equal(sum(y2023$count), 2L)

  y2024 <- list_classes_db(db_path, year = "2024")
  expect_equal(sum(y2024$count), 3L)

  # Filter by month
  m06 <- list_classes_db(db_path, month = "06")
  expect_equal(sum(m06$count), 2L)

  # Filter by instrument
  i134 <- list_classes_db(db_path, instrument = "IFCB134")
  expect_equal(sum(i134$count), 2L)

  i135 <- list_classes_db(db_path, instrument = "IFCB135")
  expect_equal(sum(i135$count), 3L)

  # Combined filter
  combo <- list_classes_db(db_path, year = "2024", instrument = "IFCB135")
  expect_equal(sum(combo$count), 3L)

  # Filter with no matches
  empty <- list_classes_db(db_path, year = "2025")
  expect_equal(nrow(empty), 0)

  # "all" values are treated as no filter
  all2 <- list_classes_db(db_path, year = "all", month = "all", instrument = "all")
  expect_equal(sum(all2$count), 5L)

  # Filter by annotator
  by_test <- list_classes_db(db_path, annotator = "test")
  expect_equal(sum(by_test$count), 5L)

  by_nobody <- list_classes_db(db_path, annotator = "nobody")
  expect_equal(nrow(by_nobody), 0)
})

test_that("load_class_annotations_db filters by year, month, instrument", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  on.exit(unlink(db_dir, recursive = TRUE), add = TRUE)
  db_path <- get_db_path(db_dir)

  class2use <- c("Diatom", "Ciliate")

  save_annotations_db(db_path, "D20230615T120000_IFCB134",
                      data.frame(file_name = sprintf("D20230615T120000_IFCB134_%05d.png", 1:2),
                                 class_name = c("Diatom", "Diatom"),
                                 stringsAsFactors = FALSE),
                      class2use, "alice")
  save_annotations_db(db_path, "D20240815T120000_IFCB135",
                      data.frame(file_name = sprintf("D20240815T120000_IFCB135_%05d.png", 1:3),
                                 class_name = c("Diatom", "Diatom", "Diatom"),
                                 stringsAsFactors = FALSE),
                      class2use, "bob")

  # No filter — all 5 Diatom
  all <- load_class_annotations_db(db_path, "Diatom")
  expect_equal(nrow(all), 5)

  # Filter by year
  y2023 <- load_class_annotations_db(db_path, "Diatom", year = "2023")
  expect_equal(nrow(y2023), 2)
  expect_true(all(grepl("D2023", y2023$sample_name)))

  # Filter by instrument
  i135 <- load_class_annotations_db(db_path, "Diatom", instrument = "IFCB135")
  expect_equal(nrow(i135), 3)

  # Filter by annotator
  by_alice <- load_class_annotations_db(db_path, "Diatom", annotator = "alice")
  expect_equal(nrow(by_alice), 2)

  by_bob <- load_class_annotations_db(db_path, "Diatom", annotator = "bob")
  expect_equal(nrow(by_bob), 3)

  by_nobody <- load_class_annotations_db(db_path, "Diatom", annotator = "nobody")
  expect_null(by_nobody)

  # Filter with no matches
  none <- load_class_annotations_db(db_path, "Diatom", year = "2025")
  expect_null(none)
})

test_that("list_annotation_metadata_db returns correct metadata", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  on.exit(unlink(db_dir, recursive = TRUE), add = TRUE)
  db_path <- get_db_path(db_dir)

  # Non-existent database
  meta <- list_annotation_metadata_db(db_path)
  expect_equal(meta$years, character())
  expect_equal(meta$months, character())
  expect_equal(meta$instruments, character())
  expect_equal(meta$annotators, character())

  class2use <- c("Diatom", "Ciliate")

  save_annotations_db(db_path, "D20230615T120000_IFCB134",
                      data.frame(file_name = "D20230615T120000_IFCB134_00001.png",
                                 class_name = "Diatom",
                                 stringsAsFactors = FALSE),
                      class2use, "alice")
  save_annotations_db(db_path, "D20240815T120000_IFCB135",
                      data.frame(file_name = "D20240815T120000_IFCB135_00001.png",
                                 class_name = "Ciliate",
                                 stringsAsFactors = FALSE),
                      class2use, "bob")

  meta <- list_annotation_metadata_db(db_path)
  expect_equal(meta$years, c("2023", "2024"))
  expect_equal(meta$months, c("06", "08"))
  expect_equal(meta$instruments, c("IFCB134", "IFCB135"))
  expect_equal(meta$annotators, c("alice", "bob"))
})

test_that("save_class_review_changes_db handles empty input", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  on.exit(unlink(db_dir, recursive = TRUE), add = TRUE)
  db_path <- get_db_path(db_dir)

  # NULL input
  expect_equal(save_class_review_changes_db(db_path, NULL, "test"), 0L)

  # Empty data frame
  empty_df <- data.frame(sample_name = character(), roi_number = integer(),
                         new_class_name = character(), stringsAsFactors = FALSE)
  expect_equal(save_class_review_changes_db(db_path, empty_df, "test"), 0L)
})

test_that("load_class_taxonomy_db returns empty map for missing database", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  on.exit(unlink(db_dir, recursive = TRUE), add = TRUE)
  db_path <- get_db_path(db_dir)

  out <- load_class_taxonomy_db(db_path)
  expect_type(out, "character")
  expect_length(out, 0)
})

test_that("save_class_taxonomy_db creates and persists class taxonomy mappings", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  on.exit(unlink(db_dir, recursive = TRUE), add = TRUE)
  db_path <- get_db_path(db_dir)

  map <- c(
    "Prorocentrum micans" = "109636",
    "Alexandrium tamarense" = "12345"
  )
  accepted <- c(
    "Prorocentrum micans" = "Prorocentrum micans",
    "Alexandrium tamarense" = "Alexandrium catenella"
  )

  ok <- save_class_taxonomy_db(db_path, map, accepted)
  expect_true(ok)
  expect_true(file.exists(db_path))

  loaded <- load_class_taxonomy_db(db_path)
  expect_equal(loaded[["Prorocentrum micans"]], "109636")
  expect_equal(loaded[["Alexandrium tamarense"]], "12345")

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  rows <- DBI::dbGetQuery(con, "SELECT class_name, aphia_id, accepted_name FROM class_taxonomy")
  expect_equal(nrow(rows), 2)
  expect_true("accepted_name" %in% names(rows))
  expect_true(any(rows$class_name == "Alexandrium tamarense" & rows$accepted_name == "Alexandrium catenella"))
})

test_that("save_class_taxonomy_db upserts and preserves accepted_name when new one is empty", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  on.exit(unlink(db_dir, recursive = TRUE), add = TRUE)
  db_path <- get_db_path(db_dir)

  ok1 <- save_class_taxonomy_db(
    db_path,
    class_aphia_map = c("Taxon A" = "111"),
    accepted_name_map = c("Taxon A" = "Accepted A")
  )
  expect_true(ok1)

  # Upsert same class with new AphiaID and no accepted_name map -> accepted name should stay.
  ok2 <- save_class_taxonomy_db(
    db_path,
    class_aphia_map = c("Taxon A" = "222"),
    accepted_name_map = NULL
  )
  expect_true(ok2)

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  row <- DBI::dbGetQuery(con, "SELECT class_name, aphia_id, accepted_name FROM class_taxonomy WHERE class_name = 'Taxon A'")
  expect_equal(nrow(row), 1)
  expect_equal(as.character(row$aphia_id[1]), "222")
  expect_equal(as.character(row$accepted_name[1]), "Accepted A")
})

test_that("save_class_taxonomy_db handles duplicate class names by keeping last", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  on.exit(unlink(db_dir, recursive = TRUE), add = TRUE)
  db_path <- get_db_path(db_dir)

  dup_map <- c("Taxon B" = "100", "Taxon B" = "200")
  ok <- save_class_taxonomy_db(db_path, dup_map)
  expect_true(ok)

  loaded <- load_class_taxonomy_db(db_path)
  expect_equal(loaded[["Taxon B"]], "200")

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  n_rows <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM class_taxonomy WHERE class_name = 'Taxon B'")$n[1]
  expect_equal(as.integer(n_rows), 1L)
})

test_that("save_class_taxonomy_db returns TRUE for empty map and does not error", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  on.exit(unlink(db_dir, recursive = TRUE), add = TRUE)
  db_path <- get_db_path(db_dir)

  ok <- save_class_taxonomy_db(db_path, setNames(character(0), character(0)))
  expect_true(ok)

  # DB may or may not exist; loading should still be safe and empty.
  loaded <- load_class_taxonomy_db(db_path)
  expect_length(loaded, 0)
})

test_that("integration: WoRMS match rows round-trip into class_taxonomy table", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  on.exit(unlink(db_dir, recursive = TRUE), add = TRUE)
  db_path <- get_db_path(db_dir)

  mock_api <- function(query, marine_only = FALSE) {
    # Return deterministic WoRMS-like records for both vector and scalar queries.
    mk <- function(q) {
      if (identical(q, "NoMatch")) return(data.frame())
      if (identical(q, "OldName")) {
        return(data.frame(
          AphiaID = 10,
          valid_AphiaID = 20,
          scientificname = "OldName",
          valid_name = "AcceptedName",
          status = "unaccepted",
          stringsAsFactors = FALSE
        ))
      }
      data.frame(
        AphiaID = 30,
        valid_AphiaID = 30,
        scientificname = q,
        valid_name = q,
        status = "accepted",
        stringsAsFactors = FALSE
      )
    }

    if (length(query) > 1) {
      return(lapply(query, mk))
    }
    mk(query)
  }

  local_mocked_bindings(
    worms_records_names_api = mock_api,
    .package = "ClassiPyR"
  )

  rows <- build_worms_match_rows(
    class_names = c("OldName", "Prorocentrum_micans", "NoMatch"),
    raw_queries = c("OldName", "Prorocentrum_micans", "NoMatch")
  )

  matched <- rows[!is.na(rows$aphia_id) & nzchar(rows$aphia_id), , drop = FALSE]
  map <- setNames(as.character(matched$aphia_id), as.character(matched$class_name))
  accepted <- setNames(as.character(matched$accepted_name), as.character(matched$class_name))
  accepted_ids <- setNames(as.character(matched$accepted_aphia_id), as.character(matched$class_name))
  scientific <- setNames(as.character(matched$scientific_name), as.character(matched$class_name))

  ok <- save_class_taxonomy_db(
    db_path, map, accepted,
    scientific_name_map = scientific,
    accepted_aphia_map = accepted_ids
  )
  expect_true(ok)

  loaded <- load_class_taxonomy_db(db_path)
  expect_equal(loaded[["OldName"]], "10")
  expect_equal(loaded[["Prorocentrum_micans"]], "30")
  expect_false("NoMatch" %in% names(loaded))

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  tax_rows <- DBI::dbGetQuery(
    con,
    "SELECT class_name, accepted_name, accepted_aphia_id, scientific_name FROM class_taxonomy"
  )
  expect_true(any(tax_rows$class_name == "OldName" & tax_rows$accepted_name == "AcceptedName"))
  expect_true(any(tax_rows$class_name == "OldName" & tax_rows$accepted_aphia_id == "20"))
  expect_true(any(tax_rows$class_name == "OldName" & tax_rows$scientific_name == "OldName"))
})

# =============================================================================
# Global class list persistence
# =============================================================================

test_that("save_global_class_list_db creates database and stores classes", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  on.exit(unlink(db_dir, recursive = TRUE), add = TRUE)
  db_path <- get_db_path(db_dir)

  class2use <- c("unclassified", "Diatom", "Ciliate", "Dinoflagellate")
  result <- save_global_class_list_db(db_path, class2use)

  expect_true(result)
  expect_true(file.exists(db_path))

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  rows <- DBI::dbGetQuery(con, "SELECT * FROM global_class_list ORDER BY class_index")
  expect_equal(nrow(rows), 4)
  expect_equal(rows$class_name, class2use)
  expect_equal(rows$class_index, 1:4)
})

test_that("load_global_class_list_db returns stored classes in order", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  on.exit(unlink(db_dir, recursive = TRUE), add = TRUE)
  db_path <- get_db_path(db_dir)

  class2use <- c("unclassified", "Diatom", "Ciliate")
  save_global_class_list_db(db_path, class2use)

  loaded <- load_global_class_list_db(db_path)
  expect_equal(loaded, class2use)
})

test_that("load_global_class_list_db returns NULL for non-existent database", {
  result <- load_global_class_list_db("/nonexistent/path/db.sqlite")
  expect_null(result)
})

test_that("load_global_class_list_db returns NULL for empty table", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  on.exit(unlink(db_dir, recursive = TRUE), add = TRUE)
  db_path <- get_db_path(db_dir)

  # Create DB with empty table
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  init_db_schema(con)
  DBI::dbDisconnect(con)
  on.exit(NULL)  # remove disconnect handler
  on.exit(unlink(db_dir, recursive = TRUE), add = TRUE)

  result <- load_global_class_list_db(db_path)
  expect_null(result)
})

test_that("save_global_class_list_db replaces existing data", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  on.exit(unlink(db_dir, recursive = TRUE), add = TRUE)
  db_path <- get_db_path(db_dir)

  save_global_class_list_db(db_path, c("unclassified", "Diatom"))
  save_global_class_list_db(db_path, c("unclassified", "Ciliate", "Dino"))

  loaded <- load_global_class_list_db(db_path)
  expect_equal(loaded, c("unclassified", "Ciliate", "Dino"))
})

test_that("save_global_class_list_db handles NULL and empty input", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  on.exit(unlink(db_dir, recursive = TRUE), add = TRUE)
  db_path <- get_db_path(db_dir)

  expect_true(save_global_class_list_db(db_path, NULL))
  expect_true(save_global_class_list_db(db_path, character(0)))
})

test_that("init_db_schema creates global_class_list table", {
  db_dir <- tempfile("db_")
  dir.create(db_dir)
  on.exit(unlink(db_dir, recursive = TRUE), add = TRUE)
  db_path <- get_db_path(db_dir)

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  init_db_schema(con)

  tables <- DBI::dbGetQuery(con, "SELECT name FROM sqlite_master WHERE type='table'")
  expect_true("global_class_list" %in% tables$name)
})
