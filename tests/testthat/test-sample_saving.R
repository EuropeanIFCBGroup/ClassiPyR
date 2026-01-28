# Tests for sample saving functions

library(testthat)

test_that("copy_images_to_class_folders creates correct folder structure", {
  # Create temp source folder with images
  src_folder <- tempfile("src_")
  dir.create(src_folder)
  file.create(file.path(src_folder, "sample_00001.png"))
  file.create(file.path(src_folder, "sample_00002.png"))
  file.create(file.path(src_folder, "sample_00003.png"))

  # Create temp and output folders
  temp_folder <- tempfile("temp_")
  output_folder <- tempfile("output_")

  # Create classifications
  classifications <- data.frame(
    file_name = c("sample_00001.png", "sample_00002.png", "sample_00003.png"),
    class_name = c("Diatom", "Ciliate", "Diatom"),
    stringsAsFactors = FALSE
  )

  # Run copy
  copy_images_to_class_folders(classifications, src_folder, temp_folder, output_folder)

  # Check temp folder structure (for ifcb_annotate_samples)
  expect_true(dir.exists(file.path(temp_folder, "Diatom")))
  expect_true(dir.exists(file.path(temp_folder, "Ciliate")))
  expect_true(file.exists(file.path(temp_folder, "Diatom", "sample_00001.png")))
  expect_true(file.exists(file.path(temp_folder, "Diatom", "sample_00003.png")))
  expect_true(file.exists(file.path(temp_folder, "Ciliate", "sample_00002.png")))

  # Check output folder structure (permanent storage)
  expect_true(dir.exists(file.path(output_folder, "Diatom")))
  expect_true(dir.exists(file.path(output_folder, "Ciliate")))
  expect_true(file.exists(file.path(output_folder, "Diatom", "sample_00001.png")))

  # Cleanup
  unlink(src_folder, recursive = TRUE)
  unlink(temp_folder, recursive = TRUE)
  unlink(output_folder, recursive = TRUE)
})

test_that("copy_images_to_class_folders handles missing source files gracefully", {
  # Create temp folders (empty src)
  src_folder <- tempfile("src_")
  temp_folder <- tempfile("temp_")
  output_folder <- tempfile("output_")
  dir.create(src_folder)

  classifications <- data.frame(
    file_name = c("nonexistent.png"),
    class_name = c("Diatom"),
    stringsAsFactors = FALSE
  )

  # Should not error, just skip missing files
  expect_no_error(
    copy_images_to_class_folders(classifications, src_folder, temp_folder, output_folder)
  )

  # Folder may or may not be created depending on implementation
  # But definitely no files should exist
  if (dir.exists(file.path(temp_folder, "Diatom"))) {
    expect_equal(
      length(list.files(file.path(temp_folder, "Diatom"))),
      0
    )
  }

  # Cleanup
  unlink(src_folder, recursive = TRUE)
  unlink(temp_folder, recursive = TRUE)
  unlink(output_folder, recursive = TRUE)
})

test_that("save_validation_statistics creates correct CSV files", {
  skip_if_not_installed("dplyr")

  sample_name <- "D20230314T001205_IFCB134"
  stats_folder <- tempfile("stats_")
  dir.create(stats_folder)

  original_classifications <- data.frame(
    file_name = c("sample_00001.png", "sample_00002.png", "sample_00003.png"),
    class_name = c("Diatom", "Ciliate", "Diatom"),
    score = c(0.95, 0.87, 0.92),
    stringsAsFactors = FALSE
  )

  # Current classifications with one change
  current_classifications <- data.frame(
    file_name = c("sample_00001.png", "sample_00002.png", "sample_00003.png"),
    class_name = c("Diatom", "Dinoflagellate", "Diatom"),  # Ciliate -> Dinoflagellate
    stringsAsFactors = FALSE
  )

  save_validation_statistics(
    sample_name = sample_name,
    classifications = current_classifications,
    original_classifications = original_classifications,
    stats_folder = stats_folder,
    annotator = "TestUser"
  )

  # Check summary stats file
  stats_file <- file.path(stats_folder, paste0(sample_name, "_validation_stats.csv"))
  expect_true(file.exists(stats_file))

  stats <- read.csv(stats_file)
  expect_equal(stats$sample, sample_name)
  expect_equal(stats$annotator, "TestUser")
  expect_equal(stats$total_images, 3)
  expect_equal(stats$correct_classifications, 2)
  expect_equal(stats$incorrect_classifications, 1)
  expect_equal(stats$accuracy, 2/3, tolerance = 0.001)

  # Check detailed stats file
  detailed_file <- file.path(stats_folder, paste0(sample_name, "_validation_detailed.csv"))
  expect_true(file.exists(detailed_file))

  detailed <- read.csv(detailed_file)
  expect_equal(nrow(detailed), 3)
  expect_true("correct" %in% names(detailed))
  expect_true("annotator" %in% names(detailed))

  # Cleanup
  unlink(stats_folder, recursive = TRUE)
})

test_that("save_validation_statistics handles all correct classifications", {
  skip_if_not_installed("dplyr")

  sample_name <- "D20230314T001205_IFCB134"
  stats_folder <- tempfile("stats_")
  dir.create(stats_folder)

  # All classifications are correct (no changes)
  classifications <- data.frame(
    file_name = c("sample_00001.png", "sample_00002.png"),
    class_name = c("Diatom", "Ciliate"),
    score = c(0.95, 0.87),
    stringsAsFactors = FALSE
  )

  save_validation_statistics(
    sample_name = sample_name,
    classifications = classifications,
    original_classifications = classifications,
    stats_folder = stats_folder,
    annotator = "TestUser"
  )

  # Check 100% accuracy
  stats_file <- file.path(stats_folder, paste0(sample_name, "_validation_stats.csv"))
  stats <- read.csv(stats_file)
  expect_equal(stats$accuracy, 1.0)
  expect_equal(stats$correct_classifications, 2)
  expect_equal(stats$incorrect_classifications, 0)

  # Cleanup
  unlink(stats_folder, recursive = TRUE)
})

test_that("save_sample_annotations returns FALSE for NULL inputs", {
  expect_false(save_sample_annotations(
    sample_name = NULL,
    classifications = data.frame(),
    original_classifications = data.frame(),
    changes_log = data.frame(image = "x", original_class = "a", new_class = "b"),
    temp_png_folder = tempdir(),
    output_folder = tempdir(),
    png_output_folder = tempdir(),
    roi_folder = tempdir(),
    class2use_path = "/tmp/class2use.txt"
  ))

  expect_false(save_sample_annotations(
    sample_name = "D20230314T001205_IFCB134",
    classifications = NULL,
    original_classifications = data.frame(),
    changes_log = data.frame(image = "x", original_class = "a", new_class = "b"),
    temp_png_folder = tempdir(),
    output_folder = tempdir(),
    png_output_folder = tempdir(),
    roi_folder = tempdir(),
    class2use_path = "/tmp/class2use.txt"
  ))

  expect_false(save_sample_annotations(
    sample_name = "D20230314T001205_IFCB134",
    classifications = data.frame(),
    original_classifications = data.frame(),
    changes_log = data.frame(image = "x", original_class = "a", new_class = "b"),
    temp_png_folder = tempdir(),
    output_folder = tempdir(),
    png_output_folder = tempdir(),
    roi_folder = tempdir(),
    class2use_path = NULL
  ))
})

test_that("save_sample_annotations returns FALSE for empty changes log", {
  empty_log <- data.frame(
    image = character(0),
    original_class = character(0),
    new_class = character(0),
    stringsAsFactors = FALSE
  )

  expect_false(save_sample_annotations(
    sample_name = "D20230314T001205_IFCB134",
    classifications = data.frame(
      file_name = "test.png",
      class_name = "Diatom",
      stringsAsFactors = FALSE
    ),
    original_classifications = data.frame(),
    changes_log = empty_log,
    temp_png_folder = tempdir(),
    output_folder = tempdir(),
    png_output_folder = tempdir(),
    roi_folder = tempdir(),
    class2use_path = "/tmp/class2use.txt"
  ))
})

# Integration test using real test data files

test_that("save_sample_annotations creates MAT file with real data", {
  skip_if_not_installed("iRfcb")
  skip_if_not_installed("dplyr")
  skip_if_not(reticulate::py_available(), "Python not available")
  skip_if_not(reticulate::py_module_available("scipy"), "scipy not available")

  sample_name <- "D20220522T000439_IFCB134"

  # Check test data files exist
  png_folder <- testthat::test_path("test_data", "png")
  roi_folder <- testthat::test_path("test_data", "raw")
  class2use_path <- testthat::test_path("test_data", "class2use.mat")

  skip_if_not(dir.exists(file.path(png_folder, sample_name)), "Test PNG folder not found")
  skip_if_not(file.exists(class2use_path), "Test class2use file not found")
  skip_if_not(
    file.exists(file.path(roi_folder, "2022", "D20220522", paste0(sample_name, ".adc"))),
    "Test ADC file not found"
  )

  # List available PNG files
  png_files <- list.files(file.path(png_folder, sample_name), pattern = "\\.png$")
  skip_if(length(png_files) < 2, "Not enough test PNG files")

  # Create classifications matching the PNG files
  original_classifications <- data.frame(
    file_name = png_files,
    class_name = rep("unclassified", length(png_files)),
    score = rep(NA_real_, length(png_files)),
    stringsAsFactors = FALSE
  )

  # Updated classifications with some changes
  current_classifications <- data.frame(
    file_name = png_files,
    class_name = c("Mesodinium_rubrum", rep("Ciliophora", length(png_files) - 1)),
    stringsAsFactors = FALSE
  )

  # Changes log (at least one change required)
  changes_log <- data.frame(
    image = png_files[1],
    original_class = "unclassified",
    new_class = "Mesodinium_rubrum",
    stringsAsFactors = FALSE
  )

  # Create temp output folders
  output_folder <- tempfile("output_")
  png_output_folder <- tempfile("png_output_")

  result <- save_sample_annotations(
    sample_name = sample_name,
    classifications = current_classifications,
    original_classifications = original_classifications,
    changes_log = changes_log,
    temp_png_folder = png_folder,
    output_folder = output_folder,
    png_output_folder = png_output_folder,
    roi_folder = roi_folder,
    class2use_path = class2use_path,
    annotator = "TestUser"
  )

  expect_true(result)

  # Check MAT file was created (directly in output folder, not in manual/ subfolder)
  mat_file <- file.path(output_folder, paste0(sample_name, ".mat"))
  expect_true(file.exists(mat_file))

  # Check statistics files were created (in validation_statistics subfolder)
  stats_file <- file.path(output_folder, "validation_statistics", paste0(sample_name, "_validation_stats.csv"))
  expect_true(file.exists(stats_file))

  detailed_file <- file.path(output_folder, "validation_statistics", paste0(sample_name, "_validation_detailed.csv"))
  expect_true(file.exists(detailed_file))

  # Check PNG output folders were created
  expect_true(dir.exists(file.path(png_output_folder, "Mesodinium_rubrum")))
  expect_true(dir.exists(file.path(png_output_folder, "Ciliophora")))

  # Cleanup
  unlink(output_folder, recursive = TRUE)
  unlink(png_output_folder, recursive = TRUE)
})
