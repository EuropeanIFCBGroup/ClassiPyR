# Tests for generic instrument mode (non-IFCB image annotation)

# Helper: write a minimal valid image of the given size at path. The image
# format follows the file extension so JPEG-named files contain JPEG bytes.
write_test_png <- function(path, width = 10, height = 8) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("jpg", "jpeg")) {
    grDevices::jpeg(path, width = width, height = height)
  } else {
    grDevices::png(path, width = width, height = height)
  }
  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot.new()
  grDevices::dev.off()
  invisible(path)
}

test_that("rescan_file_index discovers generic image-folder samples", {
  # Route config/db writes to a temp dir so the user's real cache is untouched
  withr::local_envvar(`_R_CHECK_PACKAGE_NAME_` = "ClassiPyR")

  root <- withr::local_tempdir()
  # Two arbitrary-named sample folders with mixed image extensions
  dir.create(file.path(root, "Station_A_slide1"))
  dir.create(file.path(root, "microscope-batch-2"))
  write_test_png(file.path(root, "Station_A_slide1", "cell_x.png"))
  write_test_png(file.path(root, "Station_A_slide1", "cell_y.png"))
  write_test_png(file.path(root, "microscope-batch-2", "img001.png"))
  # A folder with no images should be ignored
  dir.create(file.path(root, "empty_folder"))

  result <- rescan_file_index(
    roi_folder = root, csv_folder = root, output_folder = root,
    db_folder = withr::local_tempdir(), instrument_type = "generic",
    verbose = FALSE
  )

  expect_equal(result$instrument_type, "generic")
  expect_setequal(result$sample_names,
                  c("Station_A_slide1", "microscope-batch-2"))
  expect_true("Station_A_slide1" %in% names(result$png_sample_path_map))
  # No classifier scanning in generic mode
  expect_equal(result$classified_samples, character())
})

test_that("create_new_classifications uses file_name column when present", {
  roi_dims <- data.frame(
    roi_number = c(1L, 2L, 3L),
    file_name = c("alpha.jpg", "beta.png", "gamma.jpeg"),
    width = c(10, 30, 20),
    height = c(10, 10, 10),
    area = c(100, 300, 200),
    stringsAsFactors = FALSE
  )

  cls <- create_new_classifications("My Image Set", roi_dims)
  expect_setequal(cls$file_name, c("alpha.jpg", "beta.png", "gamma.jpeg"))
  expect_true(all(cls$class_name == "unclassified"))
  # Sorted by area descending
  expect_equal(cls$file_name[1], "beta.png")
})

test_that("create_new_classifications keeps IFCB reconstruction without file_name", {
  roi_dims <- data.frame(
    roi_number = c(1L, 2L),
    width = c(10, 20), height = c(10, 10), area = c(100, 200)
  )
  cls <- create_new_classifications("D20230101T120000_IFCB134", roi_dims)
  expect_true(all(grepl("^D20230101T120000_IFCB134_\\d{5}\\.png$", cls$file_name)))
})

test_that("scan_png_class_folder generic mode accepts arbitrary file names", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "Cat"))
  dir.create(file.path(root, "Dog"))
  write_test_png(file.path(root, "Cat", "photo one.jpg"))
  write_test_png(file.path(root, "Cat", "IMG_4421.png"))
  write_test_png(file.path(root, "Dog", "rex.jpeg"))

  res <- scan_png_class_folder(root, instrument_type = "generic")
  expect_equal(nrow(res$annotations), 3)
  expect_setequal(res$classes_found, c("Cat", "Dog"))
  # Each image becomes its own sample (name = file sans extension), roi 1
  expect_setequal(res$annotations$sample_name,
                  c("photo one", "IMG_4421", "rex"))
  expect_true(all(res$annotations$roi_number == 1L))
  # The real file name (with extension) is preserved
  expect_true("rex.jpeg" %in% res$annotations$file_name)
})

test_that("scan_png_class_folder IFCB mode still parses 5-digit ROIs", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "Diatom"))
  write_test_png(file.path(root, "Diatom", "D20230101T120000_IFCB134_00007.png"))
  suppressWarnings(
    res <- scan_png_class_folder(root)
  )
  expect_equal(nrow(res$annotations), 1)
  expect_equal(res$annotations$sample_name, "D20230101T120000_IFCB134")
  expect_equal(res$annotations$roi_number, 7L)
})

test_that("generic annotations round-trip through SQLite by file name", {
  db_path <- file.path(withr::local_tempdir(), "annotations.sqlite")

  classifications <- data.frame(
    file_name = c("photo one.jpg", "IMG_4421.png", "scan_03.jpeg"),
    class_name = c("Cat", "Dog", "Cat"),
    stringsAsFactors = FALSE
  )

  ok <- save_annotations_db(db_path, "My Image Set", classifications,
                            class2use = c("Cat", "Dog"),
                            annotator = "tester",
                            instrument_type = "generic")
  expect_true(ok)

  # Dimensions keyed by file name (as produced for generic images)
  roi_dims <- data.frame(
    roi_number = c(1L, 2L, 3L),
    file_name = c("photo one.jpg", "IMG_4421.png", "scan_03.jpeg"),
    width = c(50, 10, 30), height = c(10, 10, 10),
    area = c(500, 100, 300), stringsAsFactors = FALSE
  )

  loaded <- load_annotations_db(db_path, "My Image Set", roi_dims)
  expect_equal(nrow(loaded), 3)
  # Arbitrary file names survive the round-trip exactly
  expect_setequal(loaded$file_name,
                  c("photo one.jpg", "IMG_4421.png", "scan_03.jpeg"))
  # Class labels preserved per file
  expect_equal(loaded$class_name[loaded$file_name == "IMG_4421.png"], "Dog")
  # Dimensions matched by file name
  expect_equal(loaded$width[loaded$file_name == "photo one.jpg"], 50)
})

test_that("legacy IFCB rows without file_name reconstruct on load", {
  db_path <- file.path(withr::local_tempdir(), "annotations.sqlite")

  # Create a legacy-shaped annotations table (no file_name column)
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbExecute(con, "CREATE TABLE annotations (
      sample_name TEXT NOT NULL, roi_number INTEGER NOT NULL,
      class_name TEXT NOT NULL, annotator TEXT, timestamp TEXT,
      is_manual INTEGER NOT NULL DEFAULT 1,
      PRIMARY KEY (sample_name, roi_number))")
  DBI::dbExecute(con, "INSERT INTO annotations
      (sample_name, roi_number, class_name) VALUES
      ('D20230101T120000_IFCB134', 1, 'Diatom'),
      ('D20230101T120000_IFCB134', 12, 'Ciliate')")
  DBI::dbDisconnect(con)

  # init_db_schema migration should add the file_name column transparently
  con2 <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  ClassiPyR:::init_db_schema(con2)
  cols <- DBI::dbGetQuery(con2, "PRAGMA table_info(annotations)")
  DBI::dbDisconnect(con2)
  expect_true("file_name" %in% cols$name)

  # ADC-style dimensions (no file_name column) -> match by roi_number, and
  # the legacy NULL file_name is reconstructed via the IFCB convention
  roi_dims <- data.frame(roi_number = c(1L, 12L),
                         width = c(20, 40), height = c(10, 10),
                         area = c(200, 400))
  loaded <- load_annotations_db(db_path, "D20230101T120000_IFCB134", roi_dims)
  expect_setequal(loaded$file_name,
                  c("D20230101T120000_IFCB134_00001.png",
                    "D20230101T120000_IFCB134_00012.png"))
  expect_equal(loaded$width[loaded$file_name ==
                              "D20230101T120000_IFCB134_00012.png"], 40)
})

test_that("end-to-end generic flow keeps labels attached to the right image", {
  # Mirrors the app: discover -> infer dims -> create blank -> annotate ->
  # save -> reload, and checks that arbitrary file names round-trip with the
  # correct labels and dimensions.
  sample_dir <- withr::local_tempdir()
  write_test_png(file.path(sample_dir, "zebra.png"), width = 12, height = 6)
  write_test_png(file.path(sample_dir, "apple.jpg"), width = 20, height = 5)
  write_test_png(file.path(sample_dir, "mango.jpeg"), width = 8, height = 8)

  # Infer ROI dimensions the way the app helper does (generic profile)
  files <- list.files(sample_dir,
                      pattern = ClassiPyR::image_file_pattern("png,jpg,jpeg"),
                      ignore.case = TRUE)
  roi_numbers <- ClassiPyR::assign_roi_numbers(files, "generic")
  dims <- do.call(rbind, lapply(seq_along(files), function(i) {
    d <- ClassiPyR:::read_image_dimensions(file.path(sample_dir, files[i]))
    data.frame(roi_number = roi_numbers[i], file_name = files[i],
               width = d$width, height = d$height,
               area = d$width * d$height, stringsAsFactors = FALSE)
  }))

  # Start from scratch (all unclassified), then annotate two images
  cls <- create_new_classifications("my_sample", dims)
  expect_true(all(cls$class_name == "unclassified"))
  cls$class_name[cls$file_name == "apple.jpg"] <- "Fruit"
  cls$class_name[cls$file_name == "mango.jpeg"] <- "Fruit"

  db_path <- file.path(withr::local_tempdir(), "annotations.sqlite")
  expect_true(save_annotations_db(db_path, "my_sample", cls,
                                  class2use = c("Fruit", "Animal"),
                                  annotator = "u", instrument_type = "generic"))

  loaded <- load_annotations_db(db_path, "my_sample", dims)
  expect_setequal(loaded$file_name, c("zebra.png", "apple.jpg", "mango.jpeg"))
  expect_equal(loaded$class_name[loaded$file_name == "apple.jpg"], "Fruit")
  expect_equal(loaded$class_name[loaded$file_name == "mango.jpeg"], "Fruit")
  expect_equal(loaded$class_name[loaded$file_name == "zebra.png"], "unclassified")
  # Dimensions matched by file name, not by positional ROI number
  expect_equal(loaded$width[loaded$file_name == "apple.jpg"], 20)
  expect_equal(loaded$width[loaded$file_name == "mango.jpeg"], 8)
})

test_that("rescan_file_index treats a flat image folder as a single sample", {
  withr::local_envvar(`_R_CHECK_PACKAGE_NAME_` = "ClassiPyR")

  root <- withr::local_tempdir()
  flat <- file.path(root, "my_images")
  dir.create(flat)
  write_test_png(file.path(flat, "a.png"))
  write_test_png(file.path(flat, "b.png"))

  result <- rescan_file_index(
    roi_folder = flat, csv_folder = flat, output_folder = flat,
    db_folder = withr::local_tempdir(), instrument_type = "generic",
    verbose = FALSE
  )

  expect_true("my_images" %in% result$sample_names)
})

test_that("rescan_file_index ignores saved instrument_type when folders are explicit", {
  # Regression: explicitly-supplied folders must NOT be silently flipped into
  # generic mode by persisted global settings. Route config to a temp dir and
  # plant a settings file that requests generic mode.
  cfg_dir <- withr::local_tempdir()
  withr::local_envvar(R_USER_CONFIG_DIR = cfg_dir)
  withr::local_envvar(`_R_CHECK_PACKAGE_NAME_` = "")  # use R_user_dir, not tempdir()

  # A generic image folder referenced by the saved settings
  gen_root <- withr::local_tempdir()
  dir.create(file.path(gen_root, "slideX"))
  write_test_png(file.path(gen_root, "slideX", "img.png"))
  gen_db <- withr::local_tempdir()

  settings_path <- get_settings_path()
  dir.create(dirname(settings_path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(
    list(instrument_type = "generic", image_extensions = "png, jpg",
         roi_folder = gen_root, csv_folder = gen_root,
         output_folder = gen_root, db_folder = gen_db),
    settings_path, auto_unbox = TRUE
  )

  # IFCB raw data, folders passed explicitly, instrument_type left at default
  root <- withr::local_tempdir()
  raw <- file.path(root, "raw")
  dir.create(raw)
  file.create(file.path(raw, "D20230101T120000_IFCB134.roi"))
  file.create(file.path(raw, "D20230101T120000_IFCB134.adc"))

  result <- rescan_file_index(
    roi_folder = raw, csv_folder = raw, output_folder = raw,
    db_folder = withr::local_tempdir(), verbose = FALSE
  )

  # Stays in IFCB mode: the IFCB sample is discovered with a roi_path_map entry
  expect_identical(result$instrument_type, "IFCB")
  expect_true("D20230101T120000_IFCB134" %in% result$sample_names)
  expect_false(is.null(result$roi_path_map[["D20230101T120000_IFCB134"]]))

  # A no-arg, fully settings-driven rescan still honours the saved generic mode
  result2 <- rescan_file_index(verbose = FALSE)
  expect_identical(result2$instrument_type, "generic")
  expect_true("slideX" %in% result2$sample_names)
})

test_that("rescan_file_index warns on duplicate generic folder names", {
  withr::local_envvar(`_R_CHECK_PACKAGE_NAME_` = "ClassiPyR")

  root <- withr::local_tempdir()
  # Same basename "StationA" under two different parent folders
  dir.create(file.path(root, "2023", "StationA"), recursive = TRUE)
  dir.create(file.path(root, "2024", "StationA"), recursive = TRUE)
  write_test_png(file.path(root, "2023", "StationA", "a.png"))
  write_test_png(file.path(root, "2024", "StationA", "b.png"))

  expect_warning(
    result <- rescan_file_index(
      roi_folder = root, csv_folder = root, output_folder = root,
      db_folder = withr::local_tempdir(), instrument_type = "generic",
      verbose = FALSE
    ),
    "duplicate sample folder name"
  )

  # Only one sample survives the collision
  expect_equal(sum(result$sample_names == "StationA"), 1)
})
