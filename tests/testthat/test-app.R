# Tests for the Shiny app

library(testthat)
library(shiny)

test_that("run_app function exists and is exported", {
  expect_true(is.function(run_app))
})
test_that("inst/app files exist and are valid R", {
  app_dir <- system.file("app", package = "ClassiPyR")
  skip_if(app_dir == "", "Package not installed")

  expect_true(file.exists(file.path(app_dir, "app.R")))
  expect_true(file.exists(file.path(app_dir, "global.R")))
  expect_true(file.exists(file.path(app_dir, "ui.R")))
  expect_true(file.exists(file.path(app_dir, "server.R")))

  # Check they parse without errors
  expect_no_error(parse(file.path(app_dir, "app.R")))
  expect_no_error(parse(file.path(app_dir, "global.R")))
  expect_no_error(parse(file.path(app_dir, "ui.R")))
  expect_no_error(parse(file.path(app_dir, "server.R")))
})

test_that("DESCRIPTION file is valid", {
  desc <- packageDescription("ClassiPyR")

  expect_true(!is.null(desc$Package))
  expect_equal(desc$Package, "ClassiPyR")
  expect_true(!is.null(desc$Version))
  expect_true(!is.null(desc$Title))
})

test_that("required packages are listed in DESCRIPTION", {
  desc <- packageDescription("ClassiPyR")
  imports <- desc$Imports

  expect_true(grepl("shiny", imports))
  expect_true(grepl("shinyjs", imports))
  expect_true(grepl("shinyFiles", imports))
  expect_true(grepl("bslib", imports))
  expect_true(grepl("iRfcb", imports))
  expect_true(grepl("dplyr", imports))
  expect_true(grepl("DT", imports))
  expect_true(grepl("jsonlite", imports))
  expect_false(grepl("reticulate", imports))
})

test_that("app UI can be created without errors", {
  app_dir <- system.file("app", package = "ClassiPyR")
  skip_if(app_dir == "", "Package not installed")

  # Source UI in isolated environment
  app_env <- new.env(parent = globalenv())

  # Load required packages
  library(shiny)
  library(shinyjs)
  library(shinyFiles)
  library(bslib)
  library(DT)

  # Source ui.R
  expect_no_error(source(file.path(app_dir, "ui.R"), local = app_env))
  expect_true(exists("ui", envir = app_env))

  # Verify ui is a valid Shiny UI object
  expect_true(inherits(app_env$ui, "shiny.tag") || inherits(app_env$ui, "shiny.tag.list"))
})

test_that("app server function can be created without errors", {
  app_dir <- system.file("app", package = "ClassiPyR")
  skip_if(app_dir == "", "Package not installed")

  # Source server in isolated environment
  app_env <- new.env(parent = globalenv())

  # Source server.R
  expect_no_error(source(file.path(app_dir, "server.R"), local = app_env))
  expect_true(exists("server", envir = app_env))

  # Verify server is a function
  expect_true(is.function(app_env$server))
})

test_that("run_app errors for non-existent app directory", {
  expect_error(run_app(appDir= "not_an_app_dir"),
               "No Shiny application exists at the path")
})

test_that("class list auto-saves to and restores from DB with save_format 'both'", {
  app_dir <- system.file("app", package = "ClassiPyR")
  skip_if(app_dir == "", "Package not installed")

  module_env <- new.env(parent = globalenv())
  source(file.path(app_dir, "modules", "class_list_loading_server.R"),
         local = module_env)

  db_folder <- tempfile("classipyr_db_")
  dir.create(db_folder)
  on.exit(unlink(db_folder, recursive = TRUE), add = TRUE)

  make_server <- function() {
    function(input, output, session) {
      rv <- reactiveValues(class2use = NULL, class2use_path = NULL)
      config <- reactiveValues(save_format = "both", db_folder = db_folder)
      module_env$setup_class_list_loading_server(
        input, output, session, rv, config,
        saved_settings = list(),
        persist_settings = function(settings) invisible(NULL),
        update_month_choices = function() invisible(NULL),
        update_sample_list = function() invisible(NULL)
      )
    }
  }

  classes <- c("unclassified", "Diatoma", "Ciliophora")

  # The auto-save observer must fire when save_format is "both"
  shiny::testServer(make_server(), {
    rv$class2use <- classes
    session$flushReact()
  })

  db_path <- get_db_path(db_folder)
  expect_true(file.exists(db_path))
  expect_setequal(load_global_class_list_db(db_path), classes)

  # The startup restore observer must also fire: a fresh session picks the
  # class list up from the database
  shiny::testServer(make_server(), {
    session$flushReact()
    expect_setequal(rv$class2use, classes)
  })
})

test_that("Save button saves PNG-only samples to SQLite (no ROI file needed)", {
  app_dir <- system.file("app", package = "ClassiPyR")
  skip_if(app_dir == "", "Package not installed")

  module_env <- new.env(parent = globalenv())
  source(file.path(app_dir, "modules", "helpers_shared.R"), local = module_env)
  source(file.path(app_dir, "modules", "manual_save_server.R"),
         local = module_env)

  base_dir <- tempfile("classipyr_pngsave_")
  dir.create(base_dir)
  on.exit(unlink(base_dir, recursive = TRUE), add = TRUE)
  db_folder <- file.path(base_dir, "db")
  png_dir <- file.path(base_dir, "png", "D20230101T120000_IFCB134")
  dir.create(png_dir, recursive = TRUE)

  sample_name <- "D20230101T120000_IFCB134"
  cls <- data.frame(
    file_name = paste0(sample_name, "_", sprintf("%05d", 1:2), ".png"),
    class_name = c("Diatoma", "unclassified"),
    score = NA_real_,
    width = c(10, 20), height = c(10, 20), roi_area = c(100, 400),
    stringsAsFactors = FALSE
  )
  orig <- cls
  orig$class_name <- c("unclassified", "unclassified")
  changes <- data.frame(
    image = cls$file_name[1],
    original_class = "unclassified",
    new_class = "Diatoma",
    stringsAsFactors = FALSE
  )

  server_fn <- function(input, output, session) {
    rv <- reactiveValues(
      current_sample = sample_name,
      classifications = cls,
      original_classifications = orig,
      changes_log = changes,
      class2use = c("unclassified", "Diatoma"),
      class2use_path = NULL,
      temp_png_folder = dirname(png_dir),
      is_annotation_mode = TRUE,
      class_review_mode = FALSE,
      is_loading = FALSE
    )
    config <- reactiveValues(
      save_format = "sqlite",
      data_source = "local",
      db_folder = db_folder,
      output_folder = file.path(base_dir, "out"),
      png_output_folder = file.path(base_dir, "png_out"),
      roi_folder = base_dir,
      export_statistics = FALSE
    )
    module_env$setup_manual_save_server(
      input, output, session, rv, config,
      roi_path_map = reactiveVal(list()),  # PNG-only: no ROI files known
      annotated_samples = reactiveVal(character()),
      disable_nav_buttons = function() invisible(NULL),
      enable_nav_buttons = function() invisible(NULL),
      update_current_sample_status_fn = function(sample_name) invisible(NULL),
      find_sample_png_dir = function(sample_name) png_dir
    )
  }

  shiny::testServer(server_fn, {
    session$setInputs(annotator_name = "Tester", save_btn = 1)
  })

  db_path <- get_db_path(db_folder)
  expect_true(file.exists(db_path))
  dims <- data.frame(roi_number = 1:2, width = c(10, 20), height = c(10, 20),
                     area = c(100, 400), stringsAsFactors = FALSE)
  saved <- load_annotations_db(db_path, sample_name, dims)
  expect_true(!is.null(saved) && nrow(saved) == 2)
})

test_that("gallery handles an empty image list without a phantom row", {
  app_dir <- system.file("app", package = "ClassiPyR")
  skip_if(app_dir == "", "Package not installed")

  # The gallery module uses bare dplyr verbs (attached by global.R in the app)
  suppressPackageStartupMessages(library(dplyr))

  module_env <- new.env(parent = globalenv())
  source(file.path(app_dir, "modules", "gallery_server.R"), local = module_env)

  png_dir <- tempfile("classipyr_gallery_")
  dir.create(png_dir)
  on.exit(unlink(png_dir, recursive = TRUE), add = TRUE)

  empty_cls <- data.frame(
    file_name = character(),
    class_name = character(),
    score = numeric(),
    width = numeric(), height = numeric(), roi_area = numeric(),
    stringsAsFactors = FALSE
  )

  server_fn <- function(input, output, session) {
    rv <- reactiveValues(
      classifications = empty_cls,
      original_classifications = empty_cls,
      current_sample = "D20230101T120000_IFCB134",
      temp_png_folder = png_dir,
      is_annotation_mode = FALSE,
      class_review_mode = FALSE,
      selected_images = character(),
      current_page = 1,
      select_all_state = "first"
    )
    module_env$setup_gallery_server(input, output, session, rv)
  }

  shiny::testServer(server_fn, {
    session$setInputs(class_filter = "all", images_per_page = "100")
    # Before the fix, images[1:0, ] produced a 1-row all-NA frame, the
    # "No images to display" branch was unreachable, and page_info printed
    # "Page 1/1 (1-0 of 0)"
    expect_match(output$page_info, "0-0 of 0", fixed = TRUE)
    expect_match(as.character(output$image_gallery$html), "No images to display",
                 fixed = TRUE)
  })
})

# ---------------------------------------------------------------------------
# Whole-server tests against the bundled test sample
# ---------------------------------------------------------------------------

# The complete app server, with its settings pointing at a scratch copy of
# the test data. The config directory is redirected to the session temp
# folder, so the settings of the person running the tests are never touched.
local_app_server <- function(save_format = "sqlite", env = parent.frame()) {
  app_dir <- system.file("app", package = "ClassiPyR")
  sample_name <- "D20220522T000439_IFCB134"
  raw_dir <- testthat::test_path("test_data", "raw")
  h5_file <- testthat::test_path("test_data", paste0(sample_name, "_class.h5"))

  base <- tempfile("classipyr_app_")
  dir.create(file.path(base, "classified"), recursive = TRUE)
  file.copy(h5_file, file.path(base, "classified"))

  old_env <- Sys.getenv("_R_CHECK_PACKAGE_NAME_", unset = NA)
  Sys.setenv("_R_CHECK_PACKAGE_NAME_" = "ClassiPyR")
  old_files <- c(get_settings_path(), get_file_index_path())
  backup <- file.path(base, paste0("backup_", basename(old_files)))
  had <- file.exists(old_files)
  file.copy(old_files[had], backup[had])

  do.call(on.exit, list(substitute({
    unlink(old_files)
    file.copy(backup[had], old_files[had])
    if (is.na(old_env)) {
      Sys.unsetenv("_R_CHECK_PACKAGE_NAME_")
    } else {
      Sys.setenv("_R_CHECK_PACKAGE_NAME_" = old_env)
    }
    unlink(base, recursive = TRUE)
  }, list(old_files = old_files, backup = backup, had = had,
          old_env = old_env, base = base)), add = TRUE), envir = env)

  unlink(get_file_index_path())
  jsonlite::write_json(list(
    roi_folder = normalizePath(raw_dir),
    csv_folder = file.path(base, "classified"),
    output_folder = file.path(base, "manual"),
    png_output_folder = file.path(base, "png_out"),
    db_folder = file.path(base, "db"),
    save_format = save_format,
    export_statistics = FALSE
  ), get_settings_path(), auto_unbox = TRUE)

  suppressPackageStartupMessages({
    library(shinyjs); library(shinyFiles); library(bslib); library(iRfcb)
    library(dplyr); library(DT); library(jsonlite); library(DBI); library(RSQLite)
  })
  app_env <- new.env(parent = globalenv())
  source(file.path(app_dir, "global.R"), local = app_env)
  source(file.path(app_dir, "server.R"), local = app_env)

  list(server = app_env$server, sample_name = sample_name, base = base,
       db_path = get_db_path(file.path(base, "db")))
}

app_test_data_available <- function() {
  nzchar(system.file("app", package = "ClassiPyR")) &&
    file.exists(testthat::test_path(
      "test_data", "raw", "2022", "D20220522", "D20220522T000439_IFCB134.roi"
    )) &&
    requireNamespace("hdf5r", quietly = TRUE)
}

test_that("a sample can be validated, saved unchanged and switched between modes", {
  skip_if_not(app_test_data_available(), "App or test data not available")
  app <- local_app_server()
  sample_name <- app$sample_name

  shiny::testServer(app$server, {
    session$setInputs(annotator_name = "Tester", app_mode = "sample",
                      class_filter = "all", images_per_page = "100",
                      sample_status_filter = "all")
    session$flushReact()
    session$setInputs(sample_select = sample_name, load_sample = 1)

    # Validation mode from the H5 file, only ROIs that have an image
    expect_equal(rv$current_sample, sample_name)
    expect_false(rv$is_annotation_mode)
    n_images <- nrow(rv$classifications)
    expect_gt(n_images, 0)
    png_dir <- file.path(rv$temp_png_folder, sample_name)
    expect_setequal(rv$classifications$file_name, list.files(png_dir))

    # Saving without any correction records the sample as reviewed
    session$setInputs(save_btn = 1)
    expect_true(ClassiPyR:::sample_has_annotations_db(app$db_path, sample_name))
    expect_true(sample_name %in% annotated_samples())

    # A correction made in validation mode survives the switch to manual mode
    first <- rv$classifications$file_name[1]
    session$setInputs(toggle_image = list(img = first, time = 1))
    session$setInputs(new_class_quick = "Corrected_class", relabel_quick = 1)
    expect_equal(nrow(rv$changes_log), 1)
    session$setInputs(switch_to_annotation = 1)

    expect_true(rv$is_annotation_mode)
    expect_equal(nrow(rv$classifications), n_images)
    expect_equal(
      rv$classifications$class_name[rv$classifications$file_name == first],
      "Corrected_class"
    )
    expect_equal(nrow(rv$changes_log), 0)

    # ... and one made in manual mode survives the switch back
    second <- rv$classifications$file_name[2]
    session$setInputs(toggle_image = list(img = second, time = 2))
    session$setInputs(new_class_quick = "Manual_class", relabel_quick = 2)
    session$setInputs(switch_to_validation = 1)
    expect_false(rv$is_annotation_mode)
    expect_equal(nrow(rv$classifications), n_images)
  })

  dims <- read_roi_dimensions(testthat::test_path(
    "test_data", "raw", "2022", "D20220522", paste0(sample_name, ".adc")
  ))
  saved <- load_annotations_db(app$db_path, sample_name, dims)
  expect_true("Corrected_class" %in% saved$class_name)
  expect_true("Manual_class" %in% saved$class_name)
  # The relabeled images moved in the PNG output, they were not duplicated
  png_out <- list.files(file.path(app$base, "png_out"), recursive = TRUE)
  expect_equal(anyDuplicated(basename(png_out)), 0L)
})

test_that("class review changes are kept when the review is left", {
  skip_if_not(app_test_data_available(), "App or test data not available")
  app <- local_app_server()
  sample_name <- app$sample_name

  shiny::testServer(app$server, {
    session$setInputs(annotator_name = "Tester", app_mode = "sample",
                      class_filter = "all", images_per_page = "100",
                      sample_status_filter = "all")
    session$flushReact()
    session$setInputs(sample_select = sample_name, load_sample = 1)
    all_files <- rv$classifications$file_name
    session$setInputs(drag_select = list(images = all_files, time = 1))
    session$setInputs(new_class_quick = "Review_me", relabel_quick = 1)
    session$setInputs(save_btn = 1)

    session$setInputs(class_review_source = "database", cr_year_select = "all",
                      cr_month_select = "all", cr_instrument_select = "all",
                      cr_annotator_select = "all")
    session$setInputs(app_mode = "class_review")
    session$setInputs(class_review_select = "Review_me", load_class_review = 1)
    expect_true(rv$class_review_mode)
    expect_setequal(rv$classifications$file_name, all_files)

    session$setInputs(toggle_image = list(img = all_files[1], time = 2))
    session$setInputs(new_class_quick = "Moved", relabel_quick = 2)
    # Leaving without pressing "Save Changes"
    session$setInputs(app_mode = "sample")
    expect_false(rv$class_review_mode)
  })

  classes <- list_classes_db(app$db_path)
  expect_equal(classes$count[classes$class_name == "Moved"], 1)
})

test_that("a MAT save works with a class list that was never loaded from a file", {
  skip_if_not(app_test_data_available(), "App or test data not available")
  app <- local_app_server(save_format = "both")
  sample_name <- app$sample_name

  shiny::testServer(app$server, {
    session$setInputs(annotator_name = "Tester", app_mode = "sample",
                      class_filter = "all", images_per_page = "100",
                      sample_status_filter = "all")
    session$flushReact()
    session$setInputs(sample_select = sample_name, load_sample = 1)
    rv$class2use <- c("unclassified", "Added_in_app")
    expect_null(rv$class2use_path)

    all_files <- rv$classifications$file_name
    session$setInputs(drag_select = list(images = all_files, time = 1))
    session$setInputs(new_class_quick = "Added_in_app", relabel_quick = 1)
    session$setInputs(save_btn = 1)
  })

  mat_file <- file.path(app$base, "manual", paste0(sample_name, ".mat"))
  expect_true(file.exists(mat_file))
  classlist <- iRfcb::ifcb_get_mat_variable(mat_file, variable_name = "classlist")
  rois <- as.integer(sub(".*_(\\d+)\\.png$", "\\1", list.files(
    testthat::test_path("test_data", "png", sample_name)
  )))
  expect_equal(unique(classlist[rois, 2]), 2)

  # The SQLite -> .mat export of the same sample puts the classes on the
  # same ROIs as the direct MAT save
  export_dir <- file.path(app$base, "export")
  expect_true(export_db_to_mat(app$db_path, sample_name, export_dir))
  exported <- iRfcb::ifcb_get_mat_variable(
    file.path(export_dir, paste0(sample_name, ".mat")),
    variable_name = "classlist"
  )
  expect_equal(exported[rois, 2], classlist[rois, 2])
})

test_that("the annotator name is remembered between sessions", {
  skip_if_not(app_test_data_available(), "App or test data not available")
  app <- local_app_server()
  saved_name <- function() jsonlite::fromJSON(get_settings_path())$annotator_name

  shiny::testServer(app$server, {
    # Without a saved name the system user is the default
    expect_equal(saved_settings$annotator_name, Sys.info()[["user"]])

    # The browser sends the field's initial value when the session starts
    session$setInputs(annotator_name = Sys.info()[["user"]])
    session$elapse(2000)
    expect_null(saved_name())

    session$setInputs(annotator_name = "Ada")
    session$elapse(500)
    # Typing does not write the file for every keystroke
    expect_null(saved_name())
    session$setInputs(annotator_name = "Ada Lovelace")
    session$elapse(2000)
    expect_equal(saved_name(), "Ada Lovelace")

    # An emptied field does not replace the saved name
    session$setInputs(annotator_name = "  ")
    session$elapse(2000)
    expect_equal(saved_name(), "Ada Lovelace")
  })

  # A second session starts from the saved name
  shiny::testServer(app$server, {
    expect_equal(saved_settings$annotator_name, "Ada Lovelace")
  })
})

test_that("an unusable saved annotator name falls back to the system user", {
  module_env <- new.env(parent = globalenv())
  source(system.file("app", "modules", "helpers_shared.R", package = "ClassiPyR"),
         local = module_env)
  user <- Sys.info()[["user"]]
  expect_equal(module_env$startup_annotator_name(list()), user)
  expect_equal(module_env$startup_annotator_name(list(annotator_name = " ")), user)
  expect_equal(module_env$startup_annotator_name(list(annotator_name = 42)), "42")
  expect_true(module_env$is_storable_annotator("Ada"))
  expect_false(module_env$is_storable_annotator(""))
  expect_false(module_env$is_storable_annotator(NULL))
  expect_false(module_env$is_storable_annotator(NA_character_))
})

# A class folder holding two images of the bundled sample, as a partial PNG
# import would have
local_partial_png_folder <- function(sample_name, env = parent.frame()) {
  png_folder <- tempfile("classipyr_png_import_")
  dir.create(file.path(png_folder, "Diatom"), recursive = TRUE)
  do.call(on.exit, list(substitute(unlink(png_folder, recursive = TRUE),
                                   list(png_folder = png_folder)), add = TRUE),
          envir = env)
  images <- list.files(testthat::test_path("test_data", "png", sample_name),
                       full.names = TRUE)
  file.copy(images[1:2], file.path(png_folder, "Diatom"))
  png_folder
}

# Runs the "PNG -> SQLite" dialogs of the app and returns the classes stored
run_app_png_import <- function(app, png_folder, fill) {
  shiny::testServer(app$server, {
    session$setInputs(annotator_name = "Tester", app_mode = "sample",
                      sample_status_filter = "all")
    session$flushReact()
    session$setInputs(import_png_to_db_btn = 1)
    session$setInputs(cfg_png_import_folder = png_folder,
                      png_import_fill_unclassified = fill)
    session$setInputs(scan_png_folder_btn = 1)
    # "Diatom" is not in the class list: add it as a new class
    session$setInputs(confirm_png_class_mapping_btn = 1)
  })
  con <- DBI::dbConnect(RSQLite::SQLite(), app$db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbGetQuery(con, "SELECT roi_number, class_name, annotator, is_manual
                        FROM annotations ORDER BY roi_number")
}

test_that("the PNG import leaves the other ROIs out unless asked to add them", {
  skip_if_not(app_test_data_available(), "App or test data not available")
  app <- local_app_server()
  png_folder <- local_partial_png_folder(app$sample_name)

  rows <- run_app_png_import(app, png_folder, fill = FALSE)
  expect_equal(rows$class_name, c("Diatom", "Diatom"))
})

test_that("the PNG import can add the remaining ROIs as unclassified", {
  skip_if_not(app_test_data_available(), "App or test data not available")
  app <- local_app_server()
  png_folder <- local_partial_png_folder(app$sample_name)

  rows <- run_app_png_import(app, png_folder, fill = TRUE)

  dims <- read_roi_dimensions(testthat::test_path(
    "test_data", "raw", "2022", "D20220522", paste0(app$sample_name, ".adc")
  ))
  with_image <- dims$roi_number[dims$width > 0 & dims$height > 0]
  expect_setequal(rows$roi_number, with_image)
  expect_equal(sum(rows$class_name == "Diatom"), 2)
  filled <- rows[rows$class_name == "unclassified", ]
  expect_equal(nrow(filled), length(with_image) - 2)
  expect_true(all(filled$is_manual == 0))
  expect_true(all(filled$annotator == "Tester"))
})

test_that("the PNG import does not fill from raw data in dashboard mode", {
  module_env <- new.env(parent = globalenv())
  source(system.file("app", "modules", "helpers_shared.R", package = "ClassiPyR"),
         local = module_env)
  expect_true(module_env$can_fill_unclassified("local"))
  expect_false(module_env$can_fill_unclassified("dashboard"))
})

test_that("the PNG import notification reports what was filled", {
  module_env <- new.env(parent = globalenv())
  source(system.file("app", "modules", "helpers_shared.R", package = "ClassiPyR"),
         local = module_env)
  import <- list(success = 3L, failed = 0L)

  plain <- module_env$png_import_summary(import, filled = NULL)
  expect_match(plain, "3 samples imported, 0 failed", fixed = TRUE)
  expect_no_match(plain, "unclassified")

  filled <- module_env$png_import_summary(
    import, filled = list(added = 40L, samples = 2L, skipped = 1L)
  )
  expect_match(filled, "40 remaining ROIs added as unclassified in 2 samples",
               fixed = TRUE)
  expect_match(filled, "1 sample skipped (raw data not found)", fixed = TRUE)

  none_skipped <- module_env$png_import_summary(
    import, filled = list(added = 40L, samples = 3L, skipped = 0L)
  )
  expect_no_match(none_skipped, "skipped")
})

test_that("a {sample}_class.csv label file is used for validation", {
  skip_if_not(app_test_data_available(), "App or test data not available")
  app <- local_app_server()
  sample_name <- app$sample_name
  images <- list.files(testthat::test_path("test_data", "png", sample_name))
  write.csv(
    data.frame(file_name = images, class_name = "From_class_csv", score = 0.8),
    file.path(app$base, "classified", paste0(sample_name, "_class.csv")),
    row.names = FALSE
  )

  shiny::testServer(app$server, {
    session$setInputs(annotator_name = "Tester", app_mode = "sample",
                      class_filter = "all", images_per_page = "100",
                      sample_status_filter = "all")
    session$flushReact()
    expect_true(sample_name %in% classified_samples())
    session$setInputs(sample_select = sample_name, load_sample = 1)

    expect_false(rv$is_annotation_mode)
    expect_setequal(rv$classifications$file_name, images)
    expect_equal(unique(rv$classifications$class_name), "From_class_csv")
  })
})

# --- Renaming a class across the database ---------------------------------

rename_module_env <- function() {
  env <- new.env(parent = globalenv())
  source(system.file("app", "modules", "class_rename_server.R", package = "ClassiPyR"),
         local = env)
  env
}

test_that("a rename is a different name at the same position of the class list", {
  env <- rename_module_env()
  old <- c("unclassified", "Diatom", "Ciliate", "Dino")

  renames <- env$detect_class_renames(old, c("unclassified", "Diatoms", "Ciliate", "Dinos"))
  expect_equal(renames$old_name, c("Diatom", "Dino"))
  expect_equal(renames$new_name, c("Diatoms", "Dinos"))

  # Not renames: nothing changed, a class added or removed, two classes
  # swapped, or a name replaced by one that is elsewhere in the list
  expect_equal(nrow(env$detect_class_renames(old, old)), 0)
  expect_equal(nrow(env$detect_class_renames(old, c(old, "New"))), 0)
  expect_equal(nrow(env$detect_class_renames(old, old[-2])), 0)
  expect_equal(nrow(env$detect_class_renames(old, old[c(1, 3, 2, 4)])), 0)
  expect_equal(nrow(env$detect_class_renames(NULL, old)), 0)
})

test_that("class names in memory follow a rename", {
  env <- rename_module_env()
  renames <- data.frame(old_name = "Diatom", new_name = "Diatoms",
                        stringsAsFactors = FALSE)

  expect_equal(env$rename_classes(c("Diatom", "Ciliate", "Diatom"), renames),
               c("Diatoms", "Ciliate", "Diatoms"))
  expect_null(env$rename_classes(NULL, renames))

  classifications <- data.frame(file_name = c("a.png", "b.png"),
                                class_name = c("Diatom", "Ciliate"),
                                stringsAsFactors = FALSE)
  renamed <- env$rename_in_classifications(classifications, renames)
  expect_equal(renamed$class_name, c("Diatoms", "Ciliate"))
  expect_equal(classifications$class_name, c("Diatom", "Ciliate"))
  expect_null(env$rename_in_classifications(NULL, renames))

  log <- data.frame(image = "a.png", original_class = "Diatom",
                    new_class = "Ciliate", stringsAsFactors = FALSE)
  expect_equal(env$rename_in_changes_log(log, renames)$original_class, "Diatoms")

  entry <- list(classifications = classifications,
                original_classifications = classifications,
                changes_log = log, saved = TRUE)
  renamed_entry <- env$rename_in_cache_entry(entry, renames)
  expect_equal(renamed_entry$classifications$class_name, c("Diatoms", "Ciliate"))
  expect_equal(renamed_entry$original_classifications$class_name,
               c("Diatoms", "Ciliate"))
  expect_equal(renamed_entry$changes_log$original_class, "Diatoms")
  expect_true(renamed_entry$saved)
})

test_that("the rename plan counts annotations and flags merges", {
  env <- rename_module_env()
  renames <- data.frame(old_name = c("Diatom", "Dino", "Rare"),
                        new_name = c("Diatoms", "Ciliate", "Rarer"),
                        stringsAsFactors = FALSE)
  counts <- data.frame(class_name = c("Ciliate", "Diatom", "Dino"),
                       count = c(7L, 3L, 2L), stringsAsFactors = FALSE)

  plan <- env$class_rename_plan(renames, counts)
  expect_equal(plan$annotations, c(3, 2, 0))
  expect_equal(plan$target_annotations, c(0, 7, 0))
  expect_equal(plan$merge, c(FALSE, TRUE, FALSE))
  expect_true(env$rename_needs_confirmation(plan))
  expect_false(env$rename_needs_confirmation(plan[3, ]))
})

test_that("WoRMS renames skip names that are already in the class list", {
  env <- rename_module_env()
  matched <- data.frame(
    class_name = c("Skeleto", "Chaeto", "Chaeto_chain", "Dino", "Gone", "Same"),
    accepted_name = c("Skeletonema", "Chaetoceros", "Chaetoceros", "Ciliate",
                      "Elsewhere", "Same"),
    stringsAsFactors = FALSE
  )
  classes <- c("unclassified", "Skeleto", "Chaeto", "Chaeto_chain", "Dino",
               "Ciliate", "Same")

  # A name wanted by a class that is not in the list does not block others
  matched <- rbind(
    data.frame(class_name = "Absent", accepted_name = "Skeletonema",
               stringsAsFactors = FALSE),
    matched
  )

  found <- env$worms_rename_candidates(matched, classes)
  expect_equal(found$renames$old_name, c("Skeleto", "Chaeto"))
  expect_equal(found$renames$new_name, c("Skeletonema", "Chaetoceros"))
  # Chaeto_chain (name taken by the rename before it) and Dino (name in list)
  expect_equal(found$conflicts, 2)
})

test_that("renaming a class in the editor renames it in the database after confirmation", {
  skip_if_not(app_test_data_available(), "App or test data not available")
  app <- local_app_server()
  sample_name <- app$sample_name
  class_counts <- function() {
    counts <- list_classes_db(app$db_path)
    setNames(counts$count, counts$class_name)
  }

  shiny::testServer(app$server, {
    session$setInputs(annotator_name = "Tester", app_mode = "sample",
                      class_filter = "all", images_per_page = "100",
                      sample_status_filter = "all")
    session$flushReact()
    session$setInputs(class_list_edit = "unclassified\nAlpha\nBeta",
                      apply_class_changes = 1)
    expect_equal(rv$class2use, c("unclassified", "Alpha", "Beta"))

    session$setInputs(sample_select = sample_name, load_sample = 1)
    images <- rv$classifications$file_name
    session$setInputs(toggle_image = list(img = images[1], time = 1))
    session$setInputs(new_class_quick = "Alpha", relabel_quick = 1)
    session$setInputs(save_btn = 1)
    expect_equal(class_counts()[["Alpha"]], 1)

    # An unsaved correction, to check that work in progress follows the rename
    session$setInputs(toggle_image = list(img = images[2], time = 2))
    session$setInputs(new_class_quick = "Alpha", relabel_quick = 2)

    # Nothing is renamed until the confirmation
    session$setInputs(class_list_edit = "unclassified\nAlpha_new\nBeta",
                      apply_class_changes = 2)
    expect_equal(rv$class2use, c("unclassified", "Alpha", "Beta"))
    expect_equal(class_counts()[["Alpha"]], 1)

    session$setInputs(confirm_class_rename = 1)
    expect_equal(rv$class2use, c("unclassified", "Alpha_new", "Beta"))
    counts <- class_counts()
    expect_equal(counts[["Alpha_new"]], 1)
    expect_false("Alpha" %in% names(counts))
    expect_equal(load_global_class_list_db(app$db_path),
                 c("unclassified", "Alpha_new", "Beta"))

    # The loaded sample, its baseline and its unsaved changes use the new name
    expect_equal(rv$classifications$class_name[1:2], c("Alpha_new", "Alpha_new"))
    expect_false("Alpha" %in% rv$original_classifications$class_name)
    expect_false("Alpha" %in% c(rv$changes_log$new_class,
                                rv$changes_log$original_class))
    expect_true("Alpha_new" %in% rv$changes_log$new_class)
  })
})

test_that("a rename onto a class with annotations is a merge that must be ticked", {
  skip_if_not(app_test_data_available(), "App or test data not available")
  app <- local_app_server()
  sample_name <- app$sample_name
  class_counts <- function() {
    counts <- list_classes_db(app$db_path)
    setNames(counts$count, counts$class_name)
  }

  shiny::testServer(app$server, {
    session$setInputs(annotator_name = "Tester", app_mode = "sample",
                      class_filter = "all", images_per_page = "100",
                      sample_status_filter = "all")
    session$flushReact()
    session$setInputs(class_list_edit = "unclassified\nAlpha\nBeta",
                      apply_class_changes = 1)
    session$setInputs(sample_select = sample_name, load_sample = 1)
    images <- rv$classifications$file_name
    session$setInputs(toggle_image = list(img = images[1], time = 1))
    session$setInputs(new_class_quick = "Alpha", relabel_quick = 1)
    session$setInputs(toggle_image = list(img = images[2], time = 2))
    session$setInputs(new_class_quick = "Beta", relabel_quick = 2)
    session$setInputs(save_btn = 1)
    before <- class_counts()

    # Dropping "Beta" from the list is not a rename: its annotation stays
    session$setInputs(class_list_edit = "unclassified\nAlpha",
                      apply_class_changes = 2)
    expect_equal(rv$class2use, c("unclassified", "Alpha"))
    expect_equal(class_counts(), before)

    # "Alpha" -> "Beta" would merge it into the annotations "Beta" still has
    session$setInputs(class_list_edit = "unclassified\nBeta",
                      apply_class_changes = 3)
    session$setInputs(confirm_class_merge = FALSE, confirm_class_rename = 1)
    expect_equal(rv$class2use, c("unclassified", "Alpha"))
    expect_equal(class_counts(), before)

    session$setInputs(confirm_class_merge = TRUE, confirm_class_rename = 2)
    expect_equal(rv$class2use, c("unclassified", "Beta"))
    counts <- class_counts()
    expect_equal(counts[["Beta"]], 2)
    expect_false("Alpha" %in% names(counts))
    expect_equal(sum(counts), sum(before))
  })
})

test_that("WoRMS renames go through the database and skip names in use", {
  skip_if_not(app_test_data_available(), "App or test data not available")
  app <- local_app_server()
  sample_name <- app$sample_name
  class_counts <- function() {
    counts <- list_classes_db(app$db_path)
    setNames(counts$count, counts$class_name)
  }
  taxonomy <- function() {
    con <- DBI::dbConnect(RSQLite::SQLite(), app$db_path)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    DBI::dbGetQuery(con, "SELECT class_name, aphia_id FROM class_taxonomy")
  }

  shiny::testServer(app$server, {
    session$setInputs(annotator_name = "Tester", app_mode = "sample",
                      class_filter = "all", images_per_page = "100",
                      sample_status_filter = "all")
    session$flushReact()
    session$setInputs(class_list_edit = "unclassified\nAlpha\nBeta\nGamma",
                      apply_class_changes = 1)
    session$setInputs(sample_select = sample_name, load_sample = 1)
    images <- rv$classifications$file_name
    session$setInputs(toggle_image = list(img = images[1], time = 1))
    session$setInputs(new_class_quick = "Alpha", relabel_quick = 1)
    session$setInputs(toggle_image = list(img = images[2], time = 2))
    session$setInputs(new_class_quick = "Beta", relabel_quick = 2)
    session$setInputs(save_btn = 1)
    in_use <- setdiff(names(class_counts()), c("Alpha", "Beta"))[1]

    # Alpha gets a new name, Beta's accepted name already has annotations
    # (from the classifier), Gamma's is already in the class list
    rv$worms_matches <- data.frame(
      class_name = c("Alpha", "Beta", "Gamma"),
      aphia_id = c("101", "102", "103"),
      accepted_name = c("Alphaceae", in_use, "Alpha"),
      stringsAsFactors = FALSE
    )
    session$setInputs(worms_apply_renames = TRUE, apply_worms_matches = 1)
    expect_equal(rv$class2use, c("unclassified", "Alpha", "Beta", "Gamma"))

    session$setInputs(confirm_class_rename = 1)
    expect_equal(rv$class2use, c("unclassified", "Alphaceae", "Beta", "Gamma"))
    counts <- class_counts()
    expect_equal(counts[["Alphaceae"]], 1)
    expect_equal(counts[["Beta"]], 1)
    expect_false("Alpha" %in% names(counts))
    expect_equal(rv$classifications$class_name[1:2], c("Alphaceae", "Beta"))

    stored <- taxonomy()
    expect_equal(stored$aphia_id[stored$class_name == "Alphaceae"], "101")
    expect_equal(stored$aphia_id[stored$class_name == "Beta"], "102")
    expect_false("Alpha" %in% stored$class_name)
  })
})
