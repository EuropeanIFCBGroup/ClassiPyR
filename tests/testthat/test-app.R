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
  expect_true(grepl("reticulate", imports))
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
