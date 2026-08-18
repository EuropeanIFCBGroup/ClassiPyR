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
