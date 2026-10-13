# Tests for the launcher of the threshold review app

test_that("run_threshold_app is a function taking launch.browser and ...", {
  expect_true(is.function(run_threshold_app))
  expect_named(formals(run_threshold_app), c("launch.browser", "..."))
  expect_true(formals(run_threshold_app)$launch.browser)
})

test_that("run_threshold_app runs the app directory of the package", {
  called <- NULL
  local_mocked_bindings(
    runApp = function(appDir, ...) {
      called <<- list(appDir = appDir, args = list(...))
      invisible("ran")
    },
    .package = "shiny"
  )
  local_mocked_bindings(thr_hdf5r_available = function() TRUE)
  old <- options(ClassiPyR.startup_wd = NULL)
  on.exit(options(old), add = TRUE)

  result <- run_threshold_app(launch.browser = FALSE, port = 7462)

  expect_equal(result, "ran")
  expect_equal(called$appDir,
               system.file("threshold-app", package = "ClassiPyR"))
  expect_true(file.exists(file.path(called$appDir, "app.R")))
  expect_false(called$args$launch.browser)
  expect_equal(called$args$port, 7462)
  # The working directory is remembered for the folder browser, as in run_app()
  expect_equal(getOption("ClassiPyR.startup_wd"), getwd())
})

test_that("run_threshold_app opens a browser by default", {
  called <- NULL
  local_mocked_bindings(
    runApp = function(appDir, ...) called <<- list(...),
    .package = "shiny"
  )
  local_mocked_bindings(thr_hdf5r_available = function() TRUE)
  old <- options(ClassiPyR.startup_wd = NULL)
  on.exit(options(old), add = TRUE)

  run_threshold_app()

  expect_true(called$launch.browser)
})

test_that("run_threshold_app stops with an install hint without hdf5r", {
  ran <- FALSE
  local_mocked_bindings(
    runApp = function(appDir, ...) ran <<- TRUE,
    .package = "shiny"
  )
  local_mocked_bindings(thr_hdf5r_available = function() FALSE)

  expect_error(
    run_threshold_app(launch.browser = FALSE),
    "Package 'hdf5r' is required.*install.packages\\('hdf5r'\\)"
  )
  expect_false(ran)
})

test_that("run_threshold_app stops when the app directory is missing", {
  ran <- FALSE
  local_mocked_bindings(
    runApp = function(appDir, ...) ran <<- TRUE,
    .package = "shiny"
  )
  local_mocked_bindings(
    thr_hdf5r_available = function() TRUE,
    thr_app_dir = function() ""
  )

  expect_error(run_threshold_app(launch.browser = FALSE), "re-installing")
  expect_false(ran)
})

test_that("run_threshold_app declares itself experimental", {
  # lifecycle::signal_stage() signals nothing in current lifecycle releases,
  # so the declaration can only be checked in the code
  code <- paste(deparse(body(run_threshold_app)), collapse = " ")

  expect_match(code, "signal_stage(\"experimental\", \"run_threshold_app()\")",
               fixed = TRUE)
})

test_that("thr_hdf5r_available reports whether hdf5r can be loaded", {
  expect_equal(thr_hdf5r_available(),
               requireNamespace("hdf5r", quietly = TRUE))
})

test_that("thr_app_dir points at the app shipped with the package", {
  expect_equal(thr_app_dir(),
               system.file("threshold-app", package = "ClassiPyR"))
})
