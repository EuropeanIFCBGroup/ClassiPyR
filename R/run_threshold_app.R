# Launch the threshold review Shiny application

#' Is the hdf5r package available?
#'
#' A function of its own so that the launcher's behaviour without
#' \pkg{hdf5r} can be tested on a machine that has it.
#'
#' @return \code{TRUE} or \code{FALSE}.
#' @noRd
thr_hdf5r_available <- function() {
  requireNamespace("hdf5r", quietly = TRUE)
}

#' Directory of the threshold review app inside the installed package
#'
#' @return The path, or \code{""} when the package holds no such directory.
#' @noRd
thr_app_dir <- function() {
  system.file("threshold-app", package = "ClassiPyR")
}

#' Run the threshold review application
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Launches a second Shiny app for choosing per-class score thresholds from a
#' review of a classifier's predictions. A reviewer picks a predicted class,
#' sees a sample of the images the model gave that class, drawn across many
#' samples and spread over the score range, and marks the ones that are not
#' the class. From those verdicts the app estimates the precision above each
#' candidate threshold and exports thresholds that
#' [ifcb-classify](https://github.com/nodc-sweden/ifcb-pytorch-classify) loads
#' with `--thresholds`.
#'
#' The app is separate from [run_app()]: it has its own settings and writes
#' nothing to the annotation database. Its verdicts and its index of the
#' class-score files are kept in a review folder you choose in the app.
#'
#' The class scores are read from the HDF5 files ifcb-classify writes, which
#' requires the \pkg{hdf5r} package.
#'
#' @param launch.browser If `TRUE` (default), opens the app in the system's
#'   default web browser. If `FALSE`, no browser is opened. Set to a function
#'   to customise how the browser is launched.
#' @param ... Additional arguments passed to [shiny::runApp()].
#' @return This function does not return; it runs the Shiny app.
#' @export
#' @seealso [build_threshold_index()] to index class-score files from a
#'   script, and [write_thresholds_json()] for the file the app exports.
#' @examples
#' \dontrun{
#' # Open the threshold review app in the browser
#' run_threshold_app()
#'
#' # Run on a specific port without opening a browser
#' run_threshold_app(launch.browser = FALSE, port = 3839)
#' }
#' @md
run_threshold_app <- function(launch.browser = TRUE, ...) {
  if (!thr_hdf5r_available()) {
    stop("Package 'hdf5r' is required to read H5 classification files. ",
         "Install it with: install.packages('hdf5r')", call. = FALSE)
  }
  lifecycle::signal_stage("experimental", "run_threshold_app()")

  app_dir <- thr_app_dir()
  if (app_dir == "") {
    stop("Could not find the threshold app directory. Try re-installing ",
         "`ClassiPyR`.", call. = FALSE)
  }

  # Capture the user's working directory before Shiny changes it
  options(ClassiPyR.startup_wd = getwd())

  shiny::runApp(app_dir, launch.browser = launch.browser, ...)
}
