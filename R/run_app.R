# Launch ClassiPyR Shiny application

#' Run the ClassiPyR Shiny Application
#'
#' Launches the ClassiPyR Shiny app for manual image classification and validation of IFCB data.
#' This app relies on the iRfcb package for reading IFCB data files and requires
#' Python (via reticulate) for reading and writing MATLAB .mat files.
#'
#' @param venv_path Optional path to a Python virtual environment. If NULL (default),
#'   the app will use any saved venv path from settings, or fall back to a 'venv'
#'   folder in the current working directory. Set this to specify a custom location
#'   for the Python virtual environment used by iRfcb.
#' @param reset_settings If TRUE, deletes saved settings before starting the app.
#'   Useful for troubleshooting or starting fresh. Default is FALSE.
#' @param ... Additional arguments passed to \code{\link[shiny]{runApp}}
#' @return This function does not return; it runs the Shiny app
#' @export
#' @examples
#' \dontrun{
#' # Run with default settings
#' run_app()
#'
#' # Run with a specific Python virtual environment
#' run_app(venv_path = "/path/to/my/venv")
#'
#' # Run on a specific port
#' run_app(port = 3838)
#'
#' # Reset all settings and start fresh
#' run_app(reset_settings = TRUE)
#' }
run_app <- function(venv_path = NULL, reset_settings = FALSE, ...) {
  app_dir <- system.file("app", package = "ClassiPyR")
  if (app_dir == "") {
    stop("Could not find app directory. Try re-installing `ClassiPyR`.",
         call. = FALSE)
  }

  # Reset settings if requested

  if (isTRUE(reset_settings)) {
    settings_file <- get_settings_path()
    if (file.exists(settings_file)) {
      file.remove(settings_file)
      message("Settings reset. Starting with defaults.")
    }
  }

  # Capture user's working directory before Shiny changes it
  options(ClassiPyR.startup_wd = getwd())

  # Set venv path as option for the app to use
  if (!is.null(venv_path)) {
    options(ClassiPyR.venv_path = venv_path)
  }

  shiny::runApp(app_dir, ...)
}
