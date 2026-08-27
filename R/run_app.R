# Launch ClassiPyR Shiny application

#' Run the ClassiPyR Shiny Application
#'
#' Launches the ClassiPyR Shiny app for manual image classification and validation of IFCB data.
#' This app relies on the iRfcb package for reading and writing IFCB data files,
#' including MATLAB .mat files, entirely in R.
#'
#' @param venv_path `r lifecycle::badge("deprecated")` Ignored. ClassiPyR no
#'   longer requires Python; .mat files are read and written natively in R
#'   (iRfcb >= 0.10.0).
#' @param reset_settings If TRUE, deletes saved settings before starting the app.
#'   Useful for troubleshooting or starting fresh. Default is FALSE.
#' @param launch.browser If TRUE (default), opens the app in the system's default
#'   web browser. If FALSE, opens in RStudio viewer (if available). Set to a function
#'   to customize browser launching behavior.
#' @param ... Additional arguments passed to \code{\link[shiny]{runApp}}
#' @return This function does not return; it runs the Shiny app
#' @export
#' @examples
#' \dontrun{
#' # Run with default settings (opens in browser)
#' run_app()
#'
#' # Run on a specific port
#' run_app(port = 3838)
#'
#' # Open in RStudio viewer instead of browser
#' run_app(launch.browser = FALSE)
#'
#' # Reset all settings and start fresh
#' run_app(reset_settings = TRUE)
#' }
#' @md
run_app <- function(venv_path = deprecated(), reset_settings = FALSE, launch.browser = TRUE, ...) {
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

  if (lifecycle::is_present(venv_path)) {
    lifecycle::deprecate_warn(
      "0.3.0", "run_app(venv_path)",
      details = paste(
        "The argument is ignored: ClassiPyR no longer requires Python",
        "(iRfcb >= 0.10.0 reads and writes .mat files natively in R)."
      )
    )
  }

  shiny::runApp(app_dir, launch.browser = launch.browser, ...)
}
