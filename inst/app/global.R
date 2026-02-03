# Global settings and initialization for ClassiPyR
#
# This file is loaded when the Shiny app starts.
# Helper functions are loaded from the ClassiPyR package.

# Load required libraries (ClassiPyR imports these)
suppressPackageStartupMessages({
  library(ClassiPyR)
  library(shiny)
  library(shinyjs)
  library(shinyFiles)
  library(bslib)
  library(iRfcb)
  library(dplyr)
  library(DT)
  library(jsonlite)
  library(reticulate)
})

# Get version from package
app_version <- as.character(utils::packageVersion("ClassiPyR"))

# Session cache limit (used in server.R to evict oldest samples)
# Each cached sample stores classification metadata (~1.5 MB with 5000 ROIs)
# 20 samples ≈ 30 MB memory usage
MAX_CACHED_SAMPLES <- 20

# Get Python venv path from: 1) run_app() argument, 2) saved settings, 3) NULL (use default)
.get_venv_path <- function() {
  # First check if run_app() was called with venv_path argument
  option_path <- getOption("ClassiPyR.venv_path", default = NULL)
  if (!is.null(option_path) && nzchar(option_path)) {
    return(option_path)
  }

  # Otherwise check saved settings
  settings_file <- get_settings_path()
  if (file.exists(settings_file)) {
    tryCatch({
      saved <- jsonlite::fromJSON(settings_file)
      if (!is.null(saved$python_venv_path) && nzchar(saved$python_venv_path)) {
        return(saved$python_venv_path)
      }
    }, error = function(e) NULL)
  }
  NULL
}

# Initialize Python on app startup with configured venv path
python_available <- init_python_env(venv_path = .get_venv_path())

# S3 method for dynamic_roots: allows shinyFiles to subscript a function-based
# roots object. shinyFiles 0.9.3 internally does roots[selectedRoot] without
# checking if roots is a function, so this class bridges the gap.
`[.dynamic_roots` <- function(x, i) x()[i]

# App settings
options(shiny.launch.browser = TRUE)
