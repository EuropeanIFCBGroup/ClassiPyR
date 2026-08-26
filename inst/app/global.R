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
  library(DBI)
  library(RSQLite)
})

# Get version from package
app_version <- as.character(utils::packageVersion("ClassiPyR"))

# Session cache limit (used in server.R to evict oldest samples)
# Each cached sample stores classification metadata (~1.5 MB with 5000 ROIs)
# 20 samples ≈ 30 MB memory usage
MAX_CACHED_SAMPLES <- 20

# S3 method for dynamic_roots: allows shinyFiles to subscript a function-based
# roots object. shinyFiles 0.9.3 internally does roots[selectedRoot] without
# checking if roots is a function, so this class bridges the gap.
`[.dynamic_roots` <- function(x, i) x()[i]

# App settings
options(shiny.launch.browser = TRUE)
