# Global settings for the ClassiPyR threshold review app
#
# This file is loaded when the Shiny app starts. The review logic lives in
# the ClassiPyR package; nothing is shared with the main app in inst/app.

# Load required libraries (ClassiPyR imports these)
suppressPackageStartupMessages({
  library(ClassiPyR)
  library(shiny)
  library(shinyjs)
  library(shinyFiles)
  library(bslib)
  library(DT)
})
