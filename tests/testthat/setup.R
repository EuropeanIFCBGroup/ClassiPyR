# Setup for tests

library(reticulate)

# Check if we are on CRAN (skip Python setup on CRAN)
on_cran <- !identical(Sys.getenv("NOT_CRAN"), "true") &&
           nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_", ""))

if (!on_cran) {
  # Try to initialize Python for tests
  # First check if Python is already available
  if (!reticulate::py_available(initialize = FALSE)) {
    # Try to discover and use system Python
    python_config <- tryCatch(
      reticulate::py_discover_config(),
      error = function(e) NULL
    )

    if (!is.null(python_config) && !is.null(python_config$python)) {
      tryCatch({
        reticulate::use_python(python_config$python, required = FALSE)
      }, error = function(e) {
        message("Could not configure Python: ", e$message)
      })
    }
  }

  # Initialize Python if available
  if (reticulate::py_available(initialize = TRUE)) {
    # Check for scipy (required for MAT file operations)
    if (!reticulate::py_module_available("scipy")) {
      message("Installing scipy for tests...")
      tryCatch({
        reticulate::py_install("scipy")
      }, error = function(e) {
        message("Could not install scipy: ", e$message)
      })
    }
  }
}
