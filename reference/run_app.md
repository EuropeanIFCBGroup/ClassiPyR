# Run the ClassiPyR Shiny Application

Launches the ClassiPyR Shiny app for manual image classification and
validation of IFCB data. This app relies on the iRfcb package for
reading IFCB data files and requires Python (via reticulate) for saving
MATLAB .mat files.

## Usage

``` r
run_app(venv_path = NULL, reset_settings = FALSE, launch.browser = TRUE, ...)
```

## Arguments

- venv_path:

  Optional path to a Python virtual environment. When specified, this
  path takes priority over any saved venv path in settings, both for
  Python initialization at startup and in the Settings UI. If NULL
  (default), the app uses any saved venv path from settings, or falls
  back to a 'venv' folder in the current working directory.

- reset_settings:

  If TRUE, deletes saved settings before starting the app. Useful for
  troubleshooting or starting fresh. Default is FALSE.

- launch.browser:

  If TRUE (default), opens the app in the system's default web browser.
  If FALSE, opens in RStudio viewer (if available). Set to a function to
  customize browser launching behavior.

- ...:

  Additional arguments passed to
  [`runApp`](https://rdrr.io/pkg/shiny/man/runApp.html)

## Value

This function does not return; it runs the Shiny app

## Examples

``` r
if (FALSE) { # \dontrun{
# Run with default settings (opens in browser)
run_app()

# Run with a specific Python virtual environment
run_app(venv_path = "/path/to/my/venv")

# Run on a specific port
run_app(port = 3838)

# Open in RStudio viewer instead of browser
run_app(launch.browser = FALSE)

# Reset all settings and start fresh
run_app(reset_settings = TRUE)
} # }
```
