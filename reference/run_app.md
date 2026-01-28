# Run the ClassiPyR Shiny Application

Launches the ClassiPyR Shiny app for manual image classification and
validation of IFCB data. This app relies on the iRfcb package for
reading IFCB data files and requires Python (via reticulate) for reading
and writing MATLAB .mat files.

## Usage

``` r
run_app(venv_path = NULL, reset_settings = FALSE, ...)
```

## Arguments

- venv_path:

  Optional path to a Python virtual environment. If NULL (default), the
  app will use any saved venv path from settings, or fall back to a
  'venv' folder in the current working directory. Set this to specify a
  custom location for the Python virtual environment used by iRfcb.

- reset_settings:

  If TRUE, deletes saved settings before starting the app. Useful for
  troubleshooting or starting fresh. Default is FALSE.

- ...:

  Additional arguments passed to
  [`runApp`](https://rdrr.io/pkg/shiny/man/runApp.html)

## Value

This function does not return; it runs the Shiny app

## Examples

``` r
if (FALSE) { # \dontrun{
# Run with default settings
run_app()

# Run with a specific Python virtual environment
run_app(venv_path = "/path/to/my/venv")

# Run on a specific port
run_app(port = 3838)

# Reset all settings and start fresh
run_app(reset_settings = TRUE)
} # }
```
