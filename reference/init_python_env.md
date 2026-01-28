# Initialize Python environment for iRfcb

Checks if Python is already available via reticulate, otherwise tries to
use or create a virtual environment. Required for reading and writing
MATLAB .mat files.

## Usage

``` r
init_python_env(venv_path = NULL)
```

## Arguments

- venv_path:

  Optional path to virtual environment. If NULL (default), uses a 'venv'
  folder in the current working directory.

## Value

TRUE if Python is available, FALSE otherwise

## Examples

``` r
if (FALSE) { # \dontrun{
# Initialize with default venv path (./venv)
success <- init_python_env()

# Initialize with custom venv path
success <- init_python_env("/path/to/my/venv")

if (success) {
  message("Python ready for MAT file operations")
}
} # }
```
