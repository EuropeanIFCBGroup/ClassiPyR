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

  Optional path to virtual environment. If NULL (default), uses a `venv`
  folder in the current working directory.

## Value

TRUE if Python is available, FALSE otherwise

## Details

The resolution order is: 1. If Python is already configured via
reticulate, use it directly (installs scipy if missing). 2. If
`venv_path` is provided and the virtual environment exists, activate it.
3. If `venv_path` is provided but does not exist, create it via
[`ifcb_py_install`](https://europeanifcbgroup.github.io/iRfcb/reference/ifcb_py_install.html).
4. If `venv_path` is NULL, default to `./venv` in the current working
directory for steps 2–3.

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
