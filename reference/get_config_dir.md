# Get ClassiPyR configuration directory

Returns the path to the configuration directory for storing settings.
Uses tools::R_user_dir() for CRAN compliance. During R CMD check, uses a
temporary directory to avoid writing to user directories.

## Usage

``` r
get_config_dir()
```

## Value

Path to the configuration directory

## Examples

``` r
# Get the configuration directory path
config_dir <- get_config_dir()
print(config_dir)
#> [1] "/home/runner/.config/R/ClassiPyR"
```
