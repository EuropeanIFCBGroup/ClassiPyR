# Get persistent cache directory for dashboard downloads

Returns the path to the dashboard cache directory. During R CMD check,
uses a temporary directory.

## Usage

``` r
get_dashboard_cache_dir()
```

## Value

Path to the dashboard cache directory

## Examples

``` r
cache_dir <- get_dashboard_cache_dir()
print(cache_dir)
#> [1] "/home/runner/.cache/R/ClassiPyR/dashboard"
```
