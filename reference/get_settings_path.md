# Get path to settings file

Returns the path to the settings JSON file, creating the configuration
directory if it doesn't exist.

## Usage

``` r
get_settings_path()
```

## Value

Path to the settings JSON file

## Examples

``` r
# Get the settings file path
settings_path <- get_settings_path()
print(settings_path)
#> [1] "/home/runner/.config/R/ClassiPyR/settings.json"
```
