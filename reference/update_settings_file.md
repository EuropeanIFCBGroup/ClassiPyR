# Update settings file by merging new values

Reads the existing settings JSON file (if any), merges the given
settings into it, and writes the result back. Callers that only know a
subset of settings keys can therefore update those keys without erasing
the rest of the file. A key with a `NULL` value is removed from the
file.

## Usage

``` r
update_settings_file(settings, settings_file = get_settings_path())
```

## Arguments

- settings:

  Named list of settings to update.

- settings_file:

  Path to the settings JSON file. Defaults to
  [`get_settings_path`](https://europeanifcbgroup.github.io/ClassiPyR/reference/get_settings_path.md).

## Value

Invisibly, the full merged settings list.
