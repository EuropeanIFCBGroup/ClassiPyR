# Rescan folders and rebuild the file index cache

Scans the configured (or specified) ROI, classification, and output
folders for IFCB sample files and saves the results to the file index
cache. This can be called outside the Shiny app, e.g. from a cron job,
to keep the cache up to date without manually clicking the rescan
button.

## Usage

``` r
rescan_file_index(
  roi_folder = NULL,
  csv_folder = NULL,
  output_folder = NULL,
  verbose = TRUE,
  db_folder = NULL
)
```

## Arguments

- roi_folder:

  Path to ROI data folder. If NULL, read from saved settings.

- csv_folder:

  Path to classification folder (CSV/MAT). If NULL, read from saved
  settings.

- output_folder:

  Path to output folder for MAT annotations. If NULL, read from saved
  settings.

- verbose:

  If TRUE, print progress messages. Default TRUE.

- db_folder:

  Path to the database folder for SQLite annotations. If NULL, read from
  saved settings; if not found in settings, defaults to
  [`get_default_db_dir()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/get_default_db_dir.md).

## Value

Invisibly returns the file index list, or NULL if roi_folder is invalid.

## Details

If folder paths are not provided, they are read from saved settings.

## Examples

``` r
if (FALSE) { # \dontrun{
# Rescan using saved settings
rescan_file_index()

# Rescan with explicit paths
rescan_file_index(
  roi_folder = "/data/ifcb/raw",
  csv_folder = "/data/ifcb/classified",
  output_folder = "/data/ifcb/manual"
)

# Use in a cron job:
# Rscript -e 'ClassiPyR::rescan_file_index()'
} # }
```
