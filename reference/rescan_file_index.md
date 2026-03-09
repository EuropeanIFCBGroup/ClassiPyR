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
  db_folder = NULL,
  data_source = "local",
  dashboard_url = NULL,
  dashboard_autoclass = FALSE,
  progress = NULL
)
```

## Arguments

- roi_folder:

  Path to ROI/PNG data folder. If NULL, read from saved settings.

- csv_folder:

  Path to classification folder (CSV/H5/MAT). If NULL, read from saved
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

- data_source:

  Either `"local"` (default) for local folder scanning, or `"dashboard"`
  to fetch the sample list from a remote IFCB Dashboard.

- dashboard_url:

  When `data_source = "dashboard"`, the full Dashboard URL (e.g.
  `"https://habon-ifcb.whoi.edu/timeline?dataset=tangosund"`).

- dashboard_autoclass:

  Logical. When `TRUE` and `data_source = "dashboard"`, skip scanning
  the local `csv_folder` for classifier output files (the dashboard's
  own auto-classifications are used instead). Defaults to `FALSE`.

- progress:

  Optional callback function used to report scan progress. The callback
  receives named arguments `value` (numeric in \[0, 1\]) and `detail`
  (character message). Mainly intended for Shiny progress bars.

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

# Scan from a remote Dashboard
rescan_file_index(data_source = "dashboard",
                  dashboard_url = "https://habon-ifcb.whoi.edu/timeline?dataset=tangosund")

# Use in a cron job:
# Rscript -e 'ClassiPyR::rescan_file_index()'
} # }
```
