# Save sample annotations

Saves the current annotations for a sample. By default annotations are
stored in a local SQLite database (`annotations.sqlite` in the database
folder). Optionally, a MATLAB-compatible `.mat` file can also be written
(requires Python + scipy).

## Usage

``` r
save_sample_annotations(
  sample_name,
  classifications,
  original_classifications,
  changes_log,
  temp_png_folder,
  output_folder,
  png_output_folder,
  roi_folder,
  class2use_path,
  class2use = NULL,
  annotator = "Unknown",
  adc_folder = NULL,
  save_format = "sqlite",
  db_folder = get_default_db_dir(),
  export_statistics = TRUE
)
```

## Arguments

- sample_name:

  Sample name (e.g., "D20230101T120000_IFCB134")

- classifications:

  Current classifications data frame

- original_classifications:

  Original classifications data frame (for comparison)

- changes_log:

  Changes log data frame from
  [`create_empty_changes_log`](https://europeanifcbgroup.github.io/ClassiPyR/reference/create_empty_changes_log.md)

- temp_png_folder:

  Path to temporary folder with extracted PNG images

- output_folder:

  Output folder path for MAT files and statistics

- png_output_folder:

  PNG output folder path (organized by class)

- roi_folder:

  ROI folder path (for ADC file location, used as fallback)

- class2use_path:

  Path to class2use file

- class2use:

  Character vector of class names. When NULL (default), loaded from
  `class2use_path`.

- annotator:

  Annotator name for statistics

- adc_folder:

  Direct path to the ADC folder. When provided, this is used instead of
  constructing the path via
  [`get_sample_paths`](https://europeanifcbgroup.github.io/ClassiPyR/reference/get_sample_paths.md).
  This supports non-standard folder structures.

- save_format:

  One of `"sqlite"` (default), `"mat"`, or `"both"`. Controls which
  backend(s) are written.

- db_folder:

  Path to the database folder for SQLite storage. Defaults to
  [`get_default_db_dir()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/get_default_db_dir.md).
  Should be a local filesystem path, not a network drive.

- export_statistics:

  Logical. When `TRUE` (default), validation statistics CSV files are
  written to a `validation_statistics/` subfolder inside
  `output_folder`. Set to `FALSE` to skip this export, e.g. when
  annotating from scratch.

## Value

TRUE on success, FALSE on failure

## Examples

``` r
if (FALSE) { # \dontrun{
# Save annotations for a sample (default: SQLite)
success <- save_sample_annotations(
  sample_name = "D20230101T120000_IFCB134",
  classifications = current_classifications,
  original_classifications = original_classifications,
  changes_log = changes_log,
  temp_png_folder = "/tmp/png",
  output_folder = "/data/manual",
  png_output_folder = "/data/png_output",
  roi_folder = "/data/raw",
  class2use_path = "/data/class2use.mat",
  annotator = "John Doe"
)
} # }
```
