# Save sample annotations to MAT and statistics files

Saves the current annotations for a sample, including: - MAT file
compatible with ifcb-analysis (requires Python) - Validation statistics
CSV files - PNG images organized by class

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
  annotator = "Unknown"
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

  Output folder path for MAT files

- png_output_folder:

  PNG output folder path (organized by class)

- roi_folder:

  ROI folder path (for ADC file location)

- class2use_path:

  Path to class2use file

- annotator:

  Annotator name for statistics

## Value

TRUE on success, FALSE on failure

## Examples

``` r
if (FALSE) { # \dontrun{
# Save annotations for a sample
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
