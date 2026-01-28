# Filter classifications to only include extracted images

Filters a classifications data frame to only include ROIs that have
corresponding PNG files in the extracted folder.

## Usage

``` r
filter_to_extracted(classifications, extracted_folder)
```

## Arguments

- classifications:

  Data frame of classifications (must have file_name column)

- extracted_folder:

  Path to folder with extracted PNG images

## Value

Filtered classifications data frame

## Examples

``` r
if (FALSE) { # \dontrun{
# Filter to only images that were successfully extracted
classifications <- filter_to_extracted(
  classifications = classifications,
  extracted_folder = "/tmp/png/D20230101T120000_IFCB134"
)
} # }
```
