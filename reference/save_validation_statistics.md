# Save validation statistics to CSV files

Compares current classifications to original classifications and saves
summary and detailed statistics to CSV files.

## Usage

``` r
save_validation_statistics(
  sample_name,
  classifications,
  original_classifications,
  stats_folder,
  annotator
)
```

## Arguments

- sample_name:

  Sample name (e.g., "D20230101T120000_IFCB134")

- classifications:

  Current classifications data frame

- original_classifications:

  Original classifications data frame

- stats_folder:

  Statistics output folder path

- annotator:

  Annotator name

## Value

NULL (called for side effects)

## Examples

``` r
if (FALSE) { # \dontrun{
# Save validation statistics
save_validation_statistics(
  sample_name = "D20230101T120000_IFCB134",
  classifications = current_classifications,
  original_classifications = original_classifications,
  stats_folder = "/data/manual/validation_statistics",
  annotator = "John Doe"
)
} # }
```
