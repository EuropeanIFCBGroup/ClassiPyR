# Get sample paths from sample name

Constructs file paths to IFCB data files (ROI, ADC) based on the
standard IFCB folder structure:
roi_folder/YYYY/DYYYYMMDD/sample_name.ext

## Usage

``` r
get_sample_paths(sample_name, roi_folder)
```

## Arguments

- sample_name:

  Sample name (e.g., "D20220522T000439_IFCB134")

- roi_folder:

  Base ROI folder path

## Value

List with components: year, date_part, roi_path, adc_path, adc_folder

## Examples

``` r
# Get paths for a sample
paths <- get_sample_paths("D20230101T120000_IFCB134", "/data/ifcb/raw")
print(paths$year)       # "2023"
#> [1] "2023"
print(paths$date_part)  # "D20230101"
#> [1] "D20230101"
print(paths$roi_path)   # "/data/ifcb/raw/2023/D20230101/D20230101T120000_IFCB134.roi"
#> [1] "/data/ifcb/raw/2023/D20230101/D20230101T120000_IFCB134.roi"
```
