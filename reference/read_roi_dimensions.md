# Read ROI dimensions from ADC file

Reads an IFCB ADC file and extracts ROI dimensions (width, height, area)
for each ROI in the sample.

## Usage

``` r
read_roi_dimensions(adc_path)
```

## Arguments

- adc_path:

  Path to ADC file

## Value

Data frame with columns: roi_number, width, height, area

## Examples

``` r
if (FALSE) { # \dontrun{
# Read dimensions from an ADC file
dims <- read_roi_dimensions("/data/ifcb/raw/2023/D20230101/D20230101T120000_IFCB134.adc")
head(dims)
} # }
```
