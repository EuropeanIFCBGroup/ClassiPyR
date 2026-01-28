# Load classifications from CSV file (validation mode)

Reads a classification CSV file and returns a data frame with
classifications. Class names are processed to truncate trailing numbers
(matching iRfcb behavior).

## Usage

``` r
load_from_csv(csv_path)
```

## Arguments

- csv_path:

  Path to classification CSV file

## Value

Data frame with classifications (columns depend on CSV content)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load classifications from a CSV file
classifications <- load_from_csv("/path/to/classifications.csv")
head(classifications)
} # }
```
