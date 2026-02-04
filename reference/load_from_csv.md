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

Data frame with classifications. Expected columns: \`file_name\`,
\`class_name\`, and optionally \`score\`.

## Details

The CSV file must contain the following columns:

- file_name:

  Image filename including the \`.png\` extension (e.g.,
  \`D20230101T120000_IFCB134_00001.png\`).

- class_name:

  Predicted class name (e.g., \`Diatom\`).

An optional column may also be included:

- score:

  Classification confidence value between 0 and 1.

The CSV file must be named after the sample it describes (e.g.,
\`D20230101T120000_IFCB134.csv\`) and placed inside the Classification
Folder configured in the app (subfolders are searched recursively).

## Examples

``` r
if (FALSE) { # \dontrun{
# Load classifications from a CSV file
classifications <- load_from_csv("/path/to/D20230101T120000_IFCB134.csv")
head(classifications)
} # }
```
