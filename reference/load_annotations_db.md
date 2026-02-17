# Load annotations from the SQLite database

Reads annotations for a single sample and returns a data frame in the
same format as
[`load_from_mat`](https://europeanifcbgroup.github.io/ClassiPyR/reference/load_from_mat.md).

## Usage

``` r
load_annotations_db(db_path, sample_name, roi_dimensions)
```

## Arguments

- db_path:

  Path to the SQLite database file

- sample_name:

  Sample name

- roi_dimensions:

  Data frame from
  [`read_roi_dimensions`](https://europeanifcbgroup.github.io/ClassiPyR/reference/read_roi_dimensions.md)
  with columns `roi_number`, `width`, `height`, `area`

## Value

Data frame with columns: file_name, class_name, score, width, height,
roi_area. Returns NULL if the sample has no annotations.

## Examples

``` r
if (FALSE) { # \dontrun{
dims <- read_roi_dimensions("/data/raw/2023/D20230101/D20230101T120000_IFCB134.adc")
db_path <- get_db_path("/data/manual")
classifications <- load_annotations_db(db_path, "D20230101T120000_IFCB134", dims)
} # }
```
