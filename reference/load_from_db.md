# Load classifications from SQLite database

Reads annotations for a sample from the SQLite database and returns a
data frame in the same format as
[`load_from_mat`](https://europeanifcbgroup.github.io/ClassiPyR/reference/load_from_mat.md).

## Usage

``` r
load_from_db(db_path, sample_name, roi_dimensions)
```

## Arguments

- db_path:

  Path to the SQLite database file

- sample_name:

  Sample name (e.g., "D20230101T120000_IFCB134")

- roi_dimensions:

  Data frame from
  [`read_roi_dimensions`](https://europeanifcbgroup.github.io/ClassiPyR/reference/read_roi_dimensions.md)

## Value

Data frame with columns: file_name, class_name, score, width, height,
roi_area. Returns NULL if the sample has no annotations in the database.

## Examples

``` r
if (FALSE) { # \dontrun{
dims <- read_roi_dimensions("/data/raw/2023/D20230101/D20230101T120000_IFCB134.adc")
db_path <- get_db_path("/data/manual")
classifications <- load_from_db(db_path, "D20230101T120000_IFCB134", dims)
} # }
```
