# List all classes with counts in the annotations database

Queries the database for distinct class names and their annotation
counts. Useful for populating class review mode dropdowns. Optional
filters restrict results to annotations matching a given year, month, or
instrument.

## Usage

``` r
list_classes_db(
  db_path,
  year = NULL,
  month = NULL,
  instrument = NULL,
  annotator = NULL
)
```

## Arguments

- db_path:

  Path to the SQLite database file

- year:

  Optional year filter (e.g. `"2023"`). When not `"all"` or `NULL`,
  restricts to sample names starting with `DYYYY`.

- month:

  Optional month filter (e.g. `"03"`). When not `"all"` or `NULL`,
  restricts to sample names with that month at positions 6-7.

- instrument:

  Optional instrument filter (e.g. `"IFCB134"`). When not `"all"` or
  `NULL`, restricts to sample names ending with `_INSTRUMENT`.

- annotator:

  Optional annotator name filter (e.g. `"Jane"`). When not `"all"` or
  `NULL`, restricts to annotations by that annotator.

## Value

Data frame with columns `class_name` and `count`, ordered alphabetically
by class name. Returns an empty data frame if the database does not
exist or has no annotations.

## Examples

``` r
if (FALSE) { # \dontrun{
db_path <- get_db_path("/data/manual")
classes <- list_classes_db(db_path)
classes_2023 <- list_classes_db(db_path, year = "2023")
} # }
```
