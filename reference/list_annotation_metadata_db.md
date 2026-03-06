# List distinct years, months, and instruments from annotations

Extracts metadata from sample names in the annotations table for use as
filter options. Sample names follow the IFCB naming convention
`DYYYYMMDDTHHMMSS_INSTRUMENT`.

## Usage

``` r
list_annotation_metadata_db(db_path)
```

## Arguments

- db_path:

  Path to the SQLite database file

## Value

A list with character vectors: `years`, `months`, `instruments`, and
`annotators`. Returns empty vectors if the database does not exist or
has no annotations.

## Examples

``` r
if (FALSE) { # \dontrun{
db_path <- get_db_path("/data/manual")
meta <- list_annotation_metadata_db(db_path)
meta$years       # e.g. c("2022", "2023")
meta$months      # e.g. c("01", "06", "12")
meta$instruments # e.g. c("IFCB134", "IFCB135")
meta$annotators  # e.g. c("Jane", "imported")
} # }
```
