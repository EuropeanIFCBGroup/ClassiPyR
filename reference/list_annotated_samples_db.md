# List samples with annotations in the database

List samples with annotations in the database

## Usage

``` r
list_annotated_samples_db(db_path)
```

## Arguments

- db_path:

  Path to the SQLite database file

## Value

Character vector of sample names that have annotations

## Examples

``` r
if (FALSE) { # \dontrun{
db_path <- get_db_path("/data/manual")
samples <- list_annotated_samples_db(db_path)
} # }
```
