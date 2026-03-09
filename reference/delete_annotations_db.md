# Delete annotations for a sample from the SQLite database

Removes all rows for the given sample from both the `annotations` and
`class_lists` tables in a single transaction. This is a permanent
operation — the sample will appear unannotated after deletion.

## Usage

``` r
delete_annotations_db(db_path, sample_name)
```

## Arguments

- db_path:

  Path to the SQLite database file

- sample_name:

  Sample name to delete

## Value

`TRUE` on success, `FALSE` on error (with a warning)

## Examples

``` r
if (FALSE) { # \dontrun{
db_path <- get_db_path("/data/local_db")
delete_annotations_db(db_path, "D20230101T120000_IFCB134")
} # }
```
