# Save class review changes to the database

Performs row-level UPDATEs for reclassified images identified during
class review mode. Only the changed rows are updated; other annotations
for the same samples are left untouched.

## Usage

``` r
save_class_review_changes_db(db_path, changes_df, annotator)
```

## Arguments

- db_path:

  Path to the SQLite database file

- changes_df:

  Data frame with columns `sample_name`, `roi_number`, and
  `new_class_name`

- annotator:

  Annotator name

## Value

Integer count of rows updated

## Examples

``` r
if (FALSE) { # \dontrun{
db_path <- get_db_path("/data/manual")
changes <- data.frame(
  sample_name = "D20230101T120000_IFCB134",
  roi_number = 5L,
  new_class_name = "Ciliate"
)
save_class_review_changes_db(db_path, changes, "Jane")
} # }
```
