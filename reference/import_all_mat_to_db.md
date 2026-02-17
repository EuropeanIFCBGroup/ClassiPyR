# Bulk import .mat annotation files into the SQLite database

Scans a folder for `.mat` annotation files (excluding classifier output
files matching `*_class*.mat`) and imports each into the database. Each
file's embedded `class2use_manual` is used for class-name mapping.

## Usage

``` r
import_all_mat_to_db(mat_folder, db_path, annotator = "imported")
```

## Arguments

- mat_folder:

  Folder containing .mat annotation files

- db_path:

  Path to the SQLite database file

- annotator:

  Annotator name (defaults to `"imported"`)

## Value

Named list with counts: `success`, `failed`, `skipped`

## Examples

``` r
if (FALSE) { # \dontrun{
db_path <- get_db_path("/data/manual")
result <- import_all_mat_to_db("/data/manual", db_path)
cat(result$success, "imported,", result$failed, "failed,", result$skipped, "skipped\n")
} # }
```
