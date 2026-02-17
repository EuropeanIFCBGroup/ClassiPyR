# Bulk export all annotated samples from SQLite to .mat files

Exports every sample in the database to a MATLAB-compatible annotation
file. Requires Python with scipy.

## Usage

``` r
export_all_db_to_mat(db_path, output_folder)
```

## Arguments

- db_path:

  Path to the SQLite database file

- output_folder:

  Folder where .mat files will be written

## Value

Named list with counts: `success`, `failed`

## Examples

``` r
if (FALSE) { # \dontrun{
db_path <- get_db_path("/data/manual")
result <- export_all_db_to_mat(db_path, "/data/manual")
cat(result$success, "exported,", result$failed, "failed\n")
} # }
```
