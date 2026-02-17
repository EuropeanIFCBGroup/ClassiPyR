# Bulk export all annotated samples from SQLite to class-organized PNGs

Exports every annotated sample in the database to PNG images organized
into class subfolders.

## Usage

``` r
export_all_db_to_png(db_path, png_folder, roi_path_map, skip_class = NULL)
```

## Arguments

- db_path:

  Path to the SQLite database file

- png_folder:

  Base output folder for PNGs

- roi_path_map:

  Named list mapping sample names to `.roi` file paths. Samples without
  an entry are skipped.

- skip_class:

  Character vector of class names to exclude from export (e.g.
  `"unclassified"`). Default `NULL` exports all classes.

## Value

Named list with counts: `success`, `failed`, `skipped`

## Examples

``` r
if (FALSE) { # \dontrun{
db_path <- get_db_path("/data/manual")
roi_map <- list("D20230101T120000_IFCB134" = "/data/raw/.../D20230101T120000_IFCB134.roi")
result <- export_all_db_to_png(db_path, "/data/png_output", roi_map,
                               skip_class = "unclassified")
cat(result$success, "exported,", result$failed, "failed,", result$skipped, "skipped\n")
} # }
```
