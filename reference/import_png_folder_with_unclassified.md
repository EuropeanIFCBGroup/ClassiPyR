# Import a PNG class folder and backfill the rest as "unclassified"

Convenience wrapper that runs
[`import_png_folder_to_db`](https://europeanifcbgroup.github.io/ClassiPyR/reference/import_png_folder_to_db.md)
followed by
[`fill_unclassified_db`](https://europeanifcbgroup.github.io/ClassiPyR/reference/fill_unclassified_db.md)
in a single call. After importing the selected-taxa PNGs, it backfills
the remaining ROIs of *only the samples that were just imported* as
`"unclassified"`, so each imported sample is fully represented in the
database without touching samples from earlier import sessions.

## Usage

``` r
import_png_folder_with_unclassified(
  png_folder,
  db_path,
  class2use,
  roi_folder,
  class_mapping = NULL,
  annotator = "imported",
  fill = TRUE
)
```

## Arguments

- png_folder:

  Path to the top-level folder containing class subfolders

- db_path:

  Path to the SQLite database file

- class2use:

  Character vector of class names (preserves index order for .mat
  export)

- roi_folder:

  Base ROI folder path, following the standard IFCB folder structure
  (`roi_folder/YYYY/DYYYYMMDD/sample_name.adc`). Used by the backfill
  step to read each sample's complete ROI list.

- class_mapping:

  Optional named character vector mapping scanned class names to target
  class names. Passed to
  [`import_png_folder_to_db`](https://europeanifcbgroup.github.io/ClassiPyR/reference/import_png_folder_to_db.md).

- annotator:

  Annotator name (defaults to `"imported"`). Applied to both the import
  and the backfill.

- fill:

  Logical. When `TRUE` (the default), backfill the imported samples with
  `"unclassified"` ROIs. Set to `FALSE` to import only.

## Value

Named list with two elements: `import` (the result of
[`import_png_folder_to_db`](https://europeanifcbgroup.github.io/ClassiPyR/reference/import_png_folder_to_db.md))
and `filled` (the result of
[`fill_unclassified_db`](https://europeanifcbgroup.github.io/ClassiPyR/reference/fill_unclassified_db.md),
or zero counts when `fill` is `FALSE` or no samples were imported).

## See also

[`import_png_folder_to_db`](https://europeanifcbgroup.github.io/ClassiPyR/reference/import_png_folder_to_db.md),
[`fill_unclassified_db`](https://europeanifcbgroup.github.io/ClassiPyR/reference/fill_unclassified_db.md)

## Examples

``` r
if (FALSE) { # \dontrun{
db_path <- get_db_path("/data/manual")
class2use <- c("Diatom", "Dinoflagellate", "Ciliate")
result <- import_png_folder_with_unclassified(
  "/data/png_export", db_path, class2use,
  roi_folder = "/data/ifcb/raw"
)
cat(result$import$success, "imported,",
    result$filled$added, "backfilled\n")
} # }
```
