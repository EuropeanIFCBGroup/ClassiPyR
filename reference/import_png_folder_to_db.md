# Import annotations from a PNG class folder into the SQLite database

Scans a folder of PNG images organized in class-name subfolders (via
[`scan_png_class_folder`](https://europeanifcbgroup.github.io/ClassiPyR/reference/scan_png_class_folder.md))
and imports the annotations into the database. An optional
`class_mapping` named vector remaps class names before saving.

## Usage

``` r
import_png_folder_to_db(
  png_folder,
  db_path,
  class2use,
  class_mapping = NULL,
  annotator = "imported"
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

- class_mapping:

  Optional named character vector mapping scanned class names to target
  class names. Names are the source classes, values are the target
  classes. Classes not in the mapping are kept as-is.

- annotator:

  Annotator name (defaults to `"imported"`)

## Value

Named list with counts: `success`, `failed`

## Examples

``` r
if (FALSE) { # \dontrun{
db_path <- get_db_path("/data/manual")
class2use <- c("Diatom", "Dinoflagellate", "Ciliate")
result <- import_png_folder_to_db(
  "/data/png_export", db_path, class2use,
  class_mapping = c("OldName" = "NewName"),
  annotator = "Jane"
)
cat(result$success, "imported,", result$failed, "failed\n")
} # }
```
