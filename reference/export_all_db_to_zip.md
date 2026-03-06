# Bulk export all annotated samples from SQLite to EcoTaxa-ready ZIP

Exports annotated samples to class-organized PNG folders, writes one
inventory `.txt` file per class folder, and then zips the result using
`iRfcb::ifcb_zip_pngs(include_txt = TRUE)`.

## Usage

``` r
export_all_db_to_zip(
  db_path,
  zip_path,
  roi_path_map,
  skip_class = NULL,
  readme_file = system.file("exdata/README-template.md", package = "iRfcb")
)
```

## Arguments

- db_path:

  Path to the SQLite database file

- zip_path:

  Full output path for the resulting ZIP archive

- roi_path_map:

  Named list mapping sample names to `.roi` file paths. Samples without
  an entry are skipped.

- skip_class:

  Character vector of class names to exclude from export (e.g.
  `"unclassified"`). Default `NULL` exports all classes.

- readme_file:

  Optional README markdown file included in ZIP. Defaults to
  `system.file("exdata/README-template.md", package = "iRfcb")`.

## Value

Named list with counts: `success`, `failed`, `skipped`,
`inventory_files`, and `zip_path`.
