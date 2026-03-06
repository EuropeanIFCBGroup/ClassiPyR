# Save class taxonomy mappings to SQLite

Stores class-to-AphiaID mappings (with optional accepted names) in the
`class_taxonomy` table of the annotations database.

## Usage

``` r
save_class_taxonomy_db(
  db_path,
  class_aphia_map,
  accepted_name_map = NULL,
  scientific_name_map = NULL,
  accepted_aphia_map = NULL
)
```

## Arguments

- db_path:

  Path to the SQLite database file.

- class_aphia_map:

  Named character vector mapping class names to AphiaID.

- accepted_name_map:

  Optional named character vector mapping class names to WoRMS accepted
  names.

- scientific_name_map:

  Optional named character vector mapping class names to matched
  scientific names (query record).

- accepted_aphia_map:

  Optional named character vector mapping class names to accepted WoRMS
  AphiaID values.

## Value

Logical `TRUE` on success, `FALSE` on failure.

## Examples

``` r
if (FALSE) { # \dontrun{
db_path <- get_db_path(get_default_db_dir())
save_class_taxonomy_db(
  db_path,
  class_aphia_map = c("Prorocentrum micans" = "109636")
)
} # }
```
