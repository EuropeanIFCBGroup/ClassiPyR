# Load class taxonomy mappings from SQLite

Reads class-to-AphiaID mappings from the `class_taxonomy` table.

## Usage

``` r
load_class_taxonomy_db(db_path)
```

## Arguments

- db_path:

  Path to the SQLite database file.

## Value

Named character vector mapping class names to AphiaID. Returns empty
vector if database/table is missing or empty.

## Examples

``` r
if (FALSE) { # \dontrun{
db_path <- get_db_path(get_default_db_dir())
map <- load_class_taxonomy_db(db_path)
} # }
```
