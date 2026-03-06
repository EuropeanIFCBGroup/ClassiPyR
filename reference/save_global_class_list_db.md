# Save annotations to the SQLite database

Writes (or replaces) annotations for a single sample. The existing rows
for the sample are deleted first so that re-saving acts as an upsert.

## Usage

``` r
save_global_class_list_db(db_path, class2use)
```

## Arguments

- db_path:

  Path to the SQLite database file.

- class2use:

  Character vector of class names.

## Value

Logical `TRUE` on success, `FALSE` on failure.

## Details

Save global class list to SQLite

Replaces the contents of the `global_class_list` table with the supplied
class names, preserving their index order. This is used to auto-persist
the in-app classlist so it survives across sessions.

## Examples

``` r
if (FALSE) { # \dontrun{
db_path <- get_db_path(get_default_db_dir())
save_global_class_list_db(db_path, c("unclassified", "Diatom", "Ciliate"))
} # }
```
