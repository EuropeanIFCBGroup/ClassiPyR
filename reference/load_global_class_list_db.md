# Load global class list from SQLite

Returns the class list stored in the `global_class_list` table, ordered
by `class_index`. Returns `NULL` if the table is empty or the database
does not exist.

## Usage

``` r
load_global_class_list_db(db_path)
```

## Arguments

- db_path:

  Path to the SQLite database file.

## Value

Character vector of class names, or `NULL` if unavailable.

## Examples

``` r
if (FALSE) { # \dontrun{
db_path <- get_db_path(get_default_db_dir())
classes <- load_global_class_list_db(db_path)
} # }
```
