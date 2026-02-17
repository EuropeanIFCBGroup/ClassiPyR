# Get path to the annotations SQLite database

Returns the path to `annotations.sqlite` in the given database
directory. The database directory should be on a local filesystem, not a
network drive, because [SQLite file locking is unreliable over network
filesystems](https://www.sqlite.org/useovernet.html).

## Usage

``` r
get_db_path(db_folder)
```

## Arguments

- db_folder:

  Path to the database directory. Defaults to
  [`get_default_db_dir()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/get_default_db_dir.md),
  a persistent local directory.

## Value

Path to the SQLite database file

## See also

[`get_default_db_dir`](https://europeanifcbgroup.github.io/ClassiPyR/reference/get_default_db_dir.md)
for the default database directory

## Examples

``` r
# Use the default local database directory
get_db_path(get_default_db_dir())
#> [1] "/home/runner/.local/share/R/ClassiPyR/annotations.sqlite"

# Or specify a custom directory
get_db_path("/data/local_db")
#> [1] "/data/local_db/annotations.sqlite"
```
