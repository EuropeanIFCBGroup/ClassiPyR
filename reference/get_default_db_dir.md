# Get default database directory

Returns the default path for the SQLite annotations database. This is a
persistent, local, user-level directory that survives package
reinstalls. The database should be stored on a local filesystem, not on
a network drive, because SQLite file locking is unreliable over network
filesystems.

## Usage

``` r
get_default_db_dir()
```

## Value

Path to the default database directory

## See also

[`get_db_path`](https://europeanifcbgroup.github.io/ClassiPyR/reference/get_db_path.md)
for the full database file path

## Examples

``` r
# Get the default database directory
db_dir <- get_default_db_dir()
print(db_dir)
#> [1] "/home/runner/.local/share/R/ClassiPyR"
```
