# Export annotations from SQLite to a .mat file

Reads annotations for a single sample from the database and writes a
MATLAB-compatible annotation file using
[`iRfcb::ifcb_create_manual_file`](https://europeanifcbgroup.github.io/iRfcb/reference/ifcb_create_manual_file.html).
Requires Python with scipy.

## Usage

``` r
export_db_to_mat(db_path, sample_name, output_folder)
```

## Arguments

- db_path:

  Path to the SQLite database file

- sample_name:

  Sample name

- output_folder:

  Folder where the .mat file will be written

## Value

TRUE on success, FALSE on failure

## Examples

``` r
if (FALSE) { # \dontrun{
db_path <- get_db_path("/data/manual")
export_db_to_mat(db_path, "D20230101T120000_IFCB134", "/data/manual")
} # }
```
