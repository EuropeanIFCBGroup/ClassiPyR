# Import a .mat annotation file into the SQLite database

Reads an existing .mat annotation file and writes its data into the
SQLite database. The class list (`class2use_manual`) and classlist
indices are read directly from the .mat file to ensure a faithful
import. ROIs with NaN indices (not yet reviewed) are stored with
`is_manual = 0`.

## Usage

``` r
import_mat_to_db(mat_path, db_path, sample_name, annotator = "imported")
```

## Arguments

- mat_path:

  Path to the .mat annotation file

- db_path:

  Path to the SQLite database file

- sample_name:

  Sample name

- annotator:

  Annotator name (defaults to `"imported"`)

## Value

TRUE on success, FALSE on failure

## Examples

``` r
if (FALSE) { # \dontrun{
import_mat_to_db(
  mat_path = "/data/manual/D20230101T120000_IFCB134.mat",
  db_path = get_db_path("/data/manual"),
  sample_name = "D20230101T120000_IFCB134"
)
} # }
```
