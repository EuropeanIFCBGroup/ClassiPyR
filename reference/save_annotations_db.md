# Save annotations to the SQLite database

Writes (or replaces) annotations for a single sample. The existing rows
for the sample are deleted first so that re-saving acts as an upsert.

## Usage

``` r
save_annotations_db(
  db_path,
  sample_name,
  classifications,
  class2use,
  annotator = "Unknown",
  is_manual = NULL
)
```

## Arguments

- db_path:

  Path to the SQLite database file

- sample_name:

  Sample name (e.g., `"D20230101T120000_IFCB134"`)

- classifications:

  Data frame with at least `file_name` and `class_name` columns

- class2use:

  Character vector of class names (preserves index order for .mat
  export)

- annotator:

  Annotator name

- is_manual:

  Integer vector of 0/1 flags indicating whether each ROI was manually
  reviewed (1) or not yet reviewed (0, corresponding to NaN in .mat
  files). If `NULL` (the default), all ROIs are treated as reviewed.

## Value

TRUE on success, FALSE on failure

## Examples

``` r
if (FALSE) { # \dontrun{
db_path <- get_db_path("/data/manual")
save_annotations_db(db_path, "D20230101T120000_IFCB134",
                    classifications, class2use, "Jane")
} # }
```
