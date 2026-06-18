# Backfill missing ROIs as "unclassified" in the database

After a partial import (e.g.
[`import_png_folder_to_db`](https://europeanifcbgroup.github.io/ClassiPyR/reference/import_png_folder_to_db.md)
with only a few selected taxa per sample), the database holds
annotations for just the imported ROIs. This helper reads each sample's
complete ROI list from its `.adc` file and inserts the ROIs that are not
yet in the database as `"unclassified"`, so the full sample is
represented. Existing annotations are never modified.

## Usage

``` r
fill_unclassified_db(
  db_path,
  roi_folder,
  samples = NULL,
  class_name = "unclassified",
  annotator = "imported"
)
```

## Arguments

- db_path:

  Path to the SQLite database file

- roi_folder:

  Base ROI folder path, following the standard IFCB folder structure
  (`roi_folder/YYYY/DYYYYMMDD/sample_name.adc`). Used to locate each
  sample's `.adc` file via
  [`get_sample_paths`](https://europeanifcbgroup.github.io/ClassiPyR/reference/get_sample_paths.md).

- samples:

  Optional character vector of sample names to backfill. When `NULL`
  (the default), all annotated samples in the database are used.

- class_name:

  Class name to assign to the missing ROIs. Default `"unclassified"`.

- annotator:

  Annotator name recorded for the inserted rows. Default `"imported"`.

## Value

Named list with counts: `added` (ROIs inserted), `samples` (samples that
received at least one new ROI), `skipped` (samples with no reachable
`.adc` file).

## Details

Only ROIs with a real image (non-zero width and height) are added.
Inserted rows are marked `is_manual = 0` (not yet reviewed).

## Examples

``` r
if (FALSE) { # \dontrun{
db_path <- get_db_path("/data/manual")
fill_unclassified_db(db_path, "/data/ifcb/raw")
} # }
```
