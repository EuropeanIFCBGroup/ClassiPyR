# Export annotated images from SQLite to class-organized PNG folders

Reads annotations for a single sample from the database and extracts PNG
images from the ROI file, placing each image into a subfolder named
after its assigned class.

## Usage

``` r
export_db_to_png(db_path, sample_name, roi_path, png_folder, skip_class = NULL)
```

## Arguments

- db_path:

  Path to the SQLite database file

- sample_name:

  Sample name

- roi_path:

  Path to the `.roi` file for this sample

- png_folder:

  Base output folder. Images are written to `png_folder/<class_name>/`

- skip_class:

  Character vector of class names to exclude from export (e.g.
  `"unclassified"`). Default `NULL` exports all classes.

## Value

TRUE on success, FALSE on failure

## Examples

``` r
if (FALSE) { # \dontrun{
db_path <- get_db_path("/data/manual")
export_db_to_png(db_path, "D20230101T120000_IFCB134",
                 "/data/raw/2023/D20230101/D20230101T120000_IFCB134.roi",
                 "/data/png_output",
                 skip_class = "unclassified")
} # }
```
