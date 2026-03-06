# Scan a PNG folder with class subfolders

Scans a directory containing PNG images organized into class-name
subfolders (e.g. as exported by
[`export_db_to_png`](https://europeanifcbgroup.github.io/ClassiPyR/reference/export_db_to_png.md)
or other tools). Folder names follow the iRfcb convention where a
trailing 3-digit suffix is stripped (e.g. `Diatom_001` becomes
`Diatom`).

## Usage

``` r
scan_png_class_folder(png_folder)
```

## Arguments

- png_folder:

  Path to the top-level folder containing class subfolders

## Value

A list with components:

- annotations:

  Data frame with columns `sample_name`, `roi_number`, `file_name`, and
  `class_name`

- classes_found:

  Character vector of unique class names found

- sample_names:

  Character vector of unique sample names found

## Examples

``` r
if (FALSE) { # \dontrun{
result <- scan_png_class_folder("/data/png_export")
head(result$annotations)
result$classes_found
result$sample_names
} # }
```
