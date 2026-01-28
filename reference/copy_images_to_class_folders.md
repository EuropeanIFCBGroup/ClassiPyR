# Copy images to class-organized folders

Copies PNG images from a flat source folder into class-organized
subfolders, both for temporary use by ifcb_annotate_samples and for
permanent storage.

## Usage

``` r
copy_images_to_class_folders(
  classifications,
  src_folder,
  temp_folder,
  output_folder
)
```

## Arguments

- classifications:

  Classifications data frame with file_name and class_name columns

- src_folder:

  Source folder containing PNG images

- temp_folder:

  Temporary folder for ifcb_annotate_samples

- output_folder:

  Permanent output folder for class-organized images

## Value

NULL (called for side effects)

## Examples

``` r
if (FALSE) { # \dontrun{
# Copy images to class folders
copy_images_to_class_folders(
  classifications = data.frame(
    file_name = c("sample_00001.png", "sample_00002.png"),
    class_name = c("Diatom", "Ciliate")
  ),
  src_folder = "/tmp/png/sample",
  temp_folder = "/tmp/annotate",
  output_folder = "/data/png_output"
)
} # }
```
