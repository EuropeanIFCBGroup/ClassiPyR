# Load classifications from existing MAT annotation file

Reads a MATLAB annotation file (created by ClassiPyR or ifcb-analysis)
and converts class indices to class names using the provided class list.

## Usage

``` r
load_from_mat(mat_path, sample_name, class2use, roi_dimensions)
```

## Arguments

- mat_path:

  Path to MAT file

- sample_name:

  Sample name (e.g., "D20230101T120000_IFCB134")

- class2use:

  Character vector of class names (from class2use file)

- roi_dimensions:

  Data frame from
  [`read_roi_dimensions`](https://europeanifcbgroup.github.io/ClassiPyR/reference/read_roi_dimensions.md)

## Value

Data frame with columns: file_name, class_name, score, roi_area

## Examples

``` r
if (FALSE) { # \dontrun{
# Load existing annotations
dims <- read_roi_dimensions("/data/raw/2023/D20230101/D20230101T120000_IFCB134.adc")
class2use <- load_class_list("/data/class2use.mat")
classifications <- load_from_mat(
  mat_path = "/data/manual/D20230101T120000_IFCB134.mat",
  sample_name = "D20230101T120000_IFCB134",
  class2use = class2use,
  roi_dimensions = dims
)
head(classifications)
} # }
```
