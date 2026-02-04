# Load classifications from MATLAB classifier output file

Reads a MATLAB classifier output file (from ifcb-analysis random forest
classifier) and extracts class predictions.

## Usage

``` r
load_from_classifier_mat(
  mat_path,
  sample_name,
  class2use,
  roi_dimensions,
  use_threshold = TRUE
)
```

## Arguments

- mat_path:

  Path to classifier MAT file (matching pattern \*\_class\*.mat)

- sample_name:

  Sample name (e.g., "D20230101T120000_IFCB134")

- class2use:

  Character vector of class names (unused, kept for API consistency)

- roi_dimensions:

  Data frame from
  [`read_roi_dimensions`](https://europeanifcbgroup.github.io/ClassiPyR/reference/read_roi_dimensions.md)

- use_threshold:

  Logical, whether to use threshold-based classification
  (TBclass_above_threshold) or raw predictions (TBclass)

## Value

Data frame with columns: file_name, class_name, score, width, height,
roi_area

## Examples

``` r
if (FALSE) { # \dontrun{
# Load classifier predictions
dims <- read_roi_dimensions("/data/raw/2023/D20230101/D20230101T120000_IFCB134.adc")
classifications <- load_from_classifier_mat(
  mat_path = "/data/classified/D20230101T120000_IFCB134_class_v1.mat",
  sample_name = "D20230101T120000_IFCB134",
  class2use = NULL,
  roi_dimensions = dims,
  use_threshold = TRUE
)
head(classifications)
} # }
```
