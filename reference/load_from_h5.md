# Load classifications from HDF5 classifier output file

Reads an HDF5 classifier output file (from iRfcb 0.8.0+) and extracts
class predictions. Requires the hdf5r package.

## Usage

``` r
load_from_h5(h5_path, sample_name, roi_dimensions, use_threshold = TRUE)
```

## Arguments

- h5_path:

  Path to classifier H5 file (matching pattern \*\_class\*.h5)

- sample_name:

  Sample name (e.g., "D20220522T000439_IFCB134")

- roi_dimensions:

  Data frame from
  [`read_roi_dimensions`](https://europeanifcbgroup.github.io/ClassiPyR/reference/read_roi_dimensions.md)

- use_threshold:

  Logical, whether to use the threshold-filtered `class_name` dataset
  (default `TRUE`) or the raw `class_name_auto` dataset.

## Value

Data frame with columns: file_name, class_name, score, width, height,
roi_area

## Examples

``` r
if (FALSE) { # \dontrun{
dims <- read_roi_dimensions("/data/raw/2022/D20220522/D20220522T000439_IFCB134.adc")
classifications <- load_from_h5(
  h5_path = "/data/classified/D20220522T000439_IFCB134_class.h5",
  sample_name = "D20220522T000439_IFCB134",
  roi_dimensions = dims,
  use_threshold = TRUE
)
head(classifications)
} # }
```
