# Create new classifications for annotation mode

Creates a classifications data frame with all ROIs set to
"unclassified", for use when annotating a sample from scratch.

## Usage

``` r
create_new_classifications(sample_name, roi_dimensions)
```

## Arguments

- sample_name:

  Sample name (e.g., "D20230101T120000_IFCB134")

- roi_dimensions:

  Data frame from
  [`read_roi_dimensions`](https://europeanifcbgroup.github.io/ClassiPyR/reference/read_roi_dimensions.md)

## Value

Data frame with columns: file_name, class_name, score, width, height,
roi_area

## Examples

``` r
# Create mock ROI dimensions
roi_dims <- data.frame(
  roi_number = 1:5,
  width = c(100, 150, 80, 200, 120),
  height = c(80, 100, 60, 150, 90),
  area = c(8000, 15000, 4800, 30000, 10800)
)

# Create new classifications (all unclassified)
classifications <- create_new_classifications(
  sample_name = "D20230101T120000_IFCB134",
  roi_dimensions = roi_dims
)
print(classifications)
#>                            file_name   class_name score width height roi_area
#> 4 D20230101T120000_IFCB134_00004.png unclassified    NA   200    150    30000
#> 2 D20230101T120000_IFCB134_00002.png unclassified    NA   150    100    15000
#> 5 D20230101T120000_IFCB134_00005.png unclassified    NA   120     90    10800
#> 1 D20230101T120000_IFCB134_00001.png unclassified    NA   100     80     8000
#> 3 D20230101T120000_IFCB134_00003.png unclassified    NA    80     60     4800
```
