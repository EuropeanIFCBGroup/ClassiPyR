# Parse an IFCB Dashboard URL

Extracts the base URL and optional dataset name from a Dashboard URL.

## Usage

``` r
parse_dashboard_url(url)
```

## Arguments

- url:

  Character. A Dashboard URL, e.g. `"https://habon-ifcb.whoi.edu/"` or
  `"https://habon-ifcb.whoi.edu/timeline?dataset=tangosund"`.

## Value

A list with `base_url` (without trailing slash) and `dataset_name`
(character or NULL).

## Examples

``` r
parse_dashboard_url("https://habon-ifcb.whoi.edu/")
#> $base_url
#> [1] "https://habon-ifcb.whoi.edu"
#> 
#> $dataset_name
#> NULL
#> 
parse_dashboard_url("https://habon-ifcb.whoi.edu/timeline?dataset=tangosund")
#> $base_url
#> [1] "https://habon-ifcb.whoi.edu"
#> 
#> $dataset_name
#> [1] "tangosund"
#> 
```
