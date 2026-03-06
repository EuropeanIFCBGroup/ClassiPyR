# List bins from an IFCB Dashboard

Fetches the bin list from the Dashboard API. This is a vendored copy of
[`iRfcb::ifcb_list_dashboard_bins()`](https://europeanifcbgroup.github.io/iRfcb/reference/ifcb_list_dashboard_bins.html)
from the development version that supports the `dataset_name` parameter.

## Usage

``` r
list_dashboard_bins(base_url, dataset_name = NULL)
```

## Arguments

- base_url:

  Character. Base URL (e.g. `"https://habon-ifcb.whoi.edu"`).

- dataset_name:

  Optional character. Dataset slug (e.g. `"tangosund"`).

## Value

Character vector of bin (sample) names.

## Examples

``` r
# \donttest{
  bins <- list_dashboard_bins("https://ifcb-data.whoi.edu", "mvco")
# }
```
