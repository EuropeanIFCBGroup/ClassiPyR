# List bins from an IFCB Dashboard

Fetches the list of bin (sample) names for a dashboard dataset.
Delegates to
[`ifcb_download_dashboard_metadata`](https://europeanifcbgroup.github.io/iRfcb/reference/ifcb_download_dashboard_metadata.html),
which retrieves per-bin metadata from the `api/export_metadata`
endpoint, and returns the `pid` column.

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

## Details

The previous implementation used the `api/list_bins` endpoint, which was
removed from the upstream IFCB Dashboard (2026-03-08) and no longer
works.

## Examples

``` r
if (FALSE) { # \dontrun{
  bins <- list_dashboard_bins("https://ifcb-data.whoi.edu", "mvco")
} # }
```
