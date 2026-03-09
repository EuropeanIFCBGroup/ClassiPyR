# Resolve the dataset name for a sample from the Dashboard API

Queries the `/api/bin/<sample>` endpoint to retrieve the
`primary_dataset` field. Useful when the user did not provide a
`?dataset=` query parameter in the dashboard URL.

## Usage

``` r
resolve_sample_dataset(base_url, sample_name)
```

## Arguments

- base_url:

  Character. Dashboard base URL (no trailing slash).

- sample_name:

  Character. Sample name (bin PID).

## Value

Character dataset name, or NULL if it could not be resolved.
