# Download and extract PNG images from the Dashboard

Downloads a zip file of PNG images for a sample from the Dashboard.
Extracts into the cache directory. Skips re-download if PNGs already
exist.

## Usage

``` r
download_dashboard_images(
  base_url,
  sample_name,
  cache_dir = get_dashboard_cache_dir(),
  parallel_downloads = 5,
  sleep_time = 2,
  multi_timeout = 120,
  max_retries = 3
)
```

## Arguments

- base_url:

  Character. Dashboard base URL.

- sample_name:

  Character. Sample name (bin PID).

- cache_dir:

  Character. Cache directory. Defaults to
  [`get_dashboard_cache_dir()`](https://europeanifcbgroup.github.io/ClassiPyR/reference/get_dashboard_cache_dir.md).

- parallel_downloads:

  Integer. Number of parallel downloads.

- sleep_time:

  Numeric. Seconds to sleep between download batches.

- multi_timeout:

  Numeric. Timeout in seconds for multi-file downloads.

- max_retries:

  Integer. Maximum number of retry attempts.

## Value

Path to the folder containing extracted PNGs, or NULL on failure.
