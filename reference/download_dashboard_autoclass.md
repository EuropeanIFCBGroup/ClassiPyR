# Download and parse autoclass scores from the Dashboard

Downloads `_class_scores.csv` for a sample and extracts the winning
class (column with max score) per ROI.

## Usage

``` r
download_dashboard_autoclass(
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

  Character. Sample name.

- cache_dir:

  Character. Cache directory.

- parallel_downloads:

  Integer. Number of parallel downloads.

- sleep_time:

  Numeric. Seconds to sleep between download batches.

- multi_timeout:

  Numeric. Timeout in seconds for multi-file downloads.

- max_retries:

  Integer. Maximum number of retry attempts.

## Value

Data frame with columns `file_name`, `class_name`, `score`, or NULL on
failure.
