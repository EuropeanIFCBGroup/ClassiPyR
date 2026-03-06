# Bulk download zip archives for multiple samples from the Dashboard

Downloads zip files for all specified samples in a single batched call
to
[`ifcb_download_dashboard_data`](https://europeanifcbgroup.github.io/iRfcb/reference/ifcb_download_dashboard_data.html),
leveraging its built-in parallel download support. Samples already
cached are skipped. After download, zips are extracted and cleaned up.

## Usage

``` r
download_dashboard_images_bulk(
  base_url,
  sample_names,
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

- sample_names:

  Character vector. Sample names to download.

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

Character vector of sample names that were successfully downloaded or
already cached.
