# Download individual PNG images from the Dashboard

Downloads specific PNG files from the Dashboard's `/data/` endpoint, one
at a time. This is much faster than downloading entire zip archives when
only a subset of ROIs are needed (e.g., class review mode).

## Usage

``` r
download_dashboard_images_individual(
  base_url,
  file_names,
  dest_dir,
  max_retries = 3,
  sample_fail_threshold = 2
)
```

## Arguments

- base_url:

  Character. Dashboard base URL.

- file_names:

  Character vector. PNG file names (e.g.,
  `"D20240716T000431_IFCB134_00108.png"`).

- dest_dir:

  Character. Destination directory.

- max_retries:

  Integer. Maximum number of retry attempts per image.

- sample_fail_threshold:

  Integer. After this many consecutive failures from the same sample,
  skip all remaining images from that sample.

## Value

Character vector of successfully downloaded file names.

## Details

Samples that fail repeatedly are automatically skipped to avoid long
waits when annotations reference samples not available on the dashboard.
