# Download a single PNG image from the Dashboard

Downloads one PNG from the Dashboard's `/data/` endpoint. The image is
saved to `dest_dir/sample_name/file_name`.

## Usage

``` r
download_dashboard_image_single(
  base_url,
  sample_name,
  roi_number,
  dest_dir,
  max_retries = 3,
  timeout = 15
)
```

## Arguments

- base_url:

  Character. Dashboard base URL.

- sample_name:

  Character. Sample name (bin PID).

- roi_number:

  Integer. ROI number to download.

- dest_dir:

  Character. Destination directory.

- max_retries:

  Integer. Maximum number of retry attempts.

- timeout:

  Numeric. Request timeout in seconds.

## Value

File path to the downloaded PNG, or NULL on failure.
