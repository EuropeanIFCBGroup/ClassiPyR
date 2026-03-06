# Read PNG dimensions from image header

Reads width and height from the PNG IHDR chunk without decoding full
image data. Returns \`NA\` values when the file is missing or not a
valid PNG.

## Usage

``` r
read_png_dimensions(png_path)
```

## Arguments

- png_path:

  Path to PNG file

## Value

Named list with \`width\` and \`height\` (numeric)
