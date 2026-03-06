# Build WoRMS match rows for class names

Runs WoRMS lookup for one or more class names and returns a standardized
results table suitable for UI display or downstream processing.

## Usage

``` r
build_worms_match_rows(class_names, raw_queries)
```

## Arguments

- class_names:

  Character vector of original class names.

- raw_queries:

  Character vector of query strings (same length as `class_names`),
  typically class names or manual overrides.

## Value

Data frame with columns: `class_name`, `query_name`, `scientific_name`,
`matched_name`, `accepted_name`, `aphia_id` (query AphiaID),
`accepted_aphia_id`, `status`, and `note`.

## Details

Requires the optional worrms package.

## Examples

``` r
if (FALSE) { # \dontrun{
build_worms_match_rows(
  class_names = c("Prorocentrum_micans", "Alexandrium_cf_tamarense"),
  raw_queries = c("Prorocentrum micans", "Alexandrium tamarense")
)
} # }
```
