# Sanitize taxon names for WoRMS matching

Cleans IFCB-style class labels before querying WoRMS. Names longer than
`max_chars` are skipped (returned as empty strings).

## Usage

``` r
sanitize_worms_query(x, max_chars = 80L)
```

## Arguments

- x:

  Character vector of class/taxon labels.

- max_chars:

  Maximum allowed input length before skipping a query. Default `80`.

## Value

Character vector of sanitized query strings.

## Examples

``` r
sanitize_worms_query(c(
  "Prorocentrum_micans",
  "Alexandrium_cf._tamarense",
  "Very_very_long_label_that_should_be_skipped"
), max_chars = 30)
#> [1] "Prorocentrum micans"   "Alexandrium tamarense" ""                     
```
