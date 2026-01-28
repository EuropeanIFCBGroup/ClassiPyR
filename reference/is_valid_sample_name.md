# Validate IFCB sample name format

Checks if a sample name matches the expected IFCB naming convention:
DYYYYMMDDTHHMMSS_IFCBNNN (e.g., D20230101T120000_IFCB134).

## Usage

``` r
is_valid_sample_name(sample_name)
```

## Arguments

- sample_name:

  Sample name to validate

## Value

TRUE if valid, FALSE otherwise

## Examples

``` r
# Valid sample names
is_valid_sample_name("D20230101T120000_IFCB134")
#> [1] TRUE
is_valid_sample_name("D20220522T000439_IFCB1")
#> [1] TRUE

# Invalid sample names
is_valid_sample_name("invalid_name")
#> [1] FALSE
is_valid_sample_name("20230101T120000_IFCB134")  # Missing 'D' prefix
#> [1] FALSE
is_valid_sample_name(NULL)
#> [1] FALSE
```
