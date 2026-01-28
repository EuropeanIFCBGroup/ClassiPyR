# Create empty changes log data frame

Creates an empty data frame with the correct structure for tracking
annotation changes during a session.

## Usage

``` r
create_empty_changes_log()
```

## Value

Empty data frame with columns: image, original_class, new_class

## Examples

``` r
# Create an empty changes log
changes <- create_empty_changes_log()
print(names(changes))
#> [1] "image"          "original_class" "new_class"     
print(nrow(changes))
#> [1] 0
```
