# Sanitize string for safe use in HTML/file paths

Removes or replaces characters that could be dangerous in HTML contexts
or file paths, including XSS attack vectors and path traversal attempts.

## Usage

``` r
sanitize_string(x)
```

## Arguments

- x:

  String to sanitize

## Value

Sanitized string

## Examples

``` r
# Remove HTML special characters
sanitize_string("<script>alert('xss')</script>")
#> [1] "scriptalert(xss)script"

# Remove path traversal attempts
sanitize_string("../../../etc/passwd")
#> [1] "etcpasswd"

# Normal strings pass through
sanitize_string("Diatom_chain")
#> [1] "Diatom_chain"
```
