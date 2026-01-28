# Load class list from MAT or TXT file

Reads a class list from either a MATLAB .mat file (class2use format) or
a plain text file with one class per line. Class names are sanitized for
safe use in file paths and HTML.

## Usage

``` r
load_class_list(file_path)
```

## Arguments

- file_path:

  Path to class2use file (.mat or .txt)

## Value

Character vector of class names

## Examples

``` r
if (FALSE) { # \dontrun{
# Load from MATLAB file (requires Python)
classes <- load_class_list("/path/to/class2use.mat")

# Load from text file
classes <- load_class_list("/path/to/class2use.txt")
} # }

# Create a temporary text file for demonstration
tmp_file <- tempfile(fileext = ".txt")
writeLines(c("Diatom", "Ciliate", "Dinoflagellate"), tmp_file)
classes <- load_class_list(tmp_file)
print(classes)
#> [1] "Diatom"         "Ciliate"        "Dinoflagellate"
unlink(tmp_file)
```
