# Update the annotator name for one or more samples

Changes the annotator field for all annotations belonging to the
specified sample(s). This is useful for correcting the annotator after
bulk imports or when transferring ownership of annotations.

## Usage

``` r
update_annotator(db_path, sample_names, annotator)
```

## Arguments

- db_path:

  Path to the SQLite database file

- sample_names:

  Character vector of sample names to update

- annotator:

  New annotator name

## Value

Named integer vector with the number of rows updated per sample. Samples
not found in the database are included with a count of 0.

## Examples

``` r
if (FALSE) { # \dontrun{
db_path <- get_db_path("/data/manual")

# Update a single sample
update_annotator(db_path, "D20230101T120000_IFCB134", "Jane")

# Update multiple samples at once
update_annotator(db_path,
                 c("D20230101T120000_IFCB134", "D20230202T080000_IFCB134"),
                 "Jane")

# Update all annotated samples
all_samples <- list_annotated_samples_db(db_path)
update_annotator(db_path, all_samples, "Jane")
} # }
```
