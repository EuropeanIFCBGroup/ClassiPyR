# Load all annotations for a specific class from the database

Returns every annotation matching `class_name`, with a computed
`file_name` column for gallery display. Optional filters restrict
results by year, month, or instrument.

## Usage

``` r
load_class_annotations_db(
  db_path,
  class_name,
  year = NULL,
  month = NULL,
  instrument = NULL,
  annotator = NULL
)
```

## Arguments

- db_path:

  Path to the SQLite database file

- class_name:

  Class name to load

- year:

  Optional year filter (e.g. `"2023"`)

- month:

  Optional month filter (e.g. `"03"`)

- instrument:

  Optional instrument filter (e.g. `"IFCB134"`)

- annotator:

  Optional annotator name filter (e.g. `"Jane"`)

## Value

Data frame with columns `sample_name`, `roi_number`, `class_name`, and
`file_name`. Returns `NULL` if no annotations match.

## Examples

``` r
if (FALSE) { # \dontrun{
db_path <- get_db_path("/data/manual")
diatoms <- load_class_annotations_db(db_path, "Diatom")
diatoms_2023 <- load_class_annotations_db(db_path, "Diatom", year = "2023")
} # }
```
