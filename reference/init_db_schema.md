# Initialize the annotations database schema

Creates the `annotations` and `class_lists` tables if they do not
already exist.

## Usage

``` r
init_db_schema(con)
```

## Arguments

- con:

  A DBI connection object

## Value

NULL (called for side effects)
