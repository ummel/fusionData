# Spatial Predictor Metadata

Metadata index mapping spatial predictors to their source variables,
vintage, value coding, and statistical data types.

## Usage

``` r
spatial
```

## Format

A `tbl_df` tibble data frame with 5 variables:

- predictor:

  Spatial predictor variable name (character)

- variable_rds:

  Source variable identifier string (character)

- vintage:

  4-digit survey or dataset vintage year (character)

- values:

  Named list containing valid value definitions or categories (named
  list)

- type:

  Data type specification, e.g., 'dbl' (character)
