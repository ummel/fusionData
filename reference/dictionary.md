# Master Variable Dictionary

Combined variable metadata dictionary compiled across supported
microdata surveys and vintages.

## Usage

``` r
dictionary
```

## Format

A `tbl_df` tibble data frame with 8 variables:

- Survey:

  Abbreviated survey name, e.g., 'ACS' (character)

- Vintage:

  4-digit survey vintage year (character)

- Respondent:

  Respondent level, e.g., 'Household' or 'Person' (character)

- Variable:

  Harmonized variable code/identifier (character)

- Description:

  Variable label or human-readable description (character)

- Values:

  Encoded or factor value level descriptions (character)

- Type:

  Variable data type designation, e.g., 'fct', 'ord', 'dbl' (character)

- Custom:

  Logical indicator if the variable is a custom calculated feature
  (logical)
