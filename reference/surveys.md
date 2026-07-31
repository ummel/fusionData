# Survey Metadata Index

Summary index detailing processed survey vintages, respondent levels,
sample sizes, variable counts, and file sizes.

## Usage

``` r
surveys
```

## Format

A `tbl_df` tibble data frame with 6 variables:

- Survey:

  Abbreviated survey name (character)

- Vintage:

  4-digit survey vintage year (character)

- Respondent:

  Respondent level, e.g., 'Household' or 'Person' (character)

- Sample size:

  Total sample size count formatted string (character)

- No. of variables:

  Number of survey variables formatted string (character)

- Size on disk (MB):

  File storage footprint on disk in megabytes formatted string
  (character)
