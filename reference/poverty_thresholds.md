# Historical Official Poverty Thresholds

Historical official U.S. Census Bureau poverty thresholds organized by
family size, number of related minor children, senior status, and year.

## Usage

``` r
poverty_thresholds
```

## Format

A `data.table` data frame with 5 variables:

- year:

  4-digit calendar year (integer)

- size:

  Total family unit size (integer)

- minors:

  Number of related minor children under 18 years (integer)

- senior:

  Logical flag indicating if the householder is 65 years or older for
  single and two-person units (logical)

- threshold:

  Official dollar poverty threshold (integer)

## Source

U.S. Census Bureau, Current Population Survey (CPS) Poverty Thresholds.
