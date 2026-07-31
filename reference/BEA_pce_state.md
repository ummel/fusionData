# State Personal Consumption Expenditures (PCE)

Bureau of Economic Analysis (BEA) state-level Personal Consumption
Expenditures price index and series from 1997 to 2019.

## Usage

``` r
BEA_pce_state
```

## Format

A `data.table` data frame with 31 variables:

- line:

  BEA line item number (integer)

- state_fips:

  2-digit state FIPS code (character)

- state_name:

  State name (character)

- pce_desc:

  Description of the PCE line item category (character)

- pce_series:

  BEA series identification code (character)

- parent1:

  Hierarchical parent series ID level 1 (character)

- parent2:

  Hierarchical parent series ID level 2 (character)

- parent3:

  Hierarchical parent series ID level 3 (character)

- 1997-2019:

  Annual state-level PCE expenditure/index values by year column
  (numeric/integer)

## Source

U.S. Bureau of Economic Analysis (BEA).
