# National Personal Consumption Expenditures (PCE)

Bureau of Economic Analysis (BEA) national-level Personal Consumption
Expenditures price index and series from 1959 to 2020 used for
macroeconomic deflating and scaling.

## Usage

``` r
BEA_pce_national
```

## Format

A `data.table` data frame with 73 variables:

- line:

  BEA line item number (integer)

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

- parent4:

  Hierarchical parent series ID level 4 (character)

- parent5:

  Hierarchical parent series ID level 5 (character)

- parent6:

  Hierarchical parent series ID level 6 (character)

- parent7:

  Hierarchical parent series ID level 7 (character)

- parent8:

  Hierarchical parent series ID level 8 (character)

- 1959-2020:

  Annual PCE expenditure/index values by year column (integer)

## Source

U.S. Bureau of Economic Analysis (BEA).
