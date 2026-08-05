# Block Group Geographic Crosswalk

Geographic relationship and crosswalk table linking block groups across
2010 and 2020 Census definitions.

## Usage

``` r
bg_crosswalk
```

## Format

A data frame with 3 variables:

- bg10:

  2010 12-digit block group GEOID (character)

- bg20:

  2020 12-digit block group GEOID (character)

- xwalk_weight:

  Population (2020) of the geographic intersection (integer)

## Source

2020 Block Groups to 2010 Block Groups NHGIS crosswalk
(https://www.nhgis.org/geographic-crosswalks)
