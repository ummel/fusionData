# PUMA Geographic Crosswalk

Geographic relationship and crosswalk table linking Public Use Microdata
Areas (PUMAs) across 2010 and 2020 Census definitions.

## Usage

``` r
puma_crosswalk
```

## Format

A data frame with 4 variables:

- state:

  2-digit state FIPS code (character)

- puma20:

  2020 5-digit PUMA code (character)

- puma10:

  2010 5-digit PUMA code (character)

- xwalk_weight:

  Population (2020) of the geographic intersection (integer)

## Source

2010-2020 PUMA NHGIS crosswalk
(https://usa.ipums.org/usa/volii/pumas20.shtml)
