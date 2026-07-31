# Block group centroids circa 2010

Population-weighted block group centroids. `sf` points object. Useful
for assigning any coordinate-based spatial features to block groups to
create geographic concordance; e.g. using `st_nearest_feature`.

## Usage

``` r
bg_centroids_2010
```

## Format

A `sf` spatial data frame with 6 variables:

- state:

  State FIPS code (character)

- county10:

  2010 County FIPS code (character)

- tract10:

  2010 Census tract code (character)

- bg10:

  2010 Block group code (character)

- pop10:

  2010 total population count (integer)

- geometry:

  Centroid coordinates as `sfc_POINT` class

## Source

<https://www2.census.gov/geo/docs/reference/cenpop2010/blkgrp/>
