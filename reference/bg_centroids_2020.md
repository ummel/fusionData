# Block group centroids circa 2020

Population-weighted block group centroids. `sf` points object. Useful
for assigning any coordinate-based spatial features to block groups to
create geographic concordance; e.g. using `st_nearest_feature`.

## Usage

``` r
bg_centroids_2020
```

## Format

A `sf` spatial data frame with 6 variables:

- state:

  State FIPS code (character)

- county20:

  2020 County FIPS code (character)

- tract20:

  2020 Census tract code (character)

- bg20:

  2020 Block group code (character)

- pop20:

  2020 total population count (integer)

- geometry:

  Centroid coordinates as `sfc_POINT` class

## Source

<https://www2.census.gov/geo/docs/reference/cenpop2020/blkgrp/>
