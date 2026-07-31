# Impute and Assign Spatial Locations for Microdata

\`imputeLocation()\` is a core architectural helper used within
[`fusionInput`](https://ummel.github.io/fusionData/reference/fusionInput.md).
It reconciles geographic differences between donor and recipient
microdata datasets by imputing Public Use Microdata Areas (PUMAs) for
donor survey records and assigning common geographic intersection
variables to recipient records.

## Usage

``` r
imputeLocation(harmonized, ncores)
```

## Arguments

- harmonized:

  List. The output object from a call to
  [`harmonize`](https://ummel.github.io/fusionData/reference/harmonize.md),
  containing the paired donor and recipient microdata datasets as its
  first and second elements.

- ncores:

  Integer. Number of compute cores passed directly to
  [`gower_topn`](https://rdrr.io/pkg/gower/man/gower_topn.html) for
  parallel distance calculation across observations.

## Value

A named `list` of two `data.table` objects mirroring the structure of
`harmonized`:

- donor:

  Donor microdata containing the original household ID, target PUMA
  variables, and imputed location variables prefixed with `loc..`.

- recipient:

  Recipient microdata containing the original household ID, target PUMA
  variables, and assigned location variables prefixed with `loc..`.

The returned list carries attributes:

- `location.vars`: Character vector of assigned location column names
  (prefixed with `loc..`).

- `intersection.vars`: Character vector of spatial intersection
  variables shared across datasets.

## Details

Microdata sources often report spatial resolution at different levels of
granularity. For example, donor surveys (like AHS or RECS) may report
coarse geographies (e.g., CBSA or Census Region), whereas recipient
datasets (like the ACS) report detailed PUMAs.

\`imputeLocation()\` bridges this gap in two steps:

1.  **Donor Imputation**: Uses Gower's distance via
    [`gower_topn`](https://rdrr.io/pkg/gower/man/gower_topn.html) to
    calculate similarity across common household-level variables between
    donor and recipient households within matching geographic
    intersections. It then samples and imputes a target PUMA for each
    donor observation.

2.  **Recipient Assignment**: Maps common geographic intersection
    variables back to recipient observations by sampling from geographic
    concordance tables (`geo_concordance.fst`) proportional to household
    density (`puma_share`).

Retained spatial variables are renamed with a `loc..` prefix in the
returned list to denote confirmed or assigned spatial indicators.

## See also

[`harmonize`](https://ummel.github.io/fusionData/reference/harmonize.md),
[`fusionInput`](https://ummel.github.io/fusionData/reference/fusionInput.md)
