# Compile Universal Spatial Predictor Variables

Detects, aggregates, and harmonizes all processed spatial datasets in
`geo-processed/` into a single, standardized spatial predictor file
(`geo_predictors.fst`) and spatial dictionary (`spatial`).

## Usage

``` r
compileSpatial()
```

## Value

Invisibly returns `NULL`. As side effects, this function writes:

- `geo-processed/geo_predictors.fst` (Compressed PUMA-level predictor
  table)

- `data/spatial.rda` (Package metadata object containing spatial
  variable labels and types)

## Details

`compileSpatial()` processes geographic covariates (e.g., land use,
walkability, climate) across varying temporal vintages and spatial units
into uniform PUMA-level (Public Use Microdata Area) summaries.

The workflow proceeds as follows:

- **Spatial Aggregation:** Processes spatial datasets in parallel across
  CPU cores via `summarizeDataset()`. It matches geographic source
  geometries to 2010 and 2020 PUMA boundaries using geographic
  concordances, aggregating metrics via weighted means (numeric/logical
  variables) or weighted modes (categorical variables).

- **Metadata Extraction:** Extracts variable labels, data types, and
  observed vintage ranges into a standardized spatial dictionary dataset
  (`spatial`).

- **Dense Rank Transformation:** Converts numeric predictor variables
  into dense integer percentile ranks
  (`data.table::frank(..., ties.method = "dense")`) within each
  state-PUMA-vintage grouping prior to expansion.

- **Temporal Expansion:** Fills temporal gaps across years (2000 through
  the prior calendar year) by holding boundary vintages constant
  (front-filling older years with the earliest available vintage and
  back-filling recent years with the latest available vintage).

- **Unified Storage:** Outer-joins all processed spatial datasets across
  PUMA-vintages and exports compressed binary datasets
  (`geo_predictors.fst` and `spatial.rda`).

## Directory Requirement

**Important:** This function must be executed with your R working
directory set to the root of the local `fusionData` project folder
(e.g., `setwd("path/to/fusionData")`). It reads from `geo-processed/`
and outputs package datasets to `data/`.

## Workflow Note

Because `compileSpatial()` exports the spatial dictionary (`spatial`)
into package internal data (`data/spatial.rda`), you should rebuild or
reinstall the local package (e.g., using
[`fusionData::installPackage()`](https://ummel.github.io/fusionData/reference/installPackage.md))
after compilation so updated metadata is recognized in loaded package
sessions.

## See also

[`write_fst`](http://www.fstpackage.org/reference/write_fst.md),
[`use_data`](https://usethis.r-lib.org/reference/use_data.html),
[`fmode`](https://fastverse.org/collapse/reference/fmode.html)

## Examples

``` r
if (FALSE) { # \dontrun{
# Ensure working directory is set to the fusionData repository root
compileSpatial()

# Reinstall local package binaries so updated spatial dictionary is recognized
installPackage()
} # }
```
