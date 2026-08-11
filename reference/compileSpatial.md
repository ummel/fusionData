# Compile Spatial Predictor Variables

Detects, aggregates, and harmonizes all processed spatial datasets in
`geo-processed/` into a single, standardized spatial predictor file
(`geo_predictors.fst`) and spatial dictionary (`spatial`).

## Usage

``` r
compileSpatial()
```

## Details

Processes geographic covariates (e.g., land use, walkability, climate)
across varying temporal vintages and spatial units into uniform
PUMA-level summaries.

The workflow proceeds as follows:

- **Spatial Aggregation:** Processes spatial datasets in parallel across
  CPU cores via `summarizeDataset()`. Matches geographic source
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
  the prior calendar year) by holding boundary vintages constant.

- **Unified Storage:** Outer-joins all processed spatial datasets across
  PUMA-vintages and exports compressed binary datasets
  (`geo_predictors.fst` and `spatial.rda`).

**Important:** This function must be executed with your R working
directory set to the root of the local `fusionData` project folder
(e.g., `setwd("path/to/fusionData")`). It relies on relative directory
paths (`geo-processed/`.
