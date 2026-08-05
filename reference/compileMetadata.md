# Compile and Synchronize Package Datasets

Compiles updated datasets from source files and rebuilds the installed
package's lazy-load database in-place. These functions should be
executed following any addition, deletion, or modification to:

- Microdata dictionaries

- Spatial predictor data

- Data objects created or modified by code inside the `data-raw/`
  directory

Running `compileMetadata()` provides a streamlined way to sync updated
data directly into the active R library without needing to build the
package from source.

## Usage

``` r
compileDictionary()

compileMetadata(compile_dictionary = TRUE, compile_spatial = TRUE)

compileSpatial()
```

## Arguments

- compile_dictionary:

  Logical. If `TRUE` (default), runs `compileDictionary()` to regenerate
  dictionary objects into `./data/`.

- compile_spatial:

  Logical. If `TRUE` (default), runs `compileSpatial()` to regenerate
  spatial objects into `./data/`.

## Value

Invisibly returns a character vector of dataset names that were updated.

## Details

**`compileDictionary()`:** Aggregates individual survey metadata
codebooks stored in `survey-processed/` into a single, standardized data
dictionary (`dictionary`) and a high-level survey metadata summary
(`surveys`).

This function forms part of the **Document** step in the `fusionData`
package workflow. It scans all recursive survey metadata files
(`*_dictionary.rds`) created during survey ingest, standardizes
respondent types, calculates microdata disk footprints (including
`.processed.fst` and optional `custom.fst` files), and outputs unified
data objects needed by the package and embedded Shiny applications
(`universe` and `harmony`).

**`compileSpatial()`:** Detects, aggregates, and harmonizes all
processed spatial datasets in `geo-processed/` into a single,
standardized spatial predictor file (`geo_predictors.fst`) and spatial
dictionary (`spatial`).

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

## Directory Requirement (`compileDictionary`)

**Important:** This function must be executed with your R working
directory set to the root of the local `fusionData` project folder
(e.g., `setwd("path/to/fusionData")`). It relies on relative directory
paths (`survey-processed/`, `harmony/www/`, and `universe/www/`).

## Side Effects on Disk

Running these compilation functions executes several disk-writing
operations:

- **Local Package Data (`data/`):** Overwrites or generates `.rda`
  binary objects (`dictionary.rda`, `surveys.rda`, `spatial.rda`, etc.)
  in the project's local `./data/` folder.

- **Export Dependencies:** Writes updated dictionary and survey metadata
  to external application asset folders (`harmony/www` and
  `universe/www`).

- **Spatial Cache Files:** Generates processed geospatial outputs,
  including serializing `geo-processed/geo_predictors.fst` to disk.

- **Installed Package Database:** Modifies the lazy-load database files
  (`Rdata.rdb`, `Rdata.rdx`, and `Rdata.rds`) located in the installed
  package library directory (`system.file(package = "fusionData")`).

## Directory Requirement

**Important:** These functions must be executed with your R working
directory set to the root of the local `fusionData` project folder
(e.g., `setwd("path/to/fusionData")`). They rely on relative directory
paths (`survey-processed/`, `geo-processed/`, `harmony/www/`,
`universe/www/`, etc.).

## See also

[`write_fst`](http://www.fstpackage.org/reference/write_fst.md),
[`fmode`](https://fastverse.org/collapse/reference/fmode.html)

## Examples

``` r
if (FALSE) { # \dontrun{
# Compile both dictionary and spatial datasets and sync to package DB
compileMetadata()

# Skip spatial compilation and only update dictionary data
compileMetadata(compile_spatial = FALSE)

# Run individual compilation sub-routines directly
compileDictionary()
compileSpatial()
} # }
```
