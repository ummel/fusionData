# Compile Universal Survey Data Dictionary

Aggregates individual survey metadata codebooks stored in
`survey-processed/` into a single, standardized data dictionary
(`dictionary`) and a high-level survey metadata summary (`surveys`).

## Usage

``` r
compileDictionary()
```

## Value

Invisibly returns `NULL`. As a side effect, the function writes updated
`.rda` files containing the `dictionary` and `surveys` data frames to
disk across three locations:

- `data/dictionary.rda` and `data/surveys.rda` (Package datasets)

- `harmony/www/dictionary.rda` and `harmony/www/surveys.rda` (Harmony
  Shiny app assets)

- `universe/www/dictionary.rda` and `universe/www/surveys.rda` (Universe
  Shiny app assets)

## Details

This function forms part of the **Document** step in the `fusionData`
package workflow. It scans all recursive survey metadata files
(`*_dictionary.rds`) created during survey ingest, standardizes
respondent types, calculates microdata disk footprints (including
`.processed.fst` and optional `custom.fst` files), and outputs unified
data objects needed by the package and embedded Shiny applications
(`universe` and `harmony`).

## Directory Requirement

**Important:** This function must be executed with your R working
directory set to the root of the local `fusionData` project folder
(e.g., `setwd("path/to/fusionData")`). It relies on relative directory
paths (`survey-processed/`, `harmony/www/`, and `universe/www/`).

## Workflow Note

Because `compileDictionary()` updates datasets stored in the package's
`data/` directory, you must rebuild or reinstall the package locally
(e.g., using
[`fusionData::installPackage()`](https://ummel.github.io/fusionData/reference/installPackage.md))
after running this function for the updated package datasets to take
effect in your loaded session.

## See also

[`use_data`](https://usethis.r-lib.org/reference/use_data.html)

## Examples

``` r
if (FALSE) { # \dontrun{
# Ensure working directory is set to the fusionData repository root
compileDictionary()

# Reinstall local package binaries so updated data is recognized
installPackage()
} # }
```
