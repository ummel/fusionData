# Compile Survey Microdata Dictionaries

Aggregates individual survey dictionaries stored in `survey-processed/`
into a single, standardized data dictionary (`dictionary`) and a
high-level survey metadata summary (`surveys`).

## Usage

``` r
compileDictionary()
```

## Details

This function scans all recursive survey dictionary files
(`*_dictionary.rds`) created during survey ingest, standardizes
respondent types, calculates microdata disk footprints (including
`.processed.fst` and optional `custom.fst` files), and outputs unified
data objects needed by the embedded Shiny applications (`universe` and
`harmony`).

**Important:** This function must be executed with your R working
directory set to the root of the local `fusionData` project folder
(e.g., `setwd("path/to/fusionData")`). It relies on relative directory
paths (`survey-processed/`, `harmony/www/`, and `universe/www/`).
