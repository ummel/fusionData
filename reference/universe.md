# Open the fusionACS Universal Survey Dictionary

Launches the interactive 'universe' Shiny application to explore the
master survey dictionary and metadata compiled for the fusionACS
project.

## Usage

``` r
universe()
```

## Value

Opens the application in a new default web browser window. Returns
`NULL` invisibly upon closing.

## Details

The app provides a searchable interface containing:

- **Surveys**: Summary metadata detailing supported microdata surveys
  (e.g., ACS, AHS, CEI, NHTS, RECS, ASEC), survey vintages, respondent
  levels (Household vs. Person), sample sizes, variable counts, and file
  sizes.

- **Variables**: A searchable dictionary of harmonized variable codes,
  descriptions, value coding, and survey linkages.

Launching this function blocks the active R console session while the
application is running. To close the app and return to the R prompt,
either close the browser window or terminate the session in
RStudio/console (e.g., press `Esc` or click the stop icon).

## Examples

``` r
if (FALSE) { # \dontrun{
# Open the survey dictionary app
universe()
} # }
```
