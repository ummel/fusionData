# Open the fusionACS Survey Harmonization Tool

Launches the interactive **Survey Harmonization Tool** Shiny
application. This app provides a GUI to map, group, and align variable
categories between donor surveys (e.g., RECS, CEX, NHIS) and the
recipient American Community Survey (ACS) microdata.

## Usage

``` r
harmony()
```

## Value

Launches the Shiny application in the default web browser. Returns
`NULL` invisibly when the app is closed.

## Details

The harmonization step is critical to the `fusionACS` workflow. It
allows users to map factor levels or continuous concepts between raw
donor variables and ACS target variables, producing standardized harmony
specifications stored as `.R` files in `/harmony/harmonies`.

Running this function blocks the active R console session while the
local web application is active. To return to the R prompt, close the
browser window or press Esc / Ctrl+C in the R console.

## Examples

``` r
if (FALSE) { # \dontrun{
# Launch the interactive harmonization GUI
harmony()
} # }
```
