# Convey Existing Survey Variable Harmonies to New Survey Pairs

Transfers variable harmonization specifications from an existing harmony
file (`from`) to a new target survey pair (`to`). This function
accelerates the harmonization workflow when introducing a new vintage of
an existing survey by re-using previously defined, validated variable
mappings.

## Usage

``` r
conveyHarmony(from, to, overwrite = FALSE)
```

## Arguments

- from:

  Character. File name of an existing R harmony specification file
  located in `harmony/harmonies/` (e.g., `"RECS_2015__ACS_2019.R"`).

- to:

  Character. File name for the target harmony specification file to be
  created in `harmony/harmonies/` (e.g., `"RECS_2015__ACS_2015.R"`).

- overwrite:

  Logical. If `TRUE`, overwrites `to` if it already exists on disk.
  Defaults to `FALSE`.

## Value

Invisibly returns `NULL`. As a side effect, writes a new R script
containing the retained harmony list object to `harmony/harmonies/[to]`
if at least one valid harmony is identified.

## Details

A harmony mapping in `from` is considered **valid** and automatically
conveyed to `to` only if:

1.  Both donor and recipient variables exist in the dictionary metadata
    for the target survey pair.

2.  The expected factor level definitions in the source harmony match
    the target survey dictionary values exactly (for categorical
    variables).

If variables exist in the target surveys but their factor level
structures differ (e.g., revised response categories in a newer survey
vintage), `conveyHarmony()` excludes the mapping from the new file and
outputs a console message identifying candidates for manual
re-harmonization via the `harmony` Shiny application.

## Directory Requirement

**Important:** This function must be executed with your R working
directory set to the root of the local `fusionData` repository (e.g.,
`setwd("path/to/fusionData")`). It reads from `harmony/harmonies/` and
sources `harmony/R/harmony2dotR.R`.

## Workflow Note

While `conveyHarmony()` automates the transfer of identical mappings,
**users should always manually inspect and verify the resulting `.R`
file in `harmony/harmonies/`** or load it in the `harmony` Shiny
application.

## See also

`dictionary`

## Examples

``` r
if (FALSE) { # \dontrun{
# Ensure working directory is set to the fusionData repository root
conveyHarmony(
  from = "RECS_2015__ACS_2019.R",
  to = "RECS_2015__ACS_2015.R",
  overwrite = TRUE
)
} # }
```
