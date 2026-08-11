# Generate Fusion Input Datasets from Harmonized Donor and ACS Microdata

Constructs and formats the aligned donor (training) and ACS recipient
(prediction) microdata datasets required for downstream statistical
fusion in the `fusionData` workflow. `fusionInput()` orchestrates survey
harmonization, geographic location (PUMA) imputation, predictor
distribution quality screening, and numeric feature scaling before
writing compressed binary `.fst` files to disk.

## Usage

``` r
fusionInput(
  donor,
  acs_year,
  respondent,
  test_mode = TRUE,
  ncores = getOption("fusionData.cores"),
  note = NULL
)
```

## Arguments

- donor:

  Character. Identifier for the donor survey and vintage (e.g.
  `"RECS_2015"`, `"AHS_2023"`).

- acs_year:

  Integer. Year of ACS microdata serving as the recipient dataset (e.g.
  `2023`).

- respondent:

  Character. Unit of observation; must be either `"household"` (or
  `"H"`) or `"person"` (or `"P"`).

- test_mode:

  Logical. If `TRUE` (default), outputs are written to a scratch
  directory (`fusionData/fusion_/`) and datasets are truncated to
  ~10,000 observations for rapid testing. If `FALSE`, full-scale
  production files are written to `fusionData/fusion/` (no underscore).

- ncores:

  Integer. Number of CPU cores allocated for parallel execution during
  harmonization and file compression. Defaults to
  `getOption("fusionData.cores")`.

- note:

  Character. Optional user note recorded directly in the run execution
  log. Defaults to `NULL`.

## Value

Invisibly returns a character string containing the absolute file path
to the generated `/input` directory.

## Details

`fusionInput()` executes a multi-stage data-preparation pipeline:

- **Workspace Verification:** Ensures the active R session working
  directory is located within a valid `fusionData` repository root.

- **Microdata Harmonization:** Executes
  [`harmonize()`](https://ummel.github.io/fusionData/reference/harmonize.md)
  using the matching donor-ACS harmonization mapping script (e.g.,
  `RECS_2015__ACS_2015.R`).

- **Spatial Imputation:** Calls
  [`imputeLocation()`](https://ummel.github.io/fusionData/reference/imputeLocation.md)
  to statistically assign PUMA-level geographic identifiers to donor
  survey observations.

- **Predictor Quality Screening:** Evaluates overlapping distribution
  similarity between donor and recipient predictor variables using
  weighted similarity metrics. Predictors with similarity scores below
  0.80 are automatically dropped from model training to prevent
  distributional bias.

- **Numeric Feature Scaling:** Transforms continuous predictor variables
  using robust weighted Z-scores when necessary via
  [`scaleNumeric()`](https://ummel.github.io/fusionData/reference/scaleNumeric.md).

- **Compressed File Output:** Writes donor (`*_donor.fst`) and recipient
  (`*_recipient.fst`) datasets utilizing maximum `fst` compression
  (`compress = 100`).

## Directory Structure & Outputs

Output files are stored in structured paths based on execution mode:

- **Test Mode:**
  `fusion_/[DONOR_NAME]/[DONOR_VINTAGE]/[ACS_YEAR]/input/[DATE]/`

- **Production Mode:**
  `fusion/[DONOR_NAME]/[DONOR_VINTAGE]/[ACS_YEAR]/input/[DATE]/`

Each run creates three files in the target directory:

1.  `[DONOR]_[ACS_YEAR]_[TYPE]_donor.fst`: Scaled, harmonized donor
    training microdata.

2.  `[DONOR]_[ACS_YEAR]_[TYPE]_recipient.fst`: Scaled, harmonized ACS
    prediction microdata.

3.  `[DONOR]_[ACS_YEAR]_[TYPE]_inputlog.txt`: Execution log containing
    system details, arguments, predictor similarity scores, and timing
    summaries.

## See also

[`harmonize`](https://ummel.github.io/fusionData/reference/harmonize.md),
[`imputeLocation`](https://ummel.github.io/fusionData/reference/imputeLocation.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Run input generation in fast test mode for RECS 2015 and ACS 2015
input_dir <- fusionInput(
  donor = "RECS_2015",
  acs_year = 2015,
  respondent = "household",
  test_mode = TRUE,
  ncores = 2
)

# Inspect generated fst and log files
list.files(input_dir)
} # }
```
