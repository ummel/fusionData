# Process ACS PUMS Microdata, Codebooks, and Custom Variables

A unified suite of maintainer utilities in the fusionData pipeline
designed to ingest, parse, harmonize, and extend U.S. Census Bureau
American Community Survey (ACS) Public Use Microdata Sample (PUMS) raw
data, official data dictionaries, and derived user-defined custom
variables.

## Usage

``` r
processACScodebook(dictionary.file)

processACScustom(year)

processACSmicrodata(year, respondent = c("H", "P"))
```

## Arguments

- dictionary.file:

  Character string specifying the path to a raw Census data dictionary
  file (`.pdf`, `.txt`, or `.csv`).

- year:

  Integer survey vintage (2005 onward).

- respondent:

  Character string specifying the respondent unit: `"H"` for
  Household-level records or `"P"` for Person-level records.

- ...:

  Additional arguments passed internally to processing routines or
  underlying file read/write operations.

## Value

Depending on the function invoked:

- `processACSmicrodata()`: Writes two production files to
  `survey-processed/ACS/{year}/`: `ACS_{year}_{H|P}_dictionary.rds`
  (metadata dictionary) and `ACS_{year}_{H|P}_processed.fst` (compressed
  microdata). Returns the processed data frame invisibly.

- `processACScodebook()`: Returns a structured `data.frame` or
  `data.table` containing standardized dictionary columns: `var`
  (variable name), `desc` (description), `value` (raw value), `label`
  (factor label), `adj` (inflation adjustment flag), and `custom_desc`
  (flag for manual descriptions).

- `processACScustom()`: Writes standalone sidecar datasets
  (`ACS_{year}_{H|P}_custom.fst`) and updates the primary metadata file
  (`ACS_{year}_{H|P}_dictionary.rds`) with `custom = TRUE` tags.
  Invisibly returns the compiled custom data frame.

## Details

**processACScodebook Mechanics:** The Census Bureau distributes PUMS
data dictionaries in different file formats depending on vintage:

- Pre-2012: Distributed primarily as raw unformatted `.pdf` files.

- 2012–2016: Distributed as fixed-width / structured `.txt` files.

- 2017 Onward: Distributed as structured `.csv` files.

`processACScodebook()` abstracts these format differences into a unified
output table, identifying value ranges, applying human-readable level
descriptions, and flagging variables subject to inflation adjustment
factors (`ADJINC` or `ADJHSG`).

**processACScustom Mechanics & Script Requirements:** Standard PUMS
microdata often lacks domain-specific or recoded metrics required for
downstream modeling (e.g., custom poverty ratios, recoded race/ethnicity
categories, housing cost burdens). `processACScustom()` automates the
execution and validation of these derived variables:

1.  **Script Scanning**: Scans `survey-processed/ACS/custom/*.R` for
    individual R scripts.

2.  **Function Contract**: Each custom script must define a function
    named identically to its filename (e.g., `poverty.R` defines
    `poverty()`). The function must accept `year` as its sole argument
    and return a data frame containing primary keys (`hid`, and
    optionally `pid`).

3.  **Validation Checks**: Guarantees that zero missing (`NA`) values
    exist in output columns, verifies that explicit variable labels are
    assigned via
    [`var_label`](https://larmarange.github.io/labelled/reference/var_label.html),
    filters records to occupied housing units, and prevents column name
    collisions across scripts.

**Typical Execution Workflow:**

1.  **Primary Microdata Build (`processACSmicrodata`)**: Once raw Census
    ZIP archives (`csv_hus.zip` and `csv_pus.zip`) are placed in
    `survey-raw/ACS/{year}/`, the standard operating procedure is to
    call `processACSmicrodata()` for both respondent units in a given
    vintage:


                  processACSmicrodata(year = 2021, respondent = "H")
                  processACSmicrodata(year = 2021, respondent = "P")
                

    This orchestrates key standardization, financial inflation
    adjustments, geographic filtering, codebook label mapping,
    non-response imputation via fusionModel, and binary output
    generation.

2.  **Internal Codebook Parsing (`processACScodebook`)**: This function
    is called *internally* by `processACSmicrodata()` to automatically
    locate and parse vintage-dependent data dictionary files (`.pdf`,
    `.txt`, or `.csv`). Users generally do not need to invoke
    `processACScodebook()` separately, except when debugging dictionary
    parsing discrepancies or testing raw Census codebook structures.

3.  **Custom Variable Compilation (`processACScustom`)**: Once
    `processACSmicrodata()` has executed successfully for both household
    (`H`) and person (`P`) records, `processACScustom()` can be
    executed:


                  processACScustom(year = 2021)
                

    This dynamically scans, validates, and evaluates user-defined
    transformation scripts from `survey-processed/ACS/custom/*.R`,
    appending custom metrics to standalone sidecar files and updating
    the primary metadata dictionary.

## Directory Structure & File Conventions


    survey-raw/ACS/{year}/
    ├── Dict*.txt | Dict*.csv | Dict*.pdf  (Raw Census codebook dictionary)
    ├── csv_hus.zip                         (Raw household PUMS microdata archive)
    └── csv_pus.zip                         (Raw person PUMS microdata archive)

    survey-processed/ACS/
    ├── custom/                             (User custom R scripts: e.g., poverty.R, race.R)
    └── {year}/                             (Processed output destination)
        ├── ACS_{year}_{H|P}_dictionary.rds (R metadata dictionary data frame)
        ├── ACS_{year}_{H|P}_processed.fst  (Standardized binary PUMS microdata)
        └── ACS_{year}_{H|P}_custom.fst     (Custom derived variable sidecar)

## See also

[`read_fst`](http://www.fstpackage.org/reference/write_fst.md),
[`set_variable_labels`](https://larmarange.github.io/labelled/reference/var_label.html),
`impute`
