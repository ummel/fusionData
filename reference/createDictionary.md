# Generate Standardized Survey Data Dictionary from Microdata

Constructs a standardized variable-level codebook (tibble) from a
processed survey microdata frame. The resulting dictionary contains
variable names, human-readable descriptions, value range/level
summaries, data types, and non-missing sample counts (\$N\$).

## Usage

``` r
createDictionary(data, survey, vintage, respondent, custom = FALSE)
```

## Arguments

- data:

  Data frame. Processed survey microdata containing variable
  descriptions assigned to columns via
  [`var_label`](https://larmarange.github.io/labelled/reference/var_label.html).
  **Note:** Character columns must be converted to factors prior to
  calling `createDictionary()`.

- survey:

  Character. Unique survey abbreviation (e.g., `"RECS"`, `"CPS"`,
  `"AHS"`).

- vintage:

  Character or Numeric. Survey vintage or year (e.g., `2015` or
  `"2015-2020"`).

- respondent:

  Character. Respondent unit type; must be identifiable as Household
  (e.g., `"Household"`, `"H"`) or Person (e.g., `"Person"`, `"P"`).

- custom:

  Logical. Indicates whether the dictionary corresponds to a custom
  microdata extension file (`custom.fst`). Defaults to `FALSE`.

## Value

A [`tibble`](https://tibble.tidyverse.org/reference/tibble.html)
containing nine standardized metadata columns:

- `survey`: Unique survey identifier

- `vintage`: Survey vintage year

- `respondent`: Respondent unit type (`"H"` for Household, `"P"` for
  Person)

- `variable`: Variable name

- `description`: Human-readable variable label

- `values`: Formatted numeric range summary or factor level enumeration

- `type`: Abbreviated data type (e.g., `"fct"`, `"dbl"`, `"int"`)

- `n`: Count of non-missing observations

- `custom`: Logical flag indicating custom dataset status

## Details

`createDictionary()` is typically called at the end of an individual
survey ingest script in `survey-processing/`. The dictionary tibble
returned by this function is saved alongside processed `.fst` microdata
files as `*_dictionary.rds` and later compiled into master package
metadata using
[`compileDictionary`](https://ummel.github.io/fusionData/reference/compileDictionary.md).

Before constructing the dictionary, `createDictionary()` performs three
safety checks:

- **Type Check:** Ensures no character columns remain in `data` (all
  string variables must be factorized).

- **Uniqueness Check:** Verifies that row count matches distinct
  household/person IDs (`hid` / `pid`) to prevent duplicate
  observations.

- **Completeness Check:** Ensures all predictor columns possess
  non-empty variable labels via
  [`labelled::var_label()`](https://larmarange.github.io/labelled/reference/var_label.html).

## Workflow Note

Every variable intended for microdata assembly must have an explicit
description assigned beforehand (e.g.,
`labelled::var_label(data$col) <- "Description"`). If any columns are
missing labels, this function will raise an error identifying the
unlabeled columns.

## See also

[`compileDictionary`](https://ummel.github.io/fusionData/reference/compileDictionary.md),
[`var_label`](https://larmarange.github.io/labelled/reference/var_label.html)

## Examples

``` r
if (FALSE) { # \dontrun{
# Example survey processing snippet
library(labelled)

# Assign variable labels
var_label(df$income) <- "Total household income (USD)"
var_label(df$tenure) <- "Housing tenure status"

# Generate dictionary
dict <- createDictionary(
  data = df,
  survey = "RECS",
  vintage = 2015,
  respondent = "Household"
)
} # }
```
