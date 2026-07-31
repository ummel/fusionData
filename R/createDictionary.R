#' Generate Standardized Survey Data Dictionary from Microdata
#'
#' @description
#' Constructs a standardized variable-level codebook (tibble) from a processed
#' survey microdata frame. The resulting dictionary contains variable names,
#' human-readable descriptions, value range/level summaries, data types, and non-missing
#' sample counts ($N$).
#'
#' @param data Data frame. Processed survey microdata containing variable descriptions
#'   assigned to columns via \code{\link[labelled]{var_label}}. **Note:** Character columns
#'   must be converted to factors prior to calling `createDictionary()`.
#' @param survey Character. Unique survey abbreviation (e.g., `"RECS"`, `"CPS"`, `"AHS"`).
#' @param vintage Character or Numeric. Survey vintage or year (e.g., `2015` or `"2015-2020"`).
#' @param respondent Character. Respondent unit type; must be identifiable as Household
#'   (e.g., `"Household"`, `"H"`) or Person (e.g., `"Person"`, `"P"`).
#' @param custom Logical. Indicates whether the dictionary corresponds to a custom
#'   microdata extension file (`custom.fst`). Defaults to `FALSE`.
#'
#' @details
#' `createDictionary()` is typically called at the end of an individual survey ingest
#' script in `survey-processing/`. The dictionary tibble returned by this function is
#' saved alongside processed `.fst` microdata files as `*_dictionary.rds` and later
#' compiled into master package metadata using \code{\link[fusionData]{compileDictionary}}.
#'
#' Before constructing the dictionary, `createDictionary()` performs three safety checks:
#' * **Type Check:** Ensures no character columns remain in `data` (all string variables
#'   must be factorized).
#' * **Uniqueness Check:** Verifies that row count matches distinct household/person IDs
#'   (`hid` / `pid`) to prevent duplicate observations.
#' * **Completeness Check:** Ensures all predictor columns possess non-empty variable
#'   labels via `labelled::var_label()`.
#'
#' @section Workflow Note:
#' Every variable intended for microdata assembly must have an explicit description assigned
#' beforehand (e.g., `labelled::var_label(data$col) <- "Description"`). If any columns
#' are missing labels, this function will raise an error identifying the unlabeled columns.
#'
#' @return A \code{\link[tibble]{tibble}} containing nine standardized metadata columns:
#' \itemize{
#'   \item \code{survey}: Unique survey identifier
#'   \item \code{vintage}: Survey vintage year
#'   \item \code{respondent}: Respondent unit type (\code{"H"} for Household, \code{"P"} for Person)
#'   \item \code{variable}: Variable name
#'   \item \code{description}: Human-readable variable label
#'   \item \code{values}: Formatted numeric range summary or factor level enumeration
#'   \item \code{type}: Abbreviated data type (e.g., \code{"fct"}, \code{"dbl"}, \code{"int"})
#'   \item \code{n}: Count of non-missing observations
#'   \item \code{custom}: Logical flag indicating custom dataset status
#' }
#'
#' @seealso \code{\link[fusionData]{compileDictionary}}, \code{\link[labelled]{var_label}}
#'
#' @examples
#' \dontrun{
#' # Example survey processing snippet
#' library(labelled)
#'
#' # Assign variable labels
#' var_label(df$income) <- "Total household income (USD)"
#' var_label(df$tenure) <- "Housing tenure status"
#'
#' # Generate dictionary
#' dict <- createDictionary(
#'   data = df,
#'   survey = "RECS",
#'   vintage = 2015,
#'   respondent = "Household"
#' )
#' }
#'
#' @export

createDictionary <- function(data, survey, vintage, respondent, custom = FALSE) {

  # Validate input data types and required parameter formats
  stopifnot(exprs = {
    is.data.frame(data)
    !any(map_lgl(data, is.character))  # Character columns must be converted to factors
    length(survey) == 1
    substring(tolower(respondent), 1, 1) %in% c("h", "p")
  })

  data <- as.data.frame(data)

  # Check that unique household/person identifiers match total row count
  row.check <- data %>%
    select(any_of(c('hid', 'pid'))) %>%
    distinct() %>%
    nrow()
  if (row.check != nrow(data)) {
    cli::cli_abort(c(
      "x" = "Row count mismatch in microdata.",
      "i" = "Found {.val {row.check}} unique ID{?s} (`hid`/`pid`), but {.val {nrow(data)}} total row{?s} in {.arg data}."
    ))
  }

  # Verify that all non-excluded columns have explicit variable labels
  v <- compact(labelled::var_label(data))
  miss <- setdiff(names(data), names(v))
  if (custom) miss <- setdiff(miss, c('year', 'hid', 'pid'))
  if (length(miss)) {
    cli::cli_abort(c(
      "x" = "The following column{?s} in {.arg data} {?is/are} missing variable label{?s}:",
      "*" = "{miss}",
      "i" = "Assign labels using {.fn labelled::var_label} before generating dictionary."
    ))
  }

  # Determine respondent unit type (Household vs Person)
  hh <- substring(tolower(respondent), 1, 1) == "h"

  # Construct normalized observation weight vector to avoid integer overflow during weighted calculations
  W <- if ("weight" %in% names(data)) data$weight / mean(data$weight) else rep(1L, nrow(data))

  # Generate formatted string summaries of values (numeric ranges or factor levels) excluding ID/weight columns
  if (custom) data <- select(data, -any_of(c('year', 'hid', 'pid')))
  var.values <- data %>%
    select(-matches("^rep_\\d+$")) %>%  # Omit replicate weight columns
    map_chr(~ if (is.numeric(.x)) {numFormat(x = .x, w = W)} else {catFormat(.x)})

  # Extract variable names included in summary
  nm <- names(var.values)

  # Assemble standardized dictionary data frame
  dict <- tibble(
    survey = survey,
    vintage = as.character(vintage),
    respondent = ifelse(hh, "H", "P"),
    variable = nm,
    description = unlist(labelled::var_label(data[nm], unlist = TRUE)),
    values = var.values,
    type = map_chr(data[nm], vctrs::vec_ptype_abbr),
    n = as.integer(colSums(!is.na(data[nm]))),
    custom = custom
  ) %>%
    arrange(variable)

  return(dict)

}
