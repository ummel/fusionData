#' Generate data dictionary from survey microdata
#'
#' @description
#' Produces a data dictionary with standard structure that can be saved alongside processed microdata. Resulting dictionary can be compiled with other survey dictionaries via \link{compileDictionary}.
#'
#' @param data Data frame. Survey microdata with variable descriptions stored in columns via \code{\link[labelled]{var_label}}.
#' @param survey Character. Unique survey identifier (e.g. "RECS").
#' @param vintage Character. Survey vintage (e.g. 2015).
#' @param respondent Character. Respondent type; either "Household" or "Person" or a string identifiable as such.
#'
#' @return Returns a tibble with standard "dictionary" information based on the provided microdata.
#'
#' @examples
#'
#' @export

createDictionary <- function(data, survey, vintage, respondent) {

  # Check for valid inputs
  stopifnot(exprs = {
    is.data.frame(data)
    !any(map_lgl(data, is.character))  # There should NOT be any character columns (must be factor)
    length(survey) == 1
    substring(tolower(respondent), 1, 1) %in% c("h", "p")
  })

  # Check for complete variable labels/descriptions
  v <- compact(labelled::var_label(data))
  miss <- setdiff(names(data), names(v))
  if (length(miss) > 0) {
    stop("The following columns are missing labels (see ?labelled::var_label): ", paste(miss, collapse = ", "))
  }

  # Household-level data?
  hh <- substring(tolower(respondent), 1, 1) == "h"

  # Create 'W' vector for observation weights
  # Scaled to avoid integer overflow
  W <- if ("weight" %in% names(data)) data$weight / mean(data$weight) else rep(1L, nrow(data))

  # Variable summaries
  var.values <- data %>%
    select(-matches("^rep_\\d+$")) %>%  # Remove replicate weights
    select(-matches(paste0("^", tolower(survey), ".*_hid$"), -any_of(c("pid", "weight")))) %>%  # Remove ID and primary weight variables
    map_chr(~ if (is.numeric(.x)) {numFormat(x = .x, w = W)} else {fctFormat(.x)})

  # Variables to include in dictionary
  nm <- names(var.values)

  # Assemble dictionary data frame
  dict <- tibble(
    survey = survey,
    vintage = as.character(vintage),
    respondent = ifelse(hh, "H", "P"),
    variable = nm,
    description = unlist(labelled::var_label(data[nm], unlist = TRUE)),
    values = var.values,
    type = map_chr(data[nm], vctrs::vec_ptype_abbr),
    n = as.integer(colSums(!is.na(data[nm])))
  ) %>%
    arrange(variable)

  return(dict)

}
