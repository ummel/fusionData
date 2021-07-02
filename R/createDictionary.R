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

  # Check for complete variable labels/descriptions
  v <- compact(labelled::var_label(data))
  miss <- setdiff(names(data), names(v))
  if (any(miss)) {
    stop("The following columns are missing labels (see ?labelled::var_label): ", paste(miss, collapse = ", "))
  }

  # Check for valid inputs
  stopifnot({
    is.data.frame(data)
    !any(map_lgl(d, is.character))  # There should not be any character columns (must be factor)
    length(survey) == 1
    substring(tolower(respondent), 1, 1) %in% c("h", "p")
  })

  # Household-level data?
  hh <- substring(tolower(respondent), 1, 1) == "h"

  # Create 'W' vector for observation weights
  # Scaled to avoid integer overflow
  W <- if ("weight" %in% names(data)) data$weight / mean(data$weight) else rep(1L, nrow(data))

  # Function returning summary string for numeric variable
  numFormat <- function(x, w = NULL) {
    if (is.null(w)) w <- rep(1, length(x))
    paste(
      c("Min:", "Median:", " Mean:", "Max:"),
      cleanNumeric(c(min(x, na.rm = TRUE), weightedQuantile(x, w, p = 0.5), weighted.mean(x, w, na.rm = TRUE), max(x, na.rm = TRUE))),
      collapse = ", ")
  }

  # Function returning summary string for factor variable
  fctFormat <- function(x) {
    #u <- if (is.factor(x)) levels(x) else sort(unique(x))
    paste(paste0("[", levels(x), "]"), collapse = ", ")
  }

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
