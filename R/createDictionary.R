#' Generate data dictionary from survey microdata
#'
#' @description
#' Produces a data dictionary with standard structure that can be saved alongside processed microdata. Resulting dictionary can be compiled with other survey dictionaries via \link{compileDictionary}.
#'
#' @param data Data frame. Survey microdata with variable descriptions stored in columns via \code{\link[labelled]{var_label}}.
#' @param survey Character. Unique survey identifier (e.g. "RECS").
#' @param vintage Character. Survey vintage (e.g. "2015").
#' @param respondent Character. Respondent type; either "Household" or "Person".
#'
#' @return Returns a tibble with standard "dictionary" information based on the provided microdata.
#'
#' @examples
#'
#' @export

# TO DO: AUTO-DETECT GEOGRAPHIC VARIABLES!!!

createDictionary <- function(data, survey, vintage, respondent, geo = NULL) {

  stopifnot({
    is.data.frame(data)
    length(survey) == 1
    respondent %in% c("H", "P")
  })

  # Create 'W' vector for observation weights
  # Scaled to avoid integer overflow
  W <- if ("weight" %in% names(data)) data$weight / mean(data$weight) else rep(1L, nrow(data))

  # Formatting functions for 'values' column
  numFormat <- function(x, w) {
    paste(
      c("Min:", "Median:", " Mean:", "Max:"),
      cleanNumeric(c(min(x, na.rm = TRUE), weightedQuantile(x, w, p = 0.5), weighted.mean(x, w, na.rm = TRUE), max(x, na.rm = TRUE))),
      collapse = ", ")
  }
  fctFormat <- function(x) paste(paste0("[", levels(x), "]"), collapse = ", ")

  # Variable summaries
  var.values <- data %>%
    select(!matches("^rep_")) %>%  # Remove replicate weights
    select(!matches("_hid$"), !any_of(c("pid", "weight"))) %>%  # Remove ID and primary weight variables
    select(-one_of(geo)) %>%  # Remove geographic identifier; TO DO: automate detection
    map_chr(~ if (is.numeric(.x)) {numFormat(x = .x, w = W)} else {fctFormat(.x)})

  # Variables to include in dictionary
  nm <- names(var.values)

  # Assemble dictionary data frame
  dict <- tibble(
    survey = survey,
    vintage = as.character(vintage),
    respondent = respondent,
    variable = nm,
    description = unlist(labelled::var_label(data[nm])),
    values = var.values,
    type = map_chr(data[nm], vctrs::vec_ptype_abbr),
    n = colSums(!is.na(data[nm]))
  ) %>%
    arrange(variable)

  return(dict)

}
