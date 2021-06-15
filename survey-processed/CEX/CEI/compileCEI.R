# library(tidyverse)
# source("R/utils.R")

#----------

# Example usage:
#aggregateCEX(2015:2019)

#----------

compileCEI <- function(survey_years) {

  data(cat_assignment)
  data(ucc_assignment)

  source("survey-processed/CEX/CEI/processFMLI.R")
  source("survey-processed/CEX/CEI/processMEMI.R")
  source("survey-processed/CEX/CEI/processMTBI.R")

  #----------------

  m <- processMEMI(survey_years)

  #----------------

  d <- processFMLI(survey_years)

  #----------------

  # Process MTBI data to return expenditure data frame ready for imputation
  expend <- processMTBI(survey_years)

  #----------------

  # Prepare data for imputation
  # Restrict final output to each CU's last/final interview
  d <- d %>%
    group_by(cuid) %>%
    filter(intnum == max(intnum)) %>%
    ungroup() %>%
    inner_join(expend, by = "cuid")

  # Where are the missings?
  #na <- colSums(is.na(d))

  # Imputate NA values in 'd'
  d <- imputeMissing(data = d,
                     N = 1,
                     weight = "finlwt21",
                     y_exclude = c("state_name", "state_fips", "^wtrep"),
                     x_exclude = c("cuid", "intnum", "survey_year", "newid", "hhid", "^wtrep"),
                     sequence = map(1:4, ~ grep(paste0("_", .x, "$"), names(d), value = TRUE)))
  #grouping = list("cuid", setdiff(names(d), pd.names)))

  #-----

  # Sum expenditure variables across the 4 periods/interviews
  expend.annual <- d %>%
    select(one_of(names(expend))) %>%
    mutate_if(is.ordered, as.numeric) %>%
    pivot_longer(cols = -cuid, names_to = c("var", "i"), names_sep = "_") %>%
    filter(!var %in% c("year", "month")) %>%
    group_by(cuid, var) %>%
    summarize(value = sum(value), .groups = 'drop') %>%
    pivot_wider(id_cols = cuid, names_from = var, values_from = value) %>%
    set_variable_labels(.labels = setNames(as.list(paste(cat_assignment$category, "(annual expenditure,", max(survey_years), "dollars)")), cat_assignment$cat), .strict = FALSE)

  # Safety check
  stopifnot(nrow(d) == nrow(expend.annual))

  #----------------

  # Assemble final output
  # NOTE: var_label assignment is done after any manipulation of values/classes, because labels can be lost
  d <- d %>%
    select(-one_of(names(expend)[-1])) %>%
    left_join(expend.annual, by = "cuid") %>%
    mutate_if(is.numeric, convertInteger) %>%
    mutate_if(is.double, cleanNumeric, tol = 0.001) %>%
    set_variable_labels(.labels = setNames(as.list(codebook$desc), codebook$var), .strict = FALSE) %>%
    rename_with(tolower) %>%
    rename_with(~ gsub("wtrep", "rep_", .x, fixed = TRUE), .cols = starts_with("wtrep")) %>%  # Rename replicate weight columns to standardized names
    rename(
      cei_hid = cuid,  # Rename ID variables to standardized names
      weight = finlwt21
    ) %>%
    select(-survey_year, -intnum, -newid, -hhid, -qintrvmo, -qintrvyr, -liquidb, -irab, -stockb) %>%  # Drop these since they are unnecessary?
    select(-gasmocq, -gasmopq, -utilcq, -utilpq, -totexpcq, -totexppq) %>%  # TO DO: REMOVE THESE UPSTREAM!!! in processRawCEX()
    select(cei_hid, weight, everything(), -starts_with("rep_"), starts_with("rep_"))  # Reorder columns with replicate weights at the end

  # TO DO: UPDATE some of the income variables to match summation from person file. NECESSARY?

  #----------------

  # NOTE: commitData() does not work with CEI since it has a unique file structure 'hid' naming; ignoring issues for now (saved manually)

  # Commit data to disk
  commitData(data = m, survey = "CEX", vintage = survey_years, respondent = "P", geo = NULL, save_data = TRUE)

  # Commit data to disk
  commitData(data = d, survey = "CEX", vintage = survey_years, respondent = "H", geo = NULL, save_data = TRUE)

}
