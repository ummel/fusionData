library(fusionData)
source("R/detectDependence.R")
source("R/imputeMissing.R")
source("R/utils.R")

#----------

# Example usage:
#compileCEI(2015:2019, base.year = 2019)

# survey_years = 2015:2019
# base.year = 2019

#----------

compileCEI <- function(survey_years, base.year) {

  load("survey-processed/CEX/cat_assignment.rda")

  # Load the functions used below
  source("survey-processed/CEX/CEI/02 processCodebook.R")
  source("survey-processed/CEX/CEI/03 processFMLI.R")
  source("survey-processed/CEX/CEI/04 processMEMI.R")
  source("survey-processed/CEX/CEI/05 processMTBI.R")
  source("survey-processed/CEX/CEI/06 processOVB.R")

  #----------------

  cat("Processing codebook...\n")
  codebook <- processCodebook(survey_years)

  #----------------

  cat("Processing FMLI data...\n")
  h <- processFMLI(survey_years, codebook)

  #----------------

  cat("Processing MEMI data...\n")
  p <- processMEMI(survey_years, codebook)

  #----------------

  # Process MTBI data to return expenditure data frame ready for imputation
  cat("Processing MTBI data...\n")
  expend <- processMTBI(survey_years, base.year)

  #----------------

  # Process OVB data to return estimated total value of owned vehicles
  cat("Processing OVB data...\n")
  vehvalue <- processOVB(survey_years, codebook)

  #----------------

  # Prepare data for imputation
  # Restrict the FMLI variables merged to expenditures to each CU's last-available interview in 'd'
  # NOTE: When filter 'n() >= 2' is included, CU's are restricted to those with at least two completed interviews in 'd'
  #  This appears to result in imputed expenditure means closer to the reported/official BLS means
  # NOTE: 'vehicle_value' can be positive even when 'vehq' is zero, in event that CU has non-standard vehicles (RV's, boats) in OVB for which value is estimated

  h <- h %>%
    group_by(cuid) %>%
    filter(n() >= 2) %>%  # SEE NOTE!
    filter(intnum == max(intnum)) %>%
    ungroup() %>%
    inner_join(expend, by = "cuid") %>%  # Merge expenditures
    left_join(vehvalue, by = "cuid") %>%  # Merge total vehicle value
    mutate(vehicle_value = ifelse(is.na(vehicle_value) & vehq == 0, 0, vehicle_value))  # Set 'vehicle_value' to NA for CU's without any vehicles

  #----------------

  # Where are the missings?
  #na <- colSums(is.na(h))

  # Impute NA values in 'd'
  cat("Imputing missing values...\n")
  imp <- imputeMissing(data = h,
                       N = 2,
                       weight = "finlwt21",
                       #y_exclude = c("region", "division", "state", "ur12", "cbsa13", "cex_metro", "cex_cbsasize"),
                       y_exclude = c("state", "ur12", "cbsa13"),  # Allowing region and division to be imputed
                       x_exclude = c("cuid", "intnum", "^wtrep"),
                       sequence = map(1:4, ~ grep(paste0("_", .x, "$"), names(h), value = TRUE)))

  #----------------

  # Sum expenditure variables across the 4 periods/interviews
  cat("Aggregating annual expenditures...\n")
  expend.annual <- h %>%
    select(-any_of(names(imp))) %>%
    cbind(select(imp, any_of(names(h)))) %>%
    select(matches("_[1-4]$"), -starts_with("year_"), -starts_with("month_")) %>%  # Retain only quarterly variables with "_1", etc. endings; exclude "year_*" and "month_*" columns
    mutate(cuid = h$cuid) %>%
    pivot_longer(cols = -cuid, names_to = c("var", "i"), names_sep = "_") %>%
    group_by(cuid, var) %>%
    summarize(value = sum(value), .groups = 'drop') %>%
    pivot_wider(id_cols = cuid, names_from = var, values_from = value)

  # Safety check
  stopifnot(nrow(h) == nrow(expend.annual))

  #----------------

  # Assemble final PERSON microdata
  # Restrict final output to each CU's last/final interview
  # NOTE: var_label assignment is done after any manipulation of values/classes, because labels can be lost
  p.final <- p %>%
    inner_join(select(h, cuid, intnum), by = c("cuid", "intnum")) %>%
    mutate_if(is.numeric, convertInteger) %>%
    mutate_if(is.double, cleanNumeric, tol = 0.001) %>%
    mutate(cu_code = relevel(cu_code, "Reference person")) %>%
    addPID(hid = "cuid", refvar = "cu_code") %>%
    labelled::set_variable_labels(.labels = setNames(as.list(codebook$desc), codebook$var), .strict = FALSE) %>%
    rename(cei_hid = cuid) %>%
    select(-membno, -intnum, -survey_year) %>%
    select(cei_hid, pid, everything())

  #-----

  # Create dictionary and save to disk
  survey.range <- paste(range(survey_years), collapse = "-")
  dictionary <- createDictionary(data = p.final, survey = "CEI", vintage = survey.range, respondent = "P")
  saveRDS(object = dictionary, file = paste0("survey-processed/CEX/CEI/CEI_", survey.range, "_P_dictionary.rds"))

  # Save data to disk (.fst)
  cat("Saving person-level microdata to disk...\n")
  fst::write_fst(x = p.final, paste0("survey-processed/CEX/CEI/CEI_", survey.range, "_P_processed.fst"), compress = 100)

  gc()

  #----------------

  # Assemble final HOUSEHOLD output
  # NOTE: var_label assignment is done after any manipulation of values/classes, because labels can be lost
  h.final <- h %>%
    select(-any_of(names(imp))) %>%
    cbind(select(imp, any_of(names(h)))) %>%
    select(-matches("_[1-4]$")) %>%  # Remove any quarterly variables with "_1", etc. endings
    left_join(expend.annual, by = "cuid") %>%
    mutate(

      OWNVAL = ifelse(!grepl("Homeowner", cutenure), 0, OWNVAL),

      RNTVAL = 12 * renteqvx,
      renteqvx = NULL,

      VEHVAL = vehicle_value,
      vehicle_value = NULL,

      STDINT = 12 * studfinx,
      studfinx = NULL,

      STDDBT = studntx,
      studntx = NULL,
      studntb = NULL,

      OTHINT = 12 * (credfinx + othfinx),
      credfinx = NULL,
      othfinx = NULL,

      OTHDBT = creditx + othlonx,
      creditx = NULL,
      creditb = NULL,
      othlonx = NULL,
      othlonb = NULL,

      STOCK = stockx,
      stockx = NULL,
      stockb = NULL,

      CHECK = liquidx,
      liquidx = NULL,
      liquidb = NULL,

      RETIRE = irax,
      irax = NULL,
      irab = NULL,

      OTHFIN = othastx,
      othastx = NULL,
      othastb = NULL,

      LIFVAL = wholifx,
      wholifx = NULL,
      wholifb = NULL

    ) %>%
    mutate_if(is.character, factor) %>%  # Coerce remaining geographic character-type variables to factor
    mutate_if(is.numeric, convertInteger) %>%
    mutate_if(is.double, cleanNumeric, tol = 0.001) %>%
    labelled::set_variable_labels(.labels = setNames(as.list(ifelse(cat_assignment$major == "Other", cat_assignment$category, paste(cat_assignment$category, "(annual,", base.year, "dollars)"))), cat_assignment$cat), .strict = FALSE) %>%
    labelled::set_variable_labels(.labels = setNames(as.list(codebook$desc), codebook$var), .strict = FALSE) %>%
    rename_with(tolower) %>%
    rename_with(~ gsub("wtrep", "rep_", .x, fixed = TRUE), .cols = starts_with("wtrep")) %>%  # Rename replicate weight columns to standardized names
    rename(
      cei_hid = cuid,
      weight = finlwt21
    ) %>%
    select(-survey_year, -intnum, -qintrvmo, -qintrvyr) %>%  # Drop these since they are unnecessary
    select(cei_hid, weight, everything(), -starts_with("rep_"), starts_with("rep_"))  # Reorder columns with replicate weights at the end

  #-----

  # Create dictionary and save to disk
  dictionary <- createDictionary(data = h.final, survey = "CEI", vintage = survey.range, respondent = "H")
  saveRDS(object = dictionary, file = paste0("survey-processed/CEX/CEI/CEI_", survey.range, "_H_dictionary.rds"))

  # Save data to disk (.fst)
  cat("Saving household-level microdata to disk...\n")
  fst::write_fst(x = h.final, paste0("survey-processed/CEX/CEI/CEI_", survey.range, "_H_processed.fst"), compress = 100)

}
