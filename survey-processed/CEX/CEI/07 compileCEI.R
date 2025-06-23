library(fusionData)
library(fusionModel)
library(rpart.plot)
library(tidyverse)
library(fst)
library(data.table)

#----------

# Example usage:
#compileCEI(2015:2019, base.year = 2019)

# survey_years = 2015:2019
# base.year = 2019

#----------

compileCEI <- function(survey_years, base.year) {

  load("survey-processed/CEX/cat_assignment.rda")

  # Load the functions used below
  source("survey-processed/CEX/imputeHeatingFuel.R")
  source('R/utils.R')
  
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
  h1 <- h %>%
    group_by(cuid) %>%
    filter(n() >= 2) %>%  # SEE NOTE!
    filter(intnum == max(intnum)) %>%
    ungroup() %>%
    inner_join(expend, by = "cuid") %>%  # Merge expenditures
    left_join(vehvalue, by = "cuid") %>%  # Merge total vehicle value
    mutate(vehicle_value = ifelse(is.na(vehicle_value) & vehq == 0, 0, vehicle_value),
           division = as.factor(division),
           region = as.factor(region))  # Set 'vehicle_value' to NA for CU's without any vehicles

  #----------------

  # Where are the missings?
  na.count <- colSums(is.na(h1))
  na.count <- na.count[na.count > 0]
  na.count
  
  # Impute NA values in 'd'
  cat("Imputing missing values...\n")

  # Retain observed combinations of state, division, and region (used post-imputation)
  h1.geo <- as.data.table(h1) %>%
    distinct(state, division, region,ur12) %>%
    na.omit()
  
#New implementation using impute()
h1 <- fusionModel::impute(data = as.data.table(h1),
             weight = "finlwt21",
             ignore = c("cuid", "intnum",'cbsa13', grep("^wtrep", colnames(h1), value = TRUE)))


# This ensures the state-division-region combinations are legitimate
# impute() does a good job producing accurate combinations, but this merge-update makes sure of it
h1[h1.geo, c("division", "region",'ur12') := .(i.division, i.region,i.ur12), on = "state"]

  #----------------
  # Sum expenditure variables across the 4 periods/interviews
  cat("Aggregating annual expenditures...\n")
  expend.annual <- h1 %>%
    select(matches("_[1-4]$"), -starts_with("year_"), -starts_with("month_")) %>%  # Retain only quarterly variables with "_1", etc. endings; exclude "year_*" and "month_*" columns
    mutate(cuid = h1$cuid) %>%
    pivot_longer(cols = -cuid, names_to = c("var", "i"), names_sep = "_") %>%
    group_by(cuid, var) %>%
    summarize(value = sum(value), .groups = 'drop') %>%
    pivot_wider(id_cols = cuid, names_from = var, values_from = value)

  # Safety checks
  stopifnot({
    !anyNA(expend.annual)
    nrow(h1) == nrow(expend.annual)
  })
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
    rename(hid = cuid) %>%
    select(-membno, -intnum, -survey_year) %>%
    select(hid, pid, everything())

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
  # Obtain CEI fusion variable names  from fusionacsdata@gmail.com GDrive account
  # Note that some variables listed in the "Categories" google sheet are, in fact, harmonized and not included in 'fusion.vars'
  # https://docs.google.com/spreadsheets/d/13GRKkVZXapHtP7oK1WUh0Yu7OQ_9icd17wUGuhX-WRg/edit
  fusion.vars <- googlesheets4::read_sheet("13GRKkVZXapHtP7oK1WUh0Yu7OQ_9icd17wUGuhX-WRg", sheet = "Category Summary") %>%
    filter(major != "Other" | cat == "VEHVAL") %>%   # Retain vehicle value but drop remaining "Other" categories
    mutate(cat = tolower(cat)) %>%
    pull(cat)
  
  # Restrict 'fusion.vars' to only those variables returned by assemble()
  # This effectively removes variables used for harmonization with ACS
  fusion.vars <- intersect(fusion.vars, names(h))
  
  # Assemble final HOUSEHOLD output
  # NOTE: var_label assignment is done after any manipulation of values/classes, because labels can be lost
  h.final <- h1 %>%
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
      hid = cuid,
      weight = finlwt21
    ) %>%
    select(-survey_year, -intnum, -qintrvmo, -qintrvyr) %>%  # Drop these since they are unnecessary
    select(hid, weight, everything(), -starts_with("rep_"), starts_with("rep_")) %>% # Reorder columns with replicate weights at the end

   # All of the fusion.vars at this point are dollar expenditure values
  # Convert all to integer for better file compression
  #h <- h %>%
    mutate_at(fusion.vars, ~ as.integer(round(.x))) %>%
    
    # Add custom fusion variables "mortint_share" and "tax_rate" to CEI processed microdata
    # This is because CEI primary mortgage interest and principal are summed for creation of the 'mrtgip__mortgage' harmonized predictor (total mortgage payment)
    # i.e. Total mortgage payment is an ACS variable and the CEI P&I component variables are removed by assemble(), but we want still to fuse the percent of the payment that is interest
    # The tax rate is calculated as before-tax income minus after-tax income, divided by before-tax income (i.e. effective tax rate)
    # Ultimately, the rate is fused and then multiplied by ACS household income (experimental)
    
    mutate(mortgage = mrtgip + mrtgpp,
           mortint_share = ifelse(mortgage == 0, 0, mrtgip / mortgage),
           mortint_share = signif(round(mortint_share, 3), 3),
           tax = fincbtxm - finatxem,  # Before-tax income minus after-tax income
           tax_rate = ifelse(fincbtxm == 0, 0, tax / fincbtxm),
           tax_rate = signif(round(tax_rate, 3), 3)) 
  
  #-----
  # Impute household primary heating fuel predictor variable for CEI households
  # This is necessary because the CEI does not contain a primary heating fuel variable, but ACS does and it is critical for utilities simulation
  # Imputation is done via a LightGBM model fit to RECS 2015 microdata; see imputeHeatingFuel() function and script
  # The ACS heating fuel variable ('hfl') is then harmonized to match the RECS fuel types imputed to CEI households
  cei <- h.final
  temp <- tibble(hid = cei$hid,
                 hfl = imputeHeatingFuel(cei.h = cei)) %>%
    mutate(hfl = factor(as.character(hfl)))

  h.final <- h.final %>%
    left_join(temp, by = "hid") 
  
  #-----
  # Add labes for variables that were created after the processing step
  labelled::var_label(h.final$mortint_share) <- "Percent of the mortgage payment that is interest"
  labelled::var_label(h.final$tax_rate) <- "Before-tax income minus after-tax income, divided by before-tax income"
  labelled::var_label(h.final$hfl) <- "Imputed primary heating fuel"
   
  # Create dictionary and save to disk
  dictionary <- createDictionary(data = h.final, survey = "CEI", vintage = survey.range, respondent = "H")
  saveRDS(object = dictionary, file = paste0("survey-processed/CEX/CEI/CEI_", survey.range, "_H_dictionary.rds"))

  compileDictionary()
  
  # Save data to disk (.fst)
  cat("Saving household-level microdata to disk...\n")
  fst::write_fst(x = h.final, paste0("survey-processed/CEX/CEI/CEI_", survey.range, "_H_processed.fst"), compress = 100)

}
