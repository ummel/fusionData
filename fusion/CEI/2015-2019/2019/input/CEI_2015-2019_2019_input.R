library(fusionData)
library(fusionModel)

# Number of cores to use
ncores <- 3

# Donor and recipient survey identifiers
donor <- "CEI_2015-2019"
recipient <- "ACS_2019"

# Directory in /fusion where results will be saved
# This is automatically constructed from 'donor' and 'recipient', assuming recipient is ACS-based
acs.vintage <- substring(recipient, 5)
dir <- paste("fusion", sub("_", "/", donor), acs.vintage, "input", sep = "/")

#-----

# Obtain CEI fusion variable names  from fusionacsdata@gmail.com GDrive account
# Note that some variables listed in the "Categories" google sheet are, in fact, harmonized and not included in 'fusion.vars'
# https://docs.google.com/spreadsheets/d/13GRKkVZXapHtP7oK1WUh0Yu7OQ_9icd17wUGuhX-WRg/edit
fusion.vars <- googlesheets4::read_sheet("13GRKkVZXapHtP7oK1WUh0Yu7OQ_9icd17wUGuhX-WRg", sheet = "Category Summary") %>%
  filter(major != "Other" | cat == "VEHVAL") %>%   # Retain vehicle value but drop remaining "Other" categories
  mutate(cat = tolower(cat)) %>%
  pull(cat)

#-----

# Prepare and assemble data inputs

prep <- prepare(donor = "CEI_2015-2019",
                recipient = "ACS_2019",
                respondent = "household",
                implicates = 1)

data <- assemble(prep,
                 fusion.variables = fusion.vars,
                 window = 2)

rm(prep)

#-----

# Restrict 'fusion.vars' to only those variables returned by assemble()
# This effectively removes variables used for harmonization with ACS
fusion.vars <- intersect(fusion.vars, names(data$`CEI_2015-2019`))

# All of the fusion.vars at this point are dollar expenditure values
# Convert all to integer for better file compression
data$`CEI_2015-2019` <- data$`CEI_2015-2019` %>%
  mutate_at(fusion.vars, ~ as.integer(round(.x)))

#-----

# Add custom fusion variables "mortint_share" and "tax_rate" to CEI processed microdata
# This is because CEI primary mortgage interest and principal are summed for creation of the 'mrtgip__mortgage' harmonized predictor (total mortgage payment)
# i.e. Total mortgage payment is an ACS variable and the CEI P&I component variables are removed by assemble(), but we want still to fuse the percent of the payment that is interest
# The tax rate is calculated as before-tax income minus after-tax income, divided by before-tax income (i.e. effective tax rate)
# Ultimately, the rate is fused and then multiplied by ACS household income (experimental)

temp <- read_fst("survey-processed/CEX/CEI/CEI_2015-2019_H_processed.fst") %>%
  mutate(mortgage = mrtgip + mrtgpp,
         mortint_share = ifelse(mortgage == 0, 0, mrtgip / mortgage),
         mortint_share = signif(round(mortint_share, 3), 3),
         tax = fincbtxm - finatxem,  # Before-tax income minus after-tax income
         tax_rate = ifelse(fincbtxm == 0, 0, tax / fincbtxm),
         tax_rate = signif(round(tax_rate, 3), 3)) %>%
  select(cei_hid, mortint_share, tax_rate)

data$`CEI_2015-2019` <- data$`CEI_2015-2019` %>%
  left_join(temp, by = "cei_hid")

# Add the custom fusion variables
fusion.vars <- c(fusion.vars, "mortint_share", "tax_rate")

#-----

# Impute household primary heating fuel predictor variable for CEI households
# This is necessary because the CEI does not contain a primary heating fuel variable, but ACS does and it is critical for utilities simulation
# Imputation is done via a LightGBM model fit to RECS 2015 microdata; see imputeHeatingFuel() function and script
# The ACS heating fuel variable ('hfl') is then harmonized to match the RECS fuel types imputed to CEI households

source("fusion/CEI/2015-2019/2019/input/imputeHeatingFuel.R")
cei <- read_fst("survey-processed/CEX/CEI/CEI_2015-2019_H_processed.fst")
temp <- tibble(cei_hid = cei$cei_hid,
               hfl = imputeHeatingFuel(cei.h = cei)) %>%
  mutate(hfl = factor(as.character(hfl)),
         hfl = factor(as.integer(hfl))) %>%
  rename(hfl__hfl  = hfl)

# Merge imputed heating fuel with CEI harmonized microdata
data$`CEI_2015-2019` <- data$`CEI_2015-2019` %>%
  left_join(temp, by = "cei_hid")

# Create harmonized version of ACS heating fuel variable ('hfl')
temp <- read_fst("survey-processed/ACS/2019/ACS_2019_H_processed.fst", columns = c("acs_2019_hid", "hfl")) %>%
  mutate(hfl = recode(hfl, Electricity = 'Electricity', `Utility gas` = 'Natural gas', `Bottled, tank, or LP gas` = 'LPG', `Fuel oil, kerosene, etc.` = 'Fuel oil', .default = 'Other'),
         hfl = factor(as.character(hfl)),
         hfl = factor(as.integer(hfl))) %>%
  rename(hfl__hfl  = hfl)

data$ACS_2019 <- data$ACS_2019 %>%
  left_join(temp, by = "acs_2019_hid")

# Sanity check
# table(data$`CEI_2015-2019`$hfl) / nrow(data$`CEI_2015-2019`)
# table(data$ACS_2019$hfl) / nrow(data$ACS_2019)
rm(cei, temp)

#-----

# Specify the predictors in the harmonized donor data that will be used for validation subsets
# We select the variables that best reflect the following socioeconomic and geographic concepts:
#  -- income; race/ethnicity; education; household size; housing tenure; and a relatively high-resolution location variable
# These variables are "forced" as predictors in prepXY() and carried along in 'prep' for use by validate() in /output.R
sub.vars <- c("fincbtxm__hincp", "ref_race__rac1p", "educ_ref__schl",
              "fam_size__np", "cutenure__ten", "loc..division")

# Identify the shared 'pred.vars'
pred.vars <- setdiff(intersect(names(data[[1]]), names(data[[2]])), "weight")

# !!! TEMP TEST -- limit to smaller set of fusion variables for testing
#fusion.vars <- c("cloftw", "eathome", "airshp", "gas", "elec")

# Determine fusion order and subset of 'pred.vars' to use with each fusion variable/block
prep <- prepXY(data = data[[1]],
               y = fusion.vars,
               x = pred.vars,
               weight = "weight",
               xforce = sub.vars,
               cores = ncores)

# Save output from prepXY()
saveRDS(prep, file = file.path(dir, paste(donor, acs.vintage, "prep.rds", sep = "_")))

#-----

# Update 'pred.vars' to the subset of predictors retained in 'prep'
pred.vars <- attr(prep, "xpredictors")

# Set cores for 'fst' to use when writing to disk
threads_fst(ncores)

# Save training data to disk
data[[1]] %>%
  select(one_of(c("weight", unlist(prep$y), pred.vars))) %>%
  write_fst(path = file.path(dir, paste(donor, acs.vintage, "train.fst", sep = "_")), compress = 100)

# Save prediction data to disk
data[[2]] %>%
  select(one_of(pred.vars)) %>%
  write_fst(path = file.path(dir, paste(donor, acs.vintage, "predict.fst", sep = "_")), compress = 100)
