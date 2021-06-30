library(labelled)
library(tidyverse)

source("R/imputeMissing.R")
source("R/detectDependence.R")
source("R/utils.R")

# Question to RECS staff: There are some cases where spending is zero on a fuel but the fuel is listed as used
# Example: WOODAMT == 0, WDWARM == "Yes" -- is this correct?

# MANUAL CORRECTIONS -- Issues with the AUDIT variable
# d <- d %>%
#   mutate(FREEAUDIT = ifelse(AUDIT == 0, 0, FREEAUDIT)) # Affects a single observation
# check: d %>% select(FREEAUDIT, AUDIT) %>% add_count(FREEAUDIT, AUDIT) %>% distinct()

#-----

d <- read_csv("survey-raw/RECS/2015/recs2015_public_v4.csv")

#-----

codebook <- readxl::read_excel("survey-raw/RECS/2015/codebook_publicv4.xlsx", skip = 3) %>%
  setNames(c('var', 'type', 'length', 'desc', 'value', 'label')) %>%
  select(var, desc, value, label) %>%
  filter(
    !is.na(desc), # Drops 'Note' entry at end
    !grepl("conversion factor", tolower(desc)), # Remove conversion factor variables
    !grepl("imputation flag", tolower(desc)), # Remove imputation flag variables
  ) %>%
  mutate(
    value = gsub("- \r\n", "- ", value, fixed = TRUE),  # MANUAL CORRECTION for the DOEID 'value' entry to prevent erroneous split in following lines (DOEID 'value' has a line break when it should not)
    value = map(str_split(value, fixed("\r\n")), ~ as.list(unlist(.x))),
    label = map(str_split(label, fixed("\r\n")), ~ as.list(unlist(.x)))
  ) %>%
  unnest(cols = c(value, label)) %>%
  mutate(
    value = ifelse(var == "EQUIPMUSE" & label == "Not applicable", -2, value),  # MANUAL CORRECTION; reported to RECS staff 4/30/21
    label = ifelse(grepl("^Don.t know$", label) | grepl("Refused", label), NA, label)  # Set label to NA if value is "Don't know" or "Refused" (these observations are to be imputed eventually)
  ) %>%
  mutate_all(trimws)

#-----

# Variables with "Not applicable" values
# These are the variables for which suitable replacement values must be specified below
na.vars <- codebook %>%
  filter(label == "Not applicable") %>%
  pull(var) %>%
  unique()

#-----

# Manually constructed...
# What value should "Not applicable" take for the following variables?
na.values <- list(

  CELLAR = "No",
  BASEFIN = "No",
  ATTIC = "No",
  ATTICFIN = "No",
  STORIES = "Not a single-family home",
  PRKGPLC1 = "No",
  SIZEOFGARAGE = "No garage",
  STUDIO = "No",
  ROOFTYPE = "Unknown, large apartment building",
  HIGHCEIL = "No",
  SWIMPOOL = "No",
  POOL = "No",
  FUELPOOL = "No pool",
  FUELTUB = "No hot tub",
  OUTLET = "Unknown, large apartment building",
  BACKUP = "Unknown, large apartment building",
  SOLAR = "Unknown, apartment building",
  SIZRFRI1 = "No refrigerator",
  TYPERFR1 = "No refrigerator",
  AGERFRI1 = "No refrigerator",
  ICE = "No refrigerator",
  SIZRFRI2 = "No second refrigerator",
  TYPERFR2 = "No second refrigerator",
  AGERFRI2 = "No second refrigerator",
  LOCRFRI2 = "No second refrigerator",
  UPRTFRZR = "No freezer",
  SIZFREEZ = "No freezer",
  AGEFRZR = "No freezer",
  STOVENFUEL = "No stove",
  DUALCOOKTFUEL = "No cooktop",
  DUALOVENFUEL = "No oven",
  STOVEFUEL = "No separate cooktop",
  OVENFUEL = "No separate oven",
  OUTGRILLFUEL = "No outdoor grill",
  DWCYCLE = "No dishwasher",
  AGEDW = "No dishwasher",
  TOPFRONT = "No clothes washer in home",
  WASHTEMP = NULL, # Force drop; no good way to re-code
  RNSETEMP = NULL, # Force drop; no good way to re-code
  AGECWASH = "No clothes washer in home",
  DRYRFUEL = "No clothes dryer in home",
  AGECDRYER = "No clothes dryer in home",
  TVSIZE1 = "No TV",
  TVTYPE1 = "No TV",
  TVONWD1 = "None, no TV",
  TVONWE1 = "None, no TV",
  TVSIZE2 = "No second TV",
  TVTYPE2 = "No second TV",
  TVONWD2 = "None, no second TV",
  TVONWE2 = "None, no second TV",
  INWIRELESS = "No",
  DNTHEAT = "Have equipment, use it",
  EQUIPM = "Do not use space heating",
  FUELHEAT = "Do not use space heating",
  EQUIPAGE = "Do not use space heating",
  THERMAIN = "Do not use space heating",
  PROTHERM = "Do not use space heating",
  EQUIPMUSE = "Do not use space heating",
  EQUIPAUX = "Do not use space heating",
  EQUIPAUXTYPE = "Do not use space heating",
  FUELAUX = "Do not use space heating",
  BASEHEAT = "No",
  ATTCHEAT = "No",
  GARGHEAT = "No",
  COOLTYPE = "No air conditioning",
  CENACHP = "No air conditioning",
  AGECENAC = "No central air conditioner",
  THERMAINAC = "No central air conditioner",
  PROTHERMAC = "No central air conditioner",
  USECENAC = "No central air conditioner",
  WWACAGE = "No individual AC units",
  USEWWAC = "No individual AC units",
  BASECOOL = "No",
  ATTCCOOL = "No",
  GARGCOOL = "No",
  SWAMPCOL = "No",
  H2OHEATAPT = "Not an apartment",
  WHEATSIZ = "Water heater somewhere else in apartment building",
  MORETHAN1H2O = "Water heater somewhere else in apartment building",
  FUELH2O2 = "No secondary water heater",
  LGTOUTNUM = "None",
  LGTOUTCNTL = "No",
  AUDITCHG = "No audit",
  EELIGHTS = "No",
  FREEAUDIT = "No audit",
  REBATEAPP = "No",
  RECYCAPP = "No",
  TAXCREDITAPP = "No",
  BENOTHER = "No",
  ESCWASH = "No clothes washer",
  ESDISHW = "No dishwasher",
  ESDRYER = "No clothes dryer",
  ESFREEZE = "No freezer",
  ESFRIG = "No refrigerator",
  SMARTTHERM = "No",
  NGPAY = "No natural gas",
  LPGPAY = "No propane",
  FOPAY = "No fuel oil",
  INTDATA = "No",
  INTDATAACC = "No",
  ENERGYASST11 = "No",
  ENERGYASST12 = "No",
  ENERGYASST13 = "No",
  ENERGYASST14 = "No",
  ENERGYASST15 = "No",
  ENERGYASSTOTH = "No",
  PAYHELP = "No",
  NOHEATHELP = "No",
  NOACHELP = "No",
  WOODLOGS = "No",
  WDPELLET = "No",
  WDOTHER = "No",

  # Numeric variables where zero should be inserted for "Not applicable"
  MONPOOL = 0,
  MONTUB = 0,
  COOKTUSE = 0,
  OVENUSE = 0,
  SEPCOOKTUSE = 0,
  SEPOVENUSE = 0,
  AMTMICRO = 0,
  DWASHUSE = 0,
  WASHLOAD = 0,
  DRYRUSE = 0,
  CABLESAT = 0,
  COMBODVR = 0,
  SEPDVR = 0,
  PLAYSTA = 0,
  DVD = 0,
  VCR = 0,
  INTSTREAM = 0,
  TVAUDIOSYS = 0,
  USEMOISTURE = 0,
  NUMBERAC = 0,
  NUMWHOLEFAN = 0,
  NUMATTICFAN = 0,
  USENOTMOIST = 0,
  WHEATAGE = 0,
  NOHEATDAYS = 0,
  NOACDAYS = 0,
  WOODAMT = 0,
  PELLETAMT = 0,

  # These are cases where the variable measures a continuous/numeric concept,
  # but inserting a zero-value for "Not applicable" entries does not make sense.
  # In this case, a categorical value is assigned to override the default zero
  # and the variable should be treated as an ordered factor.
  TEMPHOME = "Do not use space heating",
  TEMPGONE = "Do not use space heating",
  TEMPNITE = "Do not use space heating",
  TEMPHOMEAC = "No air conditioning",
  TEMPGONEAC = "No air conditioning",
  TEMPNITEAC = "No air conditioning"

)

# Safety check for missing entries in 'na.values'
miss <- noquote(setdiff(na.vars, names(na.values)))
extras <- noquote(setdiff(names(na.values), na.vars))
stopifnot(length(miss) == 0 & length(extras) == 0)

#-----

# Update 'codebook' with the manually specified override values for "Not applicable"

# Drop variables set to NULL in 'na.values'
codebook <- filter(codebook, !var %in% names(which(map_lgl(na.values, is.null))))

# Update "Not applicable" values for each variable
for (v in na.vars) {
  if (!is.null(na.values[[v]])) {
    ind <- which(codebook$var == v & codebook$label == "Not applicable")  # Update the "Not applicable" label to specified label
    codebook$label[ind] <- na.values[[v]]
  }
}

# Safety check
# Ensure there are not "Not applicable" entries remaining in the codebook
stopifnot(!any(codebook$label == "Not applicable", na.rm = TRUE))

#-----

# Assign levels for ordered factors
# In general, we want to coerce unordered factor to ordered factors whenever feasible
# There is some judgement involved
# Example code to obtain unique levels: filter(codebook, var == "TVSIZE1")$label %>% unlist %>% unique

ordered.factors <- list(

  SIZEOFGARAGE = c("No garage", "One-car garage", "Two-car garage", "Three-or-more-car garage"),
  STORIES = c("Not a single-family home", "One story", "Split-level", "Two stories", "Three stories", "Four or more stories"),
  YEARMADERANGE = c("Before 1950", "1950 to 1959", "1960 to 1969", "1970 to 1979", "1980 to 1989", "1990 to 1999", "2000 to 2009", "2010 to 2015"),
  OCCUPYYRANGE = c("Before 1950", "1950 to 1959", "1960 to 1969", "1970 to 1979", "1980 to 1989", "1990 to 1999", "2000 to 2009", "2010 to 2015"),
  WINDOWS = c("1 to 2", "3 to 5", "6 to 9", "10 to 15", "16 to 19", "20 to 29", "30 or more"),
  TYPEGLASS = c("Single-pane glass", "Double-pane glass", "Triple-pane glass"),
  ADQINSUL = c("Not insulated", "Poorly insulated", "Adequately insulated", "Well insulated"),
  DRAFTY = c("Never", "Some of the time", "Most of the time", "All the time"),
  SIZRFRI1 = c("No refrigerator", "Half-size or compact", "Small (17.5 cubic feet or less)", "Medium (17.6 to 22.5 cubic feet)", "Large (22.6 to 29.5 cubic feet)", "Very large (bigger than 29.5 cubic feet)"),
  AGERFRI1 = c("No refrigerator", "Less than 2 years old", "2 to 4 years old", "5 to 9 years old", "10 to 14 years old", "15 to 19 years old", "20 years or older"),
  SIZRFRI2 =  c("No second refrigerator", "Half-size or compact", "Small (17.5 cubic feet or less)", "Medium (17.6 to 22.5 cubic feet)", "Large (22.6 to 29.5 cubic feet)", "Very large (bigger than 29.5 cubic feet)"),
  AGERFRI2 = c("No second refrigerator", "Less than 2 years old", "2 to 4 years old", "5 to 9 years old", "10 to 14 years old", "15 to 19 years old", "20 years or older"),
  SIZFREEZ = c("No freezer", "Half-size or compact", "Small (17.5 cubic feet or less)", "Medium (17.6 to 22.5 cubic feet)", "Large (22.6 to 29.5 cubic feet)", "Very large (bigger than 29.5 cubic feet)"),
  AGEFRZR =  c("No freezer", "Less than 2 years old", "2 to 4 years old", "5 to 9 years old", "10 to 14 years old", "15 to 19 years old", "20 years or older"),
  NUMMEAL = c("Never", "Less than once a week", "About once a week", "A few times each week", "Once a day", "Two times a day", "Three or more times a day"),
  AGEDW = c("No dishwasher", "Less than 2 years old", "2 to 4 years old", "5 to 9 years old", "10 to 14 years old", "15 to 19 years old", "20 years or older"),
  AGECWASH = c("No clothes washer in home", "Less than 2 years old", "2 to 4 years old", "5 to 9 years old", "10 to 14 years old", "15 to 19 years old", "20 years or older"),
  AGECDRYER = c("No clothes dryer in home", "Less than 2 years old", "2 to 4 years old", "5 to 9 years old", "10 to 14 years old", "15 to 19 years old", "20 years or older"),
  TVSIZE1 = c("No TV", "27 inches or less", "28 to 39 inches", "40 to 59 inches", "60 inches or more"),
  TVONWD1 = c("None, no TV", "Less than 1 hour", "1 to 3 hours", "4 to 6 hours", "7 to 10 hours", "More than 10 hours"),
  TVONWE1 = c("None, no TV", "Less than 1 hour", "1 to 3 hours", "4 to 6 hours", "7 to 10 hours", "More than 10 hours"),
  TVSIZE2 = c("No second TV", "27 inches or less", "28 to 39 inches", "40 to 59 inches", "60 inches or more"),
  TVONWD2 = c("None, no second TV", "Less than 1 hour", "1 to 3 hours", "4 to 6 hours", "7 to 10 hours", "More than 10 hours"),
  TVONWE2 = c("None, no second TV", "Less than 1 hour", "1 to 3 hours", "4 to 6 hours", "7 to 10 hours", "More than 10 hours"),
  TEMPHOME = c("Do not use space heating", 50:90),
  TEMPGONE = c("Do not use space heating", 50:90),
  TEMPNITE = c("Do not use space heating", 50:90),
  AGECENAC = c("No central air conditioner", "Less than 2 years old", "2 to 4 years old", "5 to 9 years old", "10 to 14 years old", "15 to 19 years old", "20 years or older"),
  WWACAGE = c("No individual AC units", "Less than 2 years old", "2 to 4 years old", "5 to 9 years old", "10 to 14 years old", "15 to 19 years old", "20 years or older"),
  TEMPHOMEAC = c("No air conditioning", 50:90),
  TEMPGONEAC = c("No air conditioning", 50:90),
  TEMPNITEAC = c("No air conditioning", 50:90),
  LGTINNUM = c("Fewer than 20 light bulbs", "20 to 39 light bulbs", "40 to 59 light bulbs", "60 to 79 light bulbs", "80 or more light bulbs"),
  LGTINCAN = c("None", "Some", "About half", "Most", "All"),
  LGTINLED = c("None", "Some", "About half", "Most", "All"),
  LGTOUTNUM = c("None", "1 to 4 bulbs", "5 to 9 bulbs", "10 or more bulbs"),
  EDUCATION = c("Less than high school diploma or GED", "High school diploma or GED", "Some college or Associate’s degree", "Bachelor’s degree (for example: BA, BS)", "Master’s, Professional, or Doctorate degree (for example: MA, MS, MBA, MD, JD, PhD)"),
  MONEYPY = c("Less than $20,000", "$20,000 - $39,999", "$40,000 - $59,999", "$60,000 to $79,999", "$80,000 to $99,999", "$100,000 to $119,999", "$120,000 to $139,999", "$140,000 or more"),
  SCALEB = c("Never", "1 or 2 months", "Some months", "Almost every month"),
  SCALEG = c("Never", "1 or 2 months", "Some months", "Almost every month"),
  SCALEE = c("Never", "1 or 2 months", "Some months", "Almost every month"),
  PERIODEL = c("No billing data collected or billing data not used", "Less than 60 days", "At least 60, but less than 146 days", "At least 146, but less than 330 days", "330 days or more"),
  PERIODNG = c("Not applicable, no natural gas consumption", "No billing data collected or billing data not used", "Less than 60 days", "At least 60, but less than 146 days", "At least 146, but less than 330 days", "330 days or more"),
  PERIODFO = c("Not applicable, no fuel oil consumption", "No delivery data collected", "Less than 365 days (incomplete)", "365 days (complete)"),
  PERIODLP = c("Not applicable, no propane consumption", "No delivery data collected", "Less than 365 days (incomplete)", "365 days (complete)")

)

# Safety check
# Detect any variables in 'ordered.factors' that are NOT in the codebook
extras <- noquote(setdiff(names(ordered.factors), codebook$var))
stopifnot(length(extras) == 0)

# Safety check
# Check for a precise match between levels specified in 'ordered.factors' and the codebook labels
# Will return helpful message if a discrepancy is detected; some discrepancies may be allowable
for (v in names(ordered.factors)) {
  of <- sort(ordered.factors[[v]])
  cb <- sort(unique(filter(codebook, var == v)$label))
  if (!identical(of, cb)) warning("Have a closer look at ", v, "\n-- Supplied levels:\n", paste(of, collapse = "\n"), "\n--Codebook levels:\n", paste(cb, collapse = "\n"))
}

#-----

# Update variable values with associated labels from 'codebook'

# Restrict 'd' to variables remaining in 'codebook'
d <- select(d, all_of(unique(codebook$var)))

# Loop through each variable in 'd', assigning labels when applicable
for (v in names(d)) {

  cb <- filter(codebook, var == v)
  x <- d[[v]]
  y <- unlist(cb$value)
  z <- unlist(cb$label)
  m <- match(x, y)
  new.labels <- z[na.omit(m)]

  # Update 'x' with new value labels
  x[!is.na(m)] <- new.labels

  # Coerce result to ordered factor, if specified
  # Note that levels are restricted to those actually present in the data
  if (v %in% names(ordered.factors)) {
    num.na <- sum(is.na(x))
    x <- factor(x, levels = intersect(ordered.factors[[v]], x), ordered = TRUE)
    stopifnot(sum(is.na(x)) == num.na)  # This is a final safety check to ensure no NA's introduced inadvertently
  }

  # Apply type.convert() to 'x'; leave ordered factors unchanged
  x <- if (is.ordered(x)) x else type.convert(x, as.is = FALSE)

  # Ensure unordered factor levels are sorted alphabetically
  if (is.factor(x) & !is.ordered(x)) x <- factor(x, levels = sort(unique(x)))

  # Update column in 'd'
  d[[v]] <- x

}

#-----

# Impute NA values in 'd'
d <- imputeMissing(data = d,
                   weight = "NWEIGHT",
                   x_exclude = c("DOEID", "^BRRWT"))

# Check for any remaining NA's
anyNA(d)

#-----

# Add/create variables for geographic concordance with variables in 'geolink'

# Variables names prior to modification/addition of geographic identifiers
d <- d %>%
  rename(recs_division = DIVISION,
         region = REGIONC,
         recs_iecc_zone = IECC_CLIMATE_PUB,
         recs_ba_zone = CLIMATE_REGION_PUB) %>%
  mutate(cbsatype15 = NA,
         cbsatype15 = ifelse(grepl("Metro", METROMICRO), "Metro", cbsatype15),
         cbsatype15 = ifelse(grepl("Micro", METROMICRO), "Micro", cbsatype15),
         ur12 = substring(UATYP10, 1, 1))

# See which variables in 'd' are also in 'geolink' and
ginfo <- readRDS("geo-processed/geolink_description.rds")
gvars <- intersect(names(ginfo), names(d))
d <- d %>%
  mutate_at(gvars, ~ factor(.x, levels = sort(unique(.x)), ordered = FALSE)) %>%  # Class any new/added geo identifiers as unordered factors
  set_variable_labels(.labels = ginfo, .strict = FALSE)  # Set variable descriptions for geo identifiers

#----------------

# Assemble final output
# NOTE: var_label assignment is done AFTER any manipulation of values/classes, because labels can be lost when classes are changed
d <- d %>%
  mutate_if(is.numeric, convertInteger) %>%
  mutate_if(is.double, cleanNumeric, tol = 0.001) %>%
  set_variable_labels(.labels = setNames(as.list(codebook$desc), codebook$var), .strict = FALSE) %>%
  rename(
    recs_2015_hid = DOEID,  # Rename ID and weight variables to standardized names
    weight = NWEIGHT
  ) %>%
  rename_with(~ gsub("BRRWT", "REP_", .x, fixed = TRUE), .cols = starts_with("BRRWT")) %>%  # Rename replicate weight columns to standardized names
  rename_with(tolower) %>%  # Convert all variable names to lowercase
  select(recs_2015_hid, weight, everything(), -starts_with("rep_"), starts_with("rep_"))  # Reorder columns with replicate weights at the end

#----------------

# Check that all variables have descriptions assigned
stopifnot(length(var_label(d)) == ncol(d))

#----------------

# Create dictionary and save to disk
dictionary <- createDictionary(data = d, survey = "RECS", vintage = 2015, respondent = "H")
saveRDS(object = dictionary, file = "survey-processed/RECS/2015/RECS_2015_H_dictionary.rds")

#----------------

# Save data to disk (.fst)
fst::write_fst(x = d, path = "survey-processed/RECS/2015/RECS_2015_H_processed.fst", compress = 100)
