library(tidyverse)
library(reshape2)
source("R/utils.R")
source("R/createDictionary.R")
source("R/imputeMissing.R")
source("R/detectDependence.R")

#-----

# Question to RECS staff: There are some cases where spending is zero on a fuel but the fuel is listed as used
# Example: WOODAMT == 0, WDWARM == "Yes" -- is this correct?

# MANUAL CORRECTIONS -- Issues with the AUDIT variable
# d <- d %>%
#   mutate(FREEAUDIT = ifelse(AUDIT == 0, 0, FREEAUDIT)) # Affects a single observation
# check: d %>% select(FREEAUDIT, AUDIT) %>% add_count(FREEAUDIT, AUDIT) %>% distinct()

#-----

# Load raw RECS 2020 data
d <- read_csv("survey-raw/RECS/2020/recs2020_public_v3.csv")

#-----

# Load and process and codebook
# NOTE: Warning about "New names:" is OK
codebook <- readxl::read_excel("survey-raw/RECS/2020/RECS 2020 Codebook for Public File - v3.xlsx", skip = 1) %>% select(.,-c('Section')) %>%
  setNames(c('var','type', 'desc','value')) %>%
  filter(
    !grepl("conversion factor", tolower(desc)), # Remove conversion factor variables
    !grepl("imputation indicator", tolower(desc)), # Remove imputation flag variables
  ) %>%
  mutate(
    value = map(str_split(value, fixed("\r\n")), ~ as.list(unlist(.x)))) %>%
    unnest(cols = c(value)) %>% 
    mutate(label = ifelse(grepl("\\d+\\s*-\\s*\\d+",value) & !grepl("Not applicable",value) ,desc,NA),
           label = ifelse(var == 'STORIES'|var == 'SIZEOFGARAGE'|var == 'TYPEGLASS'|var == 'NGPAY'| var == 'TYPEHUQ'|var == 'EMPLOYHH'|
                          var == 'SIZRFRI1'|var == 'AGEFRI1'|var == 'SIZRFRI2'|var == 'SIZFREEZ'|var == 'TVONWD1'|var == 'TVONWD2'| var == 'MONEYPY',NA,label)) 
  
#Split codebook into 2 
codebook_a1 <- codebook %>% filter (type =='Num' | var == 'UATYP10') %>% filter(is.na(label)) %>%
              separate(value, c("value", "label"), sep="^\\s*\\S+\\K\\s+")

codebook_a2 <- codebook %>% filter (type =='Num' | var == 'UATYPE10') %>% filter(!is.na(label))

codebook_b <- codebook %>% filter (type !='Num' & var != 'UATYP10') %>% mutate(label = value)

codebook1 <- rbind(codebook_a1,codebook_a2,codebook_b)
stopifnot(nrow(codebook1) == nrow(codebook))  
rm(codebook_a1,codebook_a2,codebook_b)

codebook <- codebook1 %>%
  mutate(
    label = ifelse(grepl("^Don.t know$", label) | grepl("Refused", label), NA, label)  # Set label to NA if value is "Don't know" or "Refused" (these observations are to be imputed eventually)
  ) %>% 
  mutate_all(trimws)

  new_rows <- list(
    data.frame(var = 'MEDICALDEV', desc = "Any medical devices used at home", label = ("Not applicable"), value = NA, type = 'Num'),
    data.frame(var = 'EVCHRGHOME', desc = "Any medical devices used at home", label = ("Not applicable"), value = NA, type = 'Num'))

codebook <- bind_rows(codebook,new_rows)

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
  CRAWL = "No",
  CONCRETE = "No",
  BASEOTH = "No",
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
  POOLPUMP = "No",
  FUELPOOL = "No pool or not heated",
  FUELTUB = "No hot tub",
  SIZRFRI1 = "No refrigerator",
  TYPERFR1 = "No refrigerator",
  AGERFRI1 = "No refrigerator",
  ICE = "No refrigerator",
  LOCRFRI2 = "No second refrigerator",
  SIZRFRI2 = "No second refrigerator",
  TYPERFR2 = "No second refrigerator",
  AGERFRI2 = "No second refrigerator",
  UPRTFRZR = "No freezer",
  SIZFREEZ = "No freezer",
  FREEZER = "No freezer",
  AGEFRZR = "No freezer",
  RANGEINDT = "No",
  COOKTOPFUEL = "No cooktop",
   OVENFUEL = "No separate oven",
  DWCYCLE = "No dishwasher",
  AGEDW = "No dishwasher",
  TOPFRONT = "No clothes washer in home",
  WASHTEMP = NULL, # Force drop; no good way to re-code
  AGECWASH = "No clothes washer in home",
  DRYRFUEL = "No clothes dryer in home",
  AGECDRYER = "No clothes dryer in home",
  TVSIZE1 = "No TV",
  TVTYPE1 = "No TV",
  TVUSE1 = "No TV",
  TVONWD1 = "None, no TV",
  TVONWE1 = "None, no TV",
  TVSIZE2 = "No second TV",
  TVTYPE2 = "No second TV",
  TVUSE2 = "No TV",
  TVONWD2 = "None, no second TV",
  TVONWE2 = "None, no second TV",
  TVSIZE3 = "No second TV",
  TVTYPE3 = "No second TV",
  TVUSE3 = "No TV",
  TVONWD3 = "None, no second TV",
  TVONWE3 = "None, no second TV",
  TELLDAYS = "No telephone",
  TLDESKTOP = "No teleworking",
  TLLAPTOP = "No teleworking",
  TLTABLET = "No teleworking",
  TLMONITOR = "No teleworking",
  TLOTHER = "No teleworking",
  INTYPECELL = "No cellphone",
  INTYPEBROAD = "No broadband",
  INTYPEOTH = "No other type",
  SSLIGHT = "No", 
  SSTEMP = "No", 
  SSSECURE = "No", 
  SSTV = "No", 
  SSOTHER = "No", 
  DNTHEAT = "Have equipment, use it",
  HEATAPT = "Not an apartment",
  EQUIPM = "Do not use space heating",
  FUELHEAT = "Do not use space heating",
  EQUIPAGE = "Do not use space heating",
  GEOHP = "No",
  #EQUIPAUXTYPE = "Do not use space heating",
  RANGEFUEL =  "Do not use space heating",
  FUELAUX = "Do not use space heating",
  USEEQUIPAUX = "Do not use space heating",
  BASEHEAT = "No",
  ATTCHEAT = "No",
  GARGHEAT = "No",
  COOLAPT = "Not an apartment",
  ACEQUIPAGE  = "Do not use air conditioning",
  BASECOOL = "No",
  ATTCCOOL = "No",
  GARGCOOL = "No",
  USECFAN = "No ceiling fan",
  HOUSEFAN = "No",
  ATTICFAN = "No",
  USEDEHUM = "Did not use dehumifier",
  TYPETHERM = "Does not have thermostat for heating or cooling",
  HEATCNTL = "No heat control",
  TEMPHOME = "Do not use space heating",
  TEMPGONE = "Do not use space heating",
  TEMPNITE = "Do not use space heating",
  COOLCNTL = "No cool control",
  TEMPHOMEAC = "No air conditioning",
  TEMPGONEAC = "No air conditioning",
  TEMPNITEAC = "No air conditioning",
  H2OAPT = "Not an apartment",
  H2OMAIN = "Not a single family home",
  WHEATBKT = "No",
  FUELH2O2 = "No secondary water heater",
  LGTOUTCFL = 'No',
  LGTOUTCAN = 'No',
  INTDATAACC = "No",
  MEDICALDEV = "No",
  SOLAR = "Unknown, apartment building",
  OUTLET = "Unknown, large apartment building",
  EVCHRGHOME = "No EV charger at home",
  PAYHELP = "No",
  NOHEATHELP = "No",
  NOACHELP = "No",
  ENERGYASST20 = "No",
  ENERGYASST19 = "No",
  ENERGYASST18 = "No",
  ENERGYASST17 = "No",
  ENERGYASST16 = "No",
  ENERGYASSTOTH = "No",
  SQFTINCB = "No basement",
  SQFTINCA = "No attic",
  SQFTINCG = "No garage",
  ACEQUIPM_PUB = "Do not use air conditioning",
  ACEQUIPAUXTYPE_PUB = "Do not use air conditioning",
  
  # Numeric variables where zero should be inserted for "Not applicable"
  MONPOOL = 0,
  MONTUB = 0,
  RCOOKUSE = 0,
  ROVENUSE = 0,
  COOKTOPINDT = 'No',
  COOKTOPUSE = 0,
  OVENUSE = 0,
  AMTMICRO = 0,
  DWASHUSE = 0,
  WASHLOAD = 0,
  DRYRUSE = 0,
  CABLESAT = 0,
  COMBODVR = 0,
  SEPDVR = 0,
  INTSTREAM = 0,
  PLAYSTA = 0,
  DVD = 0,
  VCR = 0,
 TVAUDIOSYS = 0,
 SMARTSPK = 0,
 NUMPORTEL = 0,
 NUMFIREPLC = 0,
 NUMDLHP = 0,
 NUMPORTHUM = 0,
 USEHUMID = 0,
 NUMDLHPAC = 0,
 NUMWWAC = 0,
 NUMPORTAC = 0,
 NUMPORTDEHUM = 0,
  LGTOUTNITE = 0,
  LGTOUTLED = 'No',
  NOHEATDAYS = 0,
  NOACDAYS = 0,
  
  # These are cases where the variable measures a continuous/numeric concept,
  # but inserting a zero-value for "Not applicable" entries does not make sense.
  # In this case, a categorical value is assigned to override the default zero
  # and the variable should be treated as an ordered factor.
  TEMPHOME = "Do not use space heating",
  TEMPGONE = "Do not use space heating",
  TEMPNITE = "Do not use space heating"
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
  YEARMADERANGE = c("Before 1950", "1950 to 1959", "1960 to 1969", "1970 to 1979", "1980 to 1989", "1990 to 1999", "2000 to 2009", "2010 to 2015","2016 to 2020"),
  WINDOWS = c("1 or 2 windows", "3 to 5 windows", "6 to 9 windows", "10 to 15 windows","16 to 19 windows", "20 to 29 windows", "30 or more windows"),
  TYPEGLASS = c("Single-pane glass", "Double-pane glass", "Triple-pane glass"),
  ADQINSUL = c("Not insulated", "Poorly insulated", "Adequately insulated", "Well insulated"),
  DRAFTY = c("Never", "Some of the time", "Most of the time", "All the time"),
  SIZRFRI1 = c("No refrigerator", "Half-size or compact", "Small (17.5 cubic feet or less)", "Medium (17.6 to 22.5 cubic feet)", "Large (22.6 to 29.5 cubic feet)", "Very large (bigger than 29.5 cubic feet)"),
  AGERFRI1 = c("No refrigerator", "Less than 2 years old", "2 to 4 years old", "5 to 9 years old", "10 to 14 years old", "15 to 19 years old", "20 or more years old"),
  SIZRFRI2 =  c("No second refrigerator", "Half-size or compact", "Small (17.5 cubic feet or less)", "Medium (17.6 to 22.5 cubic feet)", "Large (22.6 to 29.5 cubic feet)", "Very large (bigger than 29.5 cubic feet)"),
  AGERFRI2 = c("No second refrigerator", "Less than 2 years old", "2 to 4 years old", "5 to 9 years old", "10 to 14 years old", "15 to 19 years old", "20 or more years old"),
  SIZFREEZ = c("No freezer", "Half-size or compact", "Small (17.5 cubic feet or less)", "Medium (17.6 to 22.5 cubic feet)", "Large (22.6 to 29.5 cubic feet)", "Very large (bigger than 29.5 cubic feet)"),
  AGEFRZR =  c("No freezer", "Less than 2 years old", "2 to 4 years old", "5 to 9 years old", "10 to 14 years old", "15 to 19 years old", "20 or more years old"),
  NUMMEAL = c("Never", "Less than once a week", "About once a week", "A few times each week", "Once a day", "Two times a day", "Three or more times a day"),
  AGEDW = c("No dishwasher", "Less than 2 years old", "2 to 4 years old", "5 to 9 years old", "10 to 14 years old", "15 to 19 years old", "20 or more years old"),
  AGECWASH = c("No clothes washer in home", "Less than 2 years old", "2 to 4 years old", "5 to 9 years old", "10 to 14 years old", "15 to 19 years old", "20 or more years old"),
  AGECDRYER = c("No clothes dryer in home", "Less than 2 years old", "2 to 4 years old", "5 to 9 years old", "10 to 14 years old", "15 to 19 years old", "20 or more years old"),
  TVSIZE1 = c("No TV", "27 inches or less", "28 to 39 inches", "40 to 59 inches", "60 inches or more"),
  TVONWD1 = c("None, no TV", "Less than one hour", 0:24),
  TVONWE1 = c("None, no TV", "Less than one hour", 0:24),
  TVSIZE2 = c("No second TV", "27 inches or less", "28 to 39 inches", "40 to 59 inches", "60 inches or more"),
  TVONWD2 = c("None, no second TV", "Less than one hour", 0:24),
  TVONWE2 = c("None, no second TV","Less than one hour", 0:24),
  TEMPHOME = c("Do not use space heating", 50:90),
  TEMPGONE = c("Do not use space heating", 50:90),
  TEMPNITE = c("Do not use space heating", 50:90),
  TEMPHOMEAC = c("No air conditioning", 50:90),
  TEMPGONEAC = c("No air conditioning", 50:90),
  TEMPNITEAC = c("No air conditioning", 50:90),
  LGTINCAN = c("None", "Some", "About half", "Most", "All"),
  LGTINLED = c("None", "Some", "About half", "Most", "All"),
  EDUCATION = c("Less than high school diploma or GED", "High school diploma or GED","Some college or Associate’s degree", "Bachelor’s degree", "Master’s, Professional, or Doctoral degree"),
  MONEYPY = c('Less than $5,000','$5,000 - $7,499','$7,500 - $9,999','$10,000 - $12,499','$12,500 - $14,999','$15,000 - $19,999','$20,000 - $24,999', '$25,000 - $29,999',
              '$30,000 - $34,999','$35,000 - $39,999','$40,000 - $49,999','$50,000 - $59,999','$60,000 - $74,999','$75,000 - $99,999','$100,000 - $149,999','$150,000 or more'),
  SCALEB = c("Never", "1 or 2 months", "Some months", "Almost every month"),
  SCALEG = c("Never", "1 or 2 months", "Some months", "Almost every month"),
  SCALEE = c("Never", "1 or 2 months", "Some months", "Almost every month")
)

# Safety check
# Detect any variables in 'ordered.factors' that are NOT in the codebook
extras <- noquote(setdiff(names(ordered.factors), codebook$var))
stopifnot(length(extras) == 0)

# Safety check
# Check for a precise match between levels specified in 'ordered.factors' and the codebook labels
# Will return helpful message if a discrepancy is detected; some discrepancies may be allowable
# NOTE: The "TEMP*" variables will be flagged here, but that's OK: They are an odd case where we have a numeric mixed with a categorical
# for (v in names(ordered.factors)) {
#   of <- sort(ordered.factors[[v]])
#   cb <- sort(unique(filter(codebook, var == v)$label))
#   if (!identical(of, cb)) warning("Have a closer look at ", v, "\n-- Supplied levels:\n", paste(of, collapse = "\n"), "\n--Codebook levels:\n", paste(cb, collapse = "\n"))
# }

#-----

# Only retain variables remaining in the codebook
d <- d[intersect(names(d), codebook$var)]

#-----
v <- 'MEDICALDEV'

# Update variable values with associated labels from 'codebook'
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

  # Ensure unordered factor levels are sorted according to codebook order of levels
  # Originally, unordered factors were sorted alphabetically -- but there is often useful information in the codebook ordering
  # This retains a valid codebook ordering if one exists; otherwise sort levels alphabetically
  if (is.factor(x) & !is.ordered(x)) {
    num.na <- sum(is.na(x))
    if (all(x %in% cb$label)) {
      x <- factor(x, levels = intersect(cb$label, unique(x)))
    } else {
      x <- factor(x, levels = sort(unique(x)))
    }
    stopifnot(sum(is.na(x)) == num.na)  # This is a final safety check to ensure no NA's introduced inadvertently
  }

  # Update column in 'd'
  d[[v]] <- x

}

#-----

# Detect structural dependencies
#-----

# Which variables have missing values and how frequent are they?
na.count <- colSums(is.na(d))
na.count <- na.count[na.count > 0]
na.count  # See which variables have NA's

# Impute NA values in 'd'
#imp <- imputeMissing(data = d,
   #                  N = 1,
  #                   weight = "NWEIGHT",
 #                    x_exclude = c("DOEID", "^BRRWT"))

# Replace NA's in 'd' with the imputed values
#d[names(imp)] <- imp
#rm(imp)
#gc()

anyNA(d)

#-----

# Add/create variables for geographic concordance with variables in 'geo_concordance.fst'
d <- d %>%
  rename(recs_division = DIVISION, 
         region = REGIONC,
         recs20_iecc_zone = IECC_climate_code,
         recs20_ba_zone = BA_climate) %>%
  mutate(
         ur12 = substring(UATYP10, 1, 1),
         region = str_to_title(region)) %>%
  select(-UATYP10) %>%

  mutate(recs20_ba_zone = as.character(recs20_ba_zone),
    recs20_ba_zone = ifelse(recs20_iecc_zone == "4C","Marine",recs20_ba_zone),
    recs20_ba_zone = as.factor(recs20_ba_zone)) #Need to check with staff 
  
# See which variables in 'd' are also in 'geo_concordance' and
gnames <- names(fst::fst("geo-processed/concordance/geo_concordance.fst"))
gvars <- intersect(gnames, names(d))

# Class new/added geo identifiers as unordered factors
d <- d %>%
  mutate_at(gvars, ~ factor(.x, levels = sort(unique(.x)), ordered = FALSE)) 

#----------------

# Assemble final output
# NOTE: var_label assignment is done AFTER any manipulation of values/classes, because labels can be lost when classes are changed
h.final <- d %>%
  mutate_if(is.factor, safeCharacters) %>%
  mutate_if(is.numeric, convertInteger) %>%
  mutate_if(is.double, cleanNumeric, tol = 0.001) %>%
  labelled::set_variable_labels(.labels = setNames(as.list(safeCharacters(codebook$desc)), codebook$var), .strict = FALSE) %>%  # Set descriptions for codebook variables
  labelled::set_variable_labels(.labels = setNames(as.list(paste(gvars, "geographic concordance")), gvars)) %>%  # Set descriptions for geo identifiers
  rename(
    recs_2020_hid = DOEID,  # Rename ID and weight variables to standardized names
    weight = NWEIGHT
  ) %>%
  rename_with(~ gsub("BRRWT", "REP_", .x, fixed = TRUE), .cols = starts_with("BRRWT")) %>%  # Rename replicate weight columns to standardized names
  rename_with(tolower) %>%  # Convert all variable names to lowercase
  select(recs_2020_hid, weight, everything(), -starts_with("rep_"), starts_with("rep_")) %>%   # Reorder columns with replicate weights at the end
  arrange(recs_2020_hid)

#----------------

# Create dictionary and save to disk
dictionary <- createDictionary(data = h.final, survey = "RECS", vintage = 2020, respondent = "H")
saveRDS(object = dictionary, file = "survey-processed/RECS/2020/RECS_2020_H_dictionary.rds")

#----------------

# Save data to disk (.fst)
fst::write_fst(x = h.final, path = "survey-processed/RECS/2020/RECS_2020_H_processed.fst", compress = 100)
compileDictionary()


