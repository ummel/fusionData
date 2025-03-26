library(tidyverse)
source("R/utils.R")

#-----

# Question to RECS staff: There are some cases where spending is zero on a fuel but the fuel is listed as used
# Example: WOODAMT == 0, WDWARM == "Yes" -- is this correct?

# MANUAL CORRECTIONS -- Issues with the AUDIT variable
# d <- d %>%
#   mutate(FREEAUDIT = ifelse(AUDIT == 0, 0, FREEAUDIT)) # Affects a single observation
# check: d %>% select(FREEAUDIT, AUDIT) %>% add_count(FREEAUDIT, AUDIT) %>% distinct()

#-----

# Load and process and codebook
# NOTE: Warning about "New names:" is OK
codebook <- readxl::read_excel("survey-raw/RECS/2015/codebook_publicv4.xlsx", skip = 3) %>%
  setNames(c('var', 'type', 'length', 'desc', 'value', 'label')) %>%
  select(var, desc, value, label) %>%
  filter(
    !is.na(desc), # Drops 'Note' entry at end
    !grepl("conversion factor", tolower(desc)), # Remove conversion factor variables
    !grepl("imputation flag", tolower(desc)), # Remove imputation flag variables
    !(grepl("square footage", tolower(desc)) & !var %in% c('TOTSQFT_EN')) # Remove unnecessary square footage variables
  ) %>%
  mutate(
    value = gsub("- \r\n", "- ", value, fixed = TRUE),  # MANUAL CORRECTION for the DOEID 'value' entry to prevent erroneous split in following lines (DOEID 'value' has a line break when it should not)
    value = map(str_split(value, fixed("\r\n")), ~ as.list(unlist(.x))),
    label = map(str_split(label, fixed("\r\n")), ~ as.list(unlist(.x)))
  ) %>%
  unnest(cols = c(value, label)) %>%
  mutate(
    value = ifelse(var == "EQUIPMUSE" & label == "Not applicable", -2, value),  # MANUAL CORRECTION; reported to RECS staff 4/30/21
    label = ifelse(grepl("^Don.t know$", label) | grepl("Refused", label), NA, label),  # Set label to NA if value is "Don't know" or "Refused" (these observations are to be imputed eventually)
    label = gsub("^IECC climate zone.", "", label)
  ) %>%
  mutate_all(str_squish) %>%
  mutate(desc = gsub("- public file variable", "", desc),
         desc = map_chr(str_split(desc, fixed(";"), n = 2), 1),
         desc = gsub("Total usage for", "Share of total usage for", desc),
         desc = gsub("Total cost for", "Share of total cost for", desc),
         desc = ifelse(grepl("usage for", desc) & !grepl("total usage for", desc), paste0("Share of ", tolower(desc)), desc),
         desc = ifelse(grepl("Share of", desc), gsub(", main and secondary", "", desc), desc),
         desc = ifelse(grepl("Share of", desc), gsub(", in thousand Btu, 2015", "", desc), desc),
         desc = ifelse(grepl("Share of", desc), gsub(", in thousand btu, 2015", "", desc), desc),
         desc = ifelse(grepl("Share of", desc), gsub(", in dollars, 2015", "", desc), desc),
         desc = ifelse(grepl("Share of total usage", desc), paste0(desc, " including electricity, natural gas, propane, and fuel oil"), desc),
         desc = ifelse(var == "TOTSQFT_EN", "Total energy-consuming area (square footage), including all main living areas; all basements; heated, cooled, or finished attics; and heated or cooled garages", desc))  # Manually description fix-up

# Load raw RECS 2015 data; only retain variables remaining in the codebook
d <- read_csv("survey-raw/RECS/2015/recs2015_public_v4.csv") %>%
  select(all_of(codebook$var))

#-----

ordered.factors <- c("SIZEOFGARAGE" , "STORIES" , "YEARMADERANGE" , "OCCUPYYRANGE" , "WINDOWS" , "TYPEGLASS" , "ADQINSUL",
                     "DRAFTY" , "SIZRFRI1" , "AGERFRI1" , "SIZRFRI2" , "AGERFRI2" , "SIZFREEZ" , "AGEFRZR",
                     "NUMMEAL" , "AGEDW" , "AGECWASH" , "AGECDRYER" , "TVSIZE1" , "TVONWD1" , "TVONWE1",
                     "TVSIZE2" , "TVONWD2" , "TVONWE2" , "TEMPHOME" , "TEMPGONE" , "TEMPNITE" , "AGECENAC",
                     "WWACAGE" , "TEMPHOMEAC" , "TEMPGONEAC" , "TEMPNITEAC" , "LGTINNUM" , "LGTINCAN" , "LGTINLED",
                     "LGTOUTNUM" , "EDUCATION" , "MONEYPY" , "SCALEB" , "SCALEG" , "SCALEE" , "PERIODEL",
                     "PERIODNG" , "PERIODFO" , "PERIODLP")

for (v in names(d)) {
  cb <- filter(codebook, var == v)
  x <- d[[v]]
  ok <- all(tolower(x) %in% tolower(cb$value))
  i <- tolower(x) %in% tolower(cb$value)
  x[i] <- cb$label[match(x[i], cb$value)]
  x <- type.convert(x, as.is = TRUE)
  if (is.character(x)) {
    x <- factor(x, levels = if (ok) intersect(cb$label, x) else sort(unique(x)))
    if ("Not applicable" %in% x) x <- relevel(x, "Not applicable")
    if (v %in% ordered.factors) x <- as.ordered(x)
  }
  d[[v]] <- x
}

#---

# Variables with "Not applicable" values
# These are the variables for which suitable replacement values must be specified below
nap.vars <- d %>%
  select_if(~ any(.x == "Not applicable")) %>%
  names()

# Manually constructed...
# What value should "Not applicable" take for the following variables?
nap.values <- list(

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
  WASHTEMP = "No clothes washer in home",
  RNSETEMP = "No clothes washer in home",
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
  EQUIPM = "No space heating",
  FUELHEAT = "No space heating",
  EQUIPAGE = "No space heating",
  GEOHP = "No",
  RANGEFUEL =  "No space heating",
  FUELAUX = "No space heating",
  USEEQUIPAUX = "No space heating",
  BASEHEAT = "No",
  ATTCHEAT = "No",
  GARGHEAT = "No",
  COOLAPT = "Not an apartment",
  ACEQUIPAGE  = "No air conditioning",
  BASECOOL = "No",
  ATTCCOOL = "No",
  GARGCOOL = "No",
  USECFAN = "No ceiling fan",
  HOUSEFAN = "No",
  ATTICFAN = "No",
  USEDEHUM = "No dehumidifier",
  TYPETHERM = "Does not have thermostat for heating or cooling",
  HEATCNTL = "No heat control",
  COOLCNTL = "No cool control",
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
  ACEQUIPM_PUB = "No air conditioning",
  ACEQUIPAUXTYPE_PUB = "No air conditioning",
  WOODTYPE = "Wood not used",
  EVCHRGHOME = "Household does not own or lease an electric vehicle",
  EVCHRGAPT = "Household does not own or lease an electric vehicle",
  EVCHRGWKS = "Household does not own or lease an electric vehicle",
  EVCHRGBUS = "Household does not own or lease an electric vehicle",
  EVCHRGMUNI = "Household does not own or lease an electric vehicle",
  EVCHRGDLR = "Household does not own or lease an electric vehicle",
  EVCHRGHWY = "Household does not own or lease an electric vehicle",
  EVCHRGOTH = "Household does not own or lease an electric vehicle",
  EVHOMEAMT = "Household does not own or lease an electric vehicle",
  EVCHRGTYPE = "Household does not own or lease an electric vehicle",
  LGTOUTLED = 'No',

  POOL = "No",
  BACKUP = "Unknown, large apartment building",
  STOVENFUEL = "No stove",
  DUALCOOKTFUEL = "No cooktop",
  DUALOVENFUEL =  "No oven",
  STOVEFUEL =  "No separate cooktop",
  OUTGRILLFUEL = "No outdoor grill",
  INWIRELESS = "No",
  THERMAIN = "Do not use space heating",
  PROTHERM = "Do not use space heating",
  EQUIPMUSE = "Do not use space heating",
  EQUIPAUX = "Do not use space heating",
  EQUIPAUXTYPE = "Do not use space heating",
  COOLTYPE = "No air conditioning",
  CENACHP = "No air conditioning",
  AGECENAC = "No central air conditioner",
  THERMAINAC = "No central air conditioner",
  PROTHERMAC = "No central air conditioner",
  USECENAC = "No central air conditioner",
  WWACAGE = "No individual AC units",
  USEWWAC = "No individual AC units",
  SWAMPCOL = "No",
  H2OHEATAPT = "Not an apartment",
  WHEATSIZ = "Water heater somewhere else in apartment building",
  MORETHAN1H2O = "Water heater somewhere else in apartment building",
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
  ENERGYASST11 = "No",
  ENERGYASST12 = "No",
  ENERGYASST13 = "No",
  ENERGYASST14 = "No",
  ENERGYASST15 = "No",
  WOODLOGS = "No",
  WDPELLET = "No",
  WDOTHER = "No",
  COOKTOPINDT = 'No',

  # Numeric variables where zero should be inserted for "Not applicable"
  COOKTUSE = 0,
  SEPCOOKTUSE = 0,
  SEPOVENUSE = 0,
  USEMOISTURE = 0,
  NUMBERAC = 0,
  NUMWHOLEFAN = 0,
  NUMATTICFAN = 0,
  USENOTMOIST = 0,
  WHEATAGE = 0,
  WOODAMT = 0,
  PELLETAMT = 0,
  MONPOOL = 0,
  MONTUB = 0,
  RCOOKUSE = 0,
  ROVENUSE = 0,
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
  NOHEATDAYS = 0,
  NOACDAYS = 0,

  # These are cases where the variable measures a continuous/numeric concept,
  # but inserting a zero-value for "Not applicable" entries does not make sense.
  # In this case, a categorical value is assigned to override the default zero
  # and the variable should be treated as an ordered factor.
  TEMPHOME = "No space heating",
  TEMPGONE = "No space heating",
  TEMPNITE = "No space heating",
  TEMPHOMEAC = "No air conditioning",
  TEMPGONEAC = "No air conditioning",
  TEMPNITEAC = "No air conditioning"

)

# Identify variables in 'nap.vars' not specified in 'nap.values
miss <- setdiff(nap.vars, names(nap.values))
miss

#-----

for (v in nap.vars) {
  x <- d[[v]]
  if (is.factor(x)) {
    levels(x)[levels(x) == "Not applicable"] <- nap.values[[v]]
    temp <- type.convert(x, as.is = FALSE)  # Catches cases like 'NOHEATDAYS' that can be coerced to integer because Not Applicable value is numeric.
    if (!is.factor(temp)) x <- temp
  } else {
    x[x == "Not applicable"] <- nap.values[[v]]
    x <- type.convert(x, as.is = FALSE)
  }
  d[[v]] <- x
}

# Coerce any remaining character variables to unordered factors
d <- d %>% mutate_if(is.character, as.factor)

#---

# Assign levels for ordered factors
# In general, we want to coerce unordered factor to ordered factors whenever feasible
# There is some judgement involved
# Example code to obtain unique levels: filter(codebook, var == "TVSIZE1")$label %>% unlist %>% unique

custom.ordered <- list(

  # SIZEOFGARAGE = c("No garage", "One-car garage", "Two-car garage", "Three-or-more-car garage"),
  # STORIES = c("Not a single-family home", "One story", "Split-level", "Two stories", "Three stories", "Four or more stories"),
  # YEARMADERANGE = c("Before 1950", "1950 to 1959", "1960 to 1969", "1970 to 1979", "1980 to 1989", "1990 to 1999", "2000 to 2009", "2010 to 2015","2016 to 2020"),
  # WINDOWS = c("1 or 2 windows", "3 to 5 windows", "6 to 9 windows", "10 to 15 windows","16 to 19 windows", "20 to 29 windows", "30 or more windows"),
  # TYPEGLASS = c("Single-pane glass", "Double-pane glass", "Triple-pane glass"),
  # ADQINSUL = c("Not insulated", "Poorly insulated", "Adequately insulated", "Well insulated"),
  # DRAFTY = c("Never", "Some of the time", "Most of the time", "All the time"),
  # SIZRFRI1 = c("No refrigerator", "Half-size or compact", "Small (17.5 cubic feet or less)", "Medium (17.6 to 22.5 cubic feet)", "Large (22.6 to 29.5 cubic feet)", "Very large (bigger than 29.5 cubic feet)"),
  # AGERFRI1 = c("No refrigerator", "Less than 2 years old", "2 to 4 years old", "5 to 9 years old", "10 to 14 years old", "15 to 19 years old", "20 or more years old"),
  # SIZRFRI2 =  c("No second refrigerator", "Half-size or compact", "Small (17.5 cubic feet or less)", "Medium (17.6 to 22.5 cubic feet)", "Large (22.6 to 29.5 cubic feet)", "Very large (bigger than 29.5 cubic feet)"),
  # AGERFRI2 = c("No second refrigerator", "Less than 2 years old", "2 to 4 years old", "5 to 9 years old", "10 to 14 years old", "15 to 19 years old", "20 or more years old"),
  # SIZFREEZ = c("No freezer", "Half-size or compact", "Small (17.5 cubic feet or less)", "Medium (17.6 to 22.5 cubic feet)", "Large (22.6 to 29.5 cubic feet)", "Very large (bigger than 29.5 cubic feet)"),
  # AGEFRZR =  c("No freezer", "Less than 2 years old", "2 to 4 years old", "5 to 9 years old", "10 to 14 years old", "15 to 19 years old", "20 or more years old"),
  # NUMMEAL = c("Never", "Less than once a week", "About once a week", "A few times each week", "Once a day", "Two times a day", "Three or more times a day"),
  # AGEDW = c("No dishwasher", "Less than 2 years old", "2 to 4 years old", "5 to 9 years old", "10 to 14 years old", "15 to 19 years old", "20 or more years old"),
  # AGECWASH = c("No clothes washer in home", "Less than 2 years old", "2 to 4 years old", "5 to 9 years old", "10 to 14 years old", "15 to 19 years old", "20 or more years old"),
  # AGECDRYER = c("No clothes dryer in home", "Less than 2 years old", "2 to 4 years old", "5 to 9 years old", "10 to 14 years old", "15 to 19 years old", "20 or more years old"),
  # TVSIZE1 = c("No TV", "27 inches or less", "28 to 39 inches", "40 to 59 inches", "60 inches or more"),
  # TVONWD1 = c("None, no TV", "Less than one hour", 0:24),
  # TVONWE1 = c("None, no TV", "Less than one hour", 0:24),
  # TVSIZE2 = c("No second TV", "27 inches or less", "28 to 39 inches", "40 to 59 inches", "60 inches or more"),
  # TVONWD2 = c("None, no second TV", "Less than one hour", 0:24),
  # TVONWE2 = c("None, no second TV","Less than one hour", 0:24),
  # TEMPHOME = c("No space heating", 50:90),
  # TEMPGONE = c("No space heating", 50:90),
  # TEMPNITE = c("No space heating", 50:90),
  # TEMPHOMEAC = c("No air conditioning", 50:90),
  # TEMPGONEAC = c("No air conditioning", 50:90),
  # TEMPNITEAC = c("No air conditioning", 50:90),
  # LGTINCAN = c("None", "Some", "About half", "Most", "All"),
  # LGTINLED = c("None", "Some", "About half", "Most", "All"),
  # EDUCATION = c("Less than high school diploma or GED", "High school diploma or GED","Some college or Associate’s degree", "Bachelor’s degree", "Master’s, Professional, or Doctoral degree"),
  # MONEYPY = c('Less than $5,000','$5,000 - $7,499','$7,500 - $9,999','$10,000 - $12,499','$12,500 - $14,999','$15,000 - $19,999','$20,000 - $24,999', '$25,000 - $29,999',
  #             '$30,000 - $34,999','$35,000 - $39,999','$40,000 - $49,999','$50,000 - $59,999','$60,000 - $74,999','$75,000 - $99,999','$100,000 - $149,999','$150,000 or more'),
  SCALEB = c("Never", "1 or 2 months", "Some months", "Almost every month"),
  SCALEG = c("Never", "1 or 2 months", "Some months", "Almost every month"),
  SCALEE = c("Never", "1 or 2 months", "Some months", "Almost every month")
)

for (v in names(custom.ordered)) {
  d[[v]] <- factor(d[[v]], levels = custom.ordered[[v]])
}

# Ensure that "Yes/No" variables have levels with same ordering (Yes, No)
for (v in names(d)) {
  x <- d[[v]]
  if (is.factor(x)) {
    if (all(levels(x) %in% c("Yes", "No"))) {
      d[[v]] <- factor(x, levels = c("Yes", "No"))
    }
  }
}

# Safety check
# Detect any variables in 'ordered.factors' that are NOT in the codebook
# extras <- setdiff(names(ordered.factors), codebook$var)
# stopifnot(length(extras) == 0)

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

for (x in c("TOTALDOL", "TOTALBTU", "BTUEL", "BTUNG", "BTULP", "BTUFO")) {
  y <- setdiff(grep(paste0("^", x), names(d), value = TRUE), x)  # The sub-component variables
  shares <- d[y] / rowSums(d[y]) # Modify sub-components to shares that sum to 1 (except if fuel not used)
  shares[is.na(shares)] <- 0  # If NA because fuel not used, set all shares to 0
  shares[shares < 0] <- 0  # Fix-up for erroneous shares introduced in the 'TOTALDOL...' variables
  shares[shares > 1] <- 1
  shares <- shares * ifelse(rowSums(shares) == 0, 1, 1 / rowSums(shares))
  stopifnot(all(signif(rowSums(shares), 4) %in% c(0, 1)))
  d[y] <- shares
  if (grepl("^TOTAL", x)) {
    names(d)[names(d) %in% y] <- gsub("^TOTAL", "SHR", y)
  } else {
    names(d)[names(d) %in% y] <- gsub("BTU", "SHR", y)
  }
}

#-----

# Impute missing values, if present

if (anyNA(d)) {

  na.count <- colSums(is.na(d))
  na.count <- na.count[na.count > 0]
  na.count <- na.count / nrow(d)  # Proportion of values that are missing
  print(round(na.count * 100, 2))

  # Use impute() to impute missing values in 'd'
  ignore <- names(select(d, DOEID, starts_with("NWEIGHT")))
  d <- fusionModel::impute(d, weight = "NWEIGHT", ignore = ignore)

}

#-----

# USE THIS????
# Add/create variables for geographic concordance with variables in 'geo_concordance.fst'

# d <- d %>%
#   rename(recs_division = DIVISION,
#          region = REGIONC,
#          recs_iecc_zone = IECC_CLIMATE_PUB,
#          recs_ba_zone = CLIMATE_REGION_PUB) %>%
#   mutate(cbsatype15 = "None",
#          cbsatype15 = ifelse(grepl("Metro", METROMICRO), "Metro", cbsatype15),
#          cbsatype15 = ifelse(grepl("Micro", METROMICRO), "Micro", cbsatype15),
#          ur12 = substring(UATYP10, 1, 1)) %>%
#   select(-METROMICRO, -UATYP10)
#
# # See which variables in 'd' are also in 'geo_concordance' and
# gnames <- names(fst::fst("geo-processed/concordance/geo_concordance.fst"))
# gvars <- intersect(gnames, names(d))
#
# # Class new/added geo identifiers as unordered factors
# d <- d %>%
#   mutate_at(gvars, ~ factor(.x, levels = sort(unique(.x)), ordered = FALSE))

#----------------

# Variable descriptions
var.desc <- codebook %>%
  select(var, desc) %>%
  distinct() %>%
  mutate(var = ifelse(grepl("^TOTAL", var) & grepl("Share of", desc), gsub("TOTAL", "SHR", var), var),
         var = ifelse(grepl("^Share of", desc), gsub("^BTU", "SHR", var), var)) %>%
  # Add custom variable descriptions
  add_row(var = "ur12", desc = "Respondent location (urban or rural)") %>%
  add_row(var = "cbsatype15", desc = "Respondent location (metro type)") %>%
  add_row(var = "year", desc = "Survey year") %>%
  add_row(var = "hid", desc = "Household ID constructed from original RECS DOEID") %>%
  add_row(var = "weight", desc = "Household central sampling weight") %>%
  add_row(var = "state", desc = "State FIPS code")

#----------------

bup <- d

# Assemble final output
# NOTE: var_label assignment is done AFTER any manipulation of values/classes, because labels can be lost when classes are changed
d <- d %>%
  rename(hid = DOEID,  # Rename ID and weight variables to standardized names
         weight = NWEIGHT) %>%
  mutate(year = 2015L,
         ur12 = factor(substring(UATYP10, 1, 1)),
         cbsatype15 = "None",
         cbsatype15 = ifelse(grepl("Metro", METROMICRO), "Metro", cbsatype15),
         cbsatype15 = ifelse(grepl("Micro", METROMICRO), "Micro", cbsatype15),
         cbsatype15 = factor(cbsatype15),
         REGIONC = factor(str_to_title(REGIONC))) %>%
  mutate(across(starts_with("DOLLAR"), ~ as.integer(round(.x)))) %>%  # Convert total dollar amounts to integer
  mutate_if(is.factor, safeCharacters) %>%
  mutate_if(is.numeric, convertInteger, threshold = 0.99) %>%
  mutate_if(is.double, cleanNumeric, tol = 0.001) %>%
  # # Force IECC codes to be either "7" or "8" for those zones; allows easier concordance with geographic data
  # mutate(IECC_CLIMATE_PUB = as.character(IECC_CLIMATE_PUB),
  #        IECC_CLIMATE_PUB = ifelse(substring(IECC_CLIMATE_PUB, 1, 1) %in% 7:8, substring(IECC_CLIMATE_PUB, 1, 1), IECC_CLIMATE_PUB),
  #        IECC_CLIMATE_PUB = factor(IECC_CLIMATE_PUB, levels = sort(unique(IECC_CLIMATE_PUB)), ordered = TRUE)) %>%
  # # Fix apparent error in BA Zone when IECCC is 4c
  # mutate(CLIMATE_REGION_PUB = as.character(CLIMATE_REGION_PUB),
  #        CLIMATE_REGION_PUB = ifelse(IECC_CLIMATE_PUB == "4C", "Marine", CLIMATE_REGION_PUB),
  #        CLIMATE_REGION_PUB = factor(CLIMATE_REGION_PUB)) %>%
  labelled::set_variable_labels(.labels = setNames(as.list(var.desc$desc), var.desc$var), .strict = FALSE) %>%  # Set descriptions for codebook variables
  rename(recs_division = DIVISION,
         region = REGIONC,
         recs15_iecc_zone = IECC_CLIMATE_PUB,
         recs15_ba_zone = CLIMATE_REGION_PUB) %>%
  rename_with(~ gsub("BRRWT", "REP_", .x, fixed = TRUE), .cols = starts_with("BRRWT")) %>%  # Rename replicate weight columns to standardized names
  rename_with(tolower)

# Identify which variables have definitions/labels and prepare to drop those that are not defined
vlabs <- labelled::var_label(d)
vkeep <- names(which(lengths(vlabs) > 0))
drop <- setdiff(names(d), vkeep)  # Variables to be dropped due to absence of variable description (TO DO: Print to console?)
drop

# Retain desired variables and order columns
d <- d %>%
  select(all_of(vkeep)) %>%
  select(year, hid, weight, everything(), -starts_with("rep_"), starts_with("rep_")) %>%   # Reorder columns with replicate weights at the end
  arrange(hid)

#----------------

# Create dictionary and save to disk
dictionary <- fusionData::createDictionary(data = d, survey = "RECS", vintage = 2015, respondent = "H")
saveRDS(object = dictionary, file = "survey-processed/RECS/2015/RECS_2015_H_dictionary.rds")

#----------------

# Save data to disk (.fst)
fst::write_fst(x = d, path = "survey-processed/RECS/2015/RECS_2015_H_processed.fst", compress = 100)
