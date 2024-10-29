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
codebook <- readxl::read_excel("survey-raw/RECS/2020/RECS 2020 Codebook for Public File - v7.xlsx", skip = 1) %>%
  setNames(c('var', 'type', 'desc', 'label', 'section')) %>%
  filter(
    !var %in% c('state_postal', 'state_name', 'DBT1', 'DBT99', 'GWT'),  # Unnecessary variables
    !is.na(desc), # Drops 'Note' entry at end
    !grepl("Number of days covered", desc), # Remove Energy Supplier Survey metadata variables
    !grepl("conversion factor", tolower(desc)), # Remove conversion factor variables
    !grepl("imputation", tolower(desc)), # Remove imputation flag variables
    !grepl(", not imputed", desc), # Remove EV charging variables that were not imputed and difficult to impute later
    !(grepl("a derived variable", tolower(desc)) & !var %in% c('TOTROOMS', 'TOTSQFT_EN')), # Remove unnecessary derived variables
    !(grepl("square footage", tolower(desc)) & !var %in% c('TOTSQFT_EN')), # Remove unnecessary square footage variables
    #!grepl("^Total cost for", desc), # Remove total cost for end-use components (retains physical consumption versions)
    !grepl("^Calibrated.*cost", desc), # Remove cost variables for end-use components (retains physical consumption versions)
    # Retain only the calibrated BTU end-use variables for space heating/cooling, water heating, refrigerator, and other (if available)
    !(grepl("^Calibrated", desc) & !var %in% c('BTUELSPH', 'BTUELCOL', 'BTUELWTH', 'BTUELRFG', 'BTUELOTH',
                                               'BTUNGSPH', 'BTUNGWTH', 'BTUNGOTH',
                                               'BTULPSPH', 'BTULPWTH', 'BTULPOTH',
                                               'BTUFOSPH', 'BTUFOWTH', 'BTUFOOTH')),
    !(grepl("^Total.*2020", desc) & !(grepl("^TOTAL", var) | grepl("^BTU", var) | grepl("^DOLLAR", var)))
  ) %>%
  mutate(label = str_split(label, fixed("\r\n"))) %>%
  unnest(cols = label, keep_empty = TRUE) %>%
  mutate_all(str_squish) %>%
  mutate(desc = gsub("- public file variable", "", desc),
         desc = map_chr(str_split(desc, fixed(";"), n = 2), 1),
         desc = gsub("Total usage for", "Share of total usage for", desc),
         desc = gsub("Total cost for", "Share of total cost for", desc),
         desc = gsub("Calibrated", "Share of", desc),
         desc = ifelse(grepl("Share of", desc), gsub(", main and secondary", "", desc), desc),
         desc = ifelse(grepl("Share of", desc), gsub(", in thousand Btu, 2020", "", desc), desc),
         desc = ifelse(grepl("Share of", desc), gsub(", in dollars, 2020", "", desc), desc),
         desc = ifelse(var == "TOTSQFT_EN", "Total energy-consuming area (square footage), including all main living areas; all basements; heated, cooled, or finished attics; and heated or cooled garages", desc))  # Manually description fix-up

# Load raw RECS 2020 data; only retain variables remaining in the codebook
d <- read_csv("survey-raw/RECS/2020/recs2020_public_v7.csv") %>%
  select(all_of(codebook$var))

#-----

# Ordered factors
ordered.factors <- c(
  "SIZEOFGARAGE", "STORIES", "YEARMADERANGE", "WINDOWS", "TYPEGLASS", "ADQINSUL", "DRAFTY",
  "SIZRFRI1", "AGERFRI1", "SIZRFRI2", "AGERFRI2", "SIZFREEZ", "AGEFRZR", "NUMMEAL",
  "AGEDW", "AGECWASH", "AGECDRYER", "TVSIZE1", "TVONWD1", "TVONWE1", "TVSIZE2",
  "TVONWD2", "TVONWE2", "TEMPHOME", "TEMPGONE", "TEMPNITE", "TEMPHOMEAC", "TEMPGONEAC",
  "TEMPNITEAC", "LGTINCAN", "LGTINLED", "EDUCATION", "MONEYPY", "SCALEB", "SCALEG", "SCALEE"
)

for (v in names(d)) {
  cb <- filter(codebook, var == v)
  x <- d[[v]]
  ok <- all(tolower(x) %in% tolower(cb$label))
  if (!ok) {
    if (nrow(cb) > 1 | !any(grepl("-", cb$label))) {
      l <- str_split(cb$label, fixed(" "), n = 2)
      if (all(lengths(l) == 2)) {
        y <- str_squish(map_chr(l, 1))
        z <- str_squish(map_chr(l, 2))
        z[y == "\".\""] <- NA  # Value for '"." or blank Missing' in codebook
        if (all(x %in% c(y, NA))) {
          x <- z[match(x, y)]
          if (is.character(x)) x <- factor(x, levels = intersect(z, x))
          if ("Not applicable" %in% z) x <- relevel(x, "Not applicable")
          if (v %in% ordered.factors) x <- as.ordered(x)
        } else {
          x[x == y[z == "Not applicable"]] <- "Not applicable"
        }
      }
    }
  } else {
    if (is.double(x)) x <- convertInteger(x, threshold = 1)
  }
  d[[v]] <- x
}

#---

# mutate(label = ifelse(grepl("\\d+\\s*-\\s*\\d+",value) & !grepl("Not applicable",value) ,desc,NA),
#        label = ifelse(var == 'STORIES'|var == 'SIZEOFGARAGE'|var == 'TYPEGLASS'|var == 'NGPAY'| var == 'TYPEHUQ'|var == 'EMPLOYHH'|
#                         var == 'SIZRFRI1'|var == 'AGEFRI1'|var == 'SIZRFRI2'|var == 'SIZFREEZ'|var == 'TVONWD1'|var == 'TVONWD2'| var == 'MONEYPY',NA,label))
#
# #Split codebook into 2
# codebook_a1 <- codebook %>% filter (type =='Num' | var == 'UATYP10') %>% filter(is.na(label)) %>%
#   separate(value, c("value", "label"), sep="^\\s*\\S+\\K\\s+")
#
# codebook_a2 <- codebook %>% filter (type =='Num' | var == 'UATYPE10') %>% filter(!is.na(label))
#
# codebook_b <- codebook %>% filter (type !='Num' & var != 'UATYP10') %>% mutate(label = value)
#
# codebook1 <- rbind(codebook_a1,codebook_a2,codebook_b)
# stopifnot(nrow(codebook1) == nrow(codebook))
# rm(codebook_a1,codebook_a2,codebook_b)
#
# codebook <- codebook1 %>%
#   mutate(
#     label = ifelse(grepl("^Don.t know$", label) | grepl("Refused", label), NA, label)  # Set label to NA if value is "Don't know" or "Refused" (these observations are to be imputed eventually)
#   ) %>%
#   mutate_all(trimws)
#
# new_rows <- list(
#   data.frame(var = 'MEDICALDEV', desc = "Any medical devices used at home", label = ("Not applicable"), value = NA, type = 'Num'),
#   data.frame(var = 'EVCHRGHOME', desc = "Any medical devices used at home", label = ("Not applicable"), value = NA, type = 'Num'))
#
# codebook <- bind_rows(codebook,new_rows)

#-----

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
  TEMPHOME = "No space heating",
  TEMPGONE = "No space heating",
  TEMPNITE = "No space heating",
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
  NOHEATDAYS = 0,
  NOACDAYS = 0,

  # These are cases where the variable measures a continuous/numeric concept,
  # but inserting a zero-value for "Not applicable" entries does not make sense.
  # In this case, a categorical value is assigned to override the default zero
  # and the variable should be treated as an ordered factor.
  TEMPHOME = "No space heating",
  TEMPGONE = "No space heating",
  TEMPNITE = "No space heating"
)

# Identify variables in 'nap.vars' not specified in 'nap.values
miss <- setdiff(nap.vars, names(nap.values))
miss

#-----

# Update 'codebook' with the manually specified override values for "Not applicable"

# Drop variables set to NULL in 'na.values'
#codebook <- filter(codebook, !var %in% names(which(map_lgl(na.values, is.null))))

# Update "Not applicable" values for each variable
# for (v in na.vars) {
#   if (!is.null(na.values[[v]])) {
#     ind <- which(codebook$var == v & codebook$label == "Not applicable")  # Update the "Not applicable" label to specified label
#     codebook$label[ind] <- na.values[[v]]
#   }
# }

for (v in nap.vars) {
  x <- d[[v]]
  if (is.factor(x)) {
    levels(x)[levels(x) == "Not applicable"] <- nap.values[[v]]
  } else {
    x[x == "Not applicable"] <- nap.values[[v]]
    x <- type.convert(x, as.is = FALSE)
  }
  d[[v]] <- x
}

# Coerce any remaining character variables to unordered factors
d <- d %>% mutate_if(is.character, as.factor)

#---

# Could conceivably remove variables that are derivative of others...
# source("R/detectDependence.R")
# drop <- unlist(detectDependence(d))
# d[drop] <- NULL

#---

# Safety check
# Ensure there are not "Not applicable" entries remaining in the codebook
#stopifnot(!any(codebook$label == "Not applicable", na.rm = TRUE))

#-----

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
# extras <- noquote(setdiff(names(ordered.factors), codebook$var))
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

# Update variable values with associated labels from 'codebook'
# Loop through each variable in 'd', assigning labels when applicable
# for (v in names(d)) {
#
#   cb <- filter(codebook, var == v)
#   x <- d[[v]]
#   y <- unlist(cb$value)
#   z <- unlist(cb$label)
#   m <- match(x, y)
#   new.labels <- z[na.omit(m)]
#
#   # Update 'x' with new value labels
#   x[!is.na(m)] <- new.labels
#
#   # Coerce result to ordered factor, if specified
#   # Note that levels are restricted to those actually present in the data
#   if (v %in% names(ordered.factors)) {
#     num.na <- sum(is.na(x))
#     x <- factor(x, levels = intersect(ordered.factors[[v]], x), ordered = TRUE)
#     stopifnot(sum(is.na(x)) == num.na)  # This is a final safety check to ensure no NA's introduced inadvertently
#   }
#
#   # Apply type.convert() to 'x'; leave ordered factors unchanged
#   x <- if (is.ordered(x)) x else type.convert(x, as.is = FALSE)
#
#   # Ensure unordered factor levels are sorted according to codebook order of levels
#   # Originally, unordered factors were sorted alphabetically -- but there is often useful information in the codebook ordering
#   # This retains a valid codebook ordering if one exists; otherwise sort levels alphabetically
#   if (is.factor(x) & !is.ordered(x)) {
#     num.na <- sum(is.na(x))
#     if (all(x %in% cb$label)) {
#       x <- factor(x, levels = intersect(cb$label, unique(x)))
#     } else {
#       x <- factor(x, levels = sort(unique(x)))
#     }
#     stopifnot(sum(is.na(x)) == num.na)  # This is a final safety check to ensure no NA's introduced inadvertently
#   }
#
#   # Update column in 'd'
#   d[[v]] <- x
#
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

# Add/create variables for geographic concordance with variables in 'geo_concordance.fst'
# d <- d %>%
#   rename(recs_division = DIVISION,
#          region = REGIONC,
#          recs20_iecc_zone = IECC_climate_code,
#          recs20_ba_zone = BA_climate) %>%


# mutate(recs20_ba_zone = as.character(recs20_ba_zone),
#        recs20_ba_zone = ifelse(recs20_iecc_zone == "4C","Marine", recs20_ba_zone),
#        recs20_ba_zone = as.factor(recs20_ba_zone)) #Need to check with staff

# See which variables in 'd' are also in 'geo_concordance' and
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
  #mutate(var = ifelse(grepl("^Share of", desc), gsub("^BTU", "SHR", var), var)) %>%
  # Add custom variable descriptions
  add_row(var = "ur12", desc = "Respondent location (urban or rural)") %>%
  add_row(var = "year", desc = "Survey year") %>%
  add_row(var = "hid", desc = "Household ID constructed from original RECS DOEID") %>%
  add_row(var = "weight", desc = "Household central sampling weight") %>%
  add_row(var = "state", desc = "State FIPS code")

#----------------

# Assemble final output
# NOTE: var_label assignment is done AFTER any manipulation of values/classes, because labels can be lost when classes are changed
d <- d %>%
  rename(hid = DOEID,  # Rename ID and weight variables to standardized names
         weight = NWEIGHT,
         state = STATE_FIPS) %>%
  mutate(year = 2020L,
         ur12 = factor(substring(UATYP10, 1, 1)),
         REGIONC = factor(str_to_title(REGIONC))) %>%
  mutate(across(starts_with("DOLLAR"), ~ as.integer(round(.x)))) %>%  # Convert total dollar amounts to integer
  mutate_if(is.factor, safeCharacters) %>%
  mutate_if(is.numeric, convertInteger, threshold = 0.99) %>%
  mutate_if(is.double, cleanNumeric, tol = 0.001) %>%
  # Force IECC codes to be either "7" or "8" for those zones; allows easier concordance with geographic data
  mutate(IECC_climate_code = as.character(IECC_climate_code),
         IECC_climate_code = ifelse(substring(IECC_climate_code, 1, 1) %in% 7:8, substring(IECC_climate_code, 1, 1), IECC_climate_code),
         IECC_climate_code = factor(IECC_climate_code, levels = sort(unique(IECC_climate_code)), ordered = TRUE)) %>%
  # Fix apparent error in BA Zone when IECCC is 4c
  mutate(BA_climate = as.character(BA_climate),
         BA_climate = ifelse(IECC_climate_code == "4C", "Marine", BA_climate),
         BA_climate = factor(BA_climate)) %>%
  labelled::set_variable_labels(.labels = setNames(as.list(var.desc$desc), var.desc$var), .strict = FALSE) %>%  # Set descriptions for codebook variables
  rename(recs_division = DIVISION,
         region = REGIONC,
         recs20_iecc_zone = IECC_climate_code,
         recs20_ba_zone = BA_climate) %>%
  rename_with(~ gsub("NWEIGHT", "REP_", .x, fixed = TRUE), .cols = starts_with("NWEIGHT")) %>%  # Rename replicate weight columns to standardized names
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
dictionary <- fusionData::createDictionary(data = d, survey = "RECS", vintage = 2020, respondent = "H")
saveRDS(object = dictionary, file = "survey-processed/RECS/2020/RECS_2020_H_dictionary.rds")

#----------------

# Save data to disk (.fst)
fst::write_fst(x = d, path = "survey-processed/RECS/2020/RECS_2020_H_processed.fst", compress = 100)
