library(tidyverse)
library(readr) 
library(dplyr)
library(lubridate) 
library(rpart)
library(xts)
source("R/utils.R")
source("R/createDictionary.R")
source("R/imputeMissing.R")
source("R/detectDependence.R")


setwd("/Users/karthikakkiraju/Documents/fusionData")

#-----
# Load raw NHTS 2017  data
d_p0 <- read_csv("survey-raw/NHTS/2017/perpub.csv")
d_t0 <- read_csv("survey-raw/NHTS/2017/trippub.csv") # Load raw NHTS 2017 household level data
d_v0 <- read_csv("survey-raw/NHTS/2017/vehpub.csv") # Load raw NHTS 2017 household level data


# Merge person level data and trip level data 
nm1 <- intersect(names(d_p0), names(d_t0))
nm2 <- c(setdiff(names(d_t0), nm1), "HOUSEID","PERSONID")

d_0 <- left_join(d_p0, select(d_t0, all_of(nm2)), by = c("HOUSEID","PERSONID"), all.x=TRUE) %>%
  select(HOUSEID,PERSONID,TDCASEID,VEHID,TDTRPNUM, everything())

# Merge person-trip level data and vehicle level data. Note that vehicle data in merged with the VEHID in the trip level data which is the ID used on the trip.  
nm1 <- intersect(names(d_0), names(d_v0))
nm2 <- c(setdiff(names(d_v0), nm1), "HOUSEID","VEHID","PERSONID")

d <- left_join(d_0, select(d_v0, all_of(nm2)), by = c("HOUSEID","PERSONID","VEHID"))

d <- d %>%
  mutate(
    
    OD_READ = ifelse(OD_READ == -7, -77, OD_READ), #Codebook only has -77,-88,-9 values specified. Could be typos.Need to check with NHTS staff
    OD_READ = ifelse(OD_READ == -8, -88, OD_READ), 
   
    WRKTIME = ifelse(grepl("AM", WRKTIME) | grepl("PM", WRKTIME), format(strptime(WRKTIME, "%I:%M %p"), "%H:%M"), WRKTIME),
    WRKTIME = gsub(":", ".", WRKTIME), # Replace : with . in WRKTIME for consistency with ENDTIME and STRTTIME time format
    
    ENDTIME = sub("(.{2})(.*)","\\1.\\2",ENDTIME),
    STRTTIME = sub("(.{2})(.*)","\\1.\\2",STRTTIME),
    
    YEARMILE = ifelse(YEARMILE == -7, -77, YEARMILE), #Codebook only has -77,-88,-9 values specified. Could be typos.
    YEARMILE = ifelse(YEARMILE == -8, -88, YEARMILE),
    
        ) %>% select(HOUSEID,PERSONID,VEHID,TDCASEID,everything())
  
# Merging the codebooks for person, travel, and vehicle level data 
# Load and process person level codebook
codebook_p <- readxl::read_excel("survey-raw/NHTS/2017/codebook_v1.2.xlsx", sheet ="CODEBOOK_PER") %>%
 
   setNames(c('var', 'desc', 'type', 'length', 'valuelabel', 'frequency','weighted'))%>%  #value and label are in the same cell. Need to separate them into differnt columns
   select(var, desc, valuelabel) %>%
 
  fill(var, desc) %>% # Replacing NA with the appropriate  entry
  
  mutate(
   valuelabel= gsub("Responses=","",valuelabel, fixed = TRUE),  #separating the valuelabel cell at the "=" sign 
    )%>%
     
       mutate(
    value = (str_extract(valuelabel, "[^=]+")),
    label = (str_extract(valuelabel, "[^=]+$")),
    valuelabel = (NULL),
     ) %>%

    unnest(cols = c(label)) %>%
   
  # Replacing "Not ascertaine", "I don't know", "Don't know" labels with NA  
  mutate(
    label = ifelse(grepl("Not ascertained", label) | grepl("I don't know", label) | grepl("Don't know", label) | grepl("I prefer not to answer", label), NA, label)   # Set label to NA if value is "I don't know", "Not ascertained",  (these observations are to be imputed eventually)
    ) %>%

  mutate_all(trimws)

 # Load and process trip codebook
  codebook_t <- readxl::read_excel("survey-raw/NHTS/2017/codebook_v1.2.xlsx", sheet ="CODEBOOK_TRIP") %>%
    
    setNames(c('var', 'desc', 'type', 'length', 'valuelabel', 'frequency','weighted'))%>%  #value and label are in the same cell. Need to separate them into differnt columns
    select(var, desc, valuelabel) %>%
    
    fill(var, desc) %>% # Replacing NA with the appropriate  entry
    
    mutate(
      valuelabel= gsub("Responses=","",valuelabel, fixed = TRUE),  #separating the valuelabel cell at the "=" sign 
    )%>%
    
    mutate(
      value = (str_extract(valuelabel, "[^=]+")),
      label = (str_extract(valuelabel, "[^=]+$")),
      valuelabel = (NULL),
    ) %>%
    
    unnest(cols = c(label)) %>%
    
    # Replacing "Not ascertained", "I don't know", "Don't know" labels with NA  
    mutate(
      label = ifelse(grepl("Not ascertained", label) | grepl("I don't know", label) | grepl("Don't know", label) | grepl("I prefer not to answer",label) | grepl("Refused", label), NA, label)   # Set label to NA if value is "I don't know", "Not ascertained",  (these observations are to be imputed eventually)
       
       ) %>%
    
    mutate_all(trimws)
  
  # Load and process vehicle codebook
  codebook_v <- readxl::read_excel("survey-raw/NHTS/2017/codebook_v1.2.xlsx", sheet ="CODEBOOK_VEH") %>%
    
    setNames(c('var', 'desc', 'type', 'length', 'valuelabel', 'frequency','weighted'))%>%  #value and label are in the same cell. Need to separate them into differnt columns
    select(var, desc, valuelabel) %>%
    
    fill(var, desc) %>% # Replacing NA with the appropriate  entry
    
    mutate(
      valuelabel= gsub("Responses=","",valuelabel, fixed = TRUE),  #separating the valuelabel cell at the "=" sign 
    )%>%
    
    mutate(
      value = (str_extract(valuelabel, "[^=]+")),
      label = (str_extract(valuelabel, "[^=]+$")),
      valuelabel = (NULL),
    ) %>%
    
    unnest(cols = c(label)) %>%
    
    # Replacing "Not ascertained", "I don't know", "Don't know" labels with NA  
    mutate(
      label = ifelse(grepl("Not ascertained", label) | grepl("I don't know", label) | grepl("Don't know", label) | grepl("I prefer not to answer",label) | grepl("Refused", label), NA, label)   # Set label to NA if value is "I don't know", "Not ascertained",  (these observations are to be imputed eventually)
    ) %>%
    mutate_all(trimws)

  # Merge person,trip,and vehicle level codebooks 
  codebook_0 <-   bind_rows(codebook_p,codebook_t) %>% distinct(var,value,label, .keep_all = T)
  codebook <-   bind_rows(codebook_0,codebook_v) %>% distinct(var,value,label, .keep_all = T)
  
  codebook <- codebook %>%
    mutate(
     label = ifelse(var == "PHYACT" & value == "01","rarely or never does any physical activity",label), 
     label = ifelse(var == "PHYACT" & value == "02","some light or moderate physical activities",label), 
     label = ifelse(var == "PHYACT" & value == "03","some vigorous physical activities",label) 
           )%>%
    mutate_all(trimws)
  
# Variables with "Appropriate skip" values
# These are the variables for which suitable replacement values must be specified below

as.vars <- codebook %>%
  filter(label == "Appropriate skip") %>%
  pull(var) %>%
  unique()

as.values <- list(
  
  ALT_16 = "No Public Tranportation or Taxi",
  ALT_23 = "Not a passenger to Friend/Family ",
  ALT_45 = "No bike or walk",
  BIKE4EX = "No biking for exercise",
  BIKESHARE = "No bike sharing usage",
  BIKE_DFR  = "No biking",
  BIKE_GKP = "No biking",
  CARRODE = "No people in vehicle to work",
  CARSHARE = "No car share program usage",
  CONDNIGH = "No medical conditions",
  CONDPUB = "No medical conditions",
  CONDRIDE = "No medical conditions",
  CONDRIVE = "No medical conditions",
  CONDSPEC = "No medical conditions",
  CONDTAX = "No medical conditions",
  CONDTRAV = "No medical conditions",
  DELIVER = "No online delivery",
  DRIVER = "No driver status",
  EDUC = "No education",
  FLEXTIME = "No work",
  GT1JBLWK = "No job",
  LPACT = 0,
  LSTTRDAY17 ="No trip",
  MEDCOND6 = "No medical condition",
  NOCONG = 0,
  OCCAT = "No job catejory",
  PAYPROF ="No work for pay",
  PRMACT = "No primary action",
  PUBTIME = 0,
  RIDESHARE = "No app usage",
  SAMEPLC = "No trips",
  SCHTRN1 = "No mode to school",
  SCHTRN2 = "No mode frpm school",
  SCHTYP = "No student status",
  TIMETOWK = 0,
  VPACT = 0,
  WALK4EX = 0,
  WALK_DEF = "Infrastrcture not a reason for not walking more",
  WALK_GKQ= "Safety not a reason for not walking more",
  WKFMHMXX ="No days worked from home",
  WKFTPT = "Not full time or part-time worker",
  WKRMHM = "Not working",
  WORKER = "No",
  WRKTIME = "No arrival time at work",
  WRKTRANS = "No mode to work",
  WRK_HOME = "No",
  W_CANE = "No",
  W_CHAIR = "No",
  W_CRUTCH = "No",
  W_DOG = "No",
  W_MTRCHR = "No",
  W_NONE = "No",
  W_SCOOTR = "No",
  W_WHCANE = "No",
  W_WLKR = "No", 
  YRTOUS = "No year of arrival in US",
  YEARMILE = 0,
  MCUSED = 0,
  DROP_PRK = "No drop off",
  DRVR_FLG = "No driver status",
  HHMEMDRV = "No",
  NUMTRANS = "No transfer",
  ONTD_P10 = "No household indentifier",
  ONTD_P11 = "No household indentifier",
  ONTD_P12 = "No household indentifier",
  ONTD_P13 = "No household indentifier",
  ONTD_P2 = "No household indentifier",
  ONTD_P3 = "No household indentifier",
  ONTD_P4 = "No household indentifier",
  ONTD_P5 = "No household indentifier",
  ONTD_P6 = "No household indentifier",
  ONTD_P7 = "No household indentifier",
  ONTD_P8 = "No household indentifier",
  ONTD_P9 = "No household indentifier",
  PSGR_FLG = "No passenger status",
  TRACCTM = 0,
  TRACC_BUS = "No mode to transit",
  TRACC_CRL = "No mode to transit",
  TRACC_OTH = "No mode to transit",
  TRACC_POV  = "No mode to transit",
  TRACC_SUB = "No mode to transit",
  TRACC_WLK = "No mode to transit",
  TREGRTM = 0,
  TREGR_BUS = "No mode to transit",
  TREGR_CRL = "No mode to transit",
  TREGR_OTH = "No mode to transit",
  TREGR_POV = "No mode to transit",
  TREGR_SUB = "No mode to transit",
  TREGR_WLK = "No mode to transit",
  TRWAITTM = "No mode to transit",
  VEHID = "No vehicle used",
  VEHTYPE = "No vehicle type",
  VMT_MILE = 0,
  WHODROVE = "No",
  WORKER = "No worker status",
  HFUEL = "No hybrid car",
  VEHOWNMO = "No vehicle owned"
)

  # These are cases where the variable measures a continuous/numeric concept,
  # but inserting a zero-value for "Not applicable" entries does not make sense.
  # In this case, a categorical value is assigned to override the default zero
  # and the variable should be treated as an ordered factor.
 
# Safety check for missing entries in 'as.values'
miss <- noquote(setdiff(as.vars, names(as.values)))

extras <- noquote(setdiff(names(as.values), as.vars))
stopifnot(length(miss) == 0 & length(extras) == 0)

#-----

# Update 'codebook' with the manually specified override values for "Appropriate skip"

# Drop variables set to NULL in 'ap.skip'
codebook <- filter(codebook, !var %in% names(which(map_lgl(as.values, is.null))))


# Update "Appropriate skip" values for each variable
for (v in as.vars) {
  if (!is.null(as.values[[v]])) {
    ind <- which(codebook$var == v & codebook$label == "Appropriate skip")  # Update the "Appropriate skip" label to specified label
    codebook$label[ind] <- as.values[[v]]
  }
}

# Safety check
# Ensure there are not "Appropriate skip entries remaining in the codebook
stopifnot(!any(codebook$label == "Appropriate skip", na.rm = TRUE))

#-----

# Assign levels for ordered factors
# In general, we want to coerce unordered factor to ordered factors whenever feasible
# There is some judgement involved

ordered.factors <- list(

  BIKE4EX = c("No biking for exercise",0:99),
  BIKESHARE = c("No bike sharing usage",0:99),
  CARRODE = c("No people in vehicle to work",0:20),
  CARSHARE = c("No car share program usage",0:90),
  DELIVER = c("No online delivery", 0:99),
  EDUC = c("No education","Less than a high school graduate", "High school graduate or GED","Some college or associates degree","Bachelor's degree","Graduate degree or professional degree"),
  GT1JBLWK = c("No job","No", "Yes"),
  HBHTNRNT = c("0-4%","5-14%", "15-24%", "25-34%", "35-44%", "45-54%", "55-64%","65-74%","75-84%","85-94%", "95-100%"),
  HBPPOPDN = c("0-99", "100-499","500-999","1,000-1,999","2,000-3,999","4,000-9,999","10,000-24,999","25,000-999,999"),
  HBRESDN = c("0-99","100-499","500-999","1,000-1,999","2,000-3,999","4,000-9,999","10,000-24,999","25,000-999,999"),
  HEALTH = c("Poor", "Fair", "Good","Very good","Excellent"),
  HHFAMINC = c("Less than $10,000","$10,000 to $14,999","$15,000 to $24,999", "$25,000 to $34,999", "$35,000 to $49,999", "$50,000 to $74,999", "$75,000 to $99,999","$100,000 to $124,999","$125,000 to $149,999", "$150,000 to $199,999", "$200,000 or more"),
  HTEEMPDN = c("0-49","50-99","100-249","250-499","500-999","1,000-1,999", "2,000-3,999","4,000-999,999"),
  HTHTNRNT = c("0-4%","5-14%", "15-24%","25-34%","35-44%","45-54%","55-64%","65-74%","75-84%","85-94%","95-100%"),
  HTPPOPDN = c("0-99", "100-499","500-999","1,000-1,999","2,000-3,999","4,000-9,999","10,000-24,999","25,000-999,999"),
  HTRESDN = c("0-99","100-499","500-999","1,000-1,999","2,000-3,999","4,000-9,999","10,000-24,999","25,000-999,999"),
  LIF_CYC = c("one adult, no children","2+ adults, no children","one adult, youngest child 0-5","2+ adults, youngest child 0-5","one adult, youngest child 6-15","2+ adults, youngest child 6-15","one adult, youngest child 16-21","2+ adults, youngest child 16-21","one adult, retired, no children","2+ adults, retired, no children"),
  LSTTRDAY17 = c("No trip","The day before","A few days before","A week before","More than a week before but within a month","More than a month before"),
  MEDCOND = c("Yes","No"),
  MEDCOND6 = c("No medical condition","6 months or less","More than 6 months","All [$YOUR_THEIR] life"),
  MSACAT = c("Not in MSA","MSA less than 1 million","MSA of 1 million or more, and not in 1","MSA of 1 million or more, with rail"),
  MSASIZE = c("Not in MSA or CMSA","In an MSA of Less than 250,000","In an MSA of 250,000 - 499,999","In an MSA of 500,000 - 999,999","In an MSA or CMSA of 1,000,000 - 2,999,999","In an MSA or CMSA of 3 million or more"),     
  PHYACT = c("rarely or never does any physical activity","some light or moderate physical activities","some vigorous physical activities"),
  RAIL = c("MSA does not have rail, or hh not in an MSA","MSA has rail"),
  RIDESHARE = c("No app usage",0:99),
  TRAVDAY = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"),
  TRWAITTM = c("No mode to transit",0:125),
  URBAN = c("Not in urban area","In an area surrounded by urban areas", "In an urban area","In an Urban cluster"),
  URBANSIZE = c("Not in an urbanized area","50,000 - 199,999","200,000 - 499,999","500,000 - 999,999","1 million or more without heavy rail","1 million or more with heavy rail"),
  WALK4EX = c("No walk trips for exercise",0:99),
  WKFMHMXX = c("No days worked from home",0:31),
  WKFTPT = c("Not full time or part-time worker","Part-time","Full-time"),
  WRKTIME = c("No arrival time at work", c(sprintf("%05.2f",seq(00.00,23.59,0.01)))),
  YRTOUS = c("No year of arrival in US", 1905:2017)
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

options(scipen = 999) #Removes scientific notation. Scientific notation creeps in during new label creation and creates NA values upon comparison with ordered.factors.

for (v in names(d)) {

  cb <- filter(codebook, var == v )
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
    y <- ordered.factors[[v]]
    x <- factor(x, levels = intersect(y, x), ordered = TRUE)
    stopifnot(sum(is.na(x)) == num.na)  # This is a final safety check to ensure no NA's introduced inadvertently
  }

   # Apply type.convert() to 'x'; leave ordered factors unchanged
  x <- if (is.ordered(x)) x else type.convert(x, as.is = FALSE)

   #Ensure unordered factor levels are sorted according to codebook order of levels
   #Originally, unordered factors were sorted alphabetically -- but there is often useful information in the codebook ordering
   #This retains a valid codebook ordering if one exists; otherwise sort levels alphabetically
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
# Which variables have missing values and how frequent are they?
na.count <- colSums(is.na(d))
na.count <- na.count[na.count > 0]
na.count  # See which variables have NA's

## Impute NA values in 'd'
#imp <- imputeMissing(data = d,
  #                  N = 1,
 #                  weight = "WTPERFIN",
   #                  x_exclude = c("HOUSEID", "PERSONID","WHOPROXY"))

# Replace NA's in 'd' with the imputed values
#d[names(imp)] <- imp
#rm(imp)
#gc()

#anyNA(d)

#-----

# Add/create variables for geographic concordance with variables in 'geo_concordance.fst'

d <- d %>%
    rename(division = CENSUS_D,
           region = CENSUS_R,
         state = HHSTATE,
         cbsa10 = HH_CBSA 
         )

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
    pid = PERSONID,
    nhts_2017_hid = HOUSEID,
    vid = VEHID,
    tid = TDCASEID,
    weight = WTPERFIN # Rename ID and weight variables to standardized names
  ) %>%
  
  rename_with(~ gsub("WTPERFIN", "REP_", .x, fixed = TRUE), .cols = starts_with("WTHHFIN")) %>%  # Rename replicate weight columns to standardized names
  rename_with(tolower) %>%  # Convert all variable names to lowercase
  select(nhts_2017_hid, pid, everything()) %>%   # Reorder columns with replicate weights at the end
  arrange(nhts_2017_hid)

#----------------

# Create dictionary and save to disk
dictionary <- createDictionary(data = h.final, survey = "NHTS", vintage = 2017, respondent ="P")
saveRDS(object = dictionary, file = "survey-processed/NHTS/2017/NHTS_2017_P_dictionary.rds")

#----------------
# Save data to disk (.fst)
fst::write_fst(x = h.final, path = "survey-processed/NHTS/2017/NHTS_2017_P_processed.fst", compress = 100)