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

#This is to process the person level data  
setwd("/Users/karthikakkiraju/Documents/fusionData")

#-----
# Load raw NHTS 2017  data
d_p00 <- read_csv("survey-raw/NHTS/2017/perpub.csv") # Load raw NHTS 2017 person level data
d_h0 <- read_csv("survey-raw/NHTS/2017/hhpub.csv") # Load raw NHTS 2017 household level data

#Remove variables common to person and household files. This is to avoid repetition of variables (For example:state)
nm <- c(setdiff(names(d_p00), names(d_h0)), "HOUSEID")
d <- d_p00  %>% select(all_of(nm))

d <- d %>%
  mutate(
  
    WRKTIME = ifelse(grepl("AM", WRKTIME) | grepl("PM", WRKTIME), format(strptime(WRKTIME, "%I:%M %p"), "%H:%M"), WRKTIME),
    WRKTIME = gsub(":", ".", WRKTIME), # Replace : with . in WRKTIME for consistency with ENDTIME and STRTTIME time format
    
    YEARMILE = ifelse(YEARMILE == -7, -77, YEARMILE), #Codebook only has -77,-88,-9 values specified. Could be typos.
    YEARMILE = ifelse(YEARMILE == -8, -88, YEARMILE),
    
        ) %>% select(HOUSEID,PERSONID,everything())
  
# Load and process person level codebook
codebook <- readxl::read_excel("survey-raw/NHTS/2017/codebook_v1.2.xlsx", sheet ="CODEBOOK_PER") %>%
 
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
    label = ifelse(grepl("Not ascertained", label) | grepl("I don't know", label) | grepl("Don't know", label) | grepl("I prefer not to answer", label), NA, label)   # Set label to NA if value is "I don't know", "Not ascertained",  (these observations are to be imputed eventually)
    ) %>%
  mutate_all(trimws) %>% filter(var %in% nm)
  
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
  WORKER = "No worker status"
)

  # These are cases where the variable measures a continuous/numeric concept,
  # but inserting a zero-value for "Not applicable" entries does not make sense.
  # In this case, a categorical value is assigned to override the default zero
  # and the variable should be treated as an ordered factor.
 
# Safety check for missing entries in 'as.values'
miss <- noquote(setdiff(as.vars, names(as.values)))

extras <- noquote(setdiff(names(as.values), as.vars))
stopifnot(length(miss) == 0 & length(extras) == 0)
extras
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
  HEALTH = c("Poor", "Fair", "Good","Very good","Excellent"),
  LSTTRDAY17 = c("No trip","The day before","A few days before","A week before","More than a week before but within a month","More than a month before"),
  MEDCOND = c("Yes","No"),
  MEDCOND6 = c("No medical condition","6 months or less","More than 6 months","All [$YOUR_THEIR] life"),
  PHYACT = c("rarely or never does any physical activity","some light or moderate physical activities","some vigorous physical activities"),
  RIDESHARE = c("No app usage",0:99),
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

extras
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


# Select variables that would be imputed
#y_in <- c("HOMEOWN","HHFAMINC","PC","SPHONE","CAR","HH_HISP","HH_RACE","WEBUSE17","PRICE","PTRANS","PLACE","WALKS2SAVE","BIKE2SAVE")
#y_ex <- setdiff(names(na.count),y_in)

## Impute NA values in 'd'
imp <- imputeMissing(data = d,
                    N = 1,
                   weight = "WTPERFIN",
                    y_exclude = c("WRKTRANS","CARRODE","YEARMILE","GCDWORK","WKSTFIPS","DISTTOWK17","DISTTOSC17 ","BIKE_DFR","BIKE_GKP"),
                    x_exclude = c("HOUSEID", "PERSONID","WHOPROXY"))

# Replace NA's in 'd' with the imputed values
d[names(imp)] <- imp
rm(imp)
#gc()

#anyNA(d)

#-----

# Add/create variables for geographic concordance with variables in 'geo_concordance.fst'

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
  #labelled::set_variable_labels(.labels = setNames(as.list(paste(gvars, "geographic concordance")), gvars)) %>%  # Set descriptions for geo identifiers
  
  rename(
    pid = PERSONID,
    nhts_2017_hid = HOUSEID,
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
compileDictionary()
