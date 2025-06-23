library(tidyverse)
library(readr) 
library(dplyr)
library(lubridate) 
library(rpart)
library(xts)
library(data.table)
source("R/utils.R")

setwd("/Users/karthikakkiraju/Documents/FusionData/")

#This file is to process trip-level data
#This will be later summarized at the household level and merged with the household file

# Load raw NHTS 2017 trip level data
d_t0 <- read.csv("survey-raw/NHTS/2017/trippub.csv") 
#----
d_h0 <- read_csv("survey-raw/NHTS/2017/hhpub.csv") # Load raw NHTS 2017 household level data

#Remove variables common to person and household files. This is to avoid repetition of variables (For example:state)
nm1 <- c(setdiff(names(d_t0), names(d_h0)), "HOUSEID")
d_t00 <- d_t0  %>% select(all_of(nm1))

#----
d_p0 <- read_csv("survey-raw/NHTS/2017/perpub.csv") # Load raw NHTS 2017 household level data

#Remove variables common to person and household files. This is to avoid repetition of variables (For example:state)
nm2 <- c(setdiff(names(d_t00), names(d_p0)), "HOUSEID","PERSONID")
d <- d_t00  %>% select(all_of(nm2))

# Load and process codebook
codebook <- readxl::read_excel("survey-raw/NHTS/2017/codebook_v1.2.xlsx", sheet ="CODEBOOK_TRIP") %>%
 
   setNames(c('var', 'desc', 'type', 'length', 'valuelabel', 'frequency','weighted'))%>%  #value and label are in the same cell. Need to separate them into different columns
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
    label = ifelse(grepl("Not ascertained", label) | grepl("I don't know", label) | grepl("Don't know", label) | grepl("I prefer not to answer",label) | grepl("Refused", label), NA, label),
                   label = ifelse((var == 'DWELTIME' & value == '-9'),0, label)) %>%
  #Remove leading zeros to ensure compatability with 'd'
  mutate(value = gsub("^0([1-9])$", "\\1", value)) %>%
                mutate_all(trimws)

  
# Variables with "Appropriate skip" values
# These are the variables for which suitable replacement values must be specified below

as.vars <- codebook %>%
  filter(label == "Appropriate skip") %>%
  pull(var) %>%
  unique()

as.values <- list(
  DRIVER = "No driver status",
  DROP_PRK = "No drop off",
  DRVR_FLG = "No driver status",
  EDUC = "No education",
  HHMEMDRV = "No household member on drive",
  NUMTRANS = 0,
  ONTD_P10 = "No",
  ONTD_P11 = "No",
  ONTD_P12 = "No",
  ONTD_P13 = "No",
  ONTD_P2 = "No",
  ONTD_P3 = "No",
  ONTD_P4 = "No",
  ONTD_P5 = "No",
  ONTD_P6 = "No",
  ONTD_P7 = "No",
  ONTD_P8 = "No",
  ONTD_P9 = "No",
  PRMACT = "No primary activity",
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
  TRWAITTM = 0,
  VEHID = "No vehicle ID",
  VEHTYPE = "No vehicle type",
  VMT_MILE = 0,
  WHODROVE = "No",
  WORKER = "No worker status")

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

  DBHTNRNT = c("0-4%","5-14%","15-24%","25-34%","35-44%","45-54%","55-64%","65-74%","75-84%","85-94%","95-100%"),
  DBHUR = c("Second City","Rural","Suburban","Small Town","Urban"),
  DBPPOPDN = c("0-99","100-499","500-999","1,000-1,999","2,000-3,999","4,000-9,999","10,000-24,999","25,000-999,999"),
  DBRESDN = c("0-99","100-499","500-999","1,000-1,999","2,000-3,999","4,000-9,999","10,000-24,999","25,000-999,999"),
  DTEEMPDN = c("0-49","50-99","100-249","250-499","500-999","1,000-1,999","2,000-3,999","4,000-999,999"),
  DTHTNRNT  = c("0-4%","5-14%","15-24%","25-34%","35-44%","45-54%","55-64%","65-74%","75-84%","85-94%","95-100%"),
  DTPPOPDN = c("0-99","100-499","500-999","1,000-1,999","2,000-3,999","4,000-9,999","10,000-24,999","25,000-999,999"),
  DTRESDN = c("0-99","100-499","500-999","1,000-1,999","2,000-3,999","4,000-9,999","10,000-24,999","25,000-999,999"),
  EDUC = c("No education","Less than a high school graduate","High school graduate or GED","Some college or associates degree","Bachelor's degree","Graduate degree or professional degree"),
  HHFAMINC = c("Less than $10,000","$10,000 to $14,999","$15,000 to $24,999","$25,000 to $34,999","$35,000 to $49,999","$50,000 to $74,999","$75,000 to $99,999","$100,000 to $124,999","$125,000 to $149,999","$150,000 to $199,999","$200,000 or more"),
  LIF_CYC = c("one adult, no children","2+ adults, no children","one adult, youngest child 0-5","2+ adults, youngest child 0-5","one adult, youngest child 6-15","2+ adults, youngest child 6-15","one adult, youngest child 16-21","2+ adults, youngest child 16-21","one adult, retired, no children","2+ adults, retired, no children"),
  MSACAT = c("Not in MSA","MSA less than 1 million","MSA of 1 million or more, and not in 1","MSA of 1 million or more, with rail"),
  MSASIZE = c("Not in MSA or CMSA","In an MSA of Less than 250,000","In an MSA of 250,000 - 499,999","In an MSA of 500,000 - 999,999","In an MSA or CMSA of 1,000,000 - 2,999,999","In an MSA or CMSA of 3 million or more","Not in MSA or CMSA"),
  OBHTNRNT= c("0-4%","5-14%","15-24%","25-34%","35-44%","45-54%","55-64%","65-74%","75-84%","85-94%","95-100%"),
  OBHUR = c("Second City","Rural","Suburban","Small Town","Urban"), 
  OBPPOPDN = c("0-99","100-499","500-999","1,000-1,999","2,000-3,999","4,000-9,999","10,000-24,999","25,000-999,999"),
  OBRESDN = c("0-99","100-499","500-999","1,000-1,999","2,000-3,999","4,000-9,999","10,000-24,999","25,000-999,999"),
  OTEEMPDN = c("0-49","50-99","100-249","250-499","500-999","1,000-1,999","2,000-3,999","4,000-999,999"),
#  OTHTNRNT = c("0-4%","5-14%","15-24%","25-34%","35-44%","45-54%","55-64%","65-74%","75-84%","85-94%","95-100%"),
  OTPPOPDN = c("0-99","100-499","500-999","1,000-1,999","2,000-3,999","4,000-9,999","10,000-24,999","25,000-999,999"),
  OTRESDN = c("0-99","100-499","500-999","1,000-1,999","2,000-3,999","4,000-9,999","10,000-24,999","25,000-999,999"),
  RAIL =c("MSA does not have rail, or hh not in an MSA","MSA has rail"),
  URBAN = c("Not in urban area","In an area surrounded by urban areas", "In an urban area","In an Urban cluster"),
  URBANSIZE = c("Not in an urbanized area","50,000 - 199,999","200,000 - 499,999","500,000 - 999,999","1 million or more without heavy rail","1 million or more with heavy rail")
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
    x1 <- factor(x, levels = intersect(y, x), ordered = TRUE)
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

# Detect structural dependencies
# Which variables have missing values and how frequent are they?
#d1 <- d %>% select( -'TRACC_CRL' ,-starts_with('ONTD'))
#d1b <- d %>% select(setdiff(colnames(d),colnames(d1)))

na.count <- colSums(is.na(d))
na.count <- na.count[na.count > 0]
na.count  # See which variables have NA's

## Impute NA values in 'd'
d <- fusionModel::impute(data = as.data.table(d), 
                         weight = "WTTRDFIN",
                         ignore = c("HOUSEID", "PERSONID",'TDCASEID'),
                         cores = 2)

na.count <- colSums(is.na(d))
na.count <- na.count[na.count > 0]
na.count  # See which variables have NA's
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

  rename(
    hid = HOUSEID,
    pid = PERSONID,
    weight = WTTRDFIN    # Rename ID and weight variables to standardized names
  ) %>%
  
  rename_with(~ gsub("WTTRDFIN", "REP_", .x, fixed = TRUE), .cols = starts_with("WTTRDFIN")) %>%  # Rename replicate weight columns to standardized names
  rename_with(tolower) %>%  # Convert all variable names to lowercase
  select(hid, pid, everything(), starts_with("rep_")) %>%   # Reorder columns with replicate weights at the end
  arrange(hid)

#----------------

# Save data to disk (.fst)
fst::write_fst(x = h.final, path = "survey-processed/NHTS/2017/NHTS_2017_T_processed.fst", compress = 100)
