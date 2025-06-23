library(tidyverse)
library(readr) 
library(dplyr)
library(lubridate) 
library(rpart)
library(xts)
source("R/utils.R")

#This file is to process vehicle level data

# Load raw NHTS 2017 vehicle level data
d <- read_csv("survey-raw/NHTS/2017/vehpub.csv")

d <- d %>%
  mutate(
    OD_READ = ifelse(OD_READ == -7, -77, OD_READ), #Codebook only has -77,-88,-9 values specified. Could be typos.
    OD_READ = ifelse(OD_READ == -8, -88, OD_READ), 
     )

#----

# Load and process vehcile level codebook
codebook <- readxl::read_excel("survey-raw/NHTS/2017/codebook_v1.2.xlsx", sheet ="CODEBOOK_VEH") %>%
 
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

  mutate(
    label =  ifelse((var == 'OD_READ'|var == 'ANNMILES'|var == 'MODEL') & (is.na(label)),"Appropriate skip",label)) %>% 
  
  
  mutate_all(trimws)

  
# Variables with "Appropriate skip" values
# These are the variables for which suitable replacement values must be specified below

as.vars <- codebook %>%
  filter(label == "Appropriate skip") %>%
  pull(var) %>%
  unique()

as.values <- list(
  HFUEL = "No hybrid car",
  VEHOWNMO = 0,
  OD_READ = "No reading taken",
  ANNMILES = 0,
  MODEL = "No model"
  
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

  HBHTNRNT = c("0-4%","5-14%","15-24%","25-34%","35-44%","45-54%","55-64%","65-74%","75-84%","85-94%","95-100%"),
  HBHUR = c("Second City","Rural","Suburban","Small Town","Urban"),
  HBPPOPDN =c("0-99","100-499","500-999","1,000-1,999","2,000-3,999","4,000-9,999","10,000-24,999","25,000-999,999"),
  HBRESDN =c("0-99","100-499","500-999","1,000-1,999","2,000-3,999","4,000-9,999","10,000-24,999","25,000-999,999"),
  HHFAMINC = c("Less than $10,000","$10,000 to $14,999","$15,000 to $24,999","$25,000 to $34,999","$35,000 to $49,999","$50,000 to $74,999","$75,000 to $99,999","$100,000 to $124,999","$125,000 to $149,999","$150,000 to $199,999","$200,000 or more"),
  HTEEMPDN = c("0-49","50-99","100-249","250-499","500-999","1,000-1,999","2,000-3,999","4,000-999,999"),
  HTHTNRNT = c("0-4%","5-14%","15-24%","25-34%","35-44%","45-54%","55-64%","65-74%","75-84%","85-94%","95-100%"),
  HTPPOPDN = c("0-99","100-499","500-999","1,000-1,999","2,000-3,999","4,000-9,999","10,000-24,999","25,000-999,999"),
  HTRESDN = c("0-99","100-499","500-999","1,000-1,999","2,000-3,999","4,000-9,999","10,000-24,999","25,000-999,999"),
  LIF_CYC = c("one adult, no children","2+ adults, no children","one adult, youngest child 0-5","2+ adults, youngest child 0-5","one adult, youngest child 6-15","2+ adults, youngest child 6-15","one adult, youngest child 16-21","2+ adults, youngest child 16-21","one adult, retired, no children","2+ adults, retired, no children"),
  MSACAT = c("Not in MSA","MSA less than 1 million","MSA of 1 million or more, and not in 1","MSA of 1 million or more, with rail"),
  MSASIZE = c("Not in MSA or CMSA","In an MSA of Less than 250,000","In an MSA of 250,000 - 499,999","In an MSA of 500,000 - 999,999","In an MSA or CMSA of 1,000,000 - 2,999,999","In an MSA or CMSA of 3 million or more","Not in MSA or CMSA"),
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

#Setting BESTMILE, GSCOST,GSYRGAL to 0 if vehicle was owned for less than a month

#names <- c('BESTMILE','GSCOST','GSYRGAL','GSTOTCST')
#for(v in names) 
#  (is.na(d[[v]]))
#  {d[[v]]= ifelse(d$VEHOWNMO == 0,0,d[[v]])}

#-----

# Which variables have missing values and how frequent are they?
na.count <- colSums(is.na(d))
na.count <- na.count[na.count > 0]
na.count  # See which variables have NA's

# Impute NA values in 'd' and replace NA's in 'd' with the imputed values
d <- fusionModel::impute(as.data.table(d),
                         weight = "WTHHFIN",
                         ignore = c("HOUSEID", "PERSONID"))

#-----
na.count <- colSums(is.na(d))
na.count <- na.count[na.count > 0]
na.count  # See which variables have NA's

# Add/create variables for geographic concordance with variables in 'geo_concordance.fst'

d <- d %>%
    rename(division = CENSUS_D,
         state = HHSTATE,
         cbsa13 = HH_CBSA)


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
    hid = HOUSEID,
    weight = WTHHFIN    # Rename ID and weight variables to standardized names
  ) %>%
  
  rename_with(~ gsub("WTTRDFIN", "REP_", .x, fixed = TRUE), .cols = starts_with("WTTRDFIN")) %>%  # Rename replicate weight columns to standardized names
  rename_with(tolower) %>%  # Convert all variable names to lowercase
  select(hid, everything(), -starts_with("rep_"), starts_with("rep_")) %>%   # Reorder columns with replicate weights at the end
  arrange(hid)
#----------------

# Save data to disk (.fst)
fst::write_fst(x = h.final, path = "survey-processed/NHTS/2017/NHTS_2017_V_processed.fst", compress = 100)
fst::write_fst(x = codebook, path = "survey-processed/NHTS/2017/NHTS_2017_V_codebook.fst", compress = 100)