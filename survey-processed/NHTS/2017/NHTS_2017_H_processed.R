library(tidyverse)
library(readr) 
library(dplyr)
library(lubridate) 
library(rpart)
library(xts)
library(shiny)
library(fusionModel)
library(fusionData)
source("R/utils.R")
source("R/createDictionary.R")
source("R/compileDictionary.R")
source("R/imputeMissing.R")
source("R/detectDependence.R")
source("R/universe.R")
source("R/fusionData-package.R")

setwd("/Users/karthikakkiraju/Documents/fusionData")

#Read raw data 
d_0 <- read_csv("survey-raw/NHTS/2017/hhpub.csv") # Load raw NHTS 2017 household level data

# Load house weight file and attach to hpub at the end. This file needs to be downloaded separately from the website. 
wt <- read_csv("survey-raw/NHTS/2017/hhwgt.csv")
wt <- within(wt, rm("WTHHFIN"))
d <- merge(d_0,wt,by="HOUSEID")

# Load and process household codebook
codebook <- readxl::read_excel("survey-raw/NHTS/2017/codebook_v1.2.xlsx", sheet ="CODEBOOK_HH") %>%
  
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
    label = ifelse(var == 'HHSTFIPS', value,label),
    label = ifelse(var == 'HH_CBSA', value,label) ,
    label = ifelse(value == 'XXXXX',NA,label)
  ) %>%
  mutate_all(trimws)

# Add replicate household weight variables to codebook

  rep_var<-colnames(wt)
  rep_var <- rep_var[-c(1)] 

  rep_desc <- str_extract(rep_var,"\\d+$") 
  rep_desc <- paste0('Replicate Weight', rep_desc)
  

  replicate_weight_labels <- data.frame(c(rep_var),c(rep_desc), NA ,NA)   
  names(replicate_weight_labels) <- c("var", "desc","value","label") 
  common_cols <- intersect(colnames(codebook), colnames(replicate_weight_labels))
  codebook <- rbind(codebook[common_cols], replicate_weight_labels[common_cols])
  
  
 
# Variables with  "Appropriate skip" values
# These are the variables for which suitable replacement values must be specified below

as.vars <- codebook %>%
  filter(label == "Appropriate skip") %>%
  pull(var) %>%
  unique()

as.values <- list(
  CAR = "No personal car use"
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

  BIKE = c("Never","A few times a year","A few times a month","A few times a week","Daily"),
  BIKE2SAVE = c("Strongly disagree","Disagree","Neither Agree or Disagree","Agree","Strongly agree"),
  BUS = c("Never","A few times a year","A few times a month","A few times a week","Daily"),
  CAR = c("No personal car use","Never","A few times a year","A few times a month","A few times a week","Daily"),
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
  PARA =  c("Never","A few times a year","A few times a month","A few times a week","Daily"),
  PC =  c("Never","A few times a year","A few times a month","A few times a week","Daily"),
  PLACE = c("Strongly disagree","Disagree","Neither Agree or Disagree","Agree","Strongly agree"),
  PRICE =  c("Strongly disagree","Disagree","Neither Agree or Disagree","Agree","Strongly agree"),
  PTRANS =  c("Strongly disagree","Disagree","Neither Agree or Disagree","Agree","Strongly agree"),
  RAIL =c("MSA does not have rail, or hh not in an MSA","MSA has rail"),
  SPHONE =c("Never","A few times a year","A few times a month","A few times a week","Daily"),
  TAB =c("Never","A few times a year","A few times a month","A few times a week","Daily"),
  TAXI =c("Never","A few times a year","A few times a month","A few times a week","Daily"),
  TRAIN =c("Never","A few times a year","A few times a month","A few times a week","Daily"),
  URBAN = c("Not in urban area","In an area surrounded by urban areas", "In an urban area","In an Urban cluster"),
  URBANSIZE = c("Not in an urbanized area","50,000 - 199,999","200,000 - 499,999","500,000 - 999,999","1 million or more without heavy rail","1 million or more with heavy rail"),
  WALK =c("Never","A few times a year","A few times a month","A few times a week","Daily"),
  WALK2SAVE = c("Strongly disagree","Disagree","Neither Agree or Disagree","Agree","Strongly agree"),
  WEBUSE17 =c("Never","A few times a year","A few times a month","A few times a week","Daily")
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

# Detect structural dependencies


#-----

# Which variables have missing values and how frequent are they?
na.count <- colSums(is.na(d))
na.count <- na.count[na.count > 0]
na.count  # See which variables have NA's

## Impute NA values in 'd'
imp <- imputeMissing(data = d,
                    N = 1,
                   weight = "WTHHFIN",
                   y_exclude = c("HOMEOWN","PC","SPHONE","TAB","WALK","BIKE","CAR","TAXI","BUS","TRAIN","PARA","PRICE",
                                 "PLACE","WALK2SAVE","BIKE2SAVE", "PTRANS","LIF_CYC","HBHUR","HTHTNRNT", "HTPPOPDN","HTRESDN",
                                 "HTEEMPDN","HBHTNRNT", "HBPPOPDN","HBRESDN",'HH_CBSA'),
                     x_exclude = c("HOUSEID", "PERSONID","WHOPROXY"))

# Replace NA's in 'd' with the imputed values
d[names(imp)] <- imp
#rm(imp)
#gc()

#-----

# Add/create variables for geographic concordance with variables in 'geo_concordance.fst'

d1 <- d %>%
    rename(
      nhts_region = CENSUS_R,
      nhts_division = CENSUS_D,
      cbsa13 = HH_CBSA,
           state = HHSTFIPS
                 ) %>% mutate(state = str_pad(state, 2, pad ="0")) %>% mutate (state = as.factor(state))


#Remove entries with travel flag at the state-division level
geo2 <- fst::read_fst("geo-processed/concordance/geo_concordance.fst")  %>%
  select('state','division','region')  %>% unique()

d_travel_2 <-  d1  %>% 
  merge(.,geo2, by ='state') %>% 
  mutate(travel_flag = ifelse(nhts_division == division, 'No','Yes')) %>%
  filter(travel_flag == "No")  %>% select(-c('division','region','travel_flag'))


d2 <- d_travel_2 %>% filter(is.na(cbsa13))
                            
#Remove entries with travel flag at the cbsa-division level
geo1 <- fst::read_fst("geo-processed/concordance/geo_concordance.fst")  %>%
        select('cbsa13','division','region')  %>% unique()

d_travel_1 <-  d_travel_2  %>% filter(!is.na(cbsa13)) %>% 
          merge(.,geo1, by ='cbsa13') %>% 
          mutate(travel_flag = ifelse((nhts_division == division) & (nhts_region == region), 'No','Yes')) %>%
          filter(travel_flag == "No")  %>% select(-c('division','region','travel_flag'))

d <- bind_rows(d2,d_travel_1)  %>%
  rename(
    region = nhts_region ,
    division = nhts_division,
  )

#Remove entries with travel flag at the state-cbsa level
#geo3 <- fst::read_fst("geo-processed/concordance/geo_concordance.fst")  %>%
 # select('state','cbsa13')  %>% unique()

#d4 <- d %>% filter(cbsa13 != "None") %>% rename(nhts_state = state) %>%
 #     merge(.,geo3, by = 'cbsa13')  %>% 
#  mutate(travel_flag = ifelse(nhts_state == state, 'No','Yes')) %>% select(c('cbsa13','nhts_state','state','travel_flag'))
 # filter(travel_flag == "Yes")  %>% select(c('cbsa13','nhts_state','state'))

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
    nhts_2017_hid = HOUSEID,
    weight      = WTHHFIN,# Rename ID and weight variables to standardized names
  ) %>%
  
  rename_with(~ gsub("WTHHFIN", "REP_", .x, fixed = TRUE), .cols = starts_with("WTHHFIN")) %>%  # Rename replicate weight columns to standardized names
  rename_with(tolower) %>%  # Convert all variable names to lowercase
  select(nhts_2017_hid, everything(), -starts_with("rep_"), starts_with("rep_")) %>%   # Reorder columns with replicate weights at the end
  arrange(nhts_2017_hid)
#----------------


# Create dictionary and save to disk
dictionary <- createDictionary(data = h.final, survey = "NHTS", vintage = 2017, respondent = "H")
saveRDS(object = dictionary, file = "survey-processed/NHTS/2017/NHTS_2017_H_dictionary.rds")

#----------------

# Save data to disk (.fst)
fst::write_fst(x = h.final, path = "survey-processed/NHTS/2017/NHTS_2017_H_processed.fst", compress = 100)

compileDictionary()
compileSpatial()


