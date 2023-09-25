## Script to process HPS data
rm(list = ls())
options(scipen=999)

library(pacman)
p_load(here, tidyverse, data.table, ipumsr, fusionData, fusionModel)

# survey week (goal: create function to clean any week)
w = 50

# Dictionary ----
source("./survey-processed/HPS/clean_HPS_dict.R")
dict <- clean_HPS_dict(w)

# Data ----
d <- fread(paste0('./survey-raw/HPS/HPS_Week', w, '_PUF_CSV/pulse2022_puf_', w, '.csv'))
#raw <- fread(paste0('./survey-raw/HPS/HPS_Week', w, '_PUF_CSV/pulse2022_puf_', w, '.csv'))

# Variable selection ----

# lower case
names(d) <- tolower(names(d))
dict[ , variable := tolower(variable)]

# limit data and dictionary to chosen variables
varlist <- fread(paste0('./survey-processed/HPS/variable_list_week', w, '_selected.csv')) %>%
  filter(!is.na(`In ACS`) | !is.na(`For fusion`) | variable == "pricechng") %>%
  pull(variable)

# order varlist based on ordering of variables in the survey --> more readable 
varlist <- intersect(dict$variable, varlist)

d <- d[ , ..varlist]

dict <- dict[variable %in% varlist]

# Missing ----

# check which columns have missing in 
colnames(d)[colSums(is.na(d)) > 0]

# MSA - missing if city not reported - replace with -99 and add level to dictionary
d[is.na(est_msa), est_msa := -99]
vals <- dict[variable == 'est_msa', val][[1]]
lbls <- dict[variable == 'est_msa', lbl][[1]]
dict[variable == 'est_msa', val := c(vals, -99)]
dict[variable == 'est_msa', lbl := c(lbls, 'Not public')]

# KIDBSTR_LT5Y - variable not reported in the codebook and is all missing in data --> drop
if (sum(is.na(d$kidbstr_lt5y)) == nrow(d)) d[ , kidbstr_lt5y := NULL]

# shouldn't have any variables with missing data
colnames(d)[colSums(is.na(d)) > 0]

## Valid blanks ----

### ~ manual corrections ----
# for count of kids under certain ages, make 0 if numkids == 0
d[thhld_numkid == 0, kids_12_17y := 0]
d[thhld_numkid == 0, kids_5_11y := 0]
d[thhld_numkid == 0, kids_lt5y := 0]

### ~ multiple choice questions ----
# the following variables are multiple choice and when they indicate -99 it means that the category selected is not true 
# replace with 0 = "No/false"
multchoice <- dict[str_detect(as.character(val), 'c\\(\"1\", \"-99'), variable]

for (var in multchoice){
  
  # replace -99 with 0 
  d[get(var) == -99, eval(var) := 0]
  
  # add level to dictionary
  vals <- dict[variable == var, val][[1]]
  lbls <- dict[variable == var, lbl][[1]]
  dict[variable == var, val := c(vals, 0)]
  dict[variable == var, lbl := c(lbls, 'No/false/none')]
  
}

### ~ NIU ----
# for questions with defined universes, replace the missing value with -999 so it matches with the 'NIU' level
# ISSUE: when vars are dependent on each other, they don't get treated 
# --> make adjustments in order that questions appear (varorder NOT alphabetical)
for (var in dict[!is.na(denom), variable]){
  
  #var <- 'frmla_yn'
  
  denom <- dict[variable == var, denom][[1]]
  denom <- paste0('!(', denom, ')')
  
  # what variables are conditioned on in the denominator?
  convars <- str_extract_all(denom, "[:alpha:]+[[:alnum:]_]+")
  convars <- setdiff(convars[[1]], "in")
  convars

  # add these to the condition for not being missing
  # (aka - don't change any obs to -999 if the conditioning vars are INVALID BLANKS)
  # ISSUE: some of the conditioning vars haven't yet been changed to -999 and might
  #        incorrectly still have -99/-88 when they should be -999
  for (v in convars){
    denom <- paste0(denom, " & ", v, " != -99", " & ", v, " != -88")
    #denom <- paste0(denom, " & ", v, " != -88")
  }
  
  # change obs that are not in universe to -999
  d[eval(parse(text=denom)), eval(var) := -999]
  
}

d[tenure == -99 | tenure == -88, rentcur] %>% table()

### ~ CHECKS ----
# function to check missing rates for variable v
checkrate <- function(v, x){
  
  pct <- d[ , mean(get(v) == x)]
  
  t <- data.table(var = v, share = pct)
  
  return(t)
  
}

# check what fraction of observations are NIU
dNIU <- map_df(dict[!is.na(denom), variable], ~checkrate(. , -999))
summary(dNIU$share) 

# check that these are accurate --> correct
dNIU[var == 'boosterrv']
dict[variable == 'boosterrv', Universe]
d[ , mean(recvdvacc != 1 & recvdvacc != -88 & recvdvacc != -99)]

## Invalid blanks ----

# the following variables have -99 as an option but this doesn't mean anything 
# --> change to -88
no99vars <- setdiff(dict[str_detect(as.character(val), '-99'), variable], multchoice)

# est_msa - ignore b/c most cities are not public
no99vars <- setdiff(no99vars, 'est_msa')

# first, check rates of -99
d99 <- map_df(no99vars, ~checkrate(. , -99))

# the following variables have -99 rates above 1%
d99[share >= 0.01, var]

# hlthins<1:8> are a set of 8 multiple choice questions about type of health insurance coverage
# unlike other multiple choice questions, there is specifically the option to answer No (level 2)
# however there is also high rates of -99 - which suggests to me that people didn't select No and instead left blank
# -> check whether people have at least one option selected 
d[ , anyhlth := (hlthins1 == 1) | (hlthins2 == 1) | (hlthins3 == 1) | (hlthins4 == 1) | 
     (hlthins5 == 1) | (hlthins6 == 1) | (hlthins7 == 1) | (hlthins8 == 1)]
summary(d$anyhlth)

# -> for observations with at least one option selected, replace the -99 with 'No'
# assumption = they did see the category but instead of selecting no they left it blank
for (var in names(d)[str_detect(names(d), 'hlthins')]){
  d[anyhlth == T, eval(var) := ifelse(get(var) == -99 , 2, get(var))]
}
d[ , anyhlth := NULL]

# check rate of -99 again
d99 <- map_df(no99vars, ~checkrate(. , -99))
d99[share >= 0.01, var]

# highest is 2.2% for INCOME
d99[share >= 0.01]

# conclusion: assume that for these variable, -99 are people who did not answer the question --> switch to code -88
no99vars <- setdiff(no99vars, names(d)[str_detect(names(d), 'hlthins')])
d[ , (no99vars) := lapply(.SD, function(x) ifelse(x == -99, -88, x)), .SDcols = no99vars]

# --> at this point there will be no more -99 codes
# d99 <- map_df(no99vars, ~checkrate(. , -99))
# summary(d99$share)

# what are non-valid missing rates now?
d88 <- map_df(names(d), ~checkrate(. , -88))
summary(d88$share)

# replace -88 with missing so they can be imputed later
d <- d %>%
  mutate(across(everything(), ~ifelse(. == -88, NA, .)))

# Factors ----

# label factor variables with labels
v = 'energy'
for (v in dict[type == 'factor', variable]){
  
  print(v)
  vals <- dict[variable == v, val][[1]]
  lbl <- dict[variable == v, lbl][[1]]
  lbl <- str_remove_all(lbl, pattern = '-?[:digit:]+\\) ')

  d[ , eval(v) := factor(get(v), levels = vals, labels = lbl)]
  
}

# drop levels that aren't used - this will drop all -99
factvars <- dict[type == 'factor', variable]
d[ , (factvars) := lapply(.SD, droplevels), .SDcols = factvars]

table(d$income, useNA = "always")

## Ordered factors ----

# variables should only be ordered if they have a natural ordering to them 
factord <- c("eeduc", "pricestress", "expns_dif", "anxious", "down", "evict", "forclose", "energy", "hse_temp", "enrgy_bill", "income")

# the variables are ordered differently - ie. some the "biggest" thing is first and some the "biggest" thing is last - Q: do they all need to be the same ordering?
# A: when person vars are fused at the household level, ordered variables are aggregated by taking the MAX value of anyone in the household. The "biggest" value should therefore be the maximum value (excluding NIU) 
dict[variable %in% factord, lbl]

# these variables need reversing in order
revord <- c("pricestress", "evict", "forclose", "energy", "hse_temp", "enrgy_bill")
dict[variable %in% setdiff(factord, revord), lbl]

# order all ordered factors
for (v in factord){
  
  print(v)
  
  d[ , eval(v) := factor(get(v), ordered = T)]
  
}

# reverse ordering of the incorrect ones
for (v in revord){
  print(v)
  d[ , eval(v) := forcats::fct_rev(get(v))]
}

table(d$energy, useNA = "always")

# NB: this has put the "missing / did not report" first - shouldn't be an issue b/c these should all be imputed as one of the non-missing levels later

# Imputation ----
source("./R/imputeMissing.R")
source("./R/detectDependence.R")
  # changed it to be 1 core

d <- as.data.frame(d)

imp <- imputeMissing(data = d,
                     N = 1,
                     weight = "hweight", # use hweight rather than pweight - might change?
                     x_exclude = c("pweight", "scram", "week"),
                     parallel = FALSE)

# debugging imputeMissing()
data <- as.data.frame(d)
N = 1
weight = "hweight"
x_exclude = c("pweight", "scram", "week")
parallel = FALSE
y_exclude = NULL
sequence = NULL
grouping = NULL
max_ncats = 10

# Renaming ----
# --> treating responses as being at the household level (see documentation)

# the standard is to set the household id as hps_2019_hid and person identifier within the household as pid
# however, in pulse there is only respondent identifier, which is a person but gives mainly household-level characteristics 
# --> set respondent identifier (scram) as the household identifier 
setnames(d, 'scram', 'hps_2019_hid')

# Standard column names are used for observation weights; “weight” for the primary weighting variable and “rep_1”, etc. for replicate weights.
# in HPS there are person weights and household weights, and there are replicate weights for both
# --> for now, use household weight as the primary weighting variables
setnames(d, 'hweight', 'weight')

# Replicate weights - read in from another file
# --> for now, use household weight as the weighting variable
repd <- fread(paste0('./survey-raw/HPS/HPS_Week', w, '_PUF_CSV/pulse2022_repwgt_puf_', w, '.csv')) %>%
  select(SCRAM, starts_with("HWEIGHT"))
names(repd) <- tolower(names(repd))
repnames <- names(repd)[str_detect(names(repd), 'hweight')]
names(repd) <- c("hps_2019_hid", str_replace(repnames, 'hweight', 'rep_'))

# join weights
d <- left_join(d, repd, by = "hps_2019_hid")
rm(repd)

# Descriptions ----

# --> make sure go through instruction in the README
# - geo
# - I think that -88 values will need to be imputed...
# - -99 values may need to be replaced with what they actually convey (eg. NIU)




