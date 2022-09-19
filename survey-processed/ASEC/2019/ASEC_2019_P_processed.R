## Script to process ASEC person data 
rm(list = ls())
options(scipen=999)

library(pacman)
p_load(here, tidyverse, data.table, ipumsr, fusionData, fusionModel)

setwd(here())

# other functions
source("R/utils.R")
source("R/createDictionary.R")

# Data ----

if (!file.exists('./survey-processed/ASEC/2019/P_data.rds')){
  # use IPUMSR package to read in extract and DDI (codebook)
  # will output a haven_labelled tible, with var values, labels, and descriptions
  
  # set to raw data directory for read_ipums commands to work
  setwd('./survey-raw/ASEC/2019/')
  
  # when reading in, convert variable names to lower
  ddi <- read_ipums_ddi("cps_00057.xml", lower_vars = T)
  d <- read_ipums_micro(ddi)
  
  setwd(here())
  
  # Limit to occupied households (so matches ACS sample) ----
  table(d$gq) # only 74 obs in group quarters
  d <- filter(d, gq == 1)
  
  # Limit to P ----
  
  # limit to person level data - person variable start at 'pernum'
  pstart <- which(names(d) == 'pernum')
  pend <- length(names(d))
  
  # make sure to include serial and year in person level so can be merged back to H
  d <- d[ , c(1, 2, pstart:pend)]
  
  # also limit varinfo in ddi to person level vars
  var_info <- ddi$var_info
  var_info <- var_info[c(1, 2, pstart:pend), ]
  var_info <- as.data.table(var_info)
  
  # save both data and ddi to read in so don't have to do the above each time
  saveRDS(d, './survey-processed/ASEC/2019/P_data.rds')
  saveRDS(var_info, './survey-processed/ASEC/2019/P_info.rds')
}else{
  
  d <- readRDS('./survey-processed/ASEC/2019/P_data.rds')
  var_info <- readRDS('./survey-processed/ASEC/2019/P_info.rds')
  d <- as.data.table(d)

}


# Factors ----

# convert all labelled variables to factors 

# want to be selective with this and only convert variables that are actually factors, otherwise continuous variables get selected
table(var_info$var_type)  

# if var type is classed as an integer, likely to be a factor
int <- var_info$var_type == 'integer'
names(d)[int]

# convert integers to ordered factors - will be ordered based on IPUMS ordering  
# d needs to be a data.table first 
intvars <- names(d)[int]
d[ , (intvars) := lapply(.SD, as_factor, ordered = T),
   .SDcols = intvars]

## ~ vet status ----
# in the ACS,  multiple periods of military service are combined into one variable
# try to create an equivalent in the ASEC
# only use vet1, vet2, vet3 b/c ACS reports maximum 3 tours of duty
d[ , vetserv := if_else(vet2 == 'NIU' & vet3 == 'NIU' & vet4 == 'NIU', as.character(vet1), 'Served multiple periods')]
d[ , vetserv := factor(vetserv, levels = c(levels(vet1), 'Served multiple periods'), ordered = T)]

# add to list of intvars
intvars <- c(intvars, 'vetserv')

## ~ number of own children ----

# in the ACS, 'own children' are defined as never-married own children OF THE HOUSEHOLDER 
# under the age of 18 --> create an equivalent variable in the ASEC
d[ , ownchild := ifelse(age < 18 & marst == 'Never married/single' 
                        & (momloc == 1 | poploc == 1 | momloc2 == 1 | poploc2 == 1), 1, 0)]

# sum of own children of the householder
d[ , noc := sum(ownchild), by = serial]

# make own child factor
d[ , ownchild := factor(ownchild, labels = c('Not child (<18) of householder', 
                                             'Child (<18) of householder'), ordered = T)]

# presence and age of own children 
  # someone has an own child in the family *if* 
  #   - someone's momloc/poploc/momloc2/poploc2 is the same as their pernum 
  #   - that person is under 18
  # NB: even though own children only refers to the number of own children of the householder

# first, sum any observations under 18 that have parent location 1 
d[ , nchild17 := ifelse(pernum == 1, sum(which(momloc == 1 & age <18 |
                                               momloc2 == 1 & age <18 | 
                                               poploc == 1 & age <18 |
                                               poploc2 == 1 & age <18) != 0), NA), by = serial]

# then loop through possible remaining parent locations, and sum those children 
locmax <- pmax(max(d$momloc), max(d$momloc2), max(d$poploc), max(d$poploc2))

for (i in c(2:locmax)){
  d[ , nchild17 := ifelse(pernum == i, sum(which(momloc == i & age <18 |
                                                 momloc2 == i & age <18 | 
                                                 poploc == i & age <18 |
                                                 poploc2 == i & age <18) != 0), nchild17),
     by = serial]}


# for anyone with pernum above locmax - they have no children present 
d[ , nchild17 := ifelse(pernum > locmax, 0, nchild17)]

#d[serial %in% c(27, 36, 68409), c('serial', 'pernum', 'nchild', 'age', 'momloc', 'momloc2', 'poploc', 'poploc2', 'nchild17')]

# then create variable which combines nchild17 and sex - matches ACS paoc
d[ , child_pres_age := fcase(sex == 'Male' | age < 16, 0,
                             nchild17 == 0, 4, # no own children
                             eldch < 6, 1, # only children under 6
                             yngch >= 6 & nchild17 > 0, 2, # only children aged 6-17
                             default = 3)] # children under 6 and children 6-17 (must be multiple chilren)


# create labels
d[ , child_pres_age := factor(child_pres_age, labels = c('NIU', 'only kids < 6', 'kids 6-17', 
                                                         'kids < 6 and kids 6-17', 'no own kids'),
                              ordered = T)]
table(d$nchild, d$child_pres_age, useNA = 'always')

# add the factor variables to the list 
intvars <- c(intvars, 'ownchild', 'child_pres_age')

# Blank Values ----

## ~ numerical variables ----
# for these vars, the NIU values change based on length of the var
# they are equal to 10^length(var)-1 (all 9s)

# get list of numerical variables
numvars <- setdiff(names(d), intvars)

# exclude repwt vars - never missing
numvars <- numvars[!str_starts(numvars, 'repwtp')]

# look at max values for each
maxval <- d[ , lapply(.SD, max),
   .SDcols = numvars] %>%
  melt(variable.name = 'var_name') 

# compare with potential NIU value 
var_info[ , length := end-start+1]
maxval <- merge(maxval, var_info[ , c('var_name', 'length')], by = 'var_name', all.x = T)
maxval[ , miscode := 10^length - 1]

# list of variables where max value is the same as potential NIU value
varlist <- maxval[miscode == value, var_name]
varlist

# change these all to zero - indicates that obs had none of this income/hours/tax 
# note: this changes class of these variables from haven_labelled to numeric
d[ , (varlist) := lapply(.SD, FUN = function(x) ifelse(max(x) == x, 0, x)),
   .SDcols = varlist]

# check that max values now all look like actual values, and not NIU codes
maxcheck <- d[ , lapply(.SD, max),
   .SDcols = numvars] %>%
  melt(variable.name = 'var_name') 
#view(maxcheck)

# there are some income variables that are all 9s still, this is because that is 
# the topcoded value, not because that value indicates that they are NIU

# MARGTAX == 99 is NIU - should this be 0 or another label? For now, I've put it
# as zero bc it indicates that these obs are not taxed 

## ~ factor variables ----

# intvars is a list of factor variables

# exclude Q flags and topcode flags --> never missing 
intvars <- intvars[!str_starts(intvars, 'q|tinc')]

### ~ valid blanks ----
# find factor variables with label 'NIU'
NIUvars <- intvars[colSums(d[ , ..intvars] == 'NIU')>0]
length(NIUvars)

# list of replacement values - long! 
l15 = 'Less than 15'
cl = 'Born within the calendar year'

NIUreplace <- list(
  asian = 'Not of Asian descent',
  vetstat = 'Less than 17',
  eldch = 'No own child in household',
  yngch = 'No own child in household',
  yrimmig = 'Born in the US',
  empstat = 'Less than 15',
  labforce = 'Less than 15',
  occ2010 = 'Less than 15 or had not worked in past year',
  occ1990 = 'Less than 15 or had not worked in past year',
  ind1990 = 'Less than 15 or had not worked in past 5 years',
  occ1950 = 'Less than 15 or had not worked in past year',
  ind1950 = 'Less than 15 or had not worked in past 5 years',
  classwkr = 'Less than 15 or never worked',
  uhrsworkt = 'Less than 15 or unemployed',
  absent = 'Less than 15 or at work previous week', 
  whyunemp = 'Less than 15 or employed',
  whyabsnt = 'Less than 15 or not absent from work last week',
  whyptlwk = 'Less than 15 or did not work part time',
  wnftlook = 'Less than 15 or not looking for work',
  educ99 = l15,
  schlcoll = 'Less than 16 or older than 55', 
  diffhear = l15, 
  diffeye = l15,
  diffrem = l15,
  diffphys = l15,
  diffmob = l15,
  diffcare = l15,
  diffany = l15,
  occ50ly = 'Less than 15 or did not work last year',
  ind50ly = 'Less than 15 or did not work last year',
  occ90ly = 'Less than 15 or did not work last year', 
  ind90ly = 'Less than 15 or did not work last year', 
  classwly = 'Less than 15 or did not work last year',
  workly= 'Non-adult',
  wkswork2 = 'Less than 15 or did not work last year',
  wksunem2 = 'Less than 15 or did not work last year',
  fullpart = 'Less than 15 or did not work last year',
  nwlookwk = 'Less than 15 or did not work last year',
  pension = 'Less than 15 or did not work last year',
  firmsize = 'Less than 15 or did not work last year',
  wantjob = 'Less than 15 or in the labor force last week',
  whyptly = 'Less than 15 or did not work part time at least one week last year', 
  usftptlw = 'Less than 15 or did not work less than 35 hours last week', 
  payifabs = 'Less than 15 or not temporarily absent from work last week',
  wnlwnilf = 'Less than 15 or older than 49 or in the labor force last week',
  strechlk = 'Less than 15 or did not spend time looking for work last year',
  whynwly = 'Less than 15 or worked last year',
  actnlfly = 'Less than 15 or did not work more than 3 weeks last year',
  ptweeks = 'Less than 15 or did not work part time last year',
  srcsurv1 = 'Less than 15 or did not receive survivor benefits last year',
  srcsurv2 = 'Less than 15 or did not report a first source of survivor benefits last year',
  srcdisa1 = 'Less than 15 or did not receive income due to health problems',
  srcdisa2 = 'Less than 15 or did not report a first source of disability income', 
  srcret1 = 'Did not receive retirement income last year', 
  srcret2 = 'Did not have more than one source of retirement income',
  srcpen1 = 'Less than 15 or did not receive pension income last year',
  srcpen2 = 'Less than 15 or did not have more than one source of pension income', 
  srcearn = 'Less than 15 or did not work last year', 
  srceduc = 'Less than 15 or did not receive educational assistance last year', 
  srcunemp = 'Less than 15',
  srcwelfr = 'Less than 15 or did not receive welfare last year',
  srcwkcom = 'Less than 15 or did not receive workers compensation last year', 
  vetqa = 'Less than 15 or did not receive veterans payments',
  whyss1 = 'Less than 15 or did not receive SS last year',
  whyss2 = 'Less than 15 or did not give a first reason for receiving SS last year',
  whyssi1 = 'Less than 15 or did not receive Supplemental Security Income last year',
  whyssi2 = 'Less than 15 or did not give a first reason for receiving Supplemental Security Income last year',
  gotvdisa = 'Less than 15 or did not recieve veterans payments last year',
  gotveduc = 'Less than 15 or did not recieve veterans payments last year',
  gotvothe = 'Less than 15 or did not recieve veterans payments last year',
  gotvpens = 'Less than 15 or did not recieve veterans payments last year', 
  gotvsurv = 'Less than 15 or did not recieve veterans payments last year', 
  offpov = 'Secondary individual less than 15',
  offreason = 'CPS poverty matches official poverty',
  migsta1 = 'Less than one years old', 
  whymove = 'Less than one or lived in the same place a year ago',
  migrate1 = 'Less than one years old',
  disabwrk = l15,
  paidgh = 'Less than 15 or not a policyholder',
  himcaidly = 'Born within the calendar year',
  himcarely = l15,
  hichamp = 'Born within the calendar year', 
  phinsur = 'Born within the calendar year',
  phiown = 'Less than 15 or not covered by private insurance last year', 
  caidly = 'Born within the calendar year',
  caidpart = 'Born within the calendar year or not covered by Medicaid last year',
  anycovly = 'Born within the calendar year', 
  pubcovly = 'Born within the calendar year', 
  anypart = 'Born within the calendar year or not covered by any health insurance last year', 
  pubpart = 'Born within the calendar year or not covered by government health insurance last year', 
  prvtpart = 'Born within the calendar year or not covered by private health insurance last year',
  prvtcovly = cl,
  prvtdeply = 'Not covered by private health insurance last year', 
  prvtownly = 'Born within the calendar year or not covered by private health insurance last year',
  prvtcoutly = 'Born within the calendar year or not covered by private health insurance last year',
  prvtdepnw = 'Not covered by private health insurance last year', 
  prvtownnw = 'Not covered by private health insurance last year', 
  prvtcoutnw = 'Not covered by private health insurance last year',
  grpcovly = cl,
  grpdeply = 'Not covered by employment-based insurance last year', 
  grpownly = 'Not covered by employment-based insurance last year', 
  grpoutly = 'Not covered by employment-based insurance last year', 
  grpcoutly = 'Both within the calendar year or not covered by employment-based insurance last year',
  grptyply = 'Less than 15 or not covered by employment-based insurance last year',
  grpdepnw = 'Not covered by employment-based insurance last year', 
  grpownnw = 'Not covered by employment-based insurance last year', 
  grpoutnw = 'Not covered by employment-based insurance last year', 
  grpcoutnw = 'Not covered by employment-based insurance last year', 
  grptypnw = 'Not covered by employment-based insurance last year', 
  grpwhonw = 'Not covered by employment-based insurance from other household member last year',
  dpcovly = cl,
  dpdeply = 'Did not have direct-purchase private coverage last year', 
  dpownly = 'Did not have direct-purchase private coverage last year', 
  dpoutly = 'Did not have direct-purchase private coverage last year', 
  dpcoutly = 'Born within the calendar year or did not have direct-purchase private coverage last year', 
  dptyply = 'Less than 15 or did not have direct-purchase private coverage last year', 
  dpdepnw = 'Did not have direct-purchase private coverage last year', 
  dpownnw = 'Did not have direct-purchase private coverage last year', 
  dpoutnw = 'Did not have direct-purchase private coverage last year', 
  dpcoutnw = 'Did not have direct-purchase private coverage last year', 
  dptypnw = 'Did not have direct-purchase private coverage last year', 
  dpwhonw = 'Did not have direct-purchase private coverage last year', 
  mrkcovly = cl,
  mrkdeply = 'Not covered by marketplace coverage last year',
  mrkownly = 'Not covered by marketplace coverage last year',
  mrkoutly = 'Not covered by marketplace coverage last year',
  mrkcoutly = 'Not covered by marketplace coverage last year',
  mrkcoutnw = 'Not covered by marketplace coverage last year',
  mrktyply = 'Not covered by marketplace coverage last year',
  mrkwho1 = 'Not covered by marketplace coverage from other household member last year',
  mrkdepnw = 'Not covered by marketplace coverage last year',
  mrkownnw = 'Not covered by marketplace coverage last year',
  mrkoutnw = 'Not covered by marketplace coverage last year',
  mrktypnw = 'Not covered by marketplace coverage last year',
  mrkwhonw = 'Not covered by marketplace coverage from other household member last year',
  mrkscovly = cl,
  mrksdeply = 'Born within the calendar year or not covered by subsidized marketplace coverage last year',
  mrksownly = 'Born within the calendar year or not covered by subsidized marketplace coverage last year',
  mrksoutly = 'Born within the calendar year or not covered by subsidized marketplace coverage last year',
  mrkscoutly = 'Born within the calendar year or not covered by subsidized marketplace coverage last year',
  mrkstyply = 'Born within the calendar year or not covered by subsidized marketplace coverage last year',
  mrkswho1 = 'Born within the calendar year or not covered by subsidized marketplace coverage from other household member last year',
  mrksdepnw = 'Not covered by subsidized marketplace coverage last year',
  mrksownnw = 'Not covered by subsidized marketplace coverage last year',
  mrksoutnw = 'Not covered by subsidized marketplace coverage last year',
  mrkscoutnw = 'Not covered by subsidized marketplace coverage last year',
  mrkstypnw = 'Not covered by subsidized marketplace coverage last year',
  mrkswhonw = 'Not covered by subsidized marketplace coverage from other household member last year',
  mrkucovly = cl,
  mrkudeply = 'Born within the calendar year or not covered by unsubsidized marketplace coverage last year',
  mrkuownly = 'Born within the calendar year or not covered by unsubsidized marketplace coverage last year',
  mrkuoutly = 'Born within the calendar year or not covered by unsubsidized marketplace coverage last year',
  mrkucoutly = 'Born within the calendar year or not covered by unsubsidized marketplace coverage last year',
  mrkutyply = 'Born within the calendar year or not covered by unsubsidized marketplace coverage last year',
  mrkuwho1 = 'Born within the calendar year or not covered by unsubsidized marketplace coverage from other household member last year',
  mrkudepnw = 'Not covered by unsubsidized marketplace coverage last year',
  mrkuownnw = 'Not covered by unsubsidized marketplace coverage last year',
  mrkuoutnw = 'Not covered by unsubsidized marketplace coverage last year',
  mrkucoutnw = 'Not covered by unsubsidized marketplace coverage last year',
  mrkutypnw = 'Not covered by unsubsidized marketplace coverage last year',
  mrkuwhonw = 'Not covered by unsubsidized marketplace coverage from other household member last year',
  nmcovly = cl,
  nmdeply = 'Born within the calendar year or not covered by non-marketplace coverage last year',
  nmownly = 'Born within the calendar year or not covered by non-marketplace coverage last year',
  nmoutly = 'Born within the calendar year or not covered by non-marketplace coverage last year',
  nmcoutly = 'Born within the calendar year or not covered by non-marketplace coverage last year',
  nmtyply = 'Born within the calendar year or not covered by non-marketplace coverage last year',
  nmwho1 = 'Born within the calendar year or not covered by non-marketplace coverage from other household member last year',
  nmdepnw = 'Not covered by non-marketplace coverage last year',
  nmownnw = 'Not covered by non-marketplace coverage last year',
  nmoutnw = 'Not covered by non-marketplace coverage last year',
  nmcoutnw = 'Not covered by non-marketplace coverage last year',
  nmtypnw = 'Not covered by non-marketplace coverage last year',
  nmwhonw = 'Not covered by non-marketplace coverage from other household member last year',
  trccovly = cl,
  trcdeply = 'Born within the calendar year or not covered by TRICARE last year',
  trcownly = 'Born within the calendar year or not covered by TRICARE last year',
  trcoutly = 'Born within the calendar year or not covered by TRICARE last year',
  trccoutly = 'Born within the calendar year or not covered by TRICARE last year',
  trctyply = 'Born within the calendar year or not covered by TRICARE last year',
  trcwho1 = 'Born within the calendar year or not covered by TRICARE from other household member last year',
  trcdepnw = 'Not covered by TRICARE last year',
  trcownnw = 'Not covered by TRICARE last year',
  trcoutnw = 'Not covered by TRICARE last year',
  trccoutnw = 'Not covered by TRICARE last year',
  trctypnw = 'Not covered by TRICARE last year',
  trcwhonw = 'Not covered by TRICARE from other household member last year',
  champvaly = cl,
  inhcovly = cl,
  vacovly = cl,
  schiply = cl,
  multcov = cl,
  hielig = 'Self-employed or not employed or employed by employer that does not offer health insurance',
  hinelig1 = 'Self-employed or not employed or employed by employer that does not offer health insurance',
  hinelig2 = 'Self-employed or not employed or employed by employer that does not offer health insurance',
  hinelig3 = 'Self-employed or not employed or employed by employer that does not offer health insurance',
  hinelig4 = 'Self-employed or not employed or employed by employer that does not offer health insurance',
  hinelig5 = 'Self-employed or not employed or employed by employer that does not offer health insurance',
  hintake1 = 'Self-employed or not employed or employed by employer that does not offer health insurance',
  hintake2 = 'Self-employed or not employed or employed by employer that does not offer health insurance',
  hintake3 = 'Self-employed or not employed or employed by employer that does not offer health insurance',
  hintake4 = 'Self-employed or not employed or employed by employer that does not offer health insurance',
  hintake5 = 'Self-employed or not employed or employed by employer that does not offer health insurance',
  hintake6 = 'Self-employed or not employed or employed by employer that does not offer health insurance',
  hintake7 = 'Self-employed or not employed or employed by employer that does not offer health insurance',
  hintake8 = 'Self-employed or not employed or employed by employer that does not offer health insurance',
  hioffer = 'Self-employed or not employed or employed and not covered by employer-based health insurance',
  vetlast = 'Under 17 or have not served in the U.S. Armed Forces',
  vet1 = 'Under 17 or have not served in the U.S. Armed Forces',
  vet2 = 'Under 17 or have not served in the U.S. Armed Forces for 2 or more periods of service',
  vet3 = 'Under 17 or have not served in the U.S. Armed Forces for 3 or more periods of service',
  vet4 = 'Under 17 or have not served in the U.S. Armed Forces for 4 or more periods of service',
  gotwic = 'Not female',
  kidcneed = 'Over 14',
  vetserv = 'Under 17 or have not served in the U.S. Armed Forces', 
  child_pres_age = 'Male / female under 16'
)

# # specific that all these codes are NIU
for (x in names(NIUreplace)){
  
  NIUreplace[[x]] <- paste0('NIU: ', NIUreplace[[x]])
  
  d[ , (x) := recode(get(x), NIU = NIUreplace[[x]])]
}

### ~ other blanks ----

# look at value labels to see if there are other factor labels that indicate missing or NIU
# test <- map(c(1:length(intvars)), ~var_info[var_name %in% intvars, val_labels[[.x]][[2]]])
# view(test)

# for the variable NATIVITY 'Unknown' indicates that the respondant does not know - leave as is
intvars[colSums(d[ , ..intvars] == 'Unknown')>0]

# for the variable uhrswork1 treat the code 'NIU/Missing' as NIU
intvars[colSums(d[ , ..intvars] == 'NIU/Missing')>0]
d[ , uhrswork1 := recode(uhrswork1, `NIU/Missing` = 'NIU: Less than 15 or not employed last week')]

# for wkstat, treat code 'NIU, blank, or not in labor force' as NIU
d[ , wkstat := recode(wkstat, `NIU, blank, or not in labor force` = 'NIU: Non-adult or not in labor force')]

# educ, 'NIU or blank' 
intvars[colSums(d[ , ..intvars] == 'NIU or blank')>0]
d[ , educ := recode(educ, `NIU or blank` = 'NIU: Less than 15')]

# there doesn't seem to be anymore labels that indicate NIU/unknown/blank etc. 


# Renaming ----

# Standard column names are used for unique household identifiers (e.g. “acs_2019_hid”); for person-level microdata the within-household person identifier (integer) is always “pid”.
setnames(d, c('serial', 'pernum'), c('asec_2019_hid', 'pid'))

# Standard column names are used for observation weights; “weight” for the primary weighting variable and “rep_1”, etc. for replicate weights.
setnames(d, 'asecwt', 'weight')

repnames <- names(d)[str_starts(names(d), 'repwtp')]
setnames(d, repnames, str_replace(repnames, 'wtp', '_'))

# Clean up ----

# list of variables that aren't weights, quality flags, or topcode flags
varlist <- setdiff(names(d), c(names(d)[str_starts(names(d), 'rep_')], 
                               names(d)[str_starts(names(d), 'q')],
                               names(d)[str_starts(names(d), 'tinc')]))

# look at variables and check all looks correct
# d[ , ..varlist] %>% view()

# famsize is a factor but can be a numeric  
d[ , famsize := as.numeric(str_extract(as.character.factor(famsize), '[:digit:]*'))]
intvars <- setdiff(intvars, 'famsize')

# check for any missing values
colnames(d)[colSums(is.na(d)) > 0] 

# all variables with missing values are data quality flags for the detailed version of certain variables
# we do not have the detailed versions of these variables --> don't keep the quality flags for them 
flag_detail <- colnames(d)[colSums(is.na(d)) > 0] 
d[ , (flag_detail) := NULL]

# also remove from var_info
var_info <- var_info[!(var_name %in% flag_detail)]

# check no more values with missing vars
colnames(d)[colSums(is.na(d)) > 0] 

# drop levels that are not used by factors - typically levels that are not valid in this year 
d[ , (intvars) := lapply(.SD, droplevels), .SDcols = intvars]

# Dictionary ----

# label every variable with it's label from the IPUMS codebook
# add in labels for additional variables that I have constructed
d <- labelled::set_variable_labels(.data = d, 
                              .labels = setNames(as.list(c(var_info$var_label, 
                                                           "[constructed] Veteran's period(s) of service",
                                                           "[constructed] Child of householder, under 18 and never married",
                                                           "[constructed] Number of own children belonging to householder",
                                                           "[constructed] Number of own children in household under 17",
                                                           "[constructed] Presence and age of children, women only")),
                                                 names(d)))

# drop flags and top code flags
flagvars <- c(names(d)[str_starts(names(d), 'q')],
              names(d)[str_starts(names(d), 'tinc')])
d[ , (flagvars) := NULL]

# put replicate weights at the end 
setcolorder(d, setdiff(names(d), names(d)[str_starts(names(d), 'rep_')]))

# convert to data.frame
d <- as.data.frame(d)

# create dictionary 
dictionary <- createDictionary(data = d, survey = "ASEC", vintage = 2019, respondent ="P")

saveRDS(dictionary, file = "survey-processed/ASEC/2019/ASEC_2019_P_dictionary.rds")

# Save Data ----
fst::write_fst(x = d, path = "survey-processed/ASEC/2019/ASEC_2019_P_processed.fst", compress = 100)
  

# d <- fst::read_fst(path = "survey-processed/ASEC/2019/ASEC_2019_P_processed.fst") %>%  as.data.table()
# dictionary <- readRDS(file = "survey-processed/ASEC/2019/ASEC_2019_P_dictionary.rds")

# Compile Universal ----
compileDictionary()

# # check universal dictionary
# asec_dictionary <- dictionary
# load('./data/dictionary.rda')
# load('./data/surveys.rda')
# 
universe()
# harmony()

# # testing harmonize mutate code
# d <- d %>%
#   mutate(test = empstat)
# class(d$test)
# d <- d %>%
#   mutate(test = if_else(age != '15', test, factor('NIU: Less than 15', ordered = T)))
# 
# # check if incretir == incrint1 + incrint2
# d[ , check := incret1 + incret2]
# summary(d$incretir)
# summary(d$check)
# 
# d[incretir != 0 | check != 0] %>%
# ggplot() +
#   geom_density(aes(x = asinh(incretir)), color = 'red') +
#   geom_density(aes(x = asinh(check)), color = 'blue') 


# experiment with vet status
  
