## Script to process ASEC household data 
rm(list = ls())
options(scipen=999)

source("R/utils.R")
source("R/createDictionary.R")
source("R/imputeMissing.R")
source("R/detectDependence.R")

library(pacman)
p_load(here, tidyverse, data.table, ipumsr, fusionData, fusionModel)

# Data ----

# Download IPUMS data using the ipumsr package
if (!file.exists('./survey-processed/ASEC/2019/H_data.rds')){
  # use IPUMSR package to read in extract and DDI (codebook)
  # will output a haven_labelled tible, with var values, labels, and descriptions
  
  # set to raw data directory for read_ipums commands to work
  setwd('./survey-raw/ASEC/2019/')
  
  # when reading in, convert variable names to lower
  ddi <- read_ipums_ddi("cps_00057.xml", lower_vars = T)
  d <- read_ipums_micro(ddi)
  
  setwd(here())
  
  # Limit to occupied households (so matches ACS sample)
  table(d$gq) # only 74 obs in group quarters
  d <- filter(d, gq == 1)
  
  # Limit to H
  
  # limit to person level data - person variable end at 'pernum'
  d <- filter(d, pernum == 1)
  hend <- which(names(d) == 'pernum')-1
  
  # make sure to include serial and year in person level so can be merged back to H
  d <- d[ , c(1:hend)]
  
  # also limit varinfo in ddi to person level vars
  var_info <- ddi$var_info
  var_info <- var_info[c(1:hend), ]
  var_info <- as.data.table(var_info)
  
  # save both data and ddi to read in so don't have to do the above each time
  saveRDS(d, './survey-processed/ASEC/2019/H_data.rds')
  saveRDS(var_info, './survey-processed/ASEC/2019/H_info.rds')
}else{
  
  d <- readRDS('./survey-processed/ASEC/2019/H_data.rds')
  var_info <- readRDS('./survey-processed/ASEC/2019/H_info.rds')
  d <- as.data.table(d)
  
}

# Factors ----

# convert all labelled variables to factors 

# want to be selective with this and only convert variables that are actually factors, otherwise continuous variables get selected
table(var_info$var_type)  

# if var type is classed as an integer, likely to be a factor
int <- var_info$var_type == 'integer'
names(d)[int]
names(d)[!int]

# convert integers to ordered factors - will be ordered based on IPUMS ordering  
# d needs to be a data.table first 
intvars <- names(d)[int]

# remove statefip, statecensus, repwt b/c don't want them to be factors
intvars <- setdiff(intvars, c('statefip', 'statecensus', 'repwt'))

# add in metfips -> should be factor
intvars <- c(intvars, 'metfips')

d[ , (intvars) := lapply(.SD, as_factor, ordered = T),
   .SDcols = intvars]

# Blank Values ----

## ~ numerical variables ----
# check that the maximum values do not indicate missing --> they don't 

# get list of numerical variables
numvars <- setdiff(names(d), intvars)

# exclude repwt vars - never missing
numvars <- numvars[!str_starts(numvars, 'repwt')]

# look at max values for each
maxval <- d[ , lapply(.SD, max),
             .SDcols = numvars] %>%
  melt(variable.name = 'var_name') 

maxval

## ~ recode NIU ----
# we want to NIU to be meaninful (ie. specific the exlcuded universe)

# exclude Q flags and topcode flags --> never missing 
intvars <- intvars[!str_starts(intvars, 'q')]

# find factor variables with label 'NIU'
# this will ignore factors with levels NIU but no occurances of this
NIUvars <- intvars[colSums(d[ , ..intvars] == 'NIU')>0]
length(NIUvars)

# list of replacement values - long! 
NIUreplace <- list(
  pubhous = 'Owns house',
  rentsub = 'Owns house or lives in public housing',
  stampmo = 'Did not receive good stamps', 
  atelunch = 'No children ages 5-18',
  lunchsub = 'No children'
)

# recode NIU as meaningful label
for (x in names(NIUreplace)){
  
  NIUreplace[[x]] <- paste0('NIU: ', NIUreplace[[x]])
  
  d[ , (x) := recode(get(x), NIU = NIUreplace[[x]])]
}

### ~ other blanks ----

# look at value labels to see if there are other factor labels that indicate missing or NIU
test <- map(c(1:length(intvars)), ~var_info[var_name %in% intvars, val_labels[[.x]][[2]]])
#View(test)

intvars[29]
table(d$ncouples)

# number of couples, mothers, or fathers should be 0 if "O X or NIU" b/c universe is everyone
# can all be replaced as numeric, rather than factor
d[ , ncouples := recode(ncouples, '0 couples or NIU' = '0')]
d[ , nmothers := recode(nmothers, '0 mothers or NIU' = '0')]
d[ , nfathers := recode(nfathers, '0 fathers or NIU' = '0')]

d[ , ncouples := as.numeric(str_extract(as.character.factor(ncouples), '[:digit:]*'))]
d[ , nmothers := as.numeric(str_extract(as.character.factor(nmothers), '[:digit:]*'))]
d[ , nfathers := as.numeric(str_extract(as.character.factor(nfathers), '[:digit:]*'))]

# number of families has the category '1 family or N/A' which should be 1
# then can convert nfams to numeric
d[ , nfams := recode(nfams, '0 families (vacant unit)' = '0', 
                     '1 family or N/A' = '1', 
                     '2 families' = '2')]
d[ , nfams := as.numeric(str_extract(as.character.factor(nfams), '[:digit:]*'))]


intvars <- setdiff(intvars, c('ncouples', 'nmothers', 'nfathers', 'nfams'))


# Geographic Variables ----
# want to make sure that the geographic levels in the ASEC geo vars are the same as in geoconcordance
geovars <- c('region', 'statefip', 'metro', 'metarea', 'county', 
            'statecensus', 'cbsasz', 'metfips')

geocon <- fst::read_fst("geo-processed/concordance/geo_concordance.fst") 
geonames <- names(geocon)

## region -> same as recs divisions ----
# note: in geocondordance, recs_divsion has Mountain North and Mountain South, but CPS just has Mountain
# --> make asec_division its own variable in geoconcordance.fst
d[ , region := str_remove(region, ' Division')]
d[ , region := as.factor(region)]

# change the name to asec_division to match newly created var in geoconcord
setnames(d, 'region', 'asec_division')
var_info[var_name == 'region', var_name := 'asec_divsion']
intvars <- setdiff(intvars, 'region')

table(d$asec_division)
table(geocon$asec_division)

## state ----
table(d$statefip)
table(geocon$state)

# record state name as the label from statefip 
d[ , state_name := as.character(as_factor(statefip))]
d[ , state_name := as.factor(state_name)]

# state in geocon is always two characters - make state in CPS correspond to this
d[ , statefip := str_pad(statefip, 2, pad = '0')]
d[ , statefip := as.factor(statefip)]

# change name to state to match geoconcord
setnames(d, 'statefip', 'state')
var_info[var_name == 'statefip', var_name := 'state']

table(d$state_name) %>% length()
table(geocon$state_name) %>% length()


## county ----
d[ , county := str_sub(county, start = -3)]
d[county == '0', county := 'County not identified']
d[ , county := as.factor(county)]
table(d$county)

# b/c county is not always identified, it will not have all the same levels as in 
# geoconcordance --> create a new variable in geoconcordance to match 
# --> output a list of ASEC counties that are identified
dcounty <- unique(d[ , c('state', 'county')])
saveRDS(dcounty, "./geo-processed/ASEC/asec_county.rds")

# set name to asec_county so unique in geoconcordance
setnames(d, 'county', 'asec_county')
var_info[var_name == 'county', var_name := 'asec_county']

## metarea ----
# Variable METAREA has two missing codes - ‘Missing data’ and ‘Other metropolitan area unidentified’ → these seem to refer to the same type of observation based on the variable METRO, they both seem to refer to people in metro areas that are not identified due to data confidentiality
# --> I wrote to the IPUMS people about our question on the METAREA missing labels. They said that it seems like observations labeled as "missing data", with code 9999, should have been labeled "Other metropolitan areas, unidentified", with code 9997. So I think you can go ahead and re-label them, and then METAREA will have two missing codes, one for NIU and one for non-identified metro area. 
d[ , metarea := recode(metarea, 'Missing data' = 'Other metropolitan areas, unidentified')]
table(d$metarea)


# these are the geo variables that match the geoconcordance from the CPS
intersect(names(geocon), names(d))

# Renaming ----

# Standard column names are used for unique household identifiers (e.g. “acs_2019_hid”)
setnames(d, c('serial'), c('asec_2019_hid'))

# Standard column names are used for observation weights; “weight” for the primary weighting variable and “rep_1”, etc. for replicate weights.
setnames(d, 'asecwth', 'weight')

repnames <- names(d)[str_starts(names(d), 'repwt')]
setnames(d, repnames, str_replace(repnames, 'wt', '_'))  
setnames(d, 'rep_', 'rep')


# Clean up ----

# list of variables that aren't weights, quality flags, or topcode flags
varlist <- setdiff(names(d), c(names(d)[str_starts(names(d), 'rep')], 
                               names(d)[str_starts(names(d), 'q')]))

# look at variables and check all looks correct
#d[ , ..varlist] %>% View()

# check for any missing values -> should be 0
colnames(d)[colSums(is.na(d)) > 0] 

# drop levels that are not used by factors - typically levels that are not valid in this year 
d[ , (intvars) := lapply(.SD, droplevels), .SDcols = intvars]

#intvars <- setdiff(intvars, 'recs_division')

# Dictionary ----

# label every variable with it's label from the IPUMS codebook
# add in labels for additional variables that I have constructed
d <- labelled::set_variable_labels(.data = d, 
                                   .labels = setNames(as.list(c(var_info$var_label, 
                                                                "State name")),
                                                      names(d)))

# drop flags and top code flags
flagvars <- c(names(d)[str_starts(names(d), 'q')])
d[ , (flagvars) := NULL]

# put replicate weights at the end 
setcolorder(d, setdiff(names(d), names(d)[str_starts(names(d), 'rep')]))

# convert to data.frame
d <- as.data.frame(d)

# create dictionary 
dictionary <- createDictionary(data = d, survey = "ASEC", vintage = 2019, respondent ="H")

saveRDS(dictionary, file = "survey-processed/ASEC/2019/ASEC_2019_H_dictionary.rds")

# Save Data ----
fst::write_fst(x = d, path = "survey-processed/ASEC/2019/ASEC_2019_H_processed.fst", compress = 100)

fwrite(select(dictionary, variable, description), file = 'survey-processed/ASEC/2019/ASEC_2019_H_var_list.csv')

# Compile Universal ----
compileDictionary()

# # check universal dictionary
# asec_dictionary <- dictionary
# load('./data/dictionary.rda')
# load('./data/surveys.rda')
# 
universe()
harmony()


# # Old Code-----
# 
# # Manually identify the potential location variables found in geo_concordance.R
# 
# geographic_variables <- d_H %>% dplyr::select(region, statefip, metro, metarea, county, 
#                                               statecensus, cbsasz, metfips)
# 
# d_H <- d_H %>%
#   cbind(geographic_variables %>%
#   rename_all(list( ~paste0(., "_labelled"))) %>% 
#   mutate_all(~ as_factor(lbl_clean(.))))
# 
# d_H <- d_H %>%
#   mutate(county = stringr::str_sub(county,start=-3)) %>%
#   rename(
#     recs_division = region,
#     state = statefip,
#     state_name = statefip_labelled,
#     county14 = county
#   )
# 
# # Drop labelled variables
# d_H <- d_H %>%
#   select(!contains("labelled"))
# 
# # See which variables in 'd_H' are also in 'geo_concordance' and
# gnames <- names(fst::fst("geo-processed/concordance/geo_concordance.fst"))
# gvars <- intersect(gnames, names(d_H))
# 
# # Class new/added geo identifiers as unordered factors
# d_H <- d_H %>%
#   mutate_at(gvars, ~ factor(.x, levels = sort(unique(.x)), ordered = FALSE))
# 
# #-----
# 
# # Work only with the non-replication weight variables and remove Q variables
# 
# replication_weight_vars <- d_H %>% dplyr::select(starts_with("repwt"))
# non_replication_weight_vars <- d_H %>% dplyr::select(-starts_with("repwt"),
#                                                      -starts_with("q"))
# 
# value_check <- non_replication_weight_vars %>%
#   mutate_all(~ as_factor(lbl_clean(.))) 
# 
# # metro
# levels(value_check$metro) <- sub("Not identifiable", "NIU: Metro status is not identifiable",
#                                  levels(value_check$metro))
# 
# # metarea
# # IPUMS people confirmed that the missing data outcome was supposed to be coded as another value
# levels(value_check$metarea) <- sub("Missing data", "Other metropolitan areas, unidentified",
#                                    levels(value_check$metarea))
# levels(value_check$metarea) <- sub("NIU, household not in a metropolitan area", "NIU: Household is not in a metropolitan area",
#                                    levels(value_check$metarea))
# 
# # county
# levels(value_check$county14) <- sub("^0$", "NIU: County is not identifiable", 
#                                     levels(value_check$county14))
# # cbsasz
# levels(value_check$cbsasz) <- sub("Not identified or nonmetropolitan", "NIU: Nonmetropolitan or is not identifiable",
#                                   levels(value_check$cbsasz))
# 
# # metfips
# levels(value_check$metfips) <- sub("Unidentified or nonmetropolitan", "NIU: Nonmetropolitan or is not identifiable",
#                                    levels(value_check$metfips))
# 
# # NIU values: pubhous, rentsub, stampmo, atelunch, lunchsub
# 
# NIU_variables_list <- list(
#   pubhous = "Not living in public housing", #NIU
#   rentsub = "Not paying lower rent due to government subsidy", #NIU
#   stampmo = "Did not recieve food stamps", #NIU
#   atelunch = "No household members are school aged 5 to 18", #NIU
#   lunchsub = "No household members are school aged 5 to 18 or none ate a complete hot lunch"
# )
# 
# # frelunch
# levels(value_check$frelunch) <- sub("NIU -- Children didn't eat hot lunch", "NIU: No household members ate a complete hot lunch",
#                                    levels(value_check$frelunch))
# levels(value_check$frelunch) <- sub("NIU -- No children in hh", "NIU: No household members are school aged 5 to 18",
#                                     levels(value_check$frelunch))
# 
# # nfams
# levels(value_check$nfams) <- sub("1 family or N/A", "1 or no families",
#                                     levels(value_check$nfams))
# 
# # ncouples
# levels(value_check$ncouples) <- sub("0 couples or NIU", "No couples or is not identifiable",
#                                  levels(value_check$ncouples))
# 
# # nmothers
# levels(value_check$nmothers) <- sub("0 mothers or NIU", "No mothers or is not identifiable",
#                                     levels(value_check$nmothers))
# 
# # nfathers
# levels(value_check$nfathers) <- sub("0 fathers or NIU", "No fathers or is not identifiable",
#                                     levels(value_check$nfathers))
# 
# # Change all NIU-only values
# for (x in names(NIU_variables_list)){
#   NIU_variables_list[x] <- paste0('NIU: ', NIU_variables_list[x])
#   levels(value_check[, x]) <- sub("NIU", NIU_variables_list[x], levels(value_check[, x]))
# }
# 
# # Match variable names as done in person level ASEC 2019 data
# data.table::setnames(value_check, 'asecwth', 'weight')
# data.table::setnames(value_check, 'serial', 'asec_2019_hid')
# 
# # No missing values
# # colnames(value_check)[colSums(is.na(value_check)) > 0] 
# 
# # Add the replication weight variables back
# 
# value_check <- value_check %>% cbind(replication_weight_vars)
# 
# var_info$var_name[which(var_info$var_name == "serial")] <- "asec_2019_hid"
# var_info$var_name[which(var_info$var_name == "asecwth")] <- "weight"
# var_info$var_name[which(var_info$var_name == "region")] <- "recs_division"
# var_info$var_name[which(var_info$var_name == "statefip")] <- "state"
# var_info$var_name[which(var_info$var_name == "county")] <- "county14"
# 
# value_check_without_state_name <- value_check %>% dplyr::select(-state_name)
# 
# col_order <- c()
# for (i in var_info[,"var_name"]) {col_order <- c(col_order, i)}
# # Check if it works:
# # col_order
# 
# # Drop flags
# 
# flagvars <- col_order[str_starts(col_order, 'q')]
# col_order <- col_order[!col_order %in% flagvars]
# var_info <- var_info %>% filter(!str_starts(var_name, 'q'))
# 
# # Remove duplicates based on the household serial number
# 
# value_check_without_state_name <- value_check_without_state_name[!duplicated(value_check_without_state_name$asec_2019_hid), ]
# 
# # Weight is a numerical value
# 
# value_check_without_state_name <- value_check_without_state_name %>% mutate(weight = as.numeric(weight))
# 
# # Add labels from IPUMS
# 
# value_check_without_state_name <- value_check_without_state_name[,col_order]
# 
# # Turn integer variables from factor type
# # FIX - not all these should be converted to numeric from factor (eg. faminc)
# 
# value_check_without_state_name <- value_check_without_state_name %>%
#   mutate(
#     year = as.numeric(year),
#     asec_2019_hid = as.numeric(asec_2019_hid),
#     cpsid = as.integer(cpsid),
#     weight = as.integer(weight),
#     mish = ordered(mish),
#     numprec = as.integer(numprec),
#     cpi99 = as.integer(cpi99),
#     hhincome = as.integer(hhincome),
#     heatval = as.integer(heatval),
#     stampno = ordered(stampno),
#     stampval = as.integer(stampval),
#     faminc = as.integer(faminc),
#     nfams = ordered(nfams),
#     ncouples = ordered(ncouples),
#     nmothers = ordered(nmothers),
#     nfathers = ordered(nfathers),
#     hrhhid = as.integer(hrhhid),
#     hrhhid2 = as.integer(hrhhid2),
#     marbasecidh = as.integer(marbasecidh),
#     hseq = as.integer(hseq)
#   ) %>%
#   mutate(across(starts_with("repwt"), as.integer))
# 
# value_check_state_name <- labelled::set_variable_labels(.data = value_check_without_state_name, 
#                                                         .labels = setNames(as.list(var_info$var_label),
#                                                                            names(value_check_without_state_name)))
# 
# repnames <- names(value_check_state_name)[str_starts(names(value_check_state_name), 'repwt')]
# setnames(value_check_state_name, repnames, str_replace(repnames, 'wt', '_'))
# 
# d_H_final <- value_check_state_name %>% 
#   rename(state_name = statecensus)
# 
# 
# # Create dictionary 
# dictionary <- fusionData::createDictionary(data = d_H_final, survey = "ASEC", vintage = 2019, respondent = "H")
# 
# saveRDS(dictionary, file = "survey-processed/ASEC/2019/ASEC_2019_H_dictionary.rds")
# 
# # Save data
# fst::write_fst(x = d_H_final, path = "survey-processed/ASEC/2019/ASEC_2019_H_processed.fst", compress = 100)
# 
# 
# # Compile universal
# 
# fusionData::compileDictionary()
# # fusionData::universe()
# 
# 
