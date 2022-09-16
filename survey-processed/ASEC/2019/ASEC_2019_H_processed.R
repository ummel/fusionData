library(tidyverse)
source("R/utils.R")
source("R/createDictionary.R")
source("R/imputeMissing.R")
source("R/detectDependence.R")

library(pacman)
p_load(here, tidyverse, data.table, ipumsr, fusionData, fusionModel)

# Download IPUMS data using the ipumsr package

# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")
ddi <- read_ipums_ddi("~/Downloads/drive-download-20220720T171049Z-001/cps_00057.xml", lower_vars = T)
data <- read_ipums_micro(ddi)

#-----


# Collect only household data

pstart <- which(names(data) == 'pernum')
d_H <- data[,1:pstart-1]

# Collect only household variable information

var_info <- ddi$var_info
var_info <- var_info[1:pstart-1, ]
var_info <- as.data.table(var_info)

#-----

# Manually identify the potential location variables found in geo_concordance.R

geographic_variables <- d_H %>% dplyr::select(region, statefip, metro, metarea, county, 
                                              statecensus, cbsasz, metfips)

d_H <- d_H %>%
  cbind(geographic_variables %>%
  rename_all(list( ~paste0(., "_labelled"))) %>% 
  mutate_all(~ as_factor(lbl_clean(.))))

d_H <- d_H %>%
  mutate(county = stringr::str_sub(county,start=-3)) %>%
  rename(
    recs_division = region,
    state = statefip,
    state_name = statefip_labelled,
    county14 = county
  )

# Drop labelled variables
d_H <- d_H %>%
  select(!contains("labelled"))

# See which variables in 'd_H' are also in 'geo_concordance' and
gnames <- names(fst::fst("geo-processed/concordance/geo_concordance.fst"))
gvars <- intersect(gnames, names(d_H))

# Class new/added geo identifiers as unordered factors
d_H <- d_H %>%
  mutate_at(gvars, ~ factor(.x, levels = sort(unique(.x)), ordered = FALSE))

#-----

# Work only with the non-replication weight variables and remove Q variables

replication_weight_vars <- d_H %>% dplyr::select(starts_with("repwt"))
non_replication_weight_vars <- d_H %>% dplyr::select(-starts_with("repwt"),
                                                     -starts_with("q"))

value_check <- non_replication_weight_vars %>%
  mutate_all(~ as_factor(lbl_clean(.))) 

# metro
levels(value_check$metro) <- sub("Not identifiable", "NIU: Metro status is not identifiable",
                                 levels(value_check$metro))

# metarea
# IPUMS people confirmed that the missing data outcome was supposed to be coded as another value
levels(value_check$metarea) <- sub("Missing data", "Other metropolitan areas, unidentified",
                                   levels(value_check$metarea))
levels(value_check$metarea) <- sub("NIU, household not in a metropolitan area", "NIU: Household is not in a metropolitan area",
                                   levels(value_check$metarea))

# county
levels(value_check$county14) <- sub("^0$", "NIU: County is not identifiable", 
                                    levels(value_check$county14))
# cbsasz
levels(value_check$cbsasz) <- sub("Not identified or nonmetropolitan", "NIU: Nonmetropolitan or is not identifiable",
                                  levels(value_check$cbsasz))

# metfips
levels(value_check$metfips) <- sub("Unidentified or nonmetropolitan", "NIU: Nonmetropolitan or is not identifiable",
                                   levels(value_check$metfips))

# NIU values: pubhous, rentsub, stampmo, atelunch, lunchsub

NIU_variables_list <- list(
  pubhous = "Not living in public housing", #NIU
  rentsub = "Not paying lower rent due to government subsidy", #NIU
  stampmo = "Did not recieve food stamps", #NIU
  atelunch = "No household members are school aged 5 to 18", #NIU
  lunchsub = "No household members are school aged 5 to 18 or none ate a complete hot lunch"
)

# frelunch
levels(value_check$frelunch) <- sub("NIU -- Children didn't eat hot lunch", "NIU: No household members ate a complete hot lunch",
                                   levels(value_check$frelunch))
levels(value_check$frelunch) <- sub("NIU -- No children in hh", "NIU: No household members are school aged 5 to 18",
                                    levels(value_check$frelunch))

# nfams
levels(value_check$nfams) <- sub("1 family or N/A", "1 or no families",
                                    levels(value_check$nfams))

# ncouples
levels(value_check$ncouples) <- sub("0 couples or NIU", "No couples or is not identifiable",
                                 levels(value_check$ncouples))

# nmothers
levels(value_check$nmothers) <- sub("0 mothers or NIU", "No mothers or is not identifiable",
                                    levels(value_check$nmothers))

# nfathers
levels(value_check$nfathers) <- sub("0 fathers or NIU", "No fathers or is not identifiable",
                                    levels(value_check$nfathers))

# Change all NIU-only values
for (x in names(NIU_variables_list)){
  NIU_variables_list[x] <- paste0('NIU: ', NIU_variables_list[x])
  levels(value_check[, x]) <- sub("NIU", NIU_variables_list[x], levels(value_check[, x]))
}

# Match variable names as done in person level ASEC 2019 data
data.table::setnames(value_check, 'asecwth', 'weight')
data.table::setnames(value_check, 'serial', 'asec_2019_hid')

# No missing values
# colnames(value_check)[colSums(is.na(value_check)) > 0] 

# Add the replication weight variables back

value_check <- value_check %>% cbind(replication_weight_vars)

var_info$var_name[which(var_info$var_name == "serial")] <- "asec_2019_hid"
var_info$var_name[which(var_info$var_name == "asecwth")] <- "weight"
var_info$var_name[which(var_info$var_name == "region")] <- "recs_division"
var_info$var_name[which(var_info$var_name == "statefip")] <- "state"
var_info$var_name[which(var_info$var_name == "county")] <- "county14"

value_check_without_state_name <- value_check %>% dplyr::select(-state_name)

col_order <- c()
for (i in var_info[,"var_name"]) {col_order <- c(col_order, i)}
# Check if it works:
# col_order

# Drop flags

flagvars <- col_order[str_starts(col_order, 'q')]
col_order <- col_order[!col_order %in% flagvars]
var_info <- var_info %>% filter(!str_starts(var_name, 'q'))

# Remove duplicates based on the household serial number

value_check_without_state_name <- value_check_without_state_name[!duplicated(value_check_without_state_name$asec_2019_hid), ]

# Weight is a numerical value

value_check_without_state_name <- value_check_without_state_name %>% mutate(weight = as.numeric(weight))

# Add labels from IPUMS

value_check_without_state_name <- value_check_without_state_name[,col_order]

# Turn integer variables from factor type

value_check_without_state_name <- value_check_without_state_name %>%
  mutate(
    year = as.numeric(year),
    asec_2019_hid = as.numeric(asec_2019_hid),
    cpsid = as.integer(cpsid),
    weight = as.integer(weight),
    mish = ordered(mish),
    numprec = as.integer(numprec),
    cpi99 = as.integer(cpi99),
    hhincome = as.integer(hhincome),
    heatval = as.integer(heatval),
    stampno = ordered(stampno),
    stampval = as.integer(stampval),
    faminc = as.integer(faminc),
    nfams = ordered(nfams),
    ncouples = ordered(ncouples),
    nmothers = ordered(nmothers),
    nfathers = ordered(nfathers),
    hrhhid = as.integer(hrhhid),
    hrhhid2 = as.integer(hrhhid2),
    marbasecidh = as.integer(marbasecidh),
    hseq = as.integer(hseq)
  ) %>%
  mutate(across(starts_with("repwt"), as.integer))

value_check_state_name <- labelled::set_variable_labels(.data = value_check_without_state_name, 
                                                        .labels = setNames(as.list(var_info$var_label),
                                                                           names(value_check_without_state_name)))

d_H_final <- value_check_state_name %>% 
  rename(state_name = statecensus)


# Create dictionary 
dictionary <- fusionData::createDictionary(data = d_H_final, survey = "ASEC", vintage = 2019, respondent = "H")

saveRDS(dictionary, file = "survey-processed/ASEC/2019/ASEC_2019_H_dictionary.rds")

# Save data
fst::write_fst(x = d_H_final, path = "survey-processed/ASEC/2019/ASEC_2019_H_processed.fst", compress = 100)

# Compile universal

# fusionData::compileDictionary()
# fusionData::universe()


