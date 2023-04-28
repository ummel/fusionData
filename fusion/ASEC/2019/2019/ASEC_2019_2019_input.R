# make sure fusionData and fusionModel are up to date:

#devtools::install_github("ummel/fusionModel")
rm(list = ls())
## -> pull latest repo
#fusionData::installPackage() # this is an internal function
#Warning message:
#  In runHook(".onLoad", env, package.lib, package) :
#  internal error -3 in R_decompress1library(fusionData)
library(fusionData)
library(tidyverse)

# Set Up ----

# Number of cores to use
ncores <- 1

# Donor and recipient survey identifiers
donor <- "ASEC_2019"
recipient <- "ACS_2019"

# test or full run
TEST <- FALSE

# Old prep() and assemble () ----
# this takes several hours to run
# run's through without error as of 2/11/23
# debugging in fusionData\survey-processed\ASEC\2019\harmony_checks.R

# # debugging arguments
# donor = "ASEC_2019"
# recipient = "ACS_2019"
# #respondent = "person"
# implicates = 1
# collapse = FALSE
# 
# for (type in c("household", "person")){
#   
#   ## check harmonies for household data
#   prep <- prepare(donor = donor, recipient = recipient, respondent = type)
#   save(prep, file = paste0("./survey-processed/ASEC/2019/prep_output_", type, ".rda"))
#   
#   data <- assemble(prep)
#   #save(data, file = "./survey-processed/ASEC/2019/assemble_output_person.rda")
#   
# }

# New fusionInput() ----

## ~ list of vars ----

# list of ASEC variables that we always want as predictors
force_vec <- c("hhincome", "race", "educ", "numprec", "hhtenure", "state")

# list of variables to fuse - only ones that require agregation
fuse_vec <- c("hipval", "moop", "schllunch", "kidcneed", "srcpen1", "srcpen2", 
              "srcret1", "srcret2", "filestat", "whyabsnt",
              "spmcaphous", "spmcapxpns", "spmchsup", "spmchxpns", "spmchsup", 
              "spmchxpns", "spmftotval", "spmmedxpns", "spmmort", "spmpov", "spmsnap", "spmwic", "spmwkxpns", 
              "incasist", "incbus", "inccapg", "incchild", "incdisab", "incdivid", "inceduc", "incfarm", 
              "incint", "incpens", "incrann", "incrent", "incret1", "incret2", 
              "incsurv", "incother", "incunemp", "incvet", "incwkcom", "incretir", 
              "incpen1", "incpen2", "incrint1", "incrint2",
              "retcont", "health", "fedtaxac", "adjginc", "stataxac", 
              "fedretir", "fica", "heatsub", "heatval", "pubhous", "rentsub") %>%
  unique()

# check all variables exist in ASEC (either P or H)
dP <- fst::read_fst("./survey-processed/ASEC/2019/ASEC_2019_P_processed.fst", from = 1, to = 1)
dH <- fst::read_fst("./survey-processed/ASEC/2019/ASEC_2019_H_processed.fst", from = 1, to = 1)
fuse_vec %in% c(names(dP), names(dH))

# check correct number of vars
length(fuse_vec) == 56

## ~ custom aggregations ----

# make the list of custom aggregations needed 
agg_adj_list <- list(
  # numeric variables where just want *family* reference person
  hipval = ~if.else(duplicated(data.table(asec_2019_hid, famid)), 0, hipval),
  moop = ~if.else(duplicated(data.table(asec_2019_hid, famid)), 0, moop),
  schllunch = ~if.else(duplicated(data.table(asec_2019_hid, famid)), 0, schllunch),
  # numeric variables where just want *SPM unit* reference person
  spmcaphous = ~if.else(duplicated(spmfamunit), 0, spmcaphous),
  spmcapxpns = ~if.else(duplicated(spmfamunit), 0, spmcapxpns),
  spmchsup = ~if.else(duplicated(spmfamunit), 0, spmchsup),
  spmchxpns = ~if.else(duplicated(spmfamunit), 0, spmchxpns),
  spmftotval = ~if.else(duplicated(spmfamunit), 0, spmftotval),
  spmmedxpns = ~if.else(duplicated(spmfamunit), 0, spmmedxpns),
  spmsnap = ~if.else(duplicated(spmfamunit), 0, spmsnap),
  spmwic = ~if.else(duplicated(spmfamunit), 0, spmwic),
  spmwkxpns = ~if.else(duplicated(spmfamunit), 0, spmwkxpns),
  # factor variables where just want *SPM unit* reference person - not sure this will work b/c NA
  spmmort = ~if.else(duplicated(spmfamunit), NA, spmmort),
  spmpov = ~if.else(duplicated(spmfamunit), NA, spmpov),  
  # misc. adjustments
  kidcneed = ~if.else(kidcneed == "NIU: Over 14", "No", kidcneed)
)

agg_fun_list = list(
  kidcneed = "mode",
  # variable where want household reference - default would be max b/c they're ordered factors
  srcpen1 = "ref",
  srcpen2 = "ref",
  srcret1 = "ref",
  srcret2 = "ref",
  filestat = "ref",
  whyabsnt = "ref"
)


# # if running test, only text fusion on the aggregation vars
# if (TEST == T){
#   fuse_vec <- c(names(agg_adj_list), names(agg_fun_list)) %>% unique()
# }

# RUN ----
#getSurveyProcessed(survey = "ASEC_2019")
#getSurveyProcessed(survey = "ACS_2019")

input.dir <- fusionInput(donor = "ASEC_2019",
                   recipient = "ACS_2019",
                   respondent = "household",
                   force = force_vec, 
                   fuse = fuse_vec,
                   agg_adj = agg_adj_list,
                   agg_fun = agg_fun_list,
                   ncores = 1,
                   test_mode = FALSE,
                   note = "ASEC full run - household")

# check _train ---- 
if (1 == 0){
  d_train <- fst::read_fst("fusion_/ASEC/2019/2019/H/input/ASEC_2019_2019_H_train.fst") %>%
    select(spmcaphous:spmmedxpns)
  names(d_train) <- paste0(names(d_train), "_agg")
  
  # read in first 10K households from survey_processed
  dH <- fst::read_fst("survey-processed/ASEC/2019/ASEC_2019_H_processed.fst", from = 1, to = 10000, as.data.table = T) %>%
    select(asec_2019_hid)
  
  d <- cbind(dH, d_train) 
  names(d)
  
  # read in person level data and limit to first 10K asec_2019_hid
  dP <- fst::read_fst("survey-processed/ASEC/2019/ASEC_2019_P_processed.fst", as.data.table = T) 
  
  vars <- c('asec_2019_hid', 'pid', 'famid', 'spmfamunit', fuse_vec)
  dP <- dP[asec_2019_hid %in% unique(d$asec_2019_hid), ..vars]
  
  # merge in aggregations
  d <- merge(d, dP, by = 'asec_2019_hid')
  
  # check SPM sums versions 
  
  
  d %>%
    group_by(asec_2019_hid) %>%
    mutate(dist = n_distinct(spmfamunit)) %>%
    filter(dist > 1) %>%
    select(asec_2019_hid, famid, spmfamunit, spmmort, spmmort_agg) %>%
    head(n = 20)
}



# prep list of variables to fuse
# prep list of aggregations (above)
# change code so can be run in test or full 
# 

# dir
# out.dir <- fusionOutput(input = dir, ncores = 1)
# out.dir
# 
# fsd.file <- list.files(out.dir, full.names = TRUE)[1]
# fused <- fusionModel::read_fsd(fsd.file)
