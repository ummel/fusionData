## Script to check harmonies
rm(list = ls())
options(scipen=999)
library(fusionData)
library(pacman)
p_load(tidyverse, data.table)
#source("R/assignLocation.R")

# debugging arguments
donor = "ASEC_2019"
recipient = "ACS_2019"
respondent = "household"
implicates = 1
collapse = FALSE
# 
# for (type in c("household")){
#   
#   ## check harmonies for household data
#   prep <- prepare(donor = donor, recipient = recipient, respondent = type)
#   save(prep, file = paste0("./survey-processed/ASEC/2019/prep_output_", type, ".rda"))
# 
#   data <- assemble(prep)
#   save(data, file = "./survey-processed/ASEC/2019/assemble_output_person.rda")
# 
# }

# Harmonize data for specified donor and recipient surveys
#harmonized <- harmonize(harmony.file = paste0(donor, "__", recipient, ".R"), respondent = respondent)

# # fix issue with nativity__nativity having more levels in ASEC than in ACS
# harmonized$ASEC_2019$nativity__nativity %>% levels()
# harmonized$ASEC_2019 <- harmonized$ASEC_2019 %>%
#   mutate(nativity__nativity = if_else(nativity__nativity != 3, nativity__nativity, factor(2)),
#          nativity__nativity = factor(nativity__nativity, levels = c(1, 2)))
# levels(harmonized$ASEC_2019$nativity__nativity)
# saveRDS(harmonized, "./survey-processed/ASEC/2019/harmonize_output_v2.rds")

# # Impute PUMA for the donor
# harmonized <- readRDS("survey-processed/ASEC/2019/harmonize_output_v2.rds")
# location.data <- assignLocation(harmonized = harmonized, m = implicates, collapse = collapse)
# saveRDS(location.data, "./survey-processed/ASEC/2019/assignLocation_output.rds")
# 



# copied from harmonize.R for debugging
harmony.file = paste0(donor, "__", recipient, ".R")
output = "both"
type = "recipient"
respondent = "household"

  # Load the harmonization list
H <- dget(file.path("harmony/harmonies", harmony.file))

# Check inputs
stopifnot({
  respondent %in% c("household", "person")
  output %in% c("both", "donor", "recipient")
})

HH <- tolower(respondent) == "household"

# Internal helper function
splitNames <- function(x) strsplit(x, "__", fixed = TRUE)

# Get surveys from file name
survey <- unlist(splitNames(gsub(".R", "", basename(harmony.file), fixed = TRUE)))

# Check if donor microdata available for specified 'respondent' argument
temp <- list.files(path = "survey-processed", pattern = paste(survey[1], ifelse(HH, "H", "P"), "processed.fst", sep = "_"), recursive = TRUE)
if (length(temp) == 0) stop("No ", respondent, "-level microdata available for ",  survey[1], " survey")

# Details about each variable involved in harmonies
hnames <- names(H)

# Load dictionary in order to tag each variable as household- or person-level
#data("dictionary", package = "fusionData")
load("data/dictionary.rda")
dict.vars <- paste(paste(dictionary$Survey, dictionary$Vintage, sep = "_"), dictionary$Variable)

dnames <- map_chr(splitNames(hnames), 1)
dind <- match(paste(survey[1], dnames), dict.vars)
dres <- dictionary$Respondent[dind]

rnames <- map_chr(splitNames(hnames), 2)
rind <- match(paste(survey[2], rnames), dict.vars)
rres <- dictionary$Respondent[rind]

#-----

# Sequentially process donor and/or recipient microdata by 'type' (donor or recipient)

# Placeholder results list
do.types <- if (output == "both") c("donor", "recipient") else output
result <- vector(mode = "list", length = length(do.types))
names(result) <- do.types

j <- ifelse(type == "donor", 1, 2)
vnames <- if (j == 1) dnames else c(rnames, "state", "puma10")
vres <- if (j == 1) dres else rres

# Print message to console
cat("Harmonizing ", survey[j], " (", type, ") microdata at ", respondent, " level\n", sep = "")

# Load household data (NULL if unavailable or unnecessary)
hpath <- list.files(path = "survey-processed", pattern = paste(survey[j], "H", "processed.fst", sep = "_"), recursive = TRUE, full.names = TRUE)
dh <- if (length(hpath)) fst::fst(hpath) else NULL

# Load person data (NULL if unavailable or unnecessary)
ppath <- list.files(path = "survey-processed", pattern = paste(survey[j], "P", "processed.fst", sep = "_"), recursive = TRUE, full.names = TRUE)
dp <- if (length(ppath)) fst::fst(ppath) else NULL

# Identify the household and person identifier columns
s <- strsplit(tolower(survey[j]), "_")[[1]][1]
hid <- grep(paste0("^", s, ".*_hid$"), names(if (HH) dh else dp), value = TRUE)

# "adj" slot values (across all variables)
# Used only to load necessary variables to evaluate "adj" custom code
adj <- H %>%
  map(~ .x[[j]]$adj) %>%
  map(~ strsplit(gsub("[^[:alnum:] ]", " ", .x), "\\s+")[[1]]) %>%
  compact() %>%
  unlist()

# Variables (and potential matching strings) to load from disk
# This includes standard household and person identifier variables
#m <- paste(str_squish(c(vnames, adj, hid, "pid", "weight")), collapse = " ")
m <- c(vnames, adj, hid, "pid", "weight")

# Load household data (NULL if unavailable or unnecessary)
# Note that 'k' should ALWAYS return at least 1 column (hid), but > 1 needed to pull any data from disk
# NOTE: The grepl() call below is safe, but it can lead to unnecessary variables being loaded into 'dh'
#k <- sapply(names(dh), function(n) grepl(n, m, fixed = TRUE)) # Variables to load from disk
#dh <- if (length(k) > 1) dh[names(which(k))] else NULL
k <- intersect(names(dh), m)
dh <- if (length(k) > 1) dh[k] else NULL

# Load person data (NULL if unavailable or unnecessary)
# Note that 'k' should ALWAYS return at least 2 columns (hid and pid), but > 2 needed to pull any data from disk
# NOTE: The grepl() call below is safe, but it can lead to unnecessary variables being loaded into 'dp'
# k <- sapply(names(dp), function(n) grepl(n, m, fixed = TRUE)) # Variables to load from disk
# dp <- if (length(k) > 2) dp[names(which(k))] else NULL
k <- intersect(names(dp), m)
dp <- if (length(k) > 1) dp[k] else NULL

# Ensure all data sorted by 'hid'
if (!is.null(dh)) dh <- arrange(dh, across(all_of(hid)))
if (!is.null(dp)) dp <- arrange(dp, across(all_of(hid)))

# Ensure that 'dh' and 'dp' data refer to the same set of households
if (!is.null(dh) & !is.null(dp)) {
  ids <- intersect(dh[[hid]], dp[[hid]])
  dh <- filter(dh, dh[[hid]] %in% ids)
  dp <- filter(dp, dp[[hid]] %in% ids)
  if (is.factor(dh[[hid]])) {
    dh[[hid]] <- factor(dh[[hid]], levels = sort(unique(dh[[hid]])))
    dp[[hid]] <- factor(dp[[hid]], levels = levels(dh[[hid]]))
  }
}

# HH replicator index vector
# Used to replicate household observations when 'respondent' = "Person"
hh.rep <- if (!HH) match(dp[[hid]], dh[[hid]]) else NULL

# Update relationship variable to logical indicating reference person (or GQ observation), if necessary
# This should/must only be applicable in the case of ACS person-data being used to generate a HH-level variable
vrel <- "ref__person"
if (any(vres == "Person")) {
  dp[[vrel]] <- dp$pid == 1  # The reference person is always assigned 'pid' value of 1
}

# Safety checks
stopifnot({
  !is.unsorted(dh[[hid]])
  !is.unsorted(dp[[hid]])
})


#-----

# Function to perform harmonization of actual data for harmony 'i' in H
makeHarmony <- function(i) {
  
  print(paste0("working on ", i))
  
  v <- vnames[i]
  hh <- vres[i] == "Household"  # Is the variable being processed household-level?
  h <- H[[i]][[j]]
  
  # Assign "" value for 'agg' slot if none exists
  if (length(h$agg) == 0) h$agg <- ""
  
  #---
  
  # Correct output number of observations in 'x'
  N <- if (HH) nrow(dh) else nrow(dp)
  
  #---
  
  # Create 'x' vector to be modified below and then assigned to output
  if (h$adj == "") {
    x <- if (hh) dh[[v]] else dp[[v]]
  } else {
    # Apply 'adj' custom function, if necessary
    # See here: https://stackoverflow.com/questions/49469982/r-using-a-string-as-an-argument-to-mutate-verb-in-dplyr
    x <- if (hh) dh else dp
    x <- mutate(x, !!v := !!rlang::parse_quo(h$adj, env = rlang::caller_env()))[[v]]
  }
  
  # h$adj = "if_else(age != \"15\", classwkr, factor(\"NIU: Less than 15 or never worked\",\nordered = T)\n)"
  # h$adj <- "if_else(age != \"15\", classwkr, factor(\"NIU: Less than 15 or never worked\", levels = levels(classwkr),\nordered = T)\n)"
  # test <- mutate(x, classwkr := if_else(age != "15", classwkr, factor("NIU: Less than 15 or never worked", levels = levels(classwkr), ordered = T)))[[v]]
  # table(x, useNA = 'always')
  # class(x)
  
  # # this doesn't work b/c it converts the variables to an integer 
  # x <- mutate(x, classwkr := ifelse(age != "15", classwkr, factor("NIU: Less than 15 or never worked")))[[v]]
  # table(x, useNA = 'always')
  # class(x)
  
  #---
  
  # Process/harmonize 'x' using information in 'h'
  # If h$groups = 1, it is a numeric match and no modification is needed except to apply convertPercentile()
  if (length(h$groups) > 1) {
    
    if (h$breaks[1] == "") {
      
      x <- h$groups[match(x, h$levels)]
      
    } else {
      
      x <- findInterval(x, h$breaks) + 1L  # This assigns groups starting with 1
      x <- h$groups[x]  # This ensures the group assignment matches unique values in h$groups (e.g. starting at 0)
      
    }
    
    # Class 'x' as a factor variable, if specified
    if (h$agg %in% c("", "reference", "min", "max")) {
      x <- factor(x, levels = sort(unique(h$groups)), ordered = H[[i]]$ordered == TRUE)
    }
    
  }
  
  #---
  
  # Either aggregate or replicate the result in accordance with 'respondent'
  
  # Cases where we need to aggregate to household level...
  if (!hh & (HH | dres[i] == "Household")) {
    
    # Determine the default aggregation function when none is specified
    if (h$agg == "") {
      if (is.numeric(x)) h$agg <- 'mean'
      if (is.factor(x)) h$agg <- 'reference'
      if (is.ordered(x)) h$agg <- 'max'
    }
    
    # Aggregate person-level 'x' up to household level using specified method in h$agg (or default)
    if (h$agg == "reference") {
      ind <- which(dp[[vrel]])
      x <- x[ind]
    } else {
      
      # Aggregate 'x' for each household using h$agg as the aggregator function
      y <- as.vector(tapply(X = x, INDEX = dp[[hid]], FUN = get(h$agg)))
      
      # If 'x' is a factor, preserve levels and ordering of the output
      if (is.factor(x)) {
        x <- factor(levels(x)[y], levels = levels(x), ordered = is.ordered(x))
      } else {
        x <- y
      }
    }
    
    # Set 'hh' to TRUE, since 'x' is now aggregated to household level
    hh <- TRUE
    
  }
  
  # Cases where we want person-level output and 'x' is HH-level; replicate 'x' for each household
  if (!HH & hh) x <- x[hh.rep]
  
  #---
  
  # Check for valid length in 'x'
  if (anyNA(x) | length(x) != N) stop("There is a problem with output 'x' for variable ", v, " (type = ", type, ")")
  
  return(x)
  
}

# test run
for (i in 1:length(H)) makeHarmony(i)
makeHarmony(57)

# check ones that have ACS adjustments- this is where errors are coming up
# ddrs - i = 9
i = 8 # issue for this
makeHarmony(9)



dp %>%
  mutate(test = if_else(agep <= 49, wkl, 
                        factor("Less than 16 years old", ordered = T))) %>%
  select(agep, wkl, test)

"if_else(agep < 15, factor(\"NIU: Less than 15\", levels = c(\"NIU: Less than 15\", levels(schl)), ordered=T\n), factor(schl, levels = c(\"NIU: Less than 15\", levels(schl)))\n)"

