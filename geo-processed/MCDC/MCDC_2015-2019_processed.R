library(tidyverse)
library(data.table)
library(labelled)
source("R/utils.R")

#----------------

# Tract 2015-2019 estimates: https://mcdc.missouri.edu/cgi-bin/uexplore?/data/acs2019

# File path to MCDC metadata file for the vintage to be downloaded and processed
meta <- "geo-raw/MCDC/2015-2019/dexter_2015-2019_metadata.csv"

# Variables known to be mutually exclusive of other variables, so they can be safely ignored
ignore <- c("pctOver5", "pctBornInDiffState", "pctNoCashRenter", "pctWorkAtHome", "pctNoComputer", "pctOthLang", "pctFBEnteredLT2000", "pctNonFamHHpop",
            "pctNonFamHHs", "pctMales", "pctUSNative", "pctFullTimeWorkersMale", "pctHHPop", "pctHUsNoMort", "pctNonHispPop", "pctNotInLF",
            "MobileHomesPerK", "pctOneRace", "pctNonCitizen", "pctNonInstUnder65", "pctUnder18", "pctUnits1Detached")

# Process the metadata
m <- read_csv(meta) %>%
  filter(!is.na(variable_format)) %>%
  rename(var = variable_name, label = short_label) %>%
  mutate(ID = row_number()) %>%
  group_by(var) %>%
  slice(n()) %>%
  ungroup() %>%
  arrange(ID) %>%
  mutate(keep = !is.na(percent_variable) | grepl("^Average", label) | grepl("^Mean", label) | grepl("^Per-capita", label) | grepl("^Median", label) | grepl(" rate$", label) | grepl("^Per ", label) | grepl(" per ", label),
         keep = ifelse(label %in% c("Total population", "Total households", "Total housing units"), TRUE, keep),
         keep = ifelse(UnivVar %in% c('BornOutsideUS', 'BornOutsideUSNative', 'DiffHouse', 'GrandPrntsCaring', 'GrandPrntsLvngWithGrndkid', 'Over1', 'Spanish', 'USNative'), FALSE, keep),
         var = ifelse(!is.na(percent_variable), percent_variable, var),
         label = ifelse(!is.na(percent_variable), paste0(label, "; percent of ", tolower(universe_descr)), label),
         label = safeCharacters(label)) %>%
  filter(keep & !var %in% ignore) %>%
  select(var, label) %>%
  distinct() %>%
  add_row(var = "Tract", label = "Census tract GEOID", .before = TRUE) %>%
  add_row(var = "FipCo", label = "County FIPS code", .before = TRUE) %>%
  add_row(var = "State", label = "State FIPS code", .before = TRUE) %>%
  add_row(var = "vintage", label = "Time period of data collection", .before = TRUE) %>%
  mutate_all(trimws)

# List of variable names to extract from Dexter
write(setdiff(m$var, "vintage"), file = sub("_metadata.csv", "_variables.txt", meta))

# Variable names and labels
vlabs <- setNames(as.list(m$label), m$var)

#-----

fpath <- sub("_metadata.csv", "_tract.csv.zip", meta)

# Load the raw data
v <- names(data.table::fread(fpath, nrows = 0))
d <- data.table::fread(fpath, skip = 1, col.names = v, colClasses = list(character = 1:3))

# Function to safely convert dollar amounts (e.g. "$42,131") to numeric
clean <- function(x) {
  y <- suppressWarnings(readr::parse_number(x))
  ok <- all.equal(is.na(y), is.na(x) | x == "")
  if (ok) return(y) else return(x)
}
d <- d %>%
  select(any_of(names(vlabs))) %>%
  mutate(across(where(is.character) & -any_of(c('State', 'FipCo', 'Tract')), clean))  # Convert dollar amounts to numeric, if possible

# Determine which variable pairs are perfectly correlated (mutually exclusive) and remove one of the variables (e.g. Percent under 18; Percent over 18)
# https://stackoverflow.com/questions/32993097/determine-and-group-perfectly-correlated-variable-efficiently
cmat <- abs(cor(d[1:min(5e3, nrow(d)), -c(1:3)], use = "pairwise.complete.obs"))
grps <- unique(lapply(rownames(cmat), function(rname) {colnames(cmat)[cmat[rname, ] > 0.99999]}))
grps <- grps[lengths(grps) == 2]
drop <- map_chr(grps, 2)
vlabs <- vlabs[setdiff(names(vlabs), drop)]

# Determine the data years/vintage from file path
years <- strsplit(basename(fpath), "_", fixed = TRUE)[[1]][[2]]

# Remove the perfect correlates and reduce resolution
d <- d %>%
  filter(State != "", TotPop != 0) %>%   # Remove any empty rows (only noticed this with 2018-2022 data)
  mutate(vintage = years,
         FipCo = substring(FipCo, 3, 5),  # Remove state FIPS at front (result is only 3 digits)
         Tract = gsub(".", "", Tract, fixed = TRUE)) %>%
  select(all_of(names(vlabs))) %>%
  mutate_if(is.numeric, convertInteger) %>%
  mutate_if(is.double, cleanNumeric, tol = 0.001) %>%
  set_variable_labels(.labels = vlabs, .strict = TRUE) %>%
  arrange(vars(1:4))

# Rename the FipCo and Tract variables depending on the Census geography vintage they reference
ymax <- substring(years, 6, 10)
if (ymax >= 2020) d <- rename(d, state = State, county20 = FipCo, tract20 = Tract)
if (ymax %in% 2010:2019) d <- rename(d, state = State, county10 = FipCo, tract10 = Tract)
if (ymax <= 2009) d <- rename(d, state = State, county00 = FipCo, tract00 = Tract)

# Identify and drop any variables that are all NA or show no variance
drop <- names(which(colSums(is.na(d)) == nrow(d) | map_lgl(d, novary)))
drop <- setdiff(drop, names(d)[1:4])
d <- d %>% select(-all_of(drop))

# Save to disk
saveRDS(d, file.path("geo-processed/MCDC", sub("XXXX", years, "MCDC_XXXX_tract_processed.rds")), compress = TRUE)
