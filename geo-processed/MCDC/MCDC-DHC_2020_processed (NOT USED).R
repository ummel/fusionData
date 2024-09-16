library(tidyverse)
library(labelled)
source("R/utils.R")

# To load the Census DHC variables

# https://mcdc.missouri.edu/data/dhc2020x/Variables.html
# Read the header and first row, which contains text descriptions of the variables
dnames <- read_csv("geo-raw/MCDC-DHC/dexter_2422502125_extract.csv", n_max = 1)

# Read the raw data
d <- read_csv("geo-raw/MCDC-DHC/dexter_2422502125_extract.csv", skip = 2, col_names = names(dnames), guess_max = 50e3)

# Assign variable labels via 'labelled' package
var_label(d) <- as.vector(dnames)

# Clean the data
result <- d %>%
  filter(TotPop > 0) %>%
  rename(county20 = county,
         tract20 = tract) %>%
  mutate(tract20 = gsub(".", "", tract20, fixed = TRUE),
         vintage = 2020L) %>%
  select(state, county20, tract20, vintage, PopPSqmi, MedianAge, starts_with("Pct"), OwnerVacrate, RentalVacrate) %>%
  mutate_if(is.numeric, convertInteger)

saveRDS(result, "geo-processed/MCDC-DHC/MCDC_2020_processed.rds")

#----------------

# Tract 2018-2022 estimates: https://mcdc.missouri.edu/cgi-bin/uexplore?/data/acs2022
#d <- read_csv("geo-raw/MCDC-DHC/dexter_2018-2022_tracts_extract.csv")
# v <- data.table::fread("geo-raw/MCDC-DHC/dexter_2018-2022_tracts_extract.csv", nrows = 1)
# d <- data.table::fread("geo-raw/MCDC-DHC/dexter_2018-2022_tracts_extract.csv", skip = 2, col.names = names(v))

# Percent of missing values

known.dupes <- c("pctOver5", "pctBornInDiffState", "pctNoCashRenter", "pctWorkAtHome", "pctNoComputer", "pctOthLang", "pctFBEnteredLT2000", "pctNonFamHHpop",
                 "pctNonFamHHs", "pctMales", "pctUSNative", "pctFullTimeWorkersMale", "pctHHPop", "pctHUsNoMort", "pctNonHispPop", "pctNotInLF",
                 "MobileHomesPerK", "pctOneRace", "pctNonCitizen", "pctNonInstUnder65", "pctUnder18", "pctUnits1Detached")

# Process the metadata
m <- read_csv("geo-raw/MCDC-DHC/2018-2022/dexter_2018-2022_metadata_extract.csv") %>%
  filter(!is.na(variable_format)) %>%
  rename(var = variable_name, label = short_label) %>%
  group_by(var) %>%
  slice(n()) %>%
  ungroup() %>%
  mutate(keep = !is.na(percent_variable) | grepl("^Average", label) | grepl("^Mean", label) | grepl("^Per-capita", label) | grepl("^Median", label) | grepl(" rate$", label) | grepl("^Per ", label) | grepl(" per ", label),
         keep = ifelse(label %in% c("Total population", "Total households", "Total housing units"), TRUE, keep),
         keep = ifelse(UnivVar %in% c('BornOutsideUS', 'BornOutsideUSNative', 'DiffHouse', 'GrandPrntsCaring', 'GrandPrntsLvngWithGrndkid', 'Over1', 'Spanish', 'USNative'), FALSE, keep),
         var = ifelse(!is.na(percent_variable), percent_variable, var),
         label = ifelse(!is.na(percent_variable), paste0(label, "; percent of ", tolower(universe_descr)), label),
         label = safeCharacters(label)) %>%
  #filter(keep & var %in% names(d)) %>%
  filter(keep & !var %in% known.dupes) %>%
  select(var, label) %>%
  distinct() %>%
  add_row(var = "years", label = "Time period of data collection", .before = TRUE) %>%  # Manually enter names of required identifier variables in Dexter
  add_row(var = "State", label = "State FIPS code", .before = TRUE) %>%
  add_row(var = "FipCo", label = "County FIPS code", .before = TRUE) %>%
  add_row(var = "Tract", label = "Census tract GEOID", .before = TRUE) %>%
  mutate_all(trimws)

# List of variable names to extract from Dexter
#write(m$var, file = "geo-raw/MCDC-DHC/test.txt")

# Variable names and labels
vlabs <- setNames(as.list(m$label), m$var)

# Function to safely convert dollar amounts (e.g. "$42,131") to numeric
clean <- function(x) {
  y <- suppressWarnings(readr::parse_number(x))
  ok <- all.equal(is.na(y), is.na(x) | x == "")
  if (ok) return(y) else return(x)
}

#-----

# Load the raw data
v <- data.table::fread("geo-raw/MCDC-DHC/2018-2022/dexter_2018-2022_tract_extract.csv", nrows = 0)
d <- data.table::fread("geo-raw/MCDC-DHC/2018-2022/dexter_2018-2022_tract_extract.csv", skip = 1,
                       col.names = names(v), colClasses = list(character = 1:4))

d <- d %>%
  mutate(Tract = gsub(".", "", Tract, fixed = TRUE)) %>%
  select(all_of(names(vlabs))) %>%
  mutate(across(where(is.character) & -any_of(c('years', 'State', 'FipCo', 'Tract')), clean))  # Convert dollar amounts to numeric, if possible

# Determine which variable pairs are perfectly correlated (mutually exclusive) and remove one of the variables (e.g. Percent under 18; Percent over 18)
# https://stackoverflow.com/questions/32993097/determine-and-group-perfectly-correlated-variable-efficiently
cmat <- abs(cor(d[1:min(5e3, nrow(d)), -c(1:4)], use = "pairwise.complete.obs"))
grps <- unique(lapply(rownames(cmat), function(rname) { colnames(cmat)[cmat[rname, ] > 0.99999]}))
grps <- grps[lengths(grps) == 2]
drop <- map_chr(grps, 2)
vlabs <- vlabs[setdiff(names(vlabs), drop)]

# Remove the perfect correlates and reduce resolution
d <- d %>%
  select(all_of(names(vlabs))) %>%
  mutate_if(is.numeric, convertInteger) %>%
  mutate_if(is.double, cleanNumeric, tol = 0.001) %>%
  set_variable_labels(.labels = vlabs, .strict = TRUE)

test <- d %>%
  filter(State != "", TotPop != 0) %>%   # Empty rows???
  rename(vintage = years, state = State, county20 = FipCo, tract20 = Tract) %>%
  arrange(state, county20, tract20) %>%
  select(vintage, state, county20, tract20, everything())

# Save to disk
saveRDS(test, "geo-processed/MCDC-DHC/MCDC_2018-2022_ACS_processed.rds")





