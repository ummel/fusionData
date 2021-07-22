# Process the CEX data dictionary (ce_pumd_interview_diary_dictionary.xlsx)
# Outputs:
#   1) "var_info.RData": tibble providing meta information about universe of CEX variables
#   2) "var_desc.RData": tibble providing description of each non-UCC variable (merely a convenient subset of 'var_info')

#---

# Extract metadata for valid (2014 onward) variables in data dictionary
vars <- readxl::read_excel(path = "survey-raw/CEX/ce_pumd_interview_diary_dictionary.xlsx", sheet = 2, guess_max = Inf) %>%
  rename(source = Survey, file = File, var = `Variable Name`, var_desc = `Variable description`, formula = Formula, flag = `Flag name`,
         section = `Section description`,first_year = `First year`, first_qtr = `First Quarter`, last_year = `Last year`, last_qtr = `Last quarter`) %>%
  mutate(source = substring(source, 1, 1)) %>%
  select(source, file, section, var, var_desc, formula, flag, first_year, first_qtr, last_year, last_qtr) %>%
  distinct()  # Safety: Removes any erroneous duplicate entries (e.g. var = "WAGE_SP")

# This is a safety procedure to ensure there are no duplicate, non-deprecated codes remaining in 'vars'
# When duplicates occur, it retains only the most recently-added entry
vars.bup <- vars
vars <- vars %>%
  arrange(file, var, -first_year, -first_qtr) %>%
  group_by(source, file, var) %>%
  slice(1) %>%
  ungroup()

# The variables dropped by the safety filter (above) should be reported to BLS
# These are cases where there are duplicate, non-deprecated variable entries (i.e. the flagged variables need to be deprecated)
error.vars <- anti_join(vars.bup, vars) %>%
  inner_join(select(vars, file, var)) %>%
  filter(is.na(last_year)) %>%
  select(-var_desc, -formula)

# Write potential errors to disk
if (nrow(error.vars) > 0) write_csv(error.vars, paste0("survey-processed/CEX/errors/", Sys.Date(), " CEX VARIABLES flagged", ".csv"))

#-----

# Extract variable-specific codes for valid variables in data dictionary
codes <- readxl::read_excel(path = "survey-raw/CEX/ce_pumd_interview_diary_dictionary.xlsx", sheet = 3, guess_max = Inf) %>%
  rename(source = Survey, file = File, var = `Variable`, code = `Code value`, code_desc = `Code description`,
         first_year = `First year`, first_qtr = `First quarter`, last_year = `Last year`, last_qtr = `Last quarter`) %>%
  mutate(source = substring(source, 1, 1)) %>%
  select(source, file, var, code, code_desc, first_year, first_qtr, last_year, last_qtr) %>%
  distinct()  # Safety: Removes any erroneous duplicate entries

#-----

# !!! MANUAL FIX: Codes for recent vehicle years (OVB file) are apparently missing
# This code adds them automatically as necessary
# Issue has been reported to CEX staff
veh.yrs <- sort(filter(codes, var == "VEHICYR")$code_desc)
cex.yrs <- str_sub(grep("\\d{4}$", list.dirs("survey-raw/CEX",recursive = FALSE), value = TRUE), start = -4, end = -1)
cex.yrs <- c(cex.yrs, max(as.integer(cex.yrs)) + 1)  # Add additional year (i.e. if CEX data goes through 2019, this ensures there is an entry for model year 2020 vehicles)
miss.yrs <- setdiff(cex.yrs, veh.yrs)
for (yr in miss.yrs) {
  if (!any(filter(codes, var == "VEHICYR")$code_desc == yr)) {
    codes <- add_row(codes, source = "I", file = "OVB", var = "VEHICYR", code = as.character(38 + as.integer(yr) - 2018), code_desc = yr, first_year = 1996, first_qtr = 1)
  }
}

#-----

# This is a safety procedure to ensure there are no duplicate, non-deprecated codes remaining in 'codes'
# When duplicates occur, it retains only the most recently-added code
codes.bup <- codes  # Retained for anti_join() below
codes <- codes %>%
  arrange(file, var, code, -first_year, -first_qtr) %>%
  group_by(source, file, var, code) %>%
  slice(1) %>%
  ungroup()

# The codes dropped by the safety filter (above) should be reported to BLS
# These are cases where there are duplicate, non-deprecated code entries (i.e. the flagged codes need to be deprecated)
error.codes <- anti_join(codes.bup, codes) %>%
  inner_join(select(codes, file, var, code)) %>%
  filter(is.na(last_year)) %>%
  select(-code_desc)

# Write potential errors to disk
if (nrow(error.codes) > 0) write_csv(error.codes, paste0("survey-processed/CEX/errors/", Sys.Date(), " CEX CODES flagged", ".csv"))

#-----

# Assemble final output

# Merge valid variables and code descriptions
# This table is used to identify the location (file) of desired variables and the meaning of coded responses
var_info <- full_join(vars, codes) %>%
  mutate(var_desc = ifelse(is.na(formula), var_desc, paste(var_desc, formula, sep = " | "))) %>%
  mutate_if(is.character, ~ str_squish(gsub("''", "'", .))) %>%  # Replace double apostrophe with single apostrophe in text strings; also remove unnecessary white space
  arrange(var) %>%
  select(source, file, section, var, var_desc, code, code_desc, first_year, first_qtr, last_year, last_qtr, flag)

# Save output to disk
save(var_info, file = "survey-processed/CEX/var_info.rda")
