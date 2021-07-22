library(tidyverse)

data("BEA_pce_national")
data("BEA_pce_state")
load("survey-processed/CEX/pce_concordance.rda")
load("survey-processed/CEX/var_info.rda")
load("survey-processed/CEX/ucc_heading.rda")

# Identifier for the Google Sheet with consumption category information
# This is stored in the "fusionACSdata" Google Drive
gs.id <- "13GRKkVZXapHtP7oK1WUh0Yu7OQ_9icd17wUGuhX-WRg"

#-----

# Custom category aggregates
# This information is used only to ensure that the 'aggregate' column in 'cats' contains legitimate values
custom.aggregates <- googlesheets4::read_sheet(ss = gs.id, sheet = "Custom Aggregates", skip = 1)

# Load model categories and PCE link from Google Drive
cats <- googlesheets4::read_sheet(ss = gs.id, sheet = "Consumption Categories") %>%
  select(major, cat, category, component, aggregate, note) %>%
  slice(1:(which(.[[1]] == "IGNORED BELOW") - 1)) %>%
  filter(!is.na(cat)) %>%
  mutate(component = str_split(component, ",")) %>%
  unnest(component) %>%
  mutate(component = na_if(component, "NA")) %>%
  arrange(major, cat) %>%
  mutate_all(str_squish)

# Safety checks
stopifnot(length(unique(cats$cat)) == length(unique(cats$category)))
stopifnot(length(unique(cats$cat)) == cats %>% select(major, cat, category) %>% distinct() %>% nrow())
stopifnot(nrow(cats %>% group_by(cat) %>% filter(length(unique(category)) > 1)) == 0)
stopifnot(anyDuplicated(na.omit(cats$component)) == 0)

# Check that all "aggregate" values are legitimate...
aggs <- cats %>%
  mutate(aggregate = str_split(aggregate, ",")) %>%
  unnest(cols = aggregate) %>%
  pull(aggregate) %>%
  na.omit() %>%
  str_squish()
bad <- setdiff(aggs, c(BEA_pce_national$pce_series, custom.aggregates$aggregate))
stopifnot(length(bad) == 0)
stopifnot(!any(custom.aggregates$aggregate %in% BEA_pce_national$pce_series))

#----------------------------

# For each PCE series in 'BEA_pce_national', identify the linked PCE "component" to which it belongs
# A given series item may belong to a higher-level series (e.g. DNEARC belongs to DNMVRC)

# link.comp <- pce_national %>%
#   select(pce_series, starts_with("parent")) %>%
#   apply(MARGIN = 1, FUN = function(v) intersect(v, cats$component)) %>%
#   sapply(function(v) ifelse(length(v), v, NA))

f <- function(x, pce) {
  x %>%
    select(pce_series, starts_with("parent")) %>%
    apply(MARGIN = 1, FUN = function(v) intersect(v, pce)) %>%
    sapply(function(v) ifelse(length(v), v, NA))
}

temp <- BEA_pce_national %>%
  mutate(component = f(., pce = cats$component)) %>%
  filter(!is.na(component)) %>%
  select(pce_series, component) %>%
  inner_join(pce_concordance, by = "pce_series") %>%
  select(-pce_series)

# Remove UCC's in ucc_heading that are already present in crosswalk between UCC and PCE
ucc_heading2 <- ucc_heading %>%
  map(~setdiff(.x, temp$ucc))

# Assign individual UCC's associated with CEX "heading" variables
cats <- cats %>%
  left_join(temp, by = "component") %>%
  mutate(ucc = ifelse(is.na(ucc) & !is.na(suppressWarnings(as.numeric(component))), component, ucc),
         ucc = ifelse(is.na(ucc), ucc_heading2[component], ucc),
         ucc_share = ifelse(is.na(ucc_share), 1, ucc_share)) %>%
  unnest(ucc, keep_empty = TRUE) %>%
  select(aggregate, major, category, cat, ucc, ucc_share)

#----------------------------

# Set source to "I" for everything that can be. Only use diary when it is the only source of the UCC.
ucc_assignment <- cats %>%
  inner_join(var_info %>%
               filter(var == "UCC") %>%
               rename(ucc = code, ucc_desc = code_desc),
             by = "ucc") %>%
  mutate(source_rank = ifelse(source == "I", 1, 2), # Prioritize UCC's from interview survey,
         file_rank = ifelse(file == "MTBI", 1, ifelse(file == "EXPD", 2, 3)),  # Prioritize UCC's from MTBI and EXPD files
         ucc_share_temp = ifelse(ucc_share < 1, jitter(ucc_share), 1)) %>%  # Jitter needed to distinguish between UCC entries with identical ucc_share (i.e. 0.5 and 0.5)
  arrange(source_rank, file_rank) %>%
  group_by(major, cat, ucc, ucc_share) %>%
  slice(1) %>%  # Retain single, preferred source for each UCC entry
  ungroup() %>%
  select(aggregate, major, category, cat, ucc_desc, ucc, ucc_share, source, file)

# Which files are the UCC's sourced from?
# As of March 2021, there should be only 4 records from ITBI, which is effectively ignored by subsequent processing
table(filter(ucc_assignment, !is.na(ucc))$file)

#----------------------------

# MANUAL defining of 'ucc_share' for estimated rental value UCC's

# UPDATE!!!
# UCC's in the OWNRNT 'cat' typically report monthly rental values (with one exception, see below)
# The reported monthly rental equivalence is divided by 3 when entered in the MBTI file as a monthly transaction (unclear why)
# Ensuring that the final OWNRNT output summed across 4 interviews reflects ANNUAL rental value requires that the raw values be multiplied by 3;
#  i.e. set 'ucc_share' to 3, since raw MTBI values are multiplied by 'ucc_share' within processMTBI().
# Exception: UCC 910103 ("Estimated annual rental value of timeshare") is entered as annual value in MTBI file,
#  which requires that the raw value be divided by 12 to ensure correct annual value after processing.
# Further: UCC "910102" ("Estimated monthly rental value of vacation home available for rent") and "910100" (deprecated) has its 'ucc_share' additionally divided by 2,
#  to reflect partial-use of property by owner; this is how BLS staff treat this UCC when creating concordance with PCE.

ucc_assignment <- ucc_assignment %>%
  mutate(ucc_share = ifelse(cat == "OWNRNT", 3, ucc_share),
         ucc_share = ifelse(ucc == "910103", 1 / 12, ucc_share),  # Time share rental equivalence; annual values already
         ucc_share = ifelse(ucc %in% c("910102", "910100"), ucc_share / 2, ucc_share))  # UCC's for vacation homes available for rent (50%, per BLS PCE concordance)

# Check
#filter(ucc_assignment, cat == "OWNRNT")

#----------------------------

# Link between national 'pce_series' and state 'pce_series' (latter is a parent of former, in most cases)
x <- unique(BEA_pce_state$pce_series)
y <- BEA_pce_national %>%
  select(pce_series, starts_with("parent")) %>%
  mutate_all(~ ifelse(.x %in% x, .x, NA)) %>%
  apply(MARGIN = 1, FUN = function(x) na.omit(x)[1])

# Crosswalk between national PCE series (pce_series) and state-level PCE series (state_series)
national.state.pce.xwalk <- tibble(pce_series = BEA_pce_national$pce_series, state_series = y) %>%
  na.omit() %>%
  distinct()

# Link between 'cat' and national pce_series (high-resolution)
link <- ucc_assignment %>%
  mutate(pce_series = str_split(aggregate, ",")) %>%
  unnest(pce_series) %>%
  select(major, category, cat, pce_series) %>%
  distinct() %>%
  mutate_all(str_squish) %>%
  left_join(national.state.pce.xwalk, by = "pce_series") %>%
  select(major, category, cat, pce_series, state_series) %>%
  distinct() %>%
  group_by(pce_series) %>%
  mutate(national_pce_adj = ifelse(is.na(pce_series), NA, 1 / n())) %>%
  ungroup() %>%
  arrange(cat)

# MARCH 2021: Not needed if "Heating oil, LPG, and other fuels" are grouped into a single 'cat'
# MANUAL FIX: Assign shares for DFULRC categories based on relative proportion in the CEX
# link <- link %>%
#   mutate(national_pce_adj = ifelse(cat == "FOIL", 0.53, national_pce_adj),
#          national_pce_adj = ifelse(cat == "LPG", 0.40, national_pce_adj),
#          national_pce_adj = ifelse(cat == "OFUEL", 0.07, national_pce_adj))

# IDENTIFY PROBLEMS
# A conflict is detected and flagged when a 'cat' is associated with more than state PCE series
# !!!NOTE: This 'check' should consist ONLY of FOIL, LPG, OFUEL
# Normally, each national PCE series (in this case, "DFULRC") should be assigned to exactly one 'cat',
#  which would result in these three fuels being assigned to a single 'cat'
#  However, because these fuels have different, known carbon intensities, I want to model them separately
# Consequently, 'national_pce_adj' is set to 1/3 so that the PCE value associated with DFULRC can be divided by 3 in subsequent script to adjust expenditure to match known totals
# probs1 <- link %>%
#   filter(national_pce_adj < 1)
# print(probs1)
# #stopifnot(nrow(probs1) == 3)  # In case that DFULRC fuels are split into 3 separate categories
# stopifnot(nrow(probs1) == 0)  # In general case when DFULRC fuels are grouped into single category (i.e. Heating oil, LPG, and other fuels)

# link %>%
#   select(cat, pce_series, state_series) %>%
#   group_by(cat) %>%
#   mutate(n_state_series = length(unique(state_series))) %>%
#   filter(n_state_series > 1)

#-----

# MANUAL OVERRIDE: As of April, 2021, INTPHN category knowingly violates state hierarchy
# Allowing this and simply setting "state_series" to DOTSRC for offensive rows in 'link'
# This is because the services component (DOTSRC) of INTPHN is much larger than the products portion
link <- mutate(link, state_series = ifelse(cat == "INTPHN", "DOTSRC", state_series))

# MANUAL OVERRIDE: As of April, 2021, FINPAY category knowingly violates state hierarchy
# Allowing this and simply setting "state_series" to NA for offensive rows in 'link'
# This makes sense because the "FINPAY" PCE contains all kinds of imputed consumption that isn't comparable to CEX expenditures
link <- mutate(link, state_series = ifelse(cat == "FINPAY", NA, state_series))

# MANUAL OVERRIDE: As of April, 2021, AIRSHP category knowingly violates state hierarchy
# Allowing this and simply setting "state_series" to NA for offensive rows in 'link'
link <- mutate(link, state_series = ifelse(cat == "AIRSHP", NA, state_series))

# MANUAL OVERRIDE: As of April, 2021, EDUC category knowingly violates state hierarchy
# Allowing this and simply setting "state_series" to NA for offensive rows in 'link'
link <- mutate(link, state_series = ifelse(cat == "EDUC", NA, state_series))

# IDENTIFY ANY REMAINING PROBLEMS
problems <- link %>%
  select(cat, pce_series, state_series) %>%
  group_by(cat) %>%
  mutate(n_state_series = length(unique(state_series))) %>%
  filter(n_state_series > 1)

# Safety check
stopifnot(nrow(problems) == 0)

#----------------------------

# Create 'cat_assignment' table with 'type' column indicating if the category is a good or service

# Determine if a 'cat' is a good or service
temp <- BEA_pce_national %>%
  filter(!is.na(parent1)) %>%
  select(pce_series, starts_with("parent")) %>%
  mutate(type = apply(., MARGIN = 1, FUN = function(x) ifelse(any(x %in% "DGDSRC"), "Goods", "Services")))

# Assign "Good" or "Service" to each 'cat'
cat_assignment <- link %>%
  left_join(temp, by = "pce_series") %>%
  mutate(type = ifelse(major == "Housing", "Housing", type),   # Assign type = "Housing" for categories in major Housing category
         type = replace_na(type, "Services")) %>%    # Assumes that any 'cat' not already assigned is type = "Services")
  #type = ifelse(major == "Housing" & !category %in% c("Home maintenance", "Home improvement") & !grepl("Home insurance", category), "Housing", type),  # Assign type = "Housing" for categories strongly affected by home values
  arrange(major, category)

#----------------------------

# Create full description of categories and upload results to Google Sheet
out <- ucc_assignment %>%
  filter(!is.na(ucc)) %>%
  arrange(cat, -ucc_share, ucc_desc) %>%
  mutate(description = ifelse(ucc_share == 1, ucc_desc, paste0(ucc_desc, " (", round(100 * ucc_share, 1), "%)"))) %>%
  group_by(major, cat, category) %>%
  summarize(cex_line_items = paste(unique(description), collapse = "\n"), .groups = 'drop') %>%
  arrange(major, category)

googlesheets4::write_sheet(out %>% select(major, cat, category), ss = gs.id, sheet = "Category Summary")
googlesheets4::write_sheet(out %>% select(major, category, cex_line_items), ss = gs.id, sheet = "Category Details")

#----------------------------

# Save results to disk
save(cat_assignment, file = "survey-processed/CEX/cat_assignment.rda")
save(ucc_assignment, file = "survey-processed/CEX/ucc_assignment.rda")

