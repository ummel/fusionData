# Load codebook

capFirst <- function(x) {str_sub(x, 1, 1) <- toupper(str_sub(x, 1, 1)); return(x)}

codebook <- read_csv("survey-raw/ACS/2019/PUMS_Data_Dictionary_2019.csv",
                     col_names = c('record', 'var', 'type', 'length', 'value', 'value2', 'label')) %>%

  distinct() %>%   # Eliminate duplicate entries (i.e. one each for household and person-records)

  split(f = .$var) %>%

  map(~ mutate(.x,
               rng = (value != value2 & !is.na(value2)),
               miss = grepl("^(b)\\1*$", value),  # Matches "b", "bb", etc.
               value = ifelse(miss, NA, value),  # Replace "b", "bb", etc. with NA
               asis = !rng & !miss)) %>%

  map_dfr(~ tibble(var = .x$var[1],
                   desc = .x$value[1],
                   value = if (all(.x$asis)) {.x$value[-1]} else {if (!any(.x$miss)) {NA} else {c(.x$value[.x$miss], if (any(.x$rng)) {NULL} else {.x$value[!.x$miss][-1]})}},
                   label = if (all(.x$asis)) {.x$label[-1]} else {if (!any(.x$miss)) {NA} else {c(.x$label[.x$miss], if (any(.x$rng)) {NULL} else {.x$label[!.x$miss][-1]})}})) %>%

  filter(
    !(str_sub(var, 1, 3) == "RAC" & grepl("recode", tolower(desc))),  # Remove Yes/No race recode flags, since race information captures in RAC1P
    !grepl("allocation flag", tolower(desc), fixed = TRUE),  # Remove allocation flag variables
    !grepl("eligibility coverage edit", desc, fixed = TRUE),  # Remove healt insurance coverage edit variables (not necessary)
    !grepl("See 'Employment Status Recode' (ESR)", desc, fixed = TRUE),  # Remove detailed employment questions since 'ESR' variable captures this information
    !grepl("^MLP.", var)  # Remove veteran period of service recodes (all information contained in 'VPS')
  ) %>%

  mutate(
    temp = suppressWarnings(as.integer(value)),
    value = ifelse(is.na(temp), value, as.character(temp)),  # Coerces 'value' to integer (but stored as character) whenever possible in order to match raw data resulting from fread()
    temp = NULL,
    adj = ifelse(grepl("use ADJHSG", desc), "ADJHSG", NA),
    adj = ifelse(grepl("use ADJINC", desc), "ADJINC", adj)
  ) %>%

  mutate(
    desc = gsub("\\s*\\([^\\)]+\\)", "", desc),  # Remove parenthetical text in description
    desc = gsub("recode", "", desc),  # Remove word 'recode' as it doesn't seem necessary (might induce confusion)
    desc = gsub("HH", "household", desc),  # Replace 'HH' with 'household' to avoid confusion
    desc = capFirst(desc)
  ) %>%

  mutate(
    label = gsub("/ ", "/", label, fixed = TRUE),  # Manual text error fix-ups
    label = gsub(" / ", "/", label, fixed = TRUE),  # Manual text error fix-ups
    label = gsub("//", "/", label, fixed = TRUE),  # Manual text error fix-ups
    label = ifelse(str_sub(label, 1, 3) == "N/A", gsub("\\(([^()]*)\\)|.", "\\1", label, perl = TRUE), label),   # Extract subsequent parenthetical text when 'value' starts with "N/A"
    label = gsub("FT", "full-time", label, fixed = TRUE),
    label = gsub("NILF", "Not in labor force", label, fixed = TRUE),
    label = gsub("<", "less than", label, fixed = TRUE),
    noedit = grepl("/[0-9]", label) | is.na(label),
    label = ifelse(noedit, label, map_chr(strsplit(label, split = "/"), ~ paste(capFirst(.x[!tolower(.x) %in% c("gq", "vacant")]), collapse = " / "))),
    noedit = NULL,
    label = ifelse(grepl("suppress", tolower(label)), NA, label),  # Set suppressed values to NA to be imputed
    label = gsub("\\s*\\([^\\)]+\\)", "", label),  # Remove parenthetical text in label
    label = capFirst(label)
  ) %>%
  add_count(var) %>%
  mutate(label = ifelse(n == 1 & is.na(value) & !is.na(label), 0, label),
         n = NULL) %>%
  mutate_if(is.character, str_squish)
