# Process the "ce_source_integrate.xlsx" file
# Outputs:
#   1) "ucc_source.RData": tibble containing the preferred source (diary or interview) for each UCC and survey year
#   2) "ucc_heading.RData": list containing the individual UCC's associated with each CEX 'heading' variable (e.g. "OWNREPSV")

#---

# Pre-process raw data
temp <- readxl::read_excel("survey-raw/CEX/ce_source_integrate.xlsx", skip = 3) %>%
  filter(!is.na(Level)) %>%
  rename(ucc = UCC) %>%
  mutate(Level = as.integer(ifelse(nchar(Level) > 1, str_sub(Level, 3, 3), Level)))  # Clean up any Level entries like "5/6" and convert result to integer

# Function to return children UCC's for a given row in 'temp'
getChildren <- function(i, levels) {
  x <- levels
  j <- which(x > x[i])
  j <- j[j > i]
  k <- which(diff(j) > 1)[1]
  if (is.na(k)) {
    return(NA)
  } else {
    out <- temp$ucc[j[1:k]]
    out <- out[!is.na(suppressWarnings(as.numeric(out)))]
    return(out)
  }
}

# Return list with UCC's associated with each "heading" category (e.g. ...$OWNREPSP)
ind <- which(is.na(suppressWarnings(as.numeric(temp$ucc))))
ucc_heading <- lapply(ind, getChildren, levels = temp$Level)
names(ucc_heading) <- temp$ucc[ind]

# Return preferred source ("I" or "D") for each UCC and survey year
# ucc_source <- temp %>%
#   filter(!is.na(suppressWarnings(as.numeric(ucc)))) %>%
#   select(ucc, starts_with("y")) %>%
#   gather(year, source, -ucc, na.rm = TRUE) %>%
#   mutate(source = substring(trimws(source), 1, 1),
#          year = as.integer(substring(year, 2, 3)),
#          year = ifelse(year >= 90, year + 1900L, year + 2000L)) %>%
#   arrange(ucc, year, source) %>%
#   group_by(ucc, year) %>%
#   slice(n()) %>%  # Retains only single entry for each UCC-year combination (safest); preferring "I" in case of duplicate entries
#   ungroup()

# Save output to disk
save(ucc_heading, file = "survey-processed/CEX/ucc_heading.rda")
