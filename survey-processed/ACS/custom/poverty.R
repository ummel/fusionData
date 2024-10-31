# Function to add household-level official poverty thresholds and ratio of household income to the threshold
# Useful for identifying plausible poverty status of ACS households
# The official poverty threshold values are pre-processed (see: data-raw/poverty_thresholds.R)

poverty <- function(year) {
  data(poverty_thresholds, package = "fusionData")
  hfile <- list.files("survey-processed/ACS", pattern = paste0(year, "_H_processed.fst"), recursive = TRUE, full.names = TRUE)
  h <- fst::read_fst(hfile, columns = c('year', 'hid', 'np', 'hincp'), as.data.table = TRUE)
  p <- fst::read_fst(sub("_H_", "_P_", hfile), columns = c('hid', 'agep'), as.data.table = TRUE)
  p <- p[, list(minors = sum(agep < 18), senior = agep[1] >= 65), by = "hid"]
  out <- h %>%
    merge(p, by = 'hid') %>%
    mutate(size = pmin(np, 9L),
           minors = pmin(minors, 8L),
           minors = ifelse(minors == size, size - 1L, minors)) %>%  # Handle rare cases of all minors in a household
    merge(poverty_thresholds, by = key(poverty_thresholds), all.x = TRUE) %>%
    mutate(pov_ratio = pmax(0, hincp) / threshold,
           pov_thresh = threshold) %>%
    select(year, hid, pov_thresh, pov_ratio) %>%
    labelled::set_variable_labels(pov_thresh = "Household poverty threshold based on household size, number of children, and age of householder (dollars)",
                                  pov_ratio = "Ratio of household income to poverty threshold")
  return(out)
}
