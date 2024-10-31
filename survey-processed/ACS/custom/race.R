# Function to add household-level definitions of race/ethnicity of the reference person (i.e. head of household)

race <- function(year) {
  pfile <- list.files("survey-processed/ACS", pattern = paste0(year, "_P_processed.fst"), recursive = TRUE, full.names = TRUE)
  p <- fst::read_fst(pfile, columns = c('year', 'hid', 'rac1p', 'hisp'))
  p <- p[!duplicated(p$hid), ]  # Restrict to head of household (i.e. reference person), since this is the first record within each household
  race.recode <- data.frame(
    rac1p = levels(p$rac1p),
    ref_race4 = c('White', 'Black', rep('Other', 3), 'Asian', rep('Other', 3)),
    ref_race6 = c('White', 'Black', rep('Native American', 3), 'Asian', rep('Other', 2), 'Two or More Races')
  )
  out <- p %>%
    left_join(race.recode, by = 'rac1p') %>%
    mutate(ref_latino = !grepl("Not Spanish", hisp),
           ref_white = ref_race4 == "White" & !ref_latino,
           ref_black = ref_race4 == "Black",
           ref_asian = ref_race4 == "Asian",
           ref_race5 = ifelse(ref_latino, "Latino", ref_race4),
           ref_race7 = ifelse(ref_latino, "Latino", ref_race6)) %>%
    select(year, hid, starts_with("ref_")) %>%
    mutate_if(is.character, factor) %>%   # Factor levels are alphabetical by default
    labelled::set_variable_labels(ref_race4 = "Reference person race (4 categories)",
                                  ref_race5 = "Reference person race/ethnicity, including Latino (5 categories)",
                                  ref_race6 = "Reference person race (6 categories)",
                                  ref_race7 = "Reference person race/ethnicity, including Latino (7 categories)",
                                  ref_white = "Reference person identifies as Non-Latino White",
                                  ref_black = "Reference person identifies as Black",
                                  ref_asian = "Reference person identifies as Asian",
                                  ref_latino = "Reference person identifies as Latino")
  return(out)
}
