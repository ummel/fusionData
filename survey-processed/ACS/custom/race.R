# Harmonize and Recode Household Reference Person Race and Ethnicity
#
# PURPOSE & OVERVIEW FOR USERS:
# Standard ACS PUMS datasets record racial self-identification ('rac1p') and Hispanic/Latino
# origin ('hisp') as distinct person-level variables. To support standardized household-level
# demographic segmentation, equity modeling, and spatial microsimulation in fusionACS,
# this script extracts the householder (reference person) record and generates a harmonized
# suite of racial and ethnic classification scheme variables.
#
# CLASSIFICATION SCHEMES & RECODING LOGIC:
#   - 'ref_race4': 4-category racial classification (White, Black, Asian, Other).
#   - 'ref_race5': 5-category race/ethnicity classification isolating 'Latino' as a mutually
#     exclusive category (White, Black, Asian, Latino, Other).
#   - 'ref_race6': 6-category racial classification providing granular sub-categories
#     (White, Black, Native American, Asian, Two or More Races, Other).
#   - 'ref_race7': 7-category race/ethnicity classification incorporating 'Latino' alongside
#     all 6 racial categories (White, Black, Native American, Asian, Two or More Races, Latino, Other).
#   - Binary Indicators ('ref_white', 'ref_black', 'ref_asian', 'ref_latino'): Logical flags
#     for rapid subsetting, where 'ref_white' explicitly isolates Non-Latino White householders.
#
# INPUT VARIABLES REQUIRED:
#   - Person-level ('P'): 'year', 'hid', 'rac1p' (detailed race code factor), 'hisp' (Hispanic origin factor)
#
# OUTPUTS GENERATED:
# Returns a household-level ('H') data frame containing:
#   - 'year', 'hid': Household primary keys
#   - 'ref_race4', 'ref_race5', 'ref_race6', 'ref_race7': Factor variables for householder race/ethnicity (labeled)
#   - 'ref_white', 'ref_black', 'ref_asian', 'ref_latino': Binary logical indicators (labeled)

race <- function(year) {

  cli::cli_alert_info("Executing custom reference person race/ethnicity calculation for vintage {year}")

  # Locate processed person microdata for the specified vintage
  pfile <- list.files("survey-processed/ACS", pattern = paste0(year, "_P_processed.fst"), recursive = TRUE, full.names = TRUE)
  if (length(pfile) == 0) stop("Unable to locate processed person microdata for year ", year)

  # Read person-level racial and Hispanic origin data
  p <- fst::read_fst(pfile, columns = c('year', 'hid', 'rac1p', 'hisp'))

  # Restrict to head of household (reference person), which is the first person record per 'hid'
  p <- p[!duplicated(p$hid), ]

  cli::cli_alert_info("Recoding 'rac1p' and 'hisp' into standardized 4-, 5-, 6-, and 7-category schemes")

  # Define recode mapping table matching Census 'rac1p' factor levels
  race.recode <- data.frame(
    rac1p = levels(p$rac1p),
    ref_race4 = c('White', 'Black', rep('Other', 3), 'Asian', rep('Other', 3)),
    ref_race6 = c('White', 'Black', rep('Native American', 3), 'Asian', rep('Other', 2), 'Two or More Races')
  )

  # Merge recode definitions and construct mutually exclusive race/ethnicity categories and logical flags
  out <- p %>%
    left_join(race.recode, by = 'rac1p') %>%
    mutate(ref_latino = !grepl("Not Spanish", hisp),
           ref_white = ref_race4 == "White" & !ref_latino,  # Non-Latino White
           ref_black = ref_race4 == "Black",
           ref_asian = ref_race4 == "Asian",
           ref_race5 = ifelse(ref_latino, "Latino", ref_race4),
           ref_race7 = ifelse(ref_latino, "Latino", ref_race6)) %>%
    select(year, hid, starts_with("ref_")) %>%
    mutate_if(is.character, factor) %>%    # Convert character vectors to factors with alphabetical levels
    labelled::set_variable_labels(ref_race4 = "Reference person race (4 categories)",
                                  ref_race5 = "Reference person race/ethnicity, including Latino (5 categories)",
                                  ref_race6 = "Reference person race (6 categories)",
                                  ref_race7 = "Reference person race/ethnicity, including Latino (7 categories)",
                                  ref_white = "Reference person identifies as Non-Latino White",
                                  ref_black = "Reference person identifies as Black",
                                  ref_asian = "Reference person identifies as Asian",
                                  ref_latino = "Reference person identifies as Latino")

  cli::cli_alert_success("Successfully processed race and ethnicity metrics for {nrow(out)} household reference persons")

  return(out)
}
