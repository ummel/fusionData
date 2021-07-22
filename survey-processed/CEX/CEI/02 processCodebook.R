processCodebook <- function(survey_years) {

  # Load codebook
  codebook <- paste0("survey-processed/CEX/CEI/", survey_years, "/dictionary_", survey_years, ".rds") %>%
    map_dfr(readRDS) %>%
    mutate(var = tolower(var)) %>%
    group_by(var, value) %>%
    slice(1) %>%
    ungroup()

  # Clean up the codebook
  codebook <- codebook %>%
    mutate(

      # FMLI variable cleanup

      # Manual edits to variable descriptions ('desc')
      desc = gsub("CU", "consumer unit", desc),
      desc = gsub("PSU", "Primary Sampling Unit", desc),
      desc = gsub("#", "Number", desc, fixed = TRUE),
      desc = ifelse(var == "building", "Dwelling building description or type", desc),
      desc = ifelse(var == "built", "Year the property was built", desc),
      desc = ifelse(var == "cntralac", "Does this unit have central air conditioning?", desc),
      desc = ifelse(var == "windowac", "Does this unit have window air conditioning?", desc),
      desc = ifelse(var == "vehql", "Total number of leased vehicles", desc),
      desc = map_chr(strsplit(desc, ", mean of", fixed = TRUE), 1),
      desc = ifelse(grepl("replicate weight", desc), str_sub(desc, 1, 35), desc),
      desc = str_to_sentence(desc),

      # Manual edits to variable labels ('label')
      label = ifelse(var == "building", map_chr(strsplit(label, " (", fixed = TRUE), 1), label),
      label = ifelse(var == "building" & value == "10", "College dormitory", label),
      label = ifelse(var == "building" & value == "11", "Other", label),
      label = ifelse(var == "cntralac", "Yes", label),
      label = ifelse(var == "cutenure" & value == "1", "Homeowner with mortgage", label),
      label = ifelse(var == "cutenure" & value == "2", "Homeowner without mortgage", label),
      label = ifelse(var == "cutenure" & value == "3", NA, label),  # Force imputation ("Homeowner-Mortgage status not reported")
      label = ifelse(var == "cutenure" & value == "5", "Occupied without payment of cash rent", label),
      label = ifelse(var == "swimpool", "Yes", label),
      label = ifelse(var == "windowac", "Yes", label),
      label = ifelse(var == "unistrq" & value == "01", NA, label),  # Force imputation ("Only OTHER units")
      label = ifelse(label == "$1000 - $2,499", "$1,000 - $2,499", label),  # Missing comma in $1,000 (all other range units appear to be fine)
      label = ifelse(label == "Honolulu, HI", "Urban Honolulu, HI", label),
      label = ifelse(grepl("unspecified", label, ignore.case = TRUE), NA, label),
      label = ifelse(var == "state", value, label),  # Retain the "state" 2-digit fips code

      # Replace CEX-specific PSU codes with standard Census CBSA code (consistent with geo_concordance file)
      # Name-to-code lookup table is available here: https://www.census.gov/geographies/reference-files/time-series/demo/metro-micro/delineation-files.html
      label = ifelse(var == "psu" & value == "S11A", "14460", label),
      label = ifelse(var == "psu" & value == "S12A", "35620", label),
      label = ifelse(var == "psu" & value == "S12B", "37980", label),
      label = ifelse(var == "psu" & value == "S23A", "16980", label),
      label = ifelse(var == "psu" & value == "S23B", "19820", label),
      label = ifelse(var == "psu" & value == "S24A", "33460", label),
      label = ifelse(var == "psu" & value == "S24B", "41180", label),
      label = ifelse(var == "psu" & value == "S35A", "47900", label),
      label = ifelse(var == "psu" & value == "S35B", "33100", label),
      label = ifelse(var == "psu" & value == "S35C", "12060", label),
      label = ifelse(var == "psu" & value == "S35D", "45300", label),
      label = ifelse(var == "psu" & value == "S35E", "12580", label),
      label = ifelse(var == "psu" & value == "S37A", "19100", label),
      label = ifelse(var == "psu" & value == "S37B", "26420", label),
      label = ifelse(var == "psu" & value == "S48A", "38060", label),
      label = ifelse(var == "psu" & value == "S48B", "19740", label),
      label = ifelse(var == "psu" & value == "S49A", "31080", label),
      label = ifelse(var == "psu" & value == "S49B", "41860", label),
      label = ifelse(var == "psu" & value == "S49C", "40140", label),
      label = ifelse(var == "psu" & value == "S49D", "42660", label),
      label = ifelse(var == "psu" & value == "S49E", "41740", label),
      label = ifelse(var == "psu" & value == "S49F", "46520", label),
      label = ifelse(var == "psu" & value == "S49G", "11260", label),
      label = ifelse(var == "psu" & is.na(value), NA, label)

    ) %>%

    # MEMI variable cleanup

    mutate(

      # Manual edits to variable descriptions ('desc')
      desc = ifelse(var == "cucode", "Relation to reference person", desc),
      desc = ifelse(var == "educa", "Highest level of schooling member has completed or highest degree received", desc),
      desc = ifelse(var == "incnonwk", "Main reason member did not work during the past 12 months", desc),
      desc = ifelse(var == "incomey", "Employee type; refers to job with most earnings in the past 12 months", desc),
      desc = ifelse(var == "membno", "Member number within consumer unit", desc),
      desc = ifelse(var == "occucode", "Occupation type; refers to job with most earnings in the past 12 months", desc),
      desc = map_chr(strsplit(desc, ", mean of", fixed = TRUE), 1),
      desc = str_to_sentence(desc),

      # Manual edits to variable labels ('label')
      label = ifelse(var == "earner" & value == "1", "Member earns income", label),
      label = ifelse(var == "earner" & value == "2", "Member does not earn income", label),
      label = ifelse(var == "educa" & value == "1", "No schooling completed", label),
      label = ifelse(var == "educa" & value == "2", "Nursery, kindergarten, or elementary", label),
      label = ifelse(var == "educa" & value == "3", "High school, no degree", label),
      label = ifelse(var == "educa" & value == "4", "High school graduate (diploma or equivalent)", label),
      label = ifelse(var == "educa" & value == "5", "Some college, no degree", label),
      label = ifelse(var == "educa" & value == "6", "Associate's degree", label),
      label = ifelse(var == "educa" & value == "7", "Bachelor's degree", label),
      label = ifelse(var == "educa" & value == "8", "Master's, professional, or doctoral degree", label),
      label = str_to_sentence(label)
    ) %>%

    # !!! Add manual entries to the codebook
    add_row(var = "cuid", desc = "Consumer unit unique identifier", value = NA, label = NA) %>%
    add_row(var = "intnum", desc = "Interview number", value = NA, label = NA) %>%
    add_row(var = "cbsa13", desc = "Core-based statistical area (CBSA) circa 2013", value = NA, label = NA) %>%
    add_row(var = "ur12", desc = "Is consumer unit located in an urban or rural area?", value = NA, label = NA) %>%
    add_row(var = "cex_metro", desc = "Is consumer unit located in a metropolitan statistical area (MSA)?", value = NA, label = NA) %>%
    add_row(var = "cex_cbsasize", desc = "Size classification of CBSA where household is located", value = NA, label = NA) %>%
    mutate_all(str_squish)

  return(codebook)

}
