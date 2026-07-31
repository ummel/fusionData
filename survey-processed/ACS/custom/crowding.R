# Calculate Household Overcrowding Metrics via the UK Bedroom Standard
#
# PURPOSE & OVERVIEW FOR USERS:
# Housing overcrowding is a critical indicator of housing adequacy, socioeconomic
# vulnerability, and public welfare. Standard US Census measures of overcrowding
# (e.g., persons per room) are often coarse and fail to capture household composition
# nuances (such as age, sex, and familial relationships).
#
# This custom script implements the United Kingdom's "Bedroom Standard" allocation
# framework (UK Housing Act 1985 / Allocation of Accommodation Guidance) to derive
# an objective, rules-based estimate of the minimum number of bedrooms required
# by a household to avoid overcrowding ('bedroom_req').
#
# THE BEDROOM STANDARD ALLOCATION RULES:
# Under the Bedroom Standard, a separate bedroom is allocated to:
#   1. Each adult cohabiting/married couple.
#   2. Any other adult person aged 21 or over (or uncoupled adults paired heterosexually
#      within age cohorts).
#   3. Any pair of adolescents aged 10–20 of the same sex.
#   4. Any pair of children under 10 years of age regardless of sex.
#   5. Any remaining unpaired adolescent aged 10–20 or child under 10.
#
# INPUT VARIABLES REQUIRED:
#   - Household-level ('H'): 'hid', 'year'
#   - Person-level ('P'): 'hid', 'agep' (age), 'sex', 'mar' (marital status),
#     and 'relshipp' / 'rel' / 'relp' (relationship to reference person)
#
# OUTPUTS GENERATED:
# Returns a household-level ('H') data frame containing:
#   - 'year': Survey vintage year
#   - 'hid': Household identifier primary key
#   - 'bedroom_req': Minimum number of bedrooms required to meet the UK standard (labeled)
#
# TECHNICAL NOTE:
# Designed for ACS PUMS data vintages from 2008 onward due to relationship
# coding definitions ('relshipp'). Pre-2008 relationship codes are mapped to post-2008
# conventions where feasible using relative age heuristics.

crowding <- function(year) {

  cli::cli_alert_info("Executing custom overcrowding calculation ('bedroom_req') for vintage {year}")

  # Load processed household microdata for the specified vintage
  hfile <- list.files("survey-processed/ACS", pattern = paste0(year, "_H_processed.fst"), recursive = TRUE, full.names = TRUE)
  if (length(hfile) == 0) stop("Unable to locate processed household microdata for year ", year)

  v <- setdiff(names(fst::fst(hfile)), paste0("rep_", 1:80))
  h <- fst::read_fst(hfile, columns = v, as.data.table = TRUE)

  # Load corresponding person microdata, isolating person records tied to target households
  pfile <- sub("_H_", "_P_", hfile)
  if (!file.exists(pfile)) stop("Unable to locate processed person microdata for year ", year)

  v <- setdiff(names(fst::fst(pfile)), c('pid', 'puma10', names(h), paste0("rep_", 1:80)))
  p <- fst::read_fst(pfile, columns = c('hid', v), as.data.table = TRUE)
  p <- p[hid %in% h$hid]

  # Standardize relationship variable naming across different PUMS historical releases
  if ("rel" %in% names(p)) p$relshipp <- p$rel
  if ("relp" %in% names(p)) p$relshipp <- p$relp

  # Harmonize legacy pre-2008 relationship categories to modern PUMS 'relshipp' factor levels
  # Inlaws are classified as parents-in-law or children-in-law by comparing age against householder
  p[, ref_age := agep[1], by = hid]
  p <- p %>%
    mutate(relshipp = as.character(relshipp),
           relshipp = ifelse(relshipp == "Father / Mother", "Father or mother", relshipp),
           relshipp = ifelse(relshipp == "Son / Daughter", "Biological son or daughter", relshipp),
           relshipp = ifelse(relshipp == "Brother / Sister", "Brother or sister", relshipp),
           relshipp = ifelse(relshipp == "Inlaw" & agep >= ref_age, "Parent-in-law", relshipp),
           relshipp = ifelse(relshipp == "Inlaw" & agep < ref_age, "Son-in-law or daughter-in-law", relshipp))

  # Prepare demographic parameters and age bins for the UK Bedroom Standard algorithm
  # Age Group Bins: 1 = Children (<10), 2 = Adolescents (10-20), 3+ = Adults (21+)
  p <- p %>%
    mutate(relshipp = ifelse(grepl("spouse", tolower(relshipp)) | grepl("partner", tolower(relshipp)) | grepl("husband", tolower(relshipp)), "Spouse", relshipp),
           coupled = mar == "Married" | relshipp == "Spouse",
           age_group = findInterval(agep, vec = c(-Inf, 10, 21, 35, 50, 65, 80, Inf), rightmost.closed = TRUE)) %>%
    select(hid, relshipp, age_group, sex, coupled)

  # Mark reference person as coupled if a spouse or unmarried partner is present in household
  p[, coupled := replace(coupled, 1, any(relshipp == "Spouse")), by = 'hid']

  # Aggregate household demographic composition counts by relationship, sex, age, and marital status
  counts <- p[, list(N = .N), by = c('hid', 'age_group', 'sex', 'relshipp', 'coupled')]

  # Rule 1: Allocate 1 bedroom for householder and spouse/partner couples (adults 21+)
  out1 <- counts[age_group > 2 & relshipp == "Spouse", list(R = .N), by = 'hid']

  # Rule 2: Allocate 1 bedroom for each married adult couple among parents and parents-in-law
  out2 <- counts[age_group > 2 & coupled & relshipp %in% c("Father or mother", "Parent-in-law")] %>%
    dcast(formula = hid ~ sex + relshipp, value.var = "N", fill = 0L, fun.aggregate = sum) %>%
    mutate(R = pmin(`Male_Father or mother`, `Female_Parent-in-law`) + pmin(`Female_Father or mother`, `Male_Parent-in-law`))

  # Rule 3: Allocate 1 bedroom for each married adult couple among adult children and children-in-law
  out3 <- counts[age_group > 2 & coupled & grepl("daughter", relshipp)] %>%
    mutate(inlaw = grepl("in-law", relshipp)) %>%
    dcast(formula = hid ~ sex + inlaw, value.var = "N", fill = 0L, fun.aggregate = sum) %>%
    mutate(R = pmin(Male_FALSE, Female_TRUE) + pmin(Female_FALSE, Male_TRUE))

  # Rule 4: Allocate 1 bedroom for each married adult couple among siblings or non-relatives
  out4 <- counts[age_group > 2 & coupled & relshipp %in% c("Brother or sister", "Other nonrelative")] %>%
    dcast(formula = hid ~ sex + relshipp, value.var = "N", fill = 0L, fun.aggregate = sum) %>%
    mutate(R = pmin(`Male_Brother or sister`, `Female_Other nonrelative`) + pmin(`Female_Brother or sister`, `Male_Other nonrelative`))

  # Rule 5: Allocate 1 bedroom for opposite-sex adult pairs who are uncoupled but in same age cohort
  out5 <- counts[age_group > 2 & !coupled] %>%
    dcast(formula = hid + age_group ~ sex, value.var = "N", fill = 0L, fun.aggregate = sum) %>%
    mutate(R = pmin(Male, Female))

  # Rule 6: Allocate 1 bedroom for every remaining uncoupled adult who was not paired above
  temp <- rbindlist(list(out1, out2, out3, out4, out5), fill = TRUE)
  temp <- temp[, list(R = sum(R)), by = "hid"]

  out6 <- counts[age_group > 2, list(R = sum(N)), by = 'hid'] %>%
    left_join(temp, by = "hid") %>%
    mutate(R.y = replace_na(R.y, 0L),
           R = pmax(0, R.x - 2 * R.y))

  # Rule 7: Allocate bedrooms for adolescents (aged 10-20), pairing same-sex adolescents 2 per room
  out7 <- counts[age_group == 2] %>%
    dcast(formula = hid ~ sex, value.var = "N", fill = 0L, fun.aggregate = sum) %>%
    mutate(R = ceiling(Male / 2) + ceiling(Female / 2))

  # Rule 8: Allocate bedrooms for young children (aged <10), pairing 2 per room regardless of sex
  out8 <- counts[age_group == 1, list(N = sum(N)), by = "hid"] %>%
    mutate(R = ceiling(N / 2))

  # Consolidate bedroom allocations across all rules to compute total household requirements
  result <- rbindlist(list(out1, out2, out3, out4, out5, out6, out7, out8), fill = TRUE)
  result <- result[, list(R = sum(R)), by = "hid"]

  # Merge bedroom requirement metrics back to household universe and apply variable metadata
  result <- result %>%
    right_join(h, by = "hid") %>%
    rename(bedroom_req = R) %>%
    select(year, hid, bedroom_req) %>%
    labelled::set_variable_labels(bedroom_req = "Minimum number of bedrooms required by household to avoid overcrowding, according to the UK Government bedroom standard")

  cli::cli_alert_success("Successfully calculated 'bedroom_req' for {nrow(result)} households")

  return(result)

}
