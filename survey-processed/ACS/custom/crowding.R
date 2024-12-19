crowding <- function(year) {

  # See Section 4.8: https://www.gov.uk/guidance/allocation-of-accommodation-guidance-for-local-authorities/chapter-4-framing-an-allocation-scheme

  # Load household microdata
  hfile <- list.files("survey-processed/ACS", pattern = paste0(year, "_H_processed.fst"), recursive = TRUE, full.names = TRUE)
  v <- setdiff(names(fst::fst(hfile)), paste0("rep_", 1:80))
  h <- fst::read_fst(hfile, columns = v, as.data.table = TRUE)

  # Load person microdata
  # Retain only the reference person record for each household (used as predictor variables during imputation)
  pfile <- sub("_H_", "_P_", hfile)
  v <- setdiff(names(fst::fst(pfile)), c('pid', 'puma10', names(h), paste0("rep_", 1:80)))
  p <- fst::read_fst(pfile, columns = c('hid', v), as.data.table = TRUE)
  p <- p[hid %in% h$hid]

  #---

  # The name and levels of the relationship variables changes in 2019
  if ("relp" %in% names(p)) p$relshipp <- p$relp

  # Apply the "Bedroom Standard"
  p <- p %>%
    mutate(relshipp = ifelse(grepl("spouse", tolower(relshipp)) | grepl("partner", tolower(relshipp)) | grepl("husband", tolower(relshipp)), "Spouse", as.character(relshipp)),
           coupled = mar == "Married" | relshipp == "Spouse",
           age_group = findInterval(agep, vec = c(-Inf, 10, 21, 35, 50, 65, 80, Inf), rightmost.closed = TRUE)) %>%
    select(hid, relshipp, age_group, sex, coupled)

  # Ensure the reference person is marked as 'coupled' when a spouse is present
  p[, coupled := replace(coupled, 1, any(relshipp == "Spouse")), by = 'hid']

  #---

  # Number of individuals by category
  counts <- p[, list(N = .N), by = c('hid', 'age_group', 'sex', 'relshipp', 'coupled')]

  #---

  # Bedroom needed for reference person and spouse/partner

  out1 <- counts[age_group > 2 & relshipp == "Spouse", list(R = .N), by = 'hid']

  #---


  # Bedrooms needed for parent and parent-in-law married couples

  out2 <- counts[age_group > 2 & coupled & relshipp %in% c("Father or mother", "Parent-in-law")] %>%
    dcast(formula = hid ~ sex + relshipp, value.var = "N", fill = 0L, fun.aggregate = sum) %>%
    mutate(R = pmin(`Male_Father or mother`, `Female_Parent-in-law`) + pmin(`Female_Father or mother`, `Male_Parent-in-law`))

  #----

  # Bedrooms needed for adult children and children-in-law married couples

  # TO DO: Don't need age group match???

  out3 <- counts[age_group > 2 & coupled & grepl("daughter", relshipp)] %>%
    mutate(inlaw = grepl("in-law", relshipp)) %>%
    dcast(formula = hid ~ sex + inlaw, value.var = "N", fill = 0L, fun.aggregate = sum) %>%
    mutate(R = pmin(Male_FALSE, Female_TRUE) + pmin(Female_FALSE, Male_TRUE))

  #----

  # Bedrooms needed for adult siblings and other non-relative married couples

  out4 <- counts[age_group > 2 & coupled & relshipp %in% c("Brother or sister", "Other nonrelative")] %>%
    dcast(formula = hid ~ sex + relshipp, value.var = "N", fill = 0L, fun.aggregate = sum) %>%
    mutate(R = pmin(`Male_Brother or sister`, `Female_Other nonrelative`) + pmin(`Female_Brother or sister`, `Male_Other nonrelative`))

  #----

  # Bedrooms needed for presumably coupled heterosexual adults (matched on age group)

  out5 <- counts[age_group > 2 & !coupled] %>%
    dcast(formula = hid + age_group ~ sex, value.var = "N", fill = 0L, fun.aggregate = sum) %>%
    mutate(R = pmin(Male, Female))

  #----

  # Bedrooms needed for uncoupled adults

  temp <- rbindlist(list(out1, out2, out3, out4, out5), fill = TRUE)
  temp <- temp[, list(R = sum(R)), by = "hid"]

  out6 <- counts[age_group > 2, list(R = sum(N)), by = 'hid'] %>%
    left_join(temp, by = "hid") %>%
    mutate(R.y = replace_na(R.y, 0L),
           R = pmax(0, R.x - 2 * R.y))

  #----

  # Bedrooms needed for adolescents

  out7 <- counts[age_group == 2] %>%
    dcast(formula = hid ~ sex, value.var = "N", fill = 0L, fun.aggregate = sum) %>%
    mutate(R = ceiling(Male / 2) + ceiling(Female / 2))

  #----

  # Bedrooms needed for children

  out8 <- counts[age_group == 1, list(N = sum(N)), by = "hid"] %>%
    mutate(R = ceiling(N / 2))

  #----

  # Total number of bedrooms required
  result <- rbindlist(list(out1, out2, out3, out4, out5, out6, out7, out8), fill = TRUE)
  result <- result[, list(R = sum(R)), by = "hid"]

  # Compare
  result <- result %>%
    right_join(h, by = "hid") %>%
    rename(bedroom_req = R) %>%
    select(year, hid, bedroom_req) %>%
    labelled::set_variable_labels(bedroom_req = "Minimum number of bedrooms required by household to avoid overcrowding, according to the UK Government bedroom standard")

  return(result)

}

#----------------------------

# IDEA: Fit a model for the 'mv' and 'R' variables
# m <- fusionModel::train(data = slice_sample(comp, n = 100e3),
#                         y = c('mv', 'R'),
#                         x = setdiff(names(comp), c('year', 'hid', 'weight', 'puma10', 'mv', 'R')),
#                         weight = 'weight',
#                         nfolds = 0.8,
#                         cores = 3)
#
# # THEN: Age everyone by 10 years and simulate the outcomes?
# # Not easy to change the predictors manually, unless limiting to some reduced set
