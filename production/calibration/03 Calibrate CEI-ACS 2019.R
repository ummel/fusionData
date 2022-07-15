# This script generates:
# CEI-ACS_2019 calibrated.fst
# Calibrated variable descriptions.csv

#-----

library(fusionData)

# CEI-ACS fused single implicate to be calibrated
sim <- read_fst("production/v2/CEI/2015-2019/CEI_2015-2019__ACS_2019__01.fst")

p <- read_fst("production/calibration/ACS_2019 calibration inputs.fst")
fsagg <- readRDS("production/calibration/Feiveson-Sabelhaus calibration targets.fst")
nagg <- readRDS("production/calibration/National PCE calibration targets.fst")
sagg <- readRDS("production/calibration/State PCE calibration targets.fst")
eagg <- readRDS("production/calibration/State EIA calibration targets.fst")

load("survey-processed/CEX/cat_assignment.rda")

#----------------------------------

# ATTEMPT TO CALIBRATE DATA IN 'd'

fsagg$adj <- NA
nagg$adj <- NA
sagg$adj <- NA
eagg$adj <- NA

# Combine data inputs
p <- cbind(p, sim)

# Add vehicle depreciation
p$vehdep <- p$vehval * 0.15

# Columns that will be calibrated
fs.cons <- fsagg$cat[[1]]
cats <- intersect(names(p), c(fs.cons, tolower(cat_assignment$cat)))

# Convert target columns to double/numeric to avoid data.table class conflict and associated truncation messages
# Enforce non-negative values among the consumption variables to be calibrated
p <- p %>%
  mutate_at(cats, as.numeric) %>%
  mutate_at(cats, ~ pmax(0, .x))

#-----

# Convert to data.table for efficient set() operations
d <- data.table(p)

# Original values of the calibration columns; used to enforce a lower bound on calibrated values
d0 <- copy(d[, ..cats])

#-----

# # Link between 'cat' and 'major' category
cat.major <- cat_assignment %>%
  select(cat, major) %>%
  distinct() %>%
  mutate(cat = tolower(cat))

#------------------------

# Calibrate via iterative approach; 5 iterations looks to be sufficient

# Placeholder
ratio0 <- rep(0, length(cats))

for (iter in 1:5) {

  cat("--- Iteration", iter, "---\n")

  #------------------------

  # Calibrate using 'fsagg' (Feiveson-Sabelhaus consumption shares)

  # Update estimate of total FS consumption for each household
  set(d, j = "fs_cons", value = rowSums(d[, ..fs.cons]))

  # For each age_grp-cons_grp cohort, calculate total implied FS consumption, in millions of dollars
  # This consists of multiplying the cohort 'share' by national total FS consumption
  fsagg$total <- fsagg$share * sum(d$fs_cons * d$weight) / 1e6

  # Calculate initial adjustment factor for each constraint
  for (k in 1:nrow(fsagg)) {
    irows <- which(d$age_grp == fsagg$age_grp[k] & d$cons_grp == fsagg$cons_grp[k])
    jcols <- unlist(fsagg$cat[k])
    adj <- 1e6 * fsagg$total[k] / sum(d$weight * d[irows, ..jcols])
    fsagg$adj[k] <- adj
  }

  # Columns in 'd' that require an estimated adjustment factor
  adj.cols <- setdiff(cats, unlist(fsagg$cat))

  # Calculate average adjustment factor, by major category and cohort-identifying variables, for categories without an explicit constraint
  avg.adj <- fsagg %>%
    unnest(cat) %>%
    left_join(cat.major, by = "cat") %>%
    group_by(major, age_grp, cons_grp) %>%
    summarize(adj = weighted.mean(adj, total, na.rm = TRUE), .groups = "drop") %>%
    left_join(filter(cat.major, cat %in% adj.cols), by = "major") %>%
    filter(!is.na(cat)) %>%
    group_by(major, age_grp, cons_grp, adj) %>%
    summarize(cat = list(cat), .groups = "drop")

  # Apply each cohort adjustment factor
  # Enforce minimum bound on post-calibration values (cannot go below original value)
  # Update m$adj to reflect the actual mean adjustment factor, taking into account the lower bound constraint
  m <- bind_rows(fsagg, avg.adj)
  for (k in 1:nrow(m)) {
    irows <- which(d$age_grp == m$age_grp[k] & d$cons_grp == m$cons_grp[k])
    jcols <- unlist(m$cat[k])
    new <- pmax(d0[irows, ..jcols], d[irows, ..jcols] * m$adj[k])
    m$adj[k] <- weighted.mean(new / d[irows, ..jcols], w = d[irows, ..jcols])
    set(d, i = irows, j = jcols, value = new)
  }

  cat("FS shares calibration adjustment factors\n")
  print(summary(m$adj))

  #------------------------

  # Calibrate using 'eagg' (state EIA SEDS expenditure totals)

  # Calculate initial adjustment factor for each constraint
  for (k in 1:nrow(eagg)) {
    irows <- which(d$state == eagg$state[k])
    jcols <- unlist(eagg$cat[k])
    eagg$adj[k] <- 1e6 * eagg$total[k] / sum(d$weight * d[irows, ..jcols])
  }

  # Columns in 'd' that require an estimated adjustment factor
  adj.cols <- setdiff(cats, unlist(eagg$cat))

  # Calculate average adjustment factor, by major category and cohort-identifying variables, for categories without an explicit constraint
  avg.adj <- eagg %>%
    unnest(cat) %>%
    left_join(cat.major, by = "cat") %>%
    group_by(major, state) %>%
    summarize(adj = weighted.mean(adj, total, na.rm = TRUE), .groups = "drop") %>%
    left_join(filter(cat.major, cat %in% adj.cols), by = "major") %>%
    filter(!is.na(cat)) %>%
    group_by(major, state, adj) %>%
    summarize(cat = list(cat), .groups = "drop")

  # Apply each cohort adjustment factor
  # Enforce minimum bound on post-calibration values (cannot go below original value)
  # Update m$adj to reflect the actual mean adjustment factor, taking into account the lower bound constraint
  m <- bind_rows(eagg, avg.adj)
  for (k in 1:nrow(m)) {
    irows <- which(d$state == m$state[k])
    jcols <- unlist(m$cat[k])
    new <- pmax(d0[irows, ..jcols], d[irows, ..jcols] * m$adj[k])
    m$adj[k] <- weighted.mean(new / d[irows, ..jcols], w = d[irows, ..jcols])
    set(d, i = irows, j = jcols, value = new)
  }

  cat("State EIA SEDS calibration adjustment factors\n")
  print(summary(m$adj))

  #------------------------

  # Calibrate using 'sagg' (state PCE)

  # Calculate initial adjustment factor for each constraint
  for (k in 1:nrow(sagg)) {
    irows <- which(d$state == sagg$state[k])
    jcols <- unlist(sagg$cat[k])
    sagg$adj[k] <- 1e6 * sagg$total[k] / sum(d$weight * d[irows, ..jcols])
  }

  # Columns in 'd' that require an estimated adjustment factor
  adj.cols <- setdiff(cats, unlist(sagg$cat))

  # Calculate average adjustment factor, by major category and cohort-identifying variables, for categories without an explicit constraint
  avg.adj <- sagg %>%
    unnest(cat) %>%
    left_join(cat.major, by = "cat") %>%
    group_by(major, state) %>%
    summarize(adj = weighted.mean(adj, total, na.rm = TRUE), .groups = "drop") %>%
    left_join(filter(cat.major, cat %in% adj.cols), by = "major") %>%
    filter(!is.na(cat)) %>%
    group_by(major, state, adj) %>%
    summarize(cat = list(cat), .groups = "drop")

  # Apply each cohort adjustment factor
  # Enforce minimum bound on post-calibration values (cannot go below original value)
  # Update m$adj to reflect the actual mean adjustment factor, taking into account the lower bound constraint
  m <- bind_rows(sagg, avg.adj)
  for (k in 1:nrow(m)) {
    irows <- which(d$state == m$state[k])
    jcols <- unlist(m$cat[k])
    new <- pmax(d0[irows, ..jcols], d[irows, ..jcols] * m$adj[k])
    m$adj[k] <- weighted.mean(new / d[irows, ..jcols], w = d[irows, ..jcols])
    set(d, i = irows, j = jcols, value = new)
  }

  cat("State PCE calibration adjustment factors\n")
  print(summary(m$adj))

  #------------------------

  # Calibrate using 'nagg' (national PCE)

  # Calculate initial adjustment factor for each constraint
  for (k in 1:nrow(nagg)) {
    jcols <- unlist(nagg$cat[k])
    nagg$adj[k] <- 1e6 * nagg$total[k] / sum(d$weight * d[, ..jcols])
  }

  # Columns in 'd' that require an estimated adjustment factor
  adj.cols <- setdiff(cats, unlist(nagg$cat))

  # Calculate average adjustment factor, by major category and cohort-identifying variables, for categories without an explicit constraint
  avg.adj <- nagg %>%
    unnest(cat) %>%
    left_join(cat.major, by = "cat") %>%
    group_by(major) %>%
    summarize(adj = weighted.mean(adj, total, na.rm = TRUE), .groups = "drop") %>%
    left_join(filter(cat.major, cat %in% adj.cols), by = "major") %>%
    filter(!is.na(cat)) %>%
    group_by(major, adj) %>%
    summarize(cat = list(cat), .groups = "drop")

  # Apply each cohort adjustment factor
  # Enforce minimum bound on post-calibration values (cannot go below original value)
  # Update m$adj to reflect the actual mean adjustment factor, taking into account the lower bound constraint
  m <- bind_rows(nagg, avg.adj)
  for (k in 1:nrow(m)) {
    jcols <- unlist(m$cat[k])
    new <- pmax(d0[, ..jcols], d[, ..jcols] * m$adj[k])
    m$adj[k] <- weighted.mean(new / d[, ..jcols], w = d[, ..jcols])
    set(d, i = NULL, j = jcols, value = new)
  }

  cat("National calibration adjustment factors\n")
  print(summary(m$adj))

  #------------------------

  # Calculate some measure (delta) of the overall change in calibrated values
  # Idea: When delta is sufficiently small, terminate loop (not yet implemented)

  # Mean ratio of calibrated values to original/un-calibrated values for each variable
  ratio <- colMeans(d[, ..cats] / d0[, ..cats], na.rm = TRUE)

  # Mean variable-level different with ratios from prior iteration
  delta <- mean(ratio - ratio0)

  ratio0 <- ratio

  cat("Delta:", delta, "\n")

}

#------------------------

# Verify that the calibrated output matches macrodata constraints

# FS consumption shares
check <- sapply(1:nrow(fsagg), function(k) {
  irows <- which(d$age_grp == fsagg$age_grp[k] & d$cons_grp == fsagg$cons_grp[k])
  jcols <- unlist(fsagg$cat[k])
  sum(d$weight * d[irows, ..jcols])
})
plot(fsagg$share, check / sum(check))
abline(0, 1)

# State EIA SEDS
check <- sapply(1:nrow(eagg), function(k) {
  irows <- which(d$state == eagg$state[k])
  jcols <- unlist(eagg$cat[k])
  sum(d$weight * d[irows, ..jcols])
})
plot(1e6 * eagg$total, check)
abline(0, 1)

# State PCE
check <- sapply(1:nrow(sagg), function(k) {
  irows <- which(d$state == sagg$state[k])
  jcols <- unlist(sagg$cat[k])
  sum(d$weight * d[irows, ..jcols])
})
plot(1e6 * sagg$total, check)
abline(0, 1)

# National PCE
check <- sapply(1:nrow(nagg), function(k) {
  jcols <- unlist(nagg$cat[k])
  sum(d$weight * d[, ..jcols])
})
plot(1e6 * nagg$total, check)
abline(0, 1)

#------------------------

# How does consumption inequality compare pre- and post-calibration?

# Total consumption pre-calibration
pre <- d0 %>%
  as.data.frame() %>%
  mutate(total_cons = rowSums(.[fs.cons]) - rent - mrtgip - hinsp - ptaxp + rntval)
reldist::gini(pre$total_cons, weights = d$weight)

# Total consumption post-calibration
post <- d %>%
  as.data.frame() %>%
  mutate(total_cons = rowSums(.[fs.cons]) - rent - mrtgip - hinsp - ptaxp + rntval)
reldist::gini(post$total_cons, weights = d$weight)

#------------------------

exp.cats <- googlesheets4::read_sheet("13GRKkVZXapHtP7oK1WUh0Yu7OQ_9icd17wUGuhX-WRg", sheet = "Category Summary") %>%
  mutate(cat = tolower(cat))

# Final calibrated result
output <- d %>%
  cbind(select(sim, -any_of(names(.)))) %>%
  select(acs_2019_hid, weight, any_of(sort(exp.cats$cat))) %>%
  mutate_at(vars(-acs_2019_hid, -weight), ~ as.integer(round(.x, digits = -1))) %>%
  rename(SERIALNO = acs_2019_hid)

# Save variable name summary to disk
exp.cats %>%
  filter(cat %in% names(output)) %>%
  mutate(calibration = ifelse(cat %in% unlist(nagg$cat), "direct", "indirect"),
         calibration = ifelse(!cat %in% cats, "none", calibration)) %>%
  arrange(major, cat) %>%
  write_csv("production/calibration/Calibrated variable descriptions.csv")

# Save final calibrated data to disk
fst::write_fst(output, "production/calibration/CEI-ACS_2019 calibrated.fst", compress = 100)
