library(fusionData)

data(cpi_series)
data(BEA_pce_national)
data(BEA_pce_state)

load("survey-processed/CEX/cat_assignment.rda")

#----------------------------------

# Generate the 'p' microdata data frame containing CEI 2015-2019 data to be calibrated and necessary auxiliary variables
# Generate 'fs.cons' vector that gives category/variable names included in Feiveson-Sabelhaus definition of consumption
# Generate the 'fsagg' object containing information on the Feiveson-Sabelhaus macrodata constraints to be calibrated to
# Will also generate a series of plots in the Plots viewer with diagnostic data
# WARNING: This step uses the "GB2" package, which will (unfortunately) require a whole bunch of package installs due to dependencies
source("production/calibration/01 Process Feiveson-Sabelhaus macrodata.R")

# Generate the 'nagg' (national) and 'sagg' (state) BEA PCE macrodata constraints to be calibrated to
# NOTE: Ignoring SEDS state utility expenditure macrodata for now
source("production/calibration/02 Process BEA PCE macrodata.R")

#----------------------------------

# ATTEMPT TO CALIBRATE DATA IN 'd'

nagg$adj <- NA
sagg$adj <- NA
fsagg$adj <- NA

# Columns that will be calibrated
cats <- intersect(names(p), c(fs.cons, tolower(cat_assignment$cat)))

# Convert target columns to double/numeric to avoid data.table class conflict and associated truncation messages
p <- p %>% mutate_at(cats, as.numeric)

# Manual kluge for "vehusd" to eliminate negative values (not clear how to handle them)
p$vehusd <- pmax(0, p$vehusd)

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

  cat("State calibration adjustment factors\n")
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

# How does consumption inequality compare pre- and post-calibration?

# # Total consumption pre-calibration
# total.pre <- d$total_cons
# reldist::gini(total.pre, weights = d$weight)
#
# # Total consumption post-calibration
# d <- d %>%
#   as.data.frame() %>%
#   mutate(fs_cons = rowSums(.[fs.cons]),  # Sum total FS consumption at household level
#          total_cons = fs_cons - rent - mrtgip - hinsp - ptaxp + rntval)  # More conventional definition of total consumption using housing rental equivalence
# total.post <- d$total_cons
# reldist::gini(total.post, weights = d$weight)

#------------------------

# Final calibrated result
output <- subset(d, select = c("acs_2019_hid", "weight", cats)) %>%
  mutate_at(cats, ~ as.integer(round(.x))) %>%
  select(-vehdep)

# Example: save result to disk (84 MB)
fst::write_fst(output, "production/calibration/CEI_2015-2019_calibrated.fst", compress = 100)
