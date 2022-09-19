# Function that carries out iteration calibration to selected constraints

# TEST
# sim <- fst::read_fst("fusion/CEI/2015-2019/2019/CEI_2015-2019_2019_fused.fst", from = 1, to = 1276716)
# test <- calibrateCEIimplicate(sim, nimp = 5, dir = "fusion/CEI/2015-2019/2019/post")

calibrateCEIimplicate <- function(sim, output_cats, delta_stop, iter_max, dir) {

  t0 <- Sys.time()

  #-----

  # Data inputs
  load("survey-processed/CEX/cat_assignment.rda")
  acs <- read_fst(file.path(dir, "ACS_2019 calibration inputs.fst"))
  fsagg <- readRDS(file.path(dir, "Feiveson-Sabelhaus calibration targets.rds"))
  perminc.fit <- readRDS(file.path(dir, "Permanent income model.rds"))
  psplits <- readRDS(file.path(dir, "Consumption group ventiles.rds"))
  nagg <- readRDS(file.path(dir, "National PCE calibration targets.rds"))
  sagg <- readRDS(file.path(dir, "State PCE calibration targets.rds"))
  eagg <- readRDS(file.path(dir, "State EIA calibration targets.rds"))

  #-----

  # Subset of consumption variables to include in Feiveson and Sabelhaus (FS) definition of consumption
  fscats <- fsagg$cat[[1]]

  # Link between 'cat' and 'major' category
  cat.major <- cat_assignment %>%
    select(cat, major) %>%
    distinct() %>%
    mutate(cat = tolower(cat))

  # Variables in documentation but not in 'cat_assignment'
  miss <- setdiff(output_cats, cat.major$cat)
  cat("Categories documented but not in 'cat.major':", paste(miss, collapse = ", "), "\n")

  # Prepare the raw implicate data for calibration
  sim <- prepCEIsim(sim, fscats)

  #-----

  # Combine data inputs and convert to data.table
  d <- cbind(acs, sim) %>%
    mutate(perminc = NA_real_,
           cons_grp = NA_integer_) %>%
    as.data.table()

  # Columns that will be calibrated
  # This includes the FS consumption categories as well as any other consumption categories present in 'd'
  cats <- intersect(names(d), c(fscats, tolower(cat_assignment$cat)))

  # Original values of the calibration columns
  # Used to enforce a lower bound on calibrated values
  d0 <- copy(d[, ..cats])

  # Convert target columns to double/numeric to avoid data.table class conflict and associated truncation messages
  # Enforce non-negative values among the consumption variables to be calibrated
  d <- mutate_at(d, cats, as.numeric)

  #------------------------

  # CALIBRATE DATA IN 'd'
  # Calibrate via iterative approach

  fsagg$adj <- NA
  nagg$adj <- NA
  sagg$adj <- NA
  eagg$adj <- NA
  ratio0 <- rep(0, length(cats))

  delta <- delta0 <- Inf
  iter <- 0

  while (delta > delta_stop & iter < iter_max) {

    iter <- iter + 1
    cat("--- Calibration iteration", iter, "---\n")

    #------------------------

    # Calibrate using 'fsagg' (Feiveson-Sabelhaus consumption shares)

    # Update consumption group assignment of each household, as this can potentially change with each iteration
    # This appears to have little effect on results, but it is technically correct
    set(d, j = "vehdep", value = 0.15 * d$vehval)
    set(d, j = "fs_cons", value = rowSums(d[, ..fscats]))
    set(d, j = "total_cons", value = d$fs_cons - d$rent - d$mrtgip - d$hinsp - d$ptaxp + d$rntval)
    set(d, j = "perminc", value = predict(perminc.fit, newdata = d))
    consGroup <- function(perminc, weight, fs_denom, age_grp) {
      perminc_ptile <- spatstat.geom::ewcdf(perminc, weights = weight * fs_denom, normalise = TRUE)(perminc)
      cons_grp <- findInterval(perminc_ptile, c(-Inf, psplits[[first(age_grp)]], Inf))
      return(cons_grp)
    }
    d[, cons_grp := consGroup(perminc, weight, fs_denom, age_grp), by = "age_grp"]

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

    cat("FS consumption shares adjustment factors\n")
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

    cat("State EIA SEDS adjustment factors\n")
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

    cat("National PCE calibration adjustment factors\n")
    print(summary(m$adj))

    #------------------------

    # Calculate measure (delta) of the overall change in calibrated values
    # When delta is sufficiently small (delta_stop), loop is terminated

    # Mean ratio of calibrated values to original/un-calibrated values for each variable
    ratio <- colMeans(d[, ..cats] / d0[, ..cats], na.rm = TRUE)

    # Mean variable-level change in adjustment ratios from prior iteration
    delta <- mean(abs(ratio - ratio0))
    cat("Delta:", signif(delta, 3), "\n")

    # Force stop if 'delta' shows no significant change from previous iteration
    if (abs(delta - delta0) < delta_stop / 10) {
      iter <- iter_max
      cat("Stopping due to insignificant change in 'delta'\n")
    }

    delta0 <- delta
    ratio0 <- ratio

  }

  # Safety check
  stopifnot(!is.unsorted(d$acs_2019_hid))

  #------------------------

  # Verify that the calibrated output matches macrodata constraints

  # # FS consumption shares
  # check <- sapply(1:nrow(fsagg), function(k) {
  #   irows <- which(d$age_grp == fsagg$age_grp[k] & d$cons_grp == fsagg$cons_grp[k])
  #   jcols <- unlist(fsagg$cat[k])
  #   sum(d$weight * d[irows, ..jcols])
  # })
  # plot(fsagg$share, check / sum(check))
  # abline(0, 1)
  #
  # # State EIA SEDS
  # check <- sapply(1:nrow(eagg), function(k) {
  #   irows <- which(d$state == eagg$state[k])
  #   jcols <- unlist(eagg$cat[k])
  #   sum(d$weight * d[irows, ..jcols])
  # })
  # plot(1e6 * eagg$total, check)
  # abline(0, 1)
  #
  # # State PCE
  # check <- sapply(1:nrow(sagg), function(k) {
  #   irows <- which(d$state == sagg$state[k])
  #   jcols <- unlist(sagg$cat[k])
  #   sum(d$weight * d[irows, ..jcols])
  # })
  # plot(1e6 * sagg$total, check)
  # abline(0, 1)
  #
  # # National PCE
  # check <- sapply(1:nrow(nagg), function(k) {
  #   jcols <- unlist(nagg$cat[k])
  #   sum(d$weight * d[, ..jcols])
  # })
  # plot(1e6 * nagg$total, check)
  # abline(0, 1)

  #------------------------

  # How does consumption inequality compare pre- and post-calibration?

  # # Total consumption pre-calibration
  # pre <- d0 %>%
  #   as.data.frame() %>%
  #   mutate(total_cons = rowSums(.[fscats]) - rent - mrtgip - hinsp - ptaxp + rntval)
  # reldist::gini(pre$total_cons, weights = d$weight)
  #
  # # Total consumption post-calibration
  # post <- d %>%
  #   as.data.frame() %>%
  #   mutate(total_cons = rowSums(.[fscats]) - rent - mrtgip - hinsp - ptaxp + rntval)
  # reldist::gini(post$total_cons, weights = d$weight)

  #------------------------

  # Create final calibrated output
  d <- d %>%
    select(all_of(output_cats)) %>%
    mutate_all(~ as.integer(round(.x, -1)))  # Reduces precision for smaller file size

  # Report processing time
  tout <- difftime(Sys.time(), t0)
  cat("Total processing time:", signif(as.numeric(tout), 3), attr(tout, "units"), "\n", sep = " ")

  return(d)

}

#------------------

# General prep function to calculate/define necessary variables as well as FS and total consumption variables
prepCEIsim <- function(sim, fscats) {
  h.acs <- read_fst("survey-processed/ACS/2019/ACS_2019_H_processed.fst", columns = c("hincp", "mortgage", "insp", "taxamt", "rntp", "renteq"))
  sim %>%
    cbind(h.acs) %>%
    mutate(mrtgip = round(mortint_share * mortgage),
           mrtgpp = mortgage - mrtgip,
           tax = tax_rate * hincp) %>%
    rename(hinsp = insp,  # TO DO: These variables should really be removed, since they do not vary across implicates (they are just ACS variables)
           ptaxp = taxamt,
           rent = rntp,
           rntval = renteq) %>%
    select(-mortint_share, -tax_rate, -hincp, -mortgage) %>%
    mutate(vehdep = 0.15 * vehval) %>% # Estimated annual vehicle depreciation
    mutate_at(fscats, ~ pmax(0, .x)) %>% # Enforce non-negative values among the consumption variables
    mutate(fs_cons = rowSums(.[fscats]), # Sum total FS consumption at household level
           total_cons = fs_cons - rent - mrtgip - hinsp - ptaxp + rntval)  # More conventional definition of total consumption using housing rental equivalence
}

