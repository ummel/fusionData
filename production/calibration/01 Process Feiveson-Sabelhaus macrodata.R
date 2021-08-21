# Feiveson and Sabelhaus 2019: https://www.federalreserve.gov/econres/feds/files/2019010r1pap.pdf

# Feiveson-Sabelhaus income cohort data from Table 5 of publication
fs.data.orig <- googlesheets4::read_sheet(ss = "1aMnqP3-Yy4K9VD2kMABT539uPfBDgrDfwb2vc4mM3iU", sheet = "Income cohorts", skip = 5)

# Feiveson-Sabelhaus education cohort data from Table 5 of publication
#fs.data.orig <- googlesheets4::read_sheet(ss = "1aMnqP3-Yy4K9VD2kMABT539uPfBDgrDfwb2vc4mM3iU", sheet = "Education cohorts", skip = 5)

#-----

# CEX-based variables to include in Feiveson and Sabelhaus (FS) definition of consumption
# Exclude all loan principal payments: MRTGPP, MRTGPS, VEHPRN
# Exclude vehicle purchases (no effect on net worth): VEHNEW, VEHUSD
# Exclude all categories in "Other" major category
# Exclude "HMTIMP" (Home maintenance and improvement)
#  -- Note that "HMTIMP" technically includes home maintenance (as opposed to home improvement), which should be part of FS consumption
#  -- But home improvement and maintenance would have to be separate variables to capture this correctly
#  -- Assuming category is mostly home improvement and, therefore, not part of FS consumption
# Exclude "Other cash transfers" (OCASH)
#  -- Appears to captured in the FS concept of "net interfamily transfers", and so are separate from "consumption" as defined by FS

fs.cons <- cat_assignment %>%
  filter(major != "Other") %>%
  filter(!cat %in% c("MRTGPP", "MRTGPS", "VEHPRN", "VEHNEW", "VEHUSD", "HMTIMP", "OCASH")) %>%
  pull(cat) %>%
  tolower() %>%
  unique() %>%
  c("vehdep")  # Add custom "Vehicle depreciation" variable

#-----

# Load necessary variables from processed ACS-PUMS 2019 househould-level microdata
pums <- read_fst("survey-processed/ACS/2019/ACS_2019_H_processed.fst",
                 columns = c("acs_2019_hid", "weight", "state", "np", "insp", "taxamt", "rntp", "renteq")) %>%
  rename(hinsp = insp,
         ptaxp = taxamt,
         rent = rntp,
         rntval = renteq)

# Load necessary variables from fused ACS-RECS microdata
recs <- read_fst("production/fusion/RECS_2019_sim.fst",
                 columns = c("acs_2019_hid", "dollarel", "dollarng", "dollarlp", "dollarfo")) %>%
  mutate(elec = dollarel,
         ngas = dollarng,
         ofuel = dollarlp + dollarfo) %>%
  select(acs_2019_hid, elec, ngas, ofuel)

# Load necessary variables from fused ACS-CEI microdata
# Calculate total "FS consumption" (fs_cons) and total conventional consumption (total_cons)
# Note inclusion of estimated vehicle depreciation
sim <- read_fst("production/fusion/CEX_2015-2019_sim.fst") %>%
  left_join(pums, by = "acs_2019_hid") %>%
  left_join(recs, by = "acs_2019_hid") %>%
  mutate(vehdep = 0.15 * vehval,  # Estimated annual vehicle depreciation
         health = pmax(0, health),  # Enforce non-negative values
         recrp = pmax(0, recrp)) %>%
  mutate(fs_cons = rowSums(.[fs.cons]), # Sum total FS consumption at household level
         total_cons = fs_cons - rent - mrtgip - hinsp - ptaxp + rntval)  # More conventional definition of total consumption using housing rental equivalence
#select(acs_2019_hid, total_cons, fs_cons, all_of(fs.cons))

#-----

# The two measures of consumption are highly correlated overall
# cor(sim$total_cons, sim$fs_cons)
#
# # How important is estimated vehicle depreciation in the calculation of total household consumption?
# sim %>%
#   filter(vehdep > 0) %>%
#   slice_sample(prop = 0.2) %>%
#   ggplot(aes(x = total_cons, y = vehdep / total_cons)) +
#   geom_smooth() +
#   xlim(10e3, 250e3)

#-----

# Load and process ACS-PUMS 2019 househould-level microdata

p <- read_fst("survey-processed/ACS/2019/ACS_2019_P_processed.fst", columns = c("acs_2019_hid", "relshipp", "agep", "schl", "pincp")) %>%

  #filter(acs_2019_hid %in% sample(pums$acs_2019_hid, 10e3)) %>%   # SUB-SAMPLE FOR TESTING

  filter(relshipp %in% c("Reference person", "Opposite-sex husband / Wife / Spouse", "Same-sex husband / Wife / Spouse")) %>%
  arrange(acs_2019_hid, relshipp) %>%
  group_by(acs_2019_hid) %>%
  summarize(denom_total = n(), # Total FS denominator (either 1 or 2)
            #n_total = sum(n_total), # Total number of people
            #cons_total = sum(fs_cons),  # Total FS consumption
            pinc_total = sum(pincp),  # Total SCF permanent income (spouses only)
            educ_ref = max(schl),  # Maximum educational attainment of reference person or spouse (if present)
            age_ref = mean(agep),  # Average age of reference person and spouse (if present)
            .groups = "drop") %>%

  # Assign each CU to FS age cohorts
  mutate(age_grp = findInterval(age_ref, c(-Inf, seq(from = 35, to = 75, by = 10), Inf)),
         age_grp = unique(fs.data.orig$age_grp)[age_grp]) %>%

  # Assign each CU to FS education cohorts
  mutate(educ_grp = "High school or less",
         educ_grp = ifelse(educ_ref %in% c("Some college, but less than 1 year", "1 or more years of college credit, no degree", "Associate's degree"), "Some college", educ_grp),
         educ_grp = ifelse(educ_ref %in% c("Bachelor's degree", "Master's degree", "Professional degree beyond a bachelor's degree", "Doctorate degree"), "College degree or higher", educ_grp)) %>%

  #left_join(select(pums, acs_2019_hid, weight, np), by = "acs_2019_hid") %>%
  left_join(sim, by = "acs_2019_hid") %>%

  # Age-cohort specific percentile for income group assignment
  # The calculated percentile is the household's position in the sorted per-spouse income distribution
  # The percentile gives the share of the total HoH/spouse population that has a lower per-spouse income
  group_by(age_grp) %>%
  mutate(fs_inc_pc = pinc_total / denom_total,
         fs_inc_ptile = spatstat.geom::ewcdf(fs_inc_pc, weights = weight * denom_total, normalise = TRUE)(fs_inc_pc)) %>%
  ungroup() %>%

  # Assign each CU (actually, PEU) to an income group
  mutate(inc_grp = findInterval(fs_inc_ptile, c(-Inf, 0.5, 0.9, Inf)),
         inc_grp = unique(fs.data.orig$inc_grp)[inc_grp]) %>%

  # Restrict to necessary variables
  select(acs_2019_hid, weight, age_grp, educ_grp, inc_grp, fs_inc_pc, fs_inc_ptile, np, denom_total, fs_cons, total_cons, any_of(names(sim)))

#-----

# Compute FS-cohort summary variables derived from CEX-PUMS fused data
group.totals <- p %>%
  group_by(age_grp, inc_grp) %>%
  summarize(
    n = sum(weight * denom_total),  # Number of HoH or spouses in cohort (i.e. FS denominator) (millions)
    n_tot = sum(weight * np),  # Number of people in cohort (millions)
    n_peu = sum(weight),  # Number of PEU's in cohort (millions)
    cons_ce = sum(weight * fs_cons),  # Total FS consumption based on Consumer Expenditure Survey (CE) variables, in $millions
    #perm_inc = sum(weight * pinc_total)  # Total permanent income (SCF), in $millions
    #cons_adj = sum(weight * cons_total) / cons_ce,  # Adjustment factor to account for difference between total household consumption and consumption of the PEU (using CE)
    #pop_adj = sum(weight * denom_total) / n,  # Adjustment factor for FS denominator; difference between PEU and households
    pop_adj = 1,
    cons_adj = 1,
    .groups = "drop"
  )

#-----

# Add cohort summary variables to original FS data
fs.data <- fs.data.orig %>%
  left_join(group.totals, by = c("age_grp", "inc_grp"))

#-----

# Impute the seemingly erroneous "55-64" age group consumption figure for Bottom 50%
# Without this adjustment, the 55-64 age group consumption looks erroneous
# Uses a spline to impute 55-64 consumption figure that proportionally adjusts all values to maintain original total consumption for "Bottom 50%"
# x <- fs.data$consumption[1:6]
# n <- fs.data$n[1:6]
# tot <- sum(x * n)
# x[4] <- NA
# x <- spline(x, xout = 1:6)$y
# fs.data$consumption[1:6] <- tot * x * n / sum(x * n) / n

#-----

# Share of total consumption to redistribute within each income cohort
move <- rep(c(0.06, 0.06, 0.06), each = 6)
#move <- rep(c(0, 0, 0), each = 6)  # No adjustment

# Which age cohorts within each income group will be the givers/source of transfers to younger households?
givers <- c(0,0,0,0,1,1,
            0,0,0,1,1,1,
            0,0,1,1,1,1)

# Which age cohorts within each income group will be the recipient of transfers to younger households?
recipients <- c(1,0,0,0,0,0,
                1,1,1,0,0,0,
                1,1,0,0,0,0)

#---

fs.adj <- fs.data %>%
  mutate(cons_ce_share = cons_ce / sum(cons_ce), # Share of total CE PEU consumption within the cohort
         age_grp = factor(age_grp, levels = unique(fs.data$age_grp)),
         inc_grp = factor(inc_grp, levels = unique(fs.data$inc_grp)),
         m = move,
         g = givers,
         r = recipients
  ) %>%
  group_by(inc_grp) %>%
  mutate(
    wealth_stock = cumsum(wealth_change),  # Estimate total wealth of each cohort
    N = sum(consumption * n), # Total income cohort consumption
    age_weight = (seq(30, 80, by = 10) / 80) ^ 3,  # Kluge, but generates plausible results; adds an age component when allocating among givers and receivers
    loss = g * wealth_stock * age_weight * n,
    loss = m * N * (loss / sum(loss)),
    gain = ifelse(r == 1, n / age_weight, 0),
    gain = m * N * (gain / sum(gain)),
    consumption = (consumption * n - loss + gain) / n  # New, updated consumption per HOH/spouse
  ) %>%
  ungroup() %>%
  mutate(saving = income - consumption,
         cons_share_fs = (consumption * n) / sum(consumption * n),  # NO adjustment
         cons_share = (consumption * n * cons_adj) / sum(consumption * n * cons_adj)) %>%   # Adjusts for household vs. PEU total consumption difference (see comment about 'cons_adj')
  arrange(inc_grp, age_grp)

#-----

# Plot of adjusted shares...
fs.adj %>%
  group_by(inc_grp) %>%
  mutate(
    cons_ce_share = cons_ce_share / sum(cons_ce_share),
    yratio = ((cons_ce / n) - consumption) / consumption,
  ) %>%
  ggplot(aes(x = wealth_stock, y = yratio, color = inc_grp, label = as.integer(age_grp))) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_label(alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", size = 0.5) +
  scale_y_continuous("(CE consumption - FS consumption) / FS consumption (% deviation)", labels = scales::percent) +
  scale_x_continuous("Approximate mean wealth per capita (log scale)", labels = scales::dollar, trans = "log") +
  theme_bw()

#--------------------

# Custom function used to plot and extract coefficients from fitted GB2 distribution object

plotGB2 <- function(x, n = 20) {

  # Function to compute Lorenz curve coordinates from GB2 parameter values
  # https://rdrr.io/cran/GB2group/src/R/lorenz_curves.R
  lc.gb2 <- function(theta, pr) {
    a <- theta[1]
    b <- 10
    p <- theta[3]
    q <- theta[4]
    t <- GB2::qgb2(pr, a, b, p, q)
    lcgb2 <- GB2::pgb2(t, a, b, p + 1 / a, q - 1 / a)
    return(lcgb2)
  }

  #---

  # Plot density; x-axis extends to 99th percentile of theoretical distribution
  param <- x$ewmd.estimation[1, ]
  names(param) <- c("shape1", "scale", "shape2", "shape3")
  a <- param[1]
  b <- param[2]
  p <- param[3]
  q <- param[4]
  if (is.na(b)) b <- 10
  u <- GB2::qgb2(0.99,  a, b, p, q)  # Upper percentile to plot
  mu <- GB2::moment.gb2(1, a, b, p, q)   # Mean of fitted distribution
  df <- function(x) GB2::dgb2(x * mu, a, b, p, q)
  curve(df, from = 0, to = u / mu, n = 1000, xlab = "Relative value (mean = 1)", ylab = "Density")
  abline(h = 0, lty = 2)

  #---

  # Compute percentile split points that create 'n' groups with equal consumption shares
  pv <- seq(0, 1, length.out = 1000)
  lv <- lc.gb2(param, pv)
  spf <- suppressWarnings(splinefun(x = lv, y = pv, method = "monoH.FC"))
  psplits <- spf(seq(0, 1, length.out = n + 1)[2:n])

  #---

  # Plot Lorenz curve
  plot(NULL, ylab = "Cumulative consumption share", ylim = c(0, 1), xlim = c(0, 1), xlab = "Cumuative population share", panel.first = grid(col = "gray78"))
  legendtext <- paste(c(x$distribution, " distribution"), collapse = "")
  shares <- c(0, x$grouped.data[1, ], 1)
  pop <- c(0, x$grouped.data[2, ], 1)
  points(pop, shares, pch = 20)
  curve(lc.gb2(param, x), add = T, col = 2, n = 1000)
  abline(0, 1, lty = 2)
  legend("topleft", legend = legendtext, cex = 0.7, lty = 1, col = 2, ncol = 1)

  #---

  # Compute Lorenz curve coordinates given 'n' equally-spaced segments on the x-axis
  pv <- seq(0, 1, length.out = n + 1)
  lc <- lc.gb2(param, pr = pv)

  #---

  # Return information about the distribution
  result <- list(param = param,
                 lorenz = list(x = pv, y = lc),
                 psplits = psplits)

  # Return Lorenz curve y-values
  return(result)

}

#--------------------

# Determine percentile split points that create equal-consumption groups for each age cohort

# TESTING
# age_grp = "Under 35"
# fs_data = fs.adj
# ce_data = p

fitGB2 <- function(age_grp, fs_data, ce_data) {

  message("Processing age group: ", paste(age_grp, collapse = ", "))

  # FS relationship between:
  #  1) pshare: Cumulative share of population (when ranked by per-capita/spouse permanent income) and
  #  2) cshare: Cumulative share of consumption
  x <- fs_data %>%
    filter(age_grp %in% !!age_grp) %>%
    #arrange(consumption) %>%
    mutate(
      ctotal = consumption * n,
      cshare = cumsum(ctotal * cons_adj) / sum(ctotal * cons_adj), # Make adjustment for difference between PEU's and whole households
      pshare = cumsum(n * pop_adj) / sum(n * pop_adj)  # Cumulative share of FS per-capita/spouse population, adjusted for PEU vs. household difference
    ) %>%
    select(inc_grp, consumption, ctotal, pshare, cshare)

  # Total consumption in the adjusted FS data
  fs.max <- sum(x$ctotal)

  #---

  # CEX-ACS estimated relationship between:
  #  1) pshare: Cumulative share of population (when ranked by per-capita/spouse consumption)
  #  2) cshare: Cumulative share of consumption
  p2 <- ce_data %>%
    filter(age_grp %in% !!age_grp) %>%
    arrange(fs_inc_pc) %>%  # Order by per-capita/spouse permanent income (or a proxy for it; see how it is defined above)
    mutate(
      ctotal = fs_cons * weight,  # Using household FS consumption
      #ctotal = cons_total * weight,  # Using total household consumption
      cshare = cumsum(ctotal) / fs.max,
      pshare = cumsum(denom_total * weight) / sum(denom_total * weight)
    ) %>%
    select(ctotal, pshare, cshare)

  # Stop with error if the CE data has higher total consumption that adjusted FS (this shouldn't be the case)
  stopifnot(sum(p2$ctotal) <= fs.max)

  #---

  # We only have a couple of points on this line, so we don't know it's true shape
  # plot(c(0, x$pshare), c(0, x$cshare), type = "b")
  # lines(c(0, p2$pshare), c(0, p2$cshare), col = 2)
  # abline(0, 1, lty = 3)

  #---

  # Percentile to guess initial value, since we need an extra data point to fit a GB2 distribution
  # This effectively assumes that households at the 'pct' percentile of the age cohort are reporting accurately in CEI
  pct <- 0.10

  # Guess from the 'pct' percentile; i.e. assume reporting is accurate that low in the distribution
  guess <- approxfun(x = c(0, p2$pshare), y = c(0, p2$cshare))(pct)

  # Add estimated pct-th percentile from exponential fit
  x <- x %>%
    add_row(pshare = c(0, pct), cshare = c(0, guess)) %>%
    arrange(pshare)

  #-----

  # 3 iterations seems to be enough
  for (i in 1:3) {

    if (i == 1) {
      x0 <- x$pshare
      s1 <- x$cshare
    } else {
      x0 <- seq(0, 1, length.out = ifelse(i == 2, 10, 20) + 1)
      s1 <- plotGB2(fit, n = length(x0) - 1)$lorenz$y
    }

    # Create interpolation for CE data points
    s2 <- approx(x = c(0, p2$pshare), y = c(0, p2$cshare), xout = x0)$y

    # Retain larger of the two estimates at each x0
    mout <- pmax(s1, s2)

    # Plot the comparison
    plot(x0, s1, type = "b", ylim = c(0, 1))  # FS
    lines(x = x0, y = s2, col = 2)  # CE
    lines(x = x0, y = mout, col = 3)  # Larger of the two

    # Fit GBR distribution to known points
    fit <- GB2group::fitgroup.gb2(y = diff(mout),
                                  x = diff(x0),
                                  gini.e = reldist::gini(diff(mout), diff(x0)))

  }

  # Return the final fit results using 20 split points along x-axis
  gb2.out <- plotGB2(fit, n = 20)
  return(gb2.out)

}

# List of GB2 final fits for each age group
# Messages "Unable to..." are generally OK
gb2.fits <- lapply(unique(fs.data$age_grp), fitGB2, fs_data = fs.adj, ce_data = p)

# Extract 'psplits' list from 'gb2.fits'
psplits <- map(gb2.fits, "psplits") %>%
  setNames(unique(fs.data$age_grp))

#-----

# Plot implied pseudo-Lorenz curves for each age cohort
# These are not "true" Lorenz curves, because the households are not ordered by consumption
gg <- psplits %>%
  map_dfr( ~ tibble(x = c(0, .x, 1), y = seq(0, 1, by = 0.05))) %>%
  mutate(`Age cohort` = factor(rep(levels(fs.adj$age_grp), each = 21), levels = levels(fs.adj$age_grp))) %>%
  ggplot(aes(x = x, y = y, color = `Age cohort`)) +
  geom_line() +
  geom_abline(slope = 1, linetype = "dashed") +
  xlab("Cumulative share of HoH/spouse population (ordered by per-capita income)") +
  ylab("Cumulative share of FS consumption") +
  theme_bw()
gg

#-----

# Share of total FS consumption and population, by age group
# These figures are adjusted to account for the difference between true total consumption in a inc-age cohort and total consumption among PEU's only
# That makes the adjustment most relevant for younger households more likely to be in households with multiple CU's
age.shares <- fs.adj %>%
  mutate(cons_share = consumption * n * cons_adj,
         cons_share = cons_share / sum(cons_share),
         pop_share = n * pop_adj,
         pop_share = pop_share  / sum(pop_share)) %>%
  group_by(age_grp) %>%
  summarize(cons_share = sum(cons_share), # Make adjustment for difference between PEU's and whole households
            pop_share = sum(pop_share))

# Assumed age-cons_grp consumption shares
n <- length(psplits[[1]]) + 1
fsagg <- age.shares %>%
  mutate(cons_grp = list(seq(n))) %>%
  unnest(cons_grp) %>%
  mutate(share = cons_share / n,
         cat = list(fs.cons)) %>%
  select(age_grp, cons_grp, share, cat)

# Safety check
stopifnot(round(sum(fsagg$share), 3) == 1)

#-----

# Assign each household in 'p' to a consumption ventile (cons_grp)
# The 'cons_grp' column is used in conjunction with consumption shares indicated in 'fsagg' data frame
p <- p %>%
  group_by(age_grp) %>%
  mutate(cons_grp = findInterval(fs_inc_ptile, c(-Inf, psplits[[first(age_grp)]], Inf))) %>%
  ungroup()

# Cleanup
rm(pums, recs, sim)
gc()
