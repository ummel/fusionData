# This script generates and saves 5 outputs to disk:
# Permanent income model.rds
# Consumption group ventiles.rds
# Feiveson-Sabelhaus calibration targets.rds
# Pseudo Lorenz curves.png
# ACS_2019 calibration inputs.fst

#-----

library(fusionData)
source("fusion/CEI/2015-2019/2019/post/calibrateCEIimplicate.R")
load("data/token.rda")
googlesheets4::gs4_auth(token = token)

#-----

# Data inputs

# Necessary ACS household and person variables
h <- read_fst("survey-processed/ACS/2019/ACS_2019_H_processed.fst",
              columns = c("acs_2019_hid", "weight", "np", "state"))
p <- read_fst("survey-processed/ACS/2019/ACS_2019_P_processed.fst",
              columns = c("acs_2019_hid", "relshipp", "agep", "schl", "pincp"))

# First implicate of fused CEI-ACS microdata
# Assume the FS calibration targets are constant across implicates
# In which case, we can use just the first implicate to generate the targets
sim <- read_fst("fusion/CEI/2015-2019/2019/CEI_2015-2019_2019_fused.fst", from = 1, to = nrow(h))

# Full set of consumption categories to be present in final data outputs
#cats <- googlesheets4::read_sheet(ss = "1oECoLziaJcGQV21Wy26XIKO5Yn_fYth9C3xTRLBRV_Y", sheet = "Categories")$cat

# Feiveson-Sabelhaus income cohort data from Table 5 of publication
# Feiveson and Sabelhaus 2019: https://www.federalreserve.gov/econres/feds/files/2019010r1pap.pdf
fs.data.orig <- googlesheets4::read_sheet(ss = "1aMnqP3-Yy4K9VD2kMABT539uPfBDgrDfwb2vc4mM3iU", sheet = "Income cohorts", skip = 5)

# NOT USED (yet): Education cohort data
#fs.data.orig <- googlesheets4::read_sheet(ss = "1aMnqP3-Yy4K9VD2kMABT539uPfBDgrDfwb2vc4mM3iU", sheet = "Education cohorts", skip = 5)

#-----

# Identify subset of consumption variables to include in Feiveson and Sabelhaus (FS) definition of consumption
# Exclude all loan principal payments: MRTGPP, MRTGPS, VEHPRN
# Exclude vehicle purchases (no effect on net worth): VEHNEW, VEHUSD
# Exclude all categories in "Other" major category
# Exclude "HMTIMP" (Home maintenance and improvement)
#  -- Note that "HMTIMP" technically includes home maintenance (as opposed to home improvement), which should be part of FS consumption
#  -- But home improvement and maintenance would have to be separate variables to capture this correctly
#  -- Assuming category is mostly home improvement and, therefore, not part of FS consumption
# Exclude "Other cash transfers" (OCASH)
#  -- Appears to captured in the FS concept of "net interfamily transfers", and so are separate from "consumption" as defined by FS
fscats <- setdiff(names(sim), c("hmtimp", "mrtgpp", "mrtgps", "ocash", "rntval", "tax", "vehnew", "vehprn", "vehusd", "vehval"))

# Add custom "Vehicle depreciation" consumption variable
fscats <- c(fscats, "vehdep")

#-----

# Calculate total "FS consumption" (fs_cons) and total conventional consumption (total_cons)
# Note inclusion of estimated vehicle depreciation
sim <- prepCEIsim(sim, fscats)

# NOTE any 'fscats' variables that are missing in 'sim'
stopifnot(length(setdiff(fscats, names(sim))) == 0)

#-----

# The two measures of consumption are highly correlated overall
# cor(sim$total_cons, sim$fs_cons)

# # How important is estimated vehicle depreciation (vehdep) in the calculation of total household consumption?
# sim %>%
#   filter(vehdep > 0) %>%
#   slice_sample(prop = 0.1) %>%
#   ggplot(aes(x = total_cons, y = vehdep / total_cons)) +
#   geom_smooth() +
#   xlim(10e3, 250e3)

#-----

# Process ACS-PUMS 2019 person-level microdata to create household-level summary variables needed to alignment with FS cohorts
# One-time calculation to assign PUMS respondent to FS cohorts
p <- p %>%

  # !!!! SUB-SAMPLE FOR TESTING
  #filter(acs_2019_hid %in% sample(h$acs_2019_hid, 10e3)) %>%

  filter(relshipp %in% c("Reference person", "Opposite-sex husband / Wife / Spouse", "Same-sex husband / Wife / Spouse")) %>%
  arrange(acs_2019_hid, relshipp) %>%

  group_by(acs_2019_hid) %>%
  summarize(fs_denom = n(), # Total FS denominator (either 1 or 2)
            pinc_total = sum(pincp),  # Total income (HOH and spouse only)
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

  # Add household weight, state, and size ('np')
  left_join(sim, by = "acs_2019_hid") %>%

  # Arrange row-order to match original 'h' (PUMS households); this should be same row-order as in 'sim'
  arrange(match(acs_2019_hid, h$acs_2019_hid))

# Safety check
stopifnot(nrow(p) == nrow(h))
stopifnot(all(as.character(p$acs_2019_hid) == as.character(h$acs_2019_hid)))
rm(h, sim)

#-----

# Estimate proxy for "permanent income" for each household in 'p'
# Since we don't observe permanent income, it is estimated on basis of predictor variables below
perminc.fit <- lm(pinc_total / fs_denom ~ factor(fs_denom) + I(np - fs_denom) + state + age_grp + educ_grp + total_cons, data = p, weights = p$weight, model = FALSE)
p$perminc <- predict(perminc.fit)

# Slim the prediction model so it can be saved to disk at end of script
# https://stackoverflow.com/questions/21896265/how-to-minimize-size-of-object-of-class-lm-without-compromising-it-being-passe
cm <- perminc.fit
cm$residuals = c()
cm$fitted.values = c()
cm$effects = c()
cm$qr$qr = c()
cm$linear.predictors = c()
cm$weights = c()
cm$prior.weights = c()
cm$data = c()
perminc.fit <- cm

#-----

# Assign each household in 'p' to its FS permanent income cohort
p <- p %>%

  # Age-cohort specific percentile for income group assignment
  # The calculated percentile is the household's position in the sorted per-spouse income distribution
  # The percentile gives the share of the total HoH/spouse population that has a lower per-spouse income
  group_by(age_grp) %>%
  mutate(perminc_ptile = spatstat.geom::ewcdf(perminc, weights = weight * fs_denom, normalise = TRUE)(perminc)) %>%
  ungroup() %>%

  # Assign each CU (actually, PEU) to an income group
  mutate(inc_grp = findInterval(perminc_ptile, c(-Inf, 0.5, 0.9, Inf)),
         inc_grp = unique(fs.data.orig$inc_grp)[inc_grp]) %>%

  # Restrict to necessary variables
  select(acs_2019_hid, weight, state, np, fs_cons, total_cons, perminc, perminc_ptile, age_grp, educ_grp, inc_grp, np, fs_denom)

# Safety check
stopifnot(!is.unsorted(p$acs_2019_hid))

#-----

# Compute FS-cohort summary variables and add original FS values from paper
# NOTE: Adjustment of original FS figures from 2016 to 2019 dollars (inflation factor = 1.065)

fs.data <- fs.data.orig %>%
  mutate_if(is.numeric, ~ .x * 1.065) %>%  # Inflation adjustment from 2016 to 2019
  left_join(p %>%
              group_by(age_grp, inc_grp) %>%
              summarize(
                n = sum(weight * fs_denom),  # Number of HoH or spouses in cohort (i.e. FS denominator) (millions)
                cons_ce = sum(weight * fs_cons),  # Total FS consumption based on Consumer Expenditure Survey (CE) variables, in $millions
                .groups = "drop"),
            by = c("age_grp", "inc_grp")) %>%
  mutate(age_grp = factor(age_grp, levels = unique(age_grp)),
         inc_grp = factor(inc_grp, levels = unique(inc_grp))) %>%
  group_by(inc_grp) %>%
  mutate(wealth_stock = cumsum(wealth_change)) %>%
  ungroup()

#-----

# Impute the seemingly erroneous "55-64" age group consumption figure for Bottom 50%
# Without this adjustment, the 55-64 age group consumption looks erroneous
# Uses a spline to impute 55-64 consumption figure that proportionally adjusts all values to maintain original total consumption for "Bottom 50%"
x <- fs.data$consumption[1:6]
n <- fs.data$n[1:6]
tot <- sum(x * n)
x[4] <- NA
x <- spline(x, xout = 1:6)$y
fs.data$consumption[1:6] <- tot * x * n / sum(x * n) / n

#-----

# Update figures affected by consumption imputation
fs.data <- fs.data %>%
  mutate(saving = income - consumption,
         cons_fs = n * consumption)

#-----

# Function used to adjust FS consumption values; i.e. move consumption from older cohorts to younger cohorts
adjFun <- function(x, return.adjusted = FALSE) {

  # Which age cohorts within each income group will be the givers/source of transfers to younger households?
  give <- c(0,0,0,1,1,1,  # Low income
            0,0,0,1,1,1,
            0,0,0,1,1,1)  # High income

  receive <- c(1,0,0,0,0,0,  # Low income
               1,1,1,0,0,0,
               1,1,1,0,0,0)  # High income

  # Update 'give' to include parameters provided by 'x'
  give[give == 1] <- x

  #---

  fs.adj <- fs.data %>%
    mutate(g = give,
           r = receive) %>%
    group_by(inc_grp) %>%
    mutate(
      rel = consumption / mean(consumption * (1 - g)),
      gain_shr = ifelse(r == 1, (1 / rel) / sum((1 / rel)[r == 1]), 0),
      gain_shr = n * gain_shr,
      gain_shr = gain_shr / sum(gain_shr),  # Safety check (not really necessary)
      loss = g * cons_fs,  # Cohort consumption loss
      gain = sum(loss) * gain_shr,  # Cohort consumption gain
      cons_fs = cons_fs - loss + gain,
      consumption = cons_fs / n  # New, updated consumption per HOH/spouse
    ) %>%
    ungroup() %>%
    mutate(saving = income - consumption,
           fidelity = cons_ce / cons_fs) %>%
    arrange(inc_grp, age_grp)

  #-----

  # Objective value: Variance in slope of relationship between fidelity and log(wealth_stock), by income group
  obj <- sapply(split(fs.adj, fs.adj$inc_grp), function(g) {
    coef(lm(fidelity ~ log(wealth_stock), data = g, weights = n))[2]
  }) %>%
    var()

  #obj <- -1 * abs(cor(fs.adj$fidelity, log(fs.adj$wealth_stock)))

  # Return either objective value or adjusted data
  out <- if (return.adjusted) {
    fs.adj
  } else {
    obj
  }

  return(out)
}

#-----

# Optimize adjFun
# NOTE: Results appear super sensitive to the initial parameters...
opt <- optim(par = rep(0.05, 9),
             fn = adjFun,
             method = "L-BFGS-B",
             lower = rep(0, 9),
             upper = rep(0.5, 9))

# Get adjusted FS data frame
fs.adj <- adjFun(x = opt$par, return.adjusted = TRUE)

#-----

# Plot of adjusted shares...
fs.adj %>%
  ggplot(aes(x = wealth_stock, y = fidelity, color = inc_grp, label = as.integer(age_grp))) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_label(alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", size = 0.5) +
  scale_y_continuous("Ratio of CE reported consumption to FS adjusted consumption") +
  scale_x_continuous("Approximate mean wealth per capita (log scale)", labels = scales::dollar, trans = "log") +
  theme_bw()

# Total consumption redistribution from old to young
sum(fs.adj$loss) / 1e9 # In $billions
sum(fs.adj$loss) / sum(fs.adj$cons_fs)  # As share of total consumption across all cohorts

# Redistribution as share of post-adjustment consumption, by income group, limited to cohorts with consumption 'loss' via adjustment
fs.adj %>%
  group_by(inc_grp) %>%
  summarize(redist_share = sum(loss) / sum(cons_fs[loss != 0]))

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
      #cshare = cumsum(ctotal * cons_adj) / sum(ctotal * cons_adj), # Make adjustment for difference between PEU's and whole households
      #pshare = cumsum(n * pop_adj) / sum(n * pop_adj)  # Cumulative share of FS per-capita/spouse population, adjusted for PEU vs. household difference
      cshare = cumsum(ctotal) / sum(ctotal),
      pshare = cumsum(n) / sum(n)  # Cumulative share of FS per-capita/spouse population
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
    arrange(perminc) %>%  # Order by per-capita/spouse permanent income
    mutate(
      ctotal = fs_cons * weight,  # Using household FS consumption
      #ctotal = cons_total * weight,  # Using total household consumption
      cshare = cumsum(ctotal) / fs.max,
      pshare = cumsum(fs_denom * weight) / sum(fs_denom * weight)
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

# Share of total FS consumption and population, by age group
# NOT USED: These figures are adjusted to account for the difference between true total consumption in a inc-age cohort and total consumption among PEU's only
# NOT USED: That makes the adjustment most relevant for younger households more likely to be in households with multiple CU's
age.shares <- fs.adj %>%
  mutate(#cons_share = consumption * n * cons_adj,
         cons_share = consumption * n,
         cons_share = cons_share / sum(cons_share),
         #pop_share = n * pop_adj,
         pop_share = n,
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
         cat = list(fscats)) %>%
  select(age_grp, cons_grp, share, cat)

# Safety check
stopifnot(round(sum(fsagg$share), 3) == 1)

#-----

# Restrict static ACS microdata to only the necessary variables
p <- p %>%
  select(acs_2019_hid, weight, fs_denom, np, state, age_grp, educ_grp)

#-----

# Plot implied pseudo-Lorenz curves for each age cohort
# These are not "true" Lorenz curves, because the households are not ordered by consumption
gg <- psplits %>%
  map_dfr( ~ tibble(x = c(0, .x, 1), y = seq(0, 1, by = 0.05))) %>%
  mutate(`Age cohort` = factor(rep(names(psplits), each = 21), levels = names(psplits))) %>%
  ggplot(aes(x = x, y = y, color = `Age cohort`)) +
  geom_line() +
  geom_abline(slope = 1, linetype = "dashed") +
  xlab("Cumulative share of population (ordered by permanent income)") +
  ylab("Cumulative share of consumption") +
  theme_bw()

#-----

# Save outputs to disk
saveRDS(perminc.fit, file = "fusion/CEI/2015-2019/2019/post/Permanent income model.rds")
saveRDS(psplits, file = "fusion/CEI/2015-2019/2019/post/Consumption group ventiles.rds")
saveRDS(fsagg, "fusion/CEI/2015-2019/2019/post/Feiveson-Sabelhaus calibration targets.rds")
ggsave(filename = "fusion/CEI/2015-2019/2019/post/Pseudo Lorenz curves.png", plot = gg, width = 8, height = 6)
write_fst(p, "fusion/CEI/2015-2019/2019/post/ACS_2019 calibration inputs.fst", compress = 100)
