library(fusionData)
library(fusionModel)

#---------

# Data prep
prep <- prepare(donor = "RECS_2015",
                recipient = "ACS_2015",
                respondent = "household",
                implicates = 5,
                collapse = FALSE)

# Data assembly for select RECS variables
data <- assemble(prep,
                 fusion.variables = c("kwh", "cufeetng", "totsqft_en", "cooltype", "scalee", "noacel", "noacbroke", "noheatbroke", "noheatbulk", "noheatel", "noheatng"),
                 spatial.datasets = c("eia.seds", "nrel.urdb", "climate"),
                 window = 2,
                 replicates = TRUE)

#---------

# Select desired fusion variables
fusion.vars <- c("kwh", "cufeetng", "totsqft_en", "cooltype", "scalee")

# Train initial fusion model to determine a fixed fusion order
fit0 <- train(data = data$RECS_2015,
              y = fusion.vars,
              x = setdiff(names(data$ACS_2015), c("acs_2015_hid", "weight")),
              weight = "weight",
              lasso = 1,
              deriv = FALSE,
              smoothed = FALSE,
              node.obs = c(30, 15),
              complexity = 0.001,
              initial = c(1, 0.001),
              cores = 3)

#---------

# Conduct "proper synthesis"
# Use a random sample of data$RECS_2015 and fit a new fusion model to produce each implicate
# The sampling is within subgroups defined by the variables used by the survey's creators to calibrate the sample weights
# RECS 2015: "The variables used for poststratification included Census Division, housing unit type, and age of housing unit"
# See page 17: https://www.eia.gov/consumption/residential/reports/2015/methodology/pdf/RECSmethodology2015.pdf
# Note: It's possible they aggregated the poststratification variables (no information in documentation)

sim <- pbapply::pblapply(1:10, function(i) {

  # Get random, stratified sample for the current implicate
  temp <- data$RECS_2015 %>%
    group_by(loc..recs_division, typehuq__bld, yearmaderange__ybl) %>%  # Stratification variable(s)
    mutate(N = length(unique(recs_2015_hid))) %>%
    slice(sample(1:n(), size = N[1], replace = TRUE)) %>%
    select(-N) %>%
    ungroup()

  fit <- train(data = temp,
               y = fusion.vars,
               x = setdiff(names(data$ACS_2015), c("acs_2015_hid", "weight")),
               weight = "weight",
               order = names(fit0$models),
               lasso = 1,
               deriv = FALSE,
               smoothed = TRUE,
               node.obs = c(30, 15),
               complexity = 0.001,
               cores = 2)

  # FUSE ONTO THE ORIGINAL RECS_2015 DATA!
  fuse(data = data$RECS_2015, train.object = fit)

})

#---------



# General utility via the pMSE metric
xvars <- setdiff(names(data$ACS_2015), c("acs_2015_hid", "weight"))
object.data <- map(sim, ~ cbind(data$RECS_2015[xvars], .x))
pmse <- synthpop::utility.gen(object = object.data,
                              data = data$RECS_2015[names(object.data[[1]])],
                              not.synthesised = xvars,
                              method = "cart",
                              print.stats = "pMSE",
                              resamp.method = "none",  # This line causes failure UNLESS print.flag = FALSE
                              print.flag = FALSE)

# Scaled 0-1 pMSE
1 - mean(pmse$pMSE) / 0.25
