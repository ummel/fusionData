library(fusionData)
library(fusionModel)
source("R/utils.R")

#-----

# Eliminate fusion variables from consideration
# Excludes variables that don't look terribly relevant for research
# Also excludes many derivatives of other variables (e.g. same quantity but different units; kwh vs btu)
# Additional variables in 'fusion.vars' are automatically ignored by assemble() if they are used to create harmony with ACS

data(dictionary)
fusion.vars <- dictionary %>%
  filter(Survey == "RECS", Vintage == 2015) %>%
  filter(!(grepl("usage for", Description) & !grepl("in thousand Btu", Description))) %>%
  filter(!(grepl("in thousand Btu", Description) & !grepl("Total usage for", Description))) %>%
  filter(!(grepl("in dollars", Description) & grepl("cost for", Description))) %>%
  filter(!grepl(" used for", Description)) %>%
  filter(!grepl(" used by", Description)) %>%
  filter(!(grepl("[Yes]", Values, fixed = TRUE) & !grepl("Energy Star", Description))) %>%   # Removes all Yes/No variables, except Energy Star questions
  filter(!(grepl("bulbs", Description) & !grepl("LED", Description))) %>%
  filter(!grepl("Energy Supplier Survey", Description)) %>%
  filter(!grepl("degree days", tolower(Description))) %>%
  filter(!grepl("dry bulb", tolower(Description))) %>%
  filter(!grepl("door", tolower(Description))) %>%
  filter(!(grepl("oven", Description) & !grepl("Total usage", Description))) %>%
  filter(!(grepl("cook", Description) & !grepl("Total usage", Description))) %>%
  filter(!(grepl("square", Description) & !grepl("Total square footage", Description))) %>%
  filter(!(grepl("Frequency", Description) & !grepl("scale", Variable))) %>%
  filter(!(grepl("temperature", Description) & !grepl("scale", Variable))) %>%
  filter(!grepl("second most-used", tolower(Description))) %>%
  filter(!grepl("^use", Variable)) %>%
  filter(!grepl("Months", Description)) %>%
  filter(!grepl("cable", Description)) %>%
  filter(!grepl("Who pays", Description)) %>%
  filter(!Variable %in% c("totaldol", "dntheat", "door1sum", "dvd", "dwcycle", "elperiph", "h2oheatapt", "oa_lat", "sepdvr", "vcr"))

#-----

# Prepare and assemble data inputs

prep <- prepare(donor = "RECS_2015",
                recipient = "ACS_2015",
                respondent = "household",
                implicates = 5)

data <- assemble(prep,
                 fusion.variables = fusion.vars$Variable,
                 window = 2)

rm(prep)
gc()

#-----

# Sanity check
lapply(data, dim)

# Visual check of frequencies for a harmonized variable
round(table(data$RECS_2015$nhsldmem__np) / nrow(data[[1]]), 3)
round(table(data$ACS_2015$nhsldmem__np) / nrow(data[[2]]), 3)

#-----

# Train fusion models (5 min)

fit <- train(data = data$RECS_2015,
             y = attr(data, "fusion.vars"),
             ignore = "recs_2015_hid",
             weight = "weight",
             cores = 3,
             complexity = 1e-5)

#-----

# Fuse variables (18 min)
# Reduce precision of numeric fused variables for smaller file size
# Add ACS 2015 household ID column

sim <- fuse(data = data$ACS_2015,
            train.object = fit) %>%
  mutate_if(is.numeric, convertInteger) %>%
  mutate_if(is.double, cleanNumeric, tol = 0.001) %>%
  cbind(data$ACS_2015["acs_2015_hid"]) %>%
  select(acs_2015_hid, everything())

stopifnot(!anyNA(sim))

#-----

# Example: save result to disk (56 MB)
fst::write_fst(sim, "production/RECS_2015_sim.fst", compress = 100)
