imputeHeatingFuel <- function(cei.h) {

  stopifnot(basename(getwd()) == "fusionData")
  source("R/utils.R")

  # NOT USED
  # # 2009 RECS -- this is old code from CCL project; originally, 2009 and 2015 RECS were pooled for heatfuel imputation (ignoring now)
  #
  # recs.2009 <- "~/Documents/Projects/UPenn SC2/fusionACS/data-survey/RECS/2009/recs2009_public.csv" %>%
  #   read.csv(stringsAsFactors = FALSE) %>%
  #   mutate(weight = NWEIGHT,
  #          division = recode(DIVISION, "New England", "Middle Atlantic", "East North Central", "West North Central", "South Atlantic", "East South Central", "West South Central", "Mountain", "Mountain", "Pacific"),
  #          urban = recode(UR, U = "Urban", R = "Rural"),
  #
  #          # Climate region
  #          #climate = recode(Climate_Region_Pub, `1` = 'Very Cold/Cold', `2` = 'Hot-Dry/Mixed-Dry', `3` = 'Hot-Humid', `4` = 'Mixed-Humid', `5` = 'Marine'),
  #
  #          # Dwelling type
  #          #dwell_type = recode(TYPEHUQ, `1` = 'Mobile home', `2` = 'Single-family detached', `3` = 'Single-family attached', `4` = 'Apartment building 2-4 units', `5` = 'Apartment building 5+ units'),
  #          dwell_type = recode(TYPEHUQ, `1` = 'Mobile home', `2` = 'Single-family detached', .default = "Other"),
  #
  #          # Housing tenure
  #          tenure = recode(KOWNRENT, `1` = 'Own', `2` = 'Rent', `3` = 'Other'),
  #
  #          # Year built
  #          #yrbuilt = YEARMADE,
  #          yrbuilt = recode_factor(YEARMADERANGE, "Before 1950", "1950-1959", "1960-1969", "1970-1979", "1980-1989", "1990-1999", "2000-2009", "2000-2009", .ordered = TRUE),
  #
  #          # Number of rooms/bedrooms
  #          bedrooms = ifelse(BEDROOMS < 0, 1, BEDROOMS),
  #          #rooms = TOTROOMS, # Total rooms consistent with ACS definition
  #
  #          # Primary space heating fuel
  #          heatfuel = recode(FUELHEAT, `1` = 'Natural gas', `2` = 'LPG', `3` = 'Fuel oil', `4` = 'Fuel oil', `5` = 'Electricity', .default = "Other"),
  #
  #          # Binary variables indicating of utility cost is included in rent or condominium fee
  #          elec_in_rent = ifelse(PELLIGHT == 2, TRUE, FALSE),  # Uses "light and other appliances" question as proxy for all electricity
  #          #gas_in_rent = ifelse(pmax(PGASHEAT == 2, PGASHTWA == 2, PUGCOOK == 2, PUGOTH == 2, LPGPAY == 2) == 1, TRUE, FALSE), # Natural gas and LPG/propane
  #          #hfuel_in_rent = ifelse(FOPAY == 2, TRUE, FALSE),  # Uses "light and other appliances" question as proxy for all electricity
  #
  #          ELEC = DOLLAREL,
  #          NGAS = DOLLARNG,
  #          FOIL = DOLLARFO + DOLLARKER,
  #          LPG = DOLLARLP,
  #
  #          total = ELEC + NGAS + FOIL + LPG,
  #          elec_share = ELEC / total,
  #          ngas_share = NGAS / total,
  #          foil_share = FOIL / total,
  #          lpg_share = LPG / total,
  #          energy_ptile = spatstat::ewcdf(total, weights = weight / sum(weight))(total),
  #   ) %>%
  #   select(weight:elec_in_rent, ends_with("share"), energy_ptile)
  #
  # # Add additional level to ordered factor 'yrbuilt' variable
  # levels(recs.2009$yrbuilt) <- c(levels(recs.2009$yrbuilt), "2010 or later")

  #----------------------

  # 2015 RECS processed select variables for harmonization with CEI 2015-2019

  recs.2015 <- "survey-raw/RECS/2015/recs2015_public_v4.csv" %>%
    read.csv(stringsAsFactors = FALSE) %>%
    mutate(weight = NWEIGHT,
           division = recode(DIVISION, "New England", "Middle Atlantic", "East North Central", "West North Central", "South Atlantic", "East South Central", "West South Central", "Mountain", "Mountain", "Pacific"),
           urban = recode(UATYP10, U = "Urban", C = "Urban", R = "Rural"),

           # Climate region
           #climate = recode(Climate_Region_Pub, `1` = 'Very Cold/Cold', `2` = 'Hot-Dry/Mixed-Dry', `3` = 'Hot-Humid', `4` = 'Mixed-Humid', `5` = 'Marine'),

           # Dwelling type
           #dwell_type = recode(TYPEHUQ, `1` = 'Mobile home', `2` = 'Single-family detached', `3` = 'Single-family attached', `4` = 'Apartment building 2-4 units', `5` = 'Apartment building 5+ units'),
           dwell_type = recode(TYPEHUQ, `1` = 'Mobile home', `2` = 'Single-family detached', .default = "Other"),

           # Housing tenure
           tenure = recode(KOWNRENT, `1` = 'Own', `2` = 'Rent', `3` = 'Other'),

           # Year built
           yrbuilt = recode_factor(YEARMADERANGE, "Before 1950", "1950-1959", "1960-1969", "1970-1979", "1980-1989", "1990-1999", "2000-2009", "2010 or later", .ordered = TRUE),

           # Number of rooms/bedrooms
           bedrooms = ifelse(BEDROOMS < 0, 1, BEDROOMS),
           #rooms = TOTROOMS, # Total rooms consistent with ACS definition

           # Primary space heating fuel
           heatfuel = recode(FUELHEAT, `1` = 'Natural gas', `2` = 'LPG', `3` = 'Fuel oil', `5` = 'Electricity', .default = "Other"),

           # Binary variables indicating of utility cost is included in rent or condominium fee
           elec_in_rent = ifelse(ELPAY == 2, TRUE, FALSE),  # Uses "light and other appliances" question as proxy for all electricity
           #gas_in_rent = ifelse(NGPAY == 2, TRUE, FALSE), # Natural gas and LPG/propane
           #hfuel_in_rent = ifelse(FOPAY == 2, TRUE, FALSE),  # Uses "light and other appliances" question as proxy for all electricity

           elec = DOLLAREL,
           ngas = DOLLARNG,
           ofuel = DOLLARFO + DOLLARLP,
           total_energy = elec + ngas + ofuel,
           elec_share = elec / total_energy,
           ngas_share = ngas / total_energy,
           ofuel_share = ofuel / total_energy
    ) %>%
    select(weight:ofuel_share) %>%
    mutate_at(vars(elec, ngas, ofuel, total_energy), convert2scaled, w = .$weight) %>%
    mutate_if(is.character, ~ factor(.x, levels = sort(unique(.x)))) %>%
    mutate_if(is.ordered, as.integer)

  #----------------------

  # NOT USED
  # Combine 2009 and 2015 RECS observations
  # recs <- bind_rows(recs.2009, recs.2015) %>%
  #   mutate(weight = weight / mean(weight)) %>%
  #   as_tibble() %>%
  #   mutate_if(is.character, ~ factor(.x, levels = sort(unique(.x)))) %>%
  #   mutate_if(is.ordered, as.integer)
  #
  # anyNA(recs)

  #----------------------

  # Create "recipient" CEI dataset for prediction using RECS-based heat fuel model
  cei <- cei.h %>%
    mutate(
      urban = ifelse(ur12 == "U", "Urban", "Rural"),
      tenure = ifelse(grepl("Owned", cutenure), "Own", "Other"),
      tenure = ifelse(cutenure == "Rented", "Rent", tenure),
      dwell_type = ifelse(grepl("Single family detached", building), "Single-family detached", "Other"),
      dwell_type = ifelse(grepl("Mobile home", building), "Mobile home", dwell_type),
      yrrange = findInterval(built, c(-Inf, seq(1950, 2010, by = 10), Inf)),
      yrbuilt = recode_factor(yrrange, "Before 1950", "1950-1959", "1960-1969", "1970-1979", "1980-1989", "1990-1999", "2000-2009", "2010 or later", .ordered = TRUE),
      bedrooms = bedroomq,
      elec_in_rent = rtelect,
      total_energy = elec + ngas + ofuel,
      elec_share = elec / total_energy,
      ngas_share = ngas / total_energy,
      ofuel_share = ofuel / total_energy
    ) %>%
    mutate_at(vars(elec, ngas, ofuel, total_energy), convert2scaled, w = .$weight) %>%
    mutate_if(is.character, ~ factor(.x, levels = sort(unique(.x)))) %>%
    mutate_if(is.ordered, as.integer) %>%
    mutate_if(is.factor, ~ as.integer(.x) - 1L) %>%
    select(all_of(setdiff(names(recs.2015), c('weight', 'heatfuel')))) %>%
    as.matrix()

  #----------------------

  # Fit heatfuel prediction model via LightGBM

  # Create full LGB training dataset with all available observations
  xv <- setdiff(names(recs.2015), c('weight', 'heatfuel'))
  dfull <- lightgbm::lgb.Dataset(data = as.matrix(mutate_if(recs.2015[xv], is.factor, ~ as.integer(.x) - 1L)),
                                 label = as.integer(recs.2015$heatfuel) - 1L,
                                 weight = recs.2015$weight,
                                 categorical_feature = names(which(sapply(recs.2015[xv], is.factor)))) %>%
    lightgbm::lgb.Dataset.construct()

  # LGB hyperparameters
  params <- list(objective = "multiclass",
                 metric = "multi_logloss",
                 num_class = length(levels(recs.2015$heatfuel)),
                 boosting = "gbdt",
                 max_depth = 4,
                 feature_fraction = 0.8,
                 num_iterations = 5000,
                 learning_rate = 0.01)

  # Fit model
  sink <- capture.output({
    mod <- lightgbm::lgb.cv(
      params = params,
      data = dfull,
      nfold = 5,
      early_stopping_rounds = 1L,
      verbose = -1L
    )
  })

  # Fit final model with optimal number of iterations
  params$num_iterations <- mod$best_iter
  mod <- lightgbm::lgb.train(
    params = params,
    data = dfull,
    verbose = -1L
  )

  #----------------------

  # Simulate heatfuel variable for CEI households
  # NOTE: This is a random assignment based on modeled probability
  p <- predict(object = mod, data = cei, reshape = TRUE)
  for (j in 2:ncol(p)) p[, j] <- p[, j - 1] + p[, j]
  ptile <- runif(n = nrow(p))
  S <- rowSums(ptile > p, na.rm = TRUE) + 1L

  # Return heatfuel assignment (character vector)
  out <- levels(recs.2015$heatfuel)[S]
  return(out)

}
