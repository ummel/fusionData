processOVB <- function(survey_years, codebook) {

  d <- paste0("survey-processed/CEX/CEI/", survey_years, "/ovb_", survey_years, ".rds") %>%
    map_dfr(readRDS, .id = "survey_year") %>%
    mutate(
      survey_year = survey_years[as.integer(survey_year)],  # Replace index value with actual survey year
      cuid = as.integer(str_sub(NEWID, 1, -2)),
      intnum = as.integer(str_sub(NEWID, -1, -1))
    ) %>%
    rename_with(tolower) %>%
    select(-newid, -qyear)

  # Remove duplicate entries and retain data from latest available interview
  # Unclear why there are duplicate entries
  d <- d %>%
    group_by(cuid) %>%
    filter(intnum == max(intnum)) %>%
    group_by(cuid, vehicib) %>%
    slice(1) %>%
    ungroup()

  #----------------

  # Subset 'codebook' object in global environment to restrict to variables in 'd'

  codebook <- codebook %>%
    filter(var %in% names(d))

  #----------------

  # What value should "Valid blank" take for the following variables?

  # Variables with "Valid blank" values
  # These are the variables for which suitable replacement values must be specified below
  na.vars <- d %>%
    map_lgl(~ any(.x == "Valid blank", na.rm = TRUE)) %>%
    which() %>%
    names()

  na.values <- list(

    fueltype = NA,
    make = NA,
    vehicyr = NA,
    netpurx = NA,
    tradex = 0,
    principx = 0,
    vpurindv = NA,
    vehpuryr = NA,
    vehqpmt = 0,
    vintrate = 0

  )

  # Safety check for missing entries in 'na.values'
  miss <- noquote(setdiff(na.vars, names(na.values)))
  stopifnot(length(miss) == 0)

  #----------------

  # Assign levels for ordered factors
  # In general, we want to coerce unordered factor to ordered factors whenever feasible

  ordered.factors <- list(
    vehicyr = c("1969 or earlier", "1970-1974", "1975-1979", "1980-1982", "1983-1985", seq.int(from = 1986, by = 1, length.out = nrow(filter(codebook, var == "vehicyr")) - 5))
  )

  # Safety check
  # Detect any variables in 'ordered.factors' that are NOT in the codebook
  extras <- noquote(setdiff(names(ordered.factors), codebook$var))
  stopifnot(length(extras) == 0)

  #----------------

  # Update variable values with associated labels from 'codebook'

  # Loop through each variable in 'd', assigning labels when applicable
  for (v in names(d)) {

    cb <- filter(codebook, var == v)
    x <- d[[v]]
    y <- unlist(cb$value)
    z <- unlist(cb$label)
    if (v %in% names(na.values)) {
      y <- c(y, "Valid blank")
      z <- c(z, na.values[[v]])
    }
    m <- match(x, y)
    new.labels <- z[na.omit(m)]

    # Update 'x' with new value labels
    x[!is.na(m)] <- new.labels

    # Coerce result to ordered factor, if specified
    # Note that levels are restricted to those actually present in the data
    if (v %in% names(ordered.factors)) {
      num.na <- sum(is.na(x))
      x <- factor(x, levels = intersect(ordered.factors[[v]], x), ordered = TRUE)
      stopifnot(sum(is.na(x)) == num.na)  # This is a final safety check to ensure no NA's introduced inadvertently
    }

    # Apply type.convert() to 'x'; leave ordered factors unchanged
    x <- if (is.ordered(x)) x else type.convert(x, as.is = FALSE)

    # Ensure unordered factor levels are sorted according to codebook order of levels
    # Originally, unordered factors were sorted alphabetically -- but there is often useful information in the codebook ordering
    # This retains a valid codebook ordering if one exists; otherwise sort levels alphabetically
    if (is.factor(x) & !is.ordered(x)) {
      num.na <- sum(is.na(x))
      if (all(x %in% cb$label)) {
        x <- factor(x, levels = intersect(cb$label, unique(x)))
      } else {
        x <- factor(x, levels = sort(unique(x)))
      }
      stopifnot(sum(is.na(x)) == num.na)  # This is a final safety check to ensure no NA's introduced inadvertently
    }

    # Update column in 'd'
    d[[v]] <- x

  }

  # Safety check
  # Ensure there are not "Valid blank" entries remaining in the data
  stopifnot(!any(d == "Valid blank", na.rm = TRUE))

  #-----

  # Try to fix-up variables related to vehicle financing and cost

  d <- d %>%
    mutate(
      tradex = ifelse(is.na(netpurx), NA, tradex),
      principx = ifelse(principx == 0 & vfinance == "Vehicle was financed", NA, principx),
      vehqpmt = ifelse(vehqpmt == 0 & vfinance == "Vehicle was financed", NA, vehqpmt),
      vintrate = ifelse(vintrate == 0 & vfinance == "Vehicle was financed", NA, vintrate),
      principx = ifelse(vfinance == "Vehicle not financed", 0, principx),
      vehqpmt = ifelse(vfinance == "Vehicle not financed", 0, vehqpmt),
      vintrate = ifelse(vfinance == "Vehicle not financed", 0, vintrate)
    ) %>%

    # Create variable giving numver of years since vehicle was purchased
    mutate(years_since_purchase = pmax(0, survey_year - vehpuryr)) %>%
    select(-vehpuryr)

  #-----

  # Impute missing
  imp <- imputeMissing(data = d,
                       N = 2,
                       max_ncats = 15,
                       x_exclude = c("cuid", "intnum"))

  # Add imputed variables to 'd'
  d <- d %>%
    select(-any_of(names(imp))) %>%
    cbind(imp)

  #-----

  # Return estimated total current value of owned vehicles, by CUID
  # This is estimated by taking the price at purchase and depreciating by 15% each year to the present
  d <- d %>%
    mutate(price_at_purchase = netpurx + tradex,
           current_value = price_at_purchase * (1 - 0.15) ^ years_since_purchase) %>%
    group_by(cuid) %>%
    summarize(vehicle_value = sum(current_value), .groups = "drop")

  return(d)

}
