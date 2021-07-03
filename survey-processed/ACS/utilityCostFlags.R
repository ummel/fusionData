# Function to add utility fuel cost flag variables to ACS PUMS household microdata
# Prior to 2018, the utility expenditure variables (e.g. ELEP) included de-facto categorical information about payment status as an integer entry (e.g. "1" = "Included in rent or in condo fee")
# Starting in 2018, Census Bureau assigns this information to separate "cost flag variables" that are easier to work with
# The function utilityCostFlags() adds the cost flag variables and revises the original expenditure variables as necessary
# This creates consistent utility expenditure variables across years

utilityCostFlags <- function(d) {

  names(d) <- toupper(names(d))
  stopifnot(all(c("ELEP", "FULP", "GASP", "WATP") %in% names(d)))

  d$ELEFP <- c("Included in rent or in condo fee", "No charge or electricity not used")[d$ELEP]
  d$ELEFP <- factor(replace_na(d$ELEFP, replace = "Valid monthly electricity cost in ELEP"))
  labelled::var_label(d$ELEFP) <- "Electricity cost flag variable"
  d$ELEP[d$ELEP < 3] <- 0

  d$FULFP <- c("Included in rent or in condo fee", "No charge or fuel other than gas or electricity not used")[d$FULP]
  d$FULFP <- factor(replace_na(d$FULFP, replace = "Valid annual fuel cost in FULP"))
  labelled::var_label(d$FULFP) <- "Fuel cost flag variable"
  d$FULP[d$FULP < 3] <- 0

  d$GASFP <- c("Included in rent or in condo fee", "Included in electricity payment", "No charge or gas not used")[d$GASP]
  d$GASFP <- factor(replace_na(d$GASFP, replace = "Valid monthly gas cost in GASP"))
  labelled::var_label(d$GASFP) <- "Gas cost flag variable"
  d$GASP[d$GASP < 4] <- 0

  d$WATFP <- c("Included in rent or in condo fee", "No charge")[d$WATP]
  d$WATFP <- factor(replace_na(d$WATFP, replace = "Valid annual water cost in WATP"))
  labelled::var_label(d$WATFP) <- "Water cost flag variable"
  d$WATP[d$WATP < 3] <- 0

  names(d) <- tolower(names(d))
  return(d)

}
