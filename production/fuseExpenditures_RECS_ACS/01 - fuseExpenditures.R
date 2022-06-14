##############################################
# Impute fuel expenditures from RECS
# The basic assumption here is that fuel 
# prices should be more or less the same for 
# households living in the same areas, and in 
# the same type of dwellings. 
# Also, fuel prices may depend on the amount
# consumed. Nothing else is added as a predictor, 
# as there shouldn't be other factors leading
# to other types of price discrimination.
##############################################

# Note: As with all fusion codes,
# this needs to be run in the corresponding
# fusionData directory, so change the line
# below accordingly
setwd("e:/fusionData")

require(fusionData)
require(fusionModel)

# Choose the fuels to impute the expenditures
fuels <- c("el","ng","lp","fo")

# Prepare the data for the fusion
prep <- prepare(donor = "RECS_2015", 
                recipient = "ACS_2015", 
                respondent = "household",
                implicates = 5)

data <- assemble(prep,
                 fusion.variables = c(paste0("btu",fuels),paste0("dollar",fuels)),
                 window = 3,
                 pca = c(30, 0.9))

# Create the donor and recipient datasets
donor <- data$RECS_2015
recipient <- data$ACS_2015

vars_imp <- list()

# Perform fuel by fuel imputation
for (fuel in fuels) {
  
  # Create appropriate variable names
  fuelcons<-paste0("btu",fuel)
  fuelexp<-paste0("dollar",fuel)
  fuelprice<-paste0("price",fuel)
  
  # Fuse the consumption of the fuel first
  fit <- train(data = donor, 
               y = fuelcons, 
               x = setdiff(intersect(names(donor),names(recipient)),c(paste0("btu",fuels),paste0("dollar",fuels))),
               weight = "weight",
               cores = 2,
               maxcats = 10,
               complexity = 0.01)
  
  recipient[[fuelcons]] <- fuse(data = recipient, train.object = fit)[[fuelcons]]
  
  ## Uncomment to see fit
  # plot(density(donor[donor[[fuelcons]]>0,fuelcons]))
  # lines(density(recipient[recipient[[fuelcons]]>0,fuelcons]),col="red")

  # Calculate the price in the donor dataset
  donor[[fuelprice]]<-donor[[fuelexp]]/donor[[fuelcons]]
  
  # Important step here:
  # The calculated prices have some very strange outliers, it shouldn't 
  # be the case. So I am taking out the outliers using a Local Outlier 
  # Factor (LOF) algorithm, separately for each division
  for (div in unique(donor$loc..recs_division)) {
    if (sum((donor$loc..recs_division==div) & !is.na(donor[[fuelprice]]))>1) {
      donor[(donor$loc..recs_division==div) & !is.na(donor[[fuelprice]]),fuelprice][DescTools::LOF(
        donor[(donor$loc..recs_division==div) & !is.na(donor[[fuelprice]]),fuelprice],k=10)>2.75]<-NA
      
      ## Uncomment to see density with and without outliers
      # plot(density(donor[(donor$loc..recs_division==div) & !is.na(donor[[fuelprice]]),fuelprice],na.rm=T))
      # readline(div)
    }
  }
  
  
  # Fuse now the prices, using only 
  # location variables, type of building, 
  # and amount of fuel consumed
  fit <- train(data = donor[c("weight",
                              "loc..recs_division", 
                              "loc..recs_iecc_zone", 
                              "loc..ur12", 
                              "typehuq__bld", 
                              fuelcons, fuelprice)], 
               y = fuelprice,
               x = setdiff(intersect(names(donor),names(recipient)),c(paste0("btu",setdiff(fuels,fuel)),paste0("dollar",fuels))),
               weight = "weight",
               cores = 2,
               maxcats = 10,
               complexity = 0.01)
  
  vars_imp[[fuel]]<-fit$models[[fuelprice]]$variable.importance
  
  # Create the new expenditure by
  # multiplying the predicted price
  # with the predicted consumption
  recipient[[fuelexp]] <- 
    fuse(data = recipient, train.object = fit)[[fuelprice]]*
    recipient[[fuelcons]]
  
  ## Uncomment to see fit
  # plot(density(donor[donor[[fuelexp]]>0,fuelexp]))
  # lines(density(recipient[recipient[[fuelexp]]>0,fuelexp]),col="red")
  
  # Remove the created price from the donor
  donor<-donor[setdiff(names(donor),fuelprice)]
  
}

