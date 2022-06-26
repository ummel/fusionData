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
setwd("~/fusionData")

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

# Adjust this line accordingly to load the chain function
source("~/fusionModel/R/chain.R")

# Finds the best sequence to impute the fuel consumptions
y.order <- chain(donor, 
                 y = paste0("btu",fuels), 
                 x = setdiff(intersect(names(donor),c(names(recipient))),
                             c(paste0("btu",fuels),paste0("dollar",fuels),"weight")), 
                 w = "weight")

# Fit and fuse fuel consumptions
fit <- train(data = donor, 
             y = y.order, 
             x = setdiff(intersect(names(donor),c(names(recipient))),
                         c(paste0("btu",fuels),paste0("dollar",fuels),"weight")),
             weight = "weight",
             file = "./temp_model.fsn")

recipient <- cbind(recipient,fuse(data = recipient, file = "./temp_model.fsn"))
file.remove("./temp_model.fsn")


# Perform fuel by fuel imputation
for (fuel in fuels) {
  
  print(fuel)
  
  # Create appropriate variable names
  fuelcons<-paste0("btu",fuel)
  fuelexp<-paste0("dollar",fuel)
  fuelprice<-paste0("price",fuel)
  
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
    }
  }
  
  
  # Fuse now the prices, using only 
  # location variables, type of building, 
  # and amount of fuel consumed
  price.x.vars<-c("loc..recs_division", 
                  "loc..recs_iecc_zone", 
                  "loc..ur12", 
                  "typehuq__bld", 
                  fuelcons)
  fit <- train(data = donor[!is.na(donor[[fuelprice]]),
                            c("weight",price.x.vars, fuelprice)], 
               y = fuelprice,
               x = price.x.vars,
               weight = "weight",
               file = "./temp_model.fsn")
  
  # Create the new expenditure by
  # multiplying the predicted price
  # with the predicted consumption
  recipient[[fuelexp]] <- 
    fuse(data = recipient, file = "./temp_model.fsn")$V1*
    recipient[[fuelcons]]
  file.remove("./temp_model.fsn")
  
  # Remove the created price from the donor
  donor<-donor[setdiff(names(donor),fuelprice)]
  
}
