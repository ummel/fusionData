library(fusionData)
library(fusionModel)

#This for imputation of prices/expenditures 
#Run this after creating the fused fsd output for the consumption variables

# Fuel types 
fuels <- c("el","ng","lp","fo")

# Number of cores to use
ncores <- 1

# Number of implicates to generate
nimps <- 1

#Load fused output for the consumtpion variables 
sim <- read_fsd("fusion/RECS/2015/2015/output/RECS_2015_2015_fused.fsd") %>%
       select(.,c('M',"btuel","btung","btufo","btulp"))


# Donor and recipient survey identifiers
donor <- "RECS_2015"
recipient <- "ACS_2015"

# Directory in /fusion where results will be saved
# This is automatically constructed from 'donor' and 'recipient', assuming recipient is ACS-based
acs.vintage <- substring(recipient, 5)
dir <- "fusion/RECS/2015/2015/input/expenditure"

#-----
out.path <- "fusion/RECS/2015/2015/output/expenditure"

# Prepare and assemble data inputs
prep <- prepare(donor = donor,
                recipient = recipient,
                respondent = "household",
                implicates = 1)


# Specify fusion variables to be retained in harmonization results
data <- assemble(prep,
                 fusion.variables = c(paste0("btu",fuels),paste0("dollar",fuels)),
                 window = 2)

donor <- data[[1]] 
#-----
#Keep only required predictor varibales in the recipient 
recipient_acs <- data[[2]] %>% select(.,c("loc..recs_division", "loc..recs_iecc_zone", "loc..ur12", "typehuq__bld"))
recipient <- cbind(recipient_acs,sim) 
#--------
rm(prep,sim,recipient_acs)

#-----

# Specify the predictors in the harmonized donor data that will be used for validation subsets
# We select the variables that best reflect the following socioeconomic and geographic concepts:
#  -- income; race/ethnicity; education; household size; housing tenure; and a relatively high-resolution location variable
# These variables are "forced" as predictors in prepXY() and carried along in 'prep' for use by validate() in /output.R
recipient_exp <- data.frame(matrix(ncol=5,nrow=0))

for (i in 40:1)
{
  recipientM <- recipient %>% filter(M == i)
  cat("Implicate number:",i)
# Perform fuel by fuel imputation
for (fuel in fuels) {
  
  print(fuel)
  
  # Create appropriate variable names
  fuelcons<-paste0("btu",fuel)
  fuelexp<-paste0("dollar",fuel)
  fuelprice<-paste0("price",fuel)
  
  
  
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
  price.x.vars <-c("loc..recs_division", 
                  "loc..recs_iecc_zone", 
                  "loc..ur12", 
                  "typehuq__bld", 
                  fuelcons)
  
  hyper.params <- list(
    boosting = "goss",
    num_leaves = 2 ^ (4:6),
   # min_data_in_leaf = round(max(20, nrow(train.data) * 0.001)),
    num_iterations = 1000,
    feature_fraction = 0.8,
    learning_rate = 0.05
  )

  fsn.path <- train(data = donor[!is.na(donor[[fuelprice]]),
                            c("weight",price.x.vars, fuelprice)], 
               y = fuelprice,
               x = price.x.vars,
               fsn = paste0(out.path,fuel,"_model.fsn"),
               weight = "weight",
               cores = ncores,
               hyper = hyper.params)

  
  # Create the new expenditure by
  # multiplying the predicted price
  # with the predicted consumption
  sim <- fuse(data = recipientM, fsn = fsn.path,cores = ncores, M = nimps,
                                fsd = paste0(out.path,fuel,i,"fusedexp.fsd"))
  
  sim <- read_fsd(sim)    
  recipientM[[fuelexp]]   <-sim[[fuelprice]] *  recipientM[[fuelcons]]
  
  
  # Remove the created price from the dono
  donor <- donor[setdiff(names(donor),fuelprice)]
  }
  # 
    recipient_add <- recipientM %>% select(.,c('M',"dollarel","dollarfo","dollarng","dollarlp"))
    recipient_exp <- rbind(recipient_add,recipient_exp)
  }

rm(recipient_add,recipientM,sim)
gc()

export <- read_fsd("fusion/RECS/2015/2015/output/RECS_2015_2015_fused.fsd")  %>% 
       cbind(.,recipient_exp)

write_fst(export,path= "fusion/RECS/2015/2015/output/RECS_2015_2015_fused_merged.fst",compress= 100)







