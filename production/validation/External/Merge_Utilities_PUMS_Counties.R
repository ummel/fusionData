library(tidyfst)
library(tidyverse)
library(readr) 
library(dplyr)
library(lubridate) 
library(rpart)
library(xts)
library(ggplot2)
library(zoo)
library(fuzzyjoin)
source("R/utils.R")
source("R/createDictionary.R")
source("R/imputeMissing.R")
source("R/detectDependence.R")

setwd("/Users/user/Documents/Yale/FusionData/fusionData")

#This file is to create PUMA-county mapping, generate county-wise electricity  consumptionvalidation data
#This file is organized by state since the external data for each state is in a different format 
#_________________________________________________________________________________#

#Reformatting CA raw data to make compatible with ACS


#Get county-wise household units data: https://www.census.gov/data/tables/time-series/demo/popest/2010s-total-housing-units.html
#This data will be used for calculating average household electricity consumption data from the utilities 

pop_county_CA <- readxl::read_xlsx("production/validation/Electricitydata/CO-EST2019-ANNHU-06.xlsx", skip =3) 
colnames(pop_county_CA) <- c('county',"Census","Estimates Base","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019")
pop_county_CA  <- select(pop_county_CA,c('county',"2015"))
colnames(pop_county_CA) <- c('county',"hhunits")

pop_county_CA <- pop_county_CA[-1,]
pop_county_CA$county <- gsub("*County","",pop_county_CA$county)
pop_county_CA$county <- gsub("*California","",pop_county_CA$county)
pop_county_CA$county <- gsub(',',"",pop_county_CA$county)
pop_county_CA$county <- gsub('\\.',"",pop_county_CA$county)
pop_county_CA <- filter(pop_county_CA, (hhunits != "NA"))
pop_county_CA$state <- "06"


#Get PUMS data with county names. Downloaded from "https://mcdc.missouri.edu/applications/geocorr2014.html"
# This will be used to summarize ACS data at the county level 

pums_county_CA <- read.csv("production/validation/Electricitydata/geocorr2014_2204504771.csv", skip = 1) 
pums_county_CA  <- select(pums_county_CA,"puma12","County.name") 
pums_county_CA$puma12 <- sprintf("%05d",as.numeric(pums_county_CA$puma12))
pums_county_CA$state <- "06"
colnames(pums_county_CA) <- c("puma10","county","state")
pums_county_CA$county <- gsub("*CA","",pums_county_CA$county)

#Merge counties where PUMA is the same 
pums_county_CA <- distinct(pums_county_CA, .keep_all = FALSE)
pums_county_CA_2 <- aggregate(county ~ puma10, data = pums_county_CA,FUN=paste,collapse = "&")
pums_county_CA_2$state <- "06"


#External Validation data for CA from : http://www.ecdms.energy.ca.gov/elecbycounty.aspx
# This data is already at the county level. Averages will have to be calculated at the household level 

utilities_CA <- read_csv("production/validation/Electricitydata/ElectricityByCounty.csv") 
utilities_CA$County <- tolower(utilities_CA$County)
colnames(utilities_CA) = c("county","sector","vintage","totalkwh")

utilities_CA <- select(utilities_CA, c("county","totalkwh"))
utilities_CA$totalkwh = (utilities_CA$totalkwh)*1000000

pop_county_CA$county <- tolower(pop_county_CA$county)
utilities_CA_val_2 <- stringdist_left_join(utilities_CA, pop_county_CA, by = "county")  #left_join doesn't do the job here. NA's are created 
utilities_CA_val_2 <- select(utilities_CA_val_2,"county.x","totalkwh","hhunits")        # stringdist with stricter matching does the trick   
colnames(utilities_CA_val_2)[colnames(utilities_CA_val_2) == 'county.x'] <- 'county'
utilities_CA_val_2$state <- "06"

utilities_CA_val_2$averagekwh <- utilities_CA_val_2$totalkwh/utilities_CA_val_2$hhunits 

#____________________________________________________________________________________________________________#

#Reformatting MA raw data to make compatible with ACS

#Get county-wise household data: https://www.census.gov/data/tables/time-series/demo/popest/2010s-total-housing-units.html
pop_county_MA <- readxl::read_xlsx("production/validation/Electricitydata/CO-EST2019-ANNHU-25.xlsx", skip =3) 
colnames(pop_county_MA) <- c('county',"Census","Estimates Base","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019")
pop_county_MA  <- select(pop_county_MA,c('county',"2015"))
colnames(pop_county_MA) <- c('county',"hhunits")
pop_county_MA <- pop_county_MA[-1,]
pop_county_MA$county <- gsub("*County","",pop_county_MA$county)
pop_county_MA$county <- gsub("*Massachusetts","",pop_county_MA$county)
pop_county_MA$county <- gsub(',',"",pop_county_MA$county)
pop_county_MA$county <- gsub('\\.',"",pop_county_MA$county)
pop_county_MA <- filter(pop_county_MA, (hhunits != "NA"))
pop_county_MA$county <- tolower(pop_county_MA$county)
pop_county_MA$state <- "25"

#Get PUMS data with county names. Downloaded from "https://mcdc.missouri.edu/applications/geocorr2014.html"

pums_county_MA <- read.csv("production/validation/Electricitydata/geocorr2014_2204503914.csv", skip = 1) 
pums_county_MA  <- select(pums_county_MA,"puma12","County.name") 
pums_county_MA$puma12 <- sprintf("%05d",as.numeric(pums_county_MA$puma12))
pums_county_MA$state <- "25"
colnames(pums_county_MA) <- c("puma10","county","state")
pums_county_MA$county <- gsub("*MA", "", pums_county_MA$county)
pums_county_MA$county <- tolower(pums_county_MA$county)
distinct(pums_county_MA, .keep_all = FALSE)

#Merge counties where PUMA is the same 
pums_county_MA_2 <- aggregate(county ~ puma10, data = pums_county_MA,FUN=paste,collapse = "&")
pums_county_MA_2$state <- "25"


# External utliites data for MA. Obtained from Google Drive 

utilities_MA <- readxl::read_excel("production/validation/Electricitydata/2015 Geographic Report - Exported on 02-14-2022.xls",skip = 2,) 
utilities_MA <- select(utilities_MA,"County","Town","Sector","Annual") 
utilities_MA <- filter(utilities_MA, (Town != "All Towns" & Sector == "Residential"))

utilities_MA$Annual = as.numeric(gsub(",","",utilities_MA$Annual,fixed=TRUE))
utilities_MA$kwh <- as.numeric((utilities_MA$Annual))
utilities_MA <- select(utilities_MA,"County","Town","kwh") 

#countywise aggregated MA electricity data
utilities_MA_val <- utilities_MA %>%
  group_by(County) %>%
  summarize(kwh_county_tot = sum(kwh,na.rm = T)) 

#left_join(utilities_MA_val,utilities_MA_merge, by= c("County"))
#utilities_MA_sum <- unique(utilities_MA_merge[c("County", "kwh_county_tot","kwh_county_mean")])

colnames(utilities_MA_val) <- c("county","totalkwh","state")
utilities_MA_val$totalkwh = (utilities_MA_val$totalkwh)*1000
utilities_MA_val$county <- tolower(utilities_MA_val$county)

utilities_MA_val <- stringdist_left_join(utilities_MA_val, pop_county_MA, by = "county")
utilities_MA_val <- select(utilities_MA_val,"county.x","totalkwh","state","hhunits")
colnames(utilities_MA_val)[colnames(utilities_MA_val) == 'county.x'] <- 'county'

utilities_MA_val$averagekwh <- utilities_MA_val$totalkwh/utilities_MA_val$hhunits 

#_________________________________________________________________________________#

#Reformatting NY raw data to make compatible with ACS

#Get county-wise household data: https://www.census.gov/data/tables/time-series/demo/popest/2010s-total-housing-units.html

pop_county_NY <- readxl::read_xlsx("production/validation/Electricitydata/CO-EST2019-ANNHU-36.xlsx", skip =3) 
colnames(pop_county_NY ) <- c('county',"Census","Estimates Base","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019")
pop_county_NY  <- select(pop_county_NY,c('county',"2015"))
colnames(pop_county_NY) <- c('county',"hhunits")
pop_county_NY  <- pop_county_NY[-1,]
pop_county_NY$county <- gsub("*County","",pop_county_NY$county)

pop_county_NY$county <- str_replace_all(pop_county_NY$county," ", "")  
pop_county_NY$county <- gsub(",NewYork","",pop_county_NY$county)
pop_county_NY$county <- gsub(',',"",pop_county_NY$county)
pop_county_NY$county <- gsub('\\.',"",pop_county_NY$county)
pop_county_NY  <- filter(pop_county_NY, (hhunits != "NA"))
pop_county_NY$county <- tolower(pop_county_NY$county)
pop_county_NY$state <- "36"


#Get PUMS data with county names. Downloaded from "https://mcdc.missouri.edu/applications/geocorr2014.html"

pums_county_NY <- read.csv("production/validation/Electricitydata/geocorr2014_2204506870.csv", skip = 1) 
pums_county_NY  <- select(pums_county_NY,"puma12","County.name") 
pums_county_NY$puma12 <- sprintf("%05d",as.numeric(pums_county_NY$puma12))
pums_county_NY$state <- "36"
colnames(pums_county_NY) <- c("puma10","county","state")
pums_county_NY$county <- gsub("*NY", "", pums_county_NY$county)
pums_county_NY$county <- tolower(pums_county_NY$county)

#Merge counties where PUMA is the same 
pums_county_NY <- distinct(pums_county_NY, .keep_all = FALSE)
pums_county_NY_2 <- aggregate(county ~ puma10, data = pums_county_NY,FUN=paste,collapse = "&")
pums_county_NY_2$state <- "36"

# External utilities data for NY. Obtained from Google Drive 

utilities_NY <- readxl::read_excel("production/validation/Electricitydata/NY County Electric and Natural Gas Data 2017-2019.xlsx", sheet = "Sheet1") 
utilities_NY_val <- select(utilities_NY,"County Name","State Name","Sector","Year","Geography ID","Source","Consumption MMBtu","Expenditure US Dollars") 
colnames(utilities_NY_val) <- c("county", "state","sector","vintage","GEOID","source","mmbtu","exp")
utilities_NY_val <- filter(utilities_NY_val, (vintage == "2018" & source == "elec" & sector == "residential"))
utilities_NY_val$totalkwh <- as.numeric((utilities_NY_val$mmbtu))*0.293*1000
utilities_NY_val <- select(utilities_NY_val,"county","totalkwh","exp") 
names(utilities_NY_val)<-c("county","totalkwh","totalkwh_exp")
utilities_NY_val$county <- tolower(utilities_NY_val$county)

#Adding hhunits to electricity data 

utilities_NY_val <- stringdist_left_join(utilities_NY_val, pop_county_NY, by = "county")
utilities_NY_val <- select(utilities_NY_val,"county.x","totalkwh","totalkwh_exp","state","hhunits")
colnames(utilities_NY_val)[colnames(utilities_NY_val) == 'county.x'] <- 'county'
utilities_NY_val$averagekwh <- utilities_NY_val$totalkwh/utilities_NY_val$hhunits 
utilities_NY_val$state <- "36"


#_________________________________________________________________________________#

#Reformatting MD raw data to make compatible with ACS

#Get county-wise household data: https://www.census.gov/data/tables/time-series/demo/popest/2010s-total-housing-units.html
pop_county_MD <- readxl::read_xlsx("production/validation/Electricitydata/CO-EST2019-ANNHU-24.xlsx", skip =3) 
colnames(pop_county_MD ) <- c('county',"Census","Estimates Base","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019")
pop_county_MD  <- select(pop_county_MD,c('county',"2015"))
colnames(pop_county_MD) <- c('county',"hhunits")
pop_county_MD  <- pop_county_MD[-1,]
pop_county_MD$county <- gsub("*County","",pop_county_MD$county)
pop_county_MD$county <- gsub("*Maryland","",pop_county_MD$county)
pop_county_MD$county <- gsub(',',"",pop_county_MD$county)
pop_county_MD$county <- gsub('\\.',"",pop_county_MD$county)
pop_county_MD$county <- gsub("st mary's*","stmarys",pop_county_MD$county)
pop_county_MD  <- filter(pop_county_MD, (hhunits != "NA"))
pop_county_MD$county<- tolower(pop_county_MD$county)
pop_county_MD$state <- "24"

#Get PUMS data with county names. Downloaded from "https://mcdc.missouri.edu/applications/geocorr2014.html"
pums_county_MD <- read.csv("production/validation/Electricitydata/geocorr2014_2205001444.csv", skip = 1) 
pums_county_MD  <- select(pums_county_MD,"puma12","County.name") 
pums_county_MD$puma12 <- sprintf("%05d",as.numeric(pums_county_MD$puma12))
pums_county_MD$state <- "24"
colnames(pums_county_MD) <- c("puma10","county","state")
pums_county_MD$county <- gsub("*MD", "", pums_county_MD$county)
pums_county_MD$county <- gsub("st. marys*","stmarys",pums_county_MD$county) #Causes merging issues with ACS
pums_county_MD$county<- tolower(pums_county_MD$county)

#Merge counties where PUMA is the same 
pums_county_MD <- distinct(pums_county_MD, .keep_all = FALSE)
pums_county_MD_2 <- aggregate(county ~ puma10, data = pums_county_MD,FUN=paste,collapse = "&")
pums_county_MD_2$state <- "24"


# New utlities data from : https://findenergy.com/md/

utilities_MD_val <- readxl::read_xlsx("production/validation/Electricitydata/Marylandcountydata.xlsx") 
trimws(utilities_MD_val)
utilities_MD_val$totalkwh <- ((utilities_MD_val$totalkwh))*1000
utilities_MD_val$county <- tolower(utilities_MD_val$county)
utilities_MD_val$totalkwh <- as.numeric(as.character(utilities_MD_val$totalkwh))
utilities_MD_val$county <- gsub("st. mary's*","stmarys",utilities_MD_val$county)

utilities_MD_val <- stringdist_left_join(utilities_MD_val, pop_county_MD, by = "county",method='qgram')
utilities_MD_val <- select(utilities_MD_val,"county.x","totalkwh","state","hhunits")
colnames(utilities_MD_val)[colnames(utilities_MD_val) == 'county.x'] <- 'county'

utilities_MD_val$averagekwh <- utilities_MD_val$totalkwh/utilities_MD_val$hhunits 

#_________________________________________________________________________________#
#Reformatting PA raw data to make compatible with ACS

#Get county-wise household data: https://www.census.gov/data/tables/time-series/demo/popest/2010s-total-housing-units.html
pop_county_PA <- readxl::read_xlsx("production/validation/Electricitydata/CO-EST2019-ANNHU-42.xlsx", skip =3) 
colnames(pop_county_PA) <- c('county',"Census","Estimates Base","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019")
pop_county_PA  <- select(pop_county_PA,c('county',"2015"))
colnames(pop_county_PA) <- c('county',"hhunits")
pop_county_PA  <- pop_county_PA[-1,]
pop_county_PA$county<- tolower(pop_county_PA$county)
pop_county_PA$county <- gsub("*county","",pop_county_PA$county)
pop_county_PA$county <- gsub("*pennsylvania","",pop_county_PA$county)
pop_county_PA$county <- gsub(',',"",pop_county_PA$county)
pop_county_PA$county <- gsub('\\.',"",pop_county_PA$county)
pop_county_PA <- filter(pop_county_PA, (hhunits != "NA"))
pop_county_PA$state <- "42"


#Get PUMS data with county names. Downloaded from "https://mcdc.missouri.edu/applications/geocorr2014.html"
pums_county_PA <- read.csv("production/validation/Electricitydata/geocorr2014_2205308282.csv", skip = 1) 
pums_county_PA  <- select(pums_county_PA,"puma12","County.name") 
pums_county_PA$puma12 <- sprintf("%05d",as.numeric(pums_county_PA$puma12))
pums_county_PA$state <- "42"
colnames(pums_county_PA) <- c("puma10","county","state")
pums_county_PA$county <- gsub("*PA", "", pums_county_PA$county)
pums_county_PA$county<- tolower(pums_county_PA$county)

#Merge counties where PUMA is the same 
pums_county_PA <- distinct(pums_county_PA, .keep_all = FALSE)

pums_county_PA_2 <- aggregate(county ~ puma10, data = pums_county_PA,FUN=paste,collapse = "&")
pums_county_PA_2$state <- "42"

# New data: http://www.ecdms.energy.ca.gov/elecbycounty.aspx
utilities_PA_val <- readxl::read_xlsx("production/validation/Electricitydata/PAcountydata.xlsx") 
utilities_PA_val$totalkwh <- ((utilities_PA_val$totalkwh))*1000
utilities_PA_val$county <- tolower(utilities_PA_val$county)
utilities_PA_val$totalkwh <- as.numeric(as.character(utilities_PA_val$totalkwh))

utilities_PA_val <- stringdist_left_join(utilities_PA_val, pop_county_PA, by = "county")
utilities_PA_val <- select(utilities_PA_val,"county.x","totalkwh","state","hhunits")
colnames(utilities_PA_val)[colnames(utilities_PA_val) == 'county.x'] <- 'county'

utilities_PA_val$averagekwh <- utilities_PA_val$totalkwh/utilities_PA_val$hhunits 
utilities_PA_val$state <- "42"
#_________________________________________________________________________________#


# Aggregating county level utilities data for CA,MA, MD, PA and NY 
util_comb <- bind_rows(utilities_CA_val_2,utilities_MA_val,utilities_NY_val,utilities_PA_val,utilities_MD_val)
util_comb$county <- gsub("County","",util_comb$county)
fst::write_fst(util_comb, "production/validation/util_comb_val.fst", compress = 100)


# Aggregating PUMA-county mapping for CA,MA,MD,PA and NY
pums_county <- bind_rows(pums_county_NY_2,pums_county_MA_2,pums_county_CA_2,pums_county_MD_2,pums_county_PA_2)
fst::write_fst(pums_county, "production/validation/pums_county.fst", compress = 100)


# Aggregating household units information for CA,MA,MD,PA and NY
pop_county <- bind_rows(pop_county_NY,pop_county_MA,pop_county_CA,pop_county_MD,pop_county_PA)
fst::write_fst(pop_county, "production/validation/pop_county.fst", compress = 100)