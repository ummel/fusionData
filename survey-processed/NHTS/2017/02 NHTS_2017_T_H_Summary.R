library(tidyverse)
library(splitstackshape)
library(readr) 
library(dplyr)
library(lubridate) 
library(rpart)
library(xts)
source("R/utils.R")
source("R/createDictionary.R")
source("R/imputeMissing.R")
source("R/detectDependence.R")

# Code to summarize trip level data at the household level
# Uses selected numeric variables from the processed trip-level file 

setwd("/Users/karthikakkiraju/Documents/FusionData/")
colClean <- function(x){ colnames(x) <- gsub("\\.", "_", colnames(x)); x } 


codebook_t <- fst::read_fst(path = "survey-processed/NHTS/2017/NHTS_2017_T_codebook.fst")


#This will be merged with the person level data and then later summarized at the household level
nhts_h <-  fst::read_fst(path = "survey-processed/NHTS/2017/NHTS_2017_T_processed.fst") %>%
                  select(c('nhts_2017_hid','nonhhcnt','numontrp','numtrans','pubtrans',
                       'trpmiles','tregrtm','tracctm','trphhacc','trpmilad','trptrans','tdwknd',
                       'trphhveh','trvlcmin','trwaittm','vehtype','vmt_mile','whytrp1s','whyto','weight')) %>%
  
  mutate(vehtype = as.character(vehtype),
         trptrans = as.character(trptrans),
         trphhveh = as.character(trphhveh),
         tdwknd = as.character(tdwknd),
         pubtrans = as.character(pubtrans),

         whytrp1s= gsub("Social/Recreational","social",whytrp1s),
         whytrp1s= gsub("Something else","other",whytrp1s),
         whytrp1s= gsub("Shopping/Errands","shopping",whytrp1s),
         whytrp1s= gsub("School/Daycare/Religious activity","school",whytrp1s),
         whytrp1s= gsub("Medical/Dental services","medical",whytrp1s),
         whytrp1s= gsub("Transport someone","transport",whytrp1s))  %>%     
        
  mutate(whytrp1s = as.character(whytrp1s)) %>%
  
  mutate(whytrp2s = ifelse(whyto %in%  c("Regular home activities (chores, sleep)", "Work from home (paid)"),"home",
                     ifelse(whyto %in%  c("Work", "Work-related meeting / trip"),"work", 
                    ifelse(whyto %in%  c("Health care visit (medical, dental, therapy)"),"medical",
                    ifelse(whyto %in%  c("Buy goods (groceries, clothes, appliances, gas)",
                                             "Buy services (dry cleaners, banking, service a car, pet care)",
                                             "Other general errands (post office, library)"),"shopping",
                      ifelse(whyto %in%  c("Recreational activities (visit parks, movies, bars, museums)",
                                                       "Exercise (go for a jog, walk, walk the dog, go to the gym)",
                                                   "Visit friends or relatives","Religious or other community activities",
                                                   "Volunteer activities (not paid)"),"social",
                       ifelse(whyto %in%  c("Buy meals (go out for a meal, snack, carry-out)"),"meals", 
                      ifelse(whyto %in%  c("Drop off /pick up someone","Attend child care","Attend adult care"),"care", 
                     ifelse(whyto %in%  c("Attend school as a student"),"school",whytrp1s))))))))) %>%

  mutate(
        trptrans = ifelse(trptrans %in% c("Car","SUV","Van","Pickup truck","Golf cart / Segway","Motorcycle / Moped","Private / Charter / Tour / Shuttle bus","RV (motor home, ATV, snowmobile)"),
                           "personal",as.character(trptrans)),
        
        trptrans = gsub("Bicycle","bicycle",as.character(trptrans)),
        
        trptrans = gsub("Walk","walk",as.character(trptrans)),
        
        trptrans = gsub("Something Else","other",as.character(trptrans)),
        
        trptrans = ifelse(trptrans %in% c("Public or commuter bus","Amtrak / Commuter rail","Subway / elevated / light rail / street car","School bus","Paratransit / Dial-a-ride",
                                    "City-to-city bus (Greyhound, Megabus)","Airplane","Boat / ferry / water taxi","Airplane"),"public",as.character(trptrans)),

        trptrans = ifelse(trptrans %in% c("Taxi / limo (including Uber / Lyft)","Rental car (Including Zipcar / Car2Go)"),
                      "rideshare/rental", as.character(trptrans)))


#Summarize numeric trip variables by trip type at the household level

nhts_h_s1 <- nhts_h %>% select(whytrp2s, nhts_2017_hid,which(sapply(., is.numeric))) %>% drop_na() %>%
            group_by(nhts_2017_hid,whytrp2s) %>% 
            summarise(
              trpmilad = weighted.mean(trpmilad,weight,na.rm = TRUE),
              trpmiles = weighted.mean(trpmiles,weight,na.rm = TRUE),
              vmt_mile = weighted.mean(vmt_mile,weight,na.rm = TRUE),
              trwaittm = weighted.mean(trwaittm,weight,na.rm = TRUE),
              numtrans = weighted.mean(numtrans,weight,na.rm = TRUE),
              tregrtm = weighted.mean(tregrtm,weight,na.rm = TRUE),
              trvlcmin = weighted.mean(trvlcmin,weight,na.rm = TRUE)
            )

nhts_h_s1_res <-  reshape(getanID(nhts_h_s1, c('nhts_2017_hid','whytrp2s')), 
                        timevar="whytrp2s",idvar=c("nhts_2017_hid"),direction="wide") 

#Summarize numeric trip variables at the household level overall 
nhts_h_s2 <- nhts_h %>% select(nhts_2017_hid,weight,which(sapply(., is.numeric))) %>%
                      group_by(nhts_2017_hid) %>% 
                      summarise(
                        trpmilad = weighted.mean(trpmilad,weight,na.rm = TRUE),
                        trpmiles = weighted.mean(trpmiles,weight,na.rm = TRUE),
                        vmt_mile = weighted.mean(vmt_mile,weight,na.rm = TRUE),
                        trwaittm = weighted.mean(trwaittm,weight,na.rm = TRUE),
                        numtrans = weighted.mean(numtrans,weight,na.rm = TRUE),
                        tregrtm = weighted.mean(tregrtm,weight,na.rm = TRUE),
                        trvlcmin = weighted.mean(trvlcmin,weight,na.rm = TRUE))

#Count number of trips by trip type 
#nhts_h_count <- nhts_h %>% select(whytrp2s, nhts_2017_hid,which(sapply(., is.numeric))) %>%
 #               group_by(nhts_2017_hid,whytrp2s) %>% 
  #              dplyr::count(whytrp2s) %>% 
   #             dplyr::rename(tripnum = n) 
  
#nhts_h_trp_count_res <-  reshape(getanID(nhts_h_count, c('nhts_2017_hid','whytrp2s')), 
 #                       timevar="whytrp2s",idvar=c("nhts_2017_hid"),direction="wide") 


#Merging the summarized files 
d <- merge(nhts_h_s1_res,nhts_h_s2 ,by = c('nhts_2017_hid'), all = T) %>%
  #   merge(nhts_h_trp_count_res,by = c('nhts_2017_hid'), all = T) %>%
    colClean()   %>%
   rename_with(tolower)  

#Replace NA with 0 for all variables
#Need to reconsider changing variables to categoricals to avoid the 0's or use a flag
d[is.na(d)] <- 0 

#----------------
#Create new codebook entries  for numeric variables
dx <- d %>% select(which(sapply(., is.numeric))) 
codebook_d = data.frame(var = colnames(dx)) %>%  
  mutate(
    value_min = apply(dx,2,min,na.rm=T),
    value_max = apply(dx,2,max,na.rm=T),
    value = paste0(value_min,"-",value_max)) %>%  
  subset(., select = -c(value_min,value_max))  %>% mutate_all(trimws)
 
#----------------
codebook_t <- fst::read_fst(path = "survey-processed/NHTS/2017/NHTS_2017_T_codebook.fst")  %>%
  select(c('var','desc','label')) %>%
  mutate(var = tolower(var))  %>%
  mutate(
label = gsub("Social/Recreational","Social",label),
label = gsub("Something else","other",label),
label = gsub("Shopping/Errands","Shopping",label),
label= gsub("School/Daycare/Religious activity","School",label),
label = gsub("Transport someone","Transport",label),
label = gsub("Medical/Dental services","Medical",label)) %>%
  filter(var %in% c('nhts_2017_hid','pid','numtrans','trpmiles','tregrtm','tracctm','trpmilad',
                    'trvlcmin','trwaittm','vehtype','vmt_mile','whytrp1s')) %>%
  filter(label != "N/A") %>%
  mutate(across(where(is.character), tolower)) %>% 
  mutate(
    desc = ifelse(var == "whytrp1s", label,desc)) %>% 
  mutate_all(trimws)

whytrp1s <- codebook_t %>% 
  filter(var == 'whytrp1s') %>% subset(., select = c(desc)) %>% rename(whytrp1s = desc)


codebook_t <- codebook_t %>% 
  slice(rep(1:n(), each = 9)) %>% cbind(whytrp1s) %>% 
  mutate(desc = paste(desc,"for"),
         desc = paste(desc,whytrp1s),
         desc = paste(desc,"type")) %>%  
  mutate(var = paste(var,whytrp1s,sep = "_")) %>%
  subset(., select = c(var,desc)) 

#Create new variable description 
codebook <- left_join(codebook_d,codebook_t, by = 'var') %>% 
            distinct()  %>% 
  mutate(
    desc = ifelse(var == 'nhts_2017_hid',"Household Identifier",desc),
    desc = ifelse(var == 'pid',"Household Person Identifier",desc),
   # desc = ifelse(var == 'nonhhcnt',"Total number of non-household members on trip",desc),
  #  desc = ifelse(var == 'numontrp',"Total number of people on trip including respondent",desc),
    desc = ifelse(var == 'numtrans',"Total count of Transfers",desc),
    desc = ifelse(var == 'trpmiles',"Total trip distance in miles, derived from route geometry returned by Google Maps API, or from reported loop-trip distance",desc),
    desc = ifelse(var == 'tregrtm',"Total time to Destination from Transit in Minutes",desc),
    desc = ifelse(var == 'tracctm',"Total trip Time to Transit Station in Minutes",desc),
   # desc = ifelse(var == 'trphhacc',"Total count of Household Members on Trip",desc),
    desc = ifelse(var == 'trpmilad',"Total trip distance in miles, adjusted for comparability to past surveys",desc),
    desc = ifelse(var == 'trvlcmin',"Total trip Duration in Minutes",desc),
    desc = ifelse(var == 'trwaittm',"Total transit wait time in minutes",desc),
    desc = ifelse(var == 'vmt_mile',"Total trip distance in miles for personally driven vehicle trips, derived from route geometry returned by Google Maps API",desc),
    
  #  desc = ifelse(var == 'tripnum_home',"Total number of home trips",desc),
  #  desc = ifelse(var == 'tripnum_school',"Total number of school trips",desc),
  #  desc = ifelse(var == 'tripnum_work',"Total number of work trips",desc),
  #  desc = ifelse(var == 'tripnum_social',"Total number of social trips",desc),
   # desc = ifelse(var == 'tripnum_medical',"Total number of medical trips",desc),
  #  desc = ifelse(var == 'tripnum_somethingelse',"Total number of something else trips",desc),
  #  desc = ifelse(var == 'tripnum_shopping',"Total number of shopping trips",desc),
   # desc = ifelse(var == 'tripnum_meals',"Total number of meals trips",desc),
  #  desc = ifelse(var == 'tripnum_transport',"Total number of transport trips",desc),
    desc = ifelse(var == 'trptrans_care',"trip mode derived for care type",desc),
  #  desc = ifelse(var == 'tripnum_care',"total number of care trips",desc),
   # desc = ifelse(var == 'tripnum_other',"total number of other trips",desc),
    desc = ifelse(var == 'nonhhcnt_care',"number of non-household members on trip for care type",desc),
    desc = ifelse(var == 'numontrp_care',"number of people on trip including respondent for care type",desc),
    desc = ifelse(var == 'numtrans_care',"count of transfers for care type",desc),
    desc = ifelse(var == 'tracctm_care',"trip time to transit station in minutes for care type",desc),
    desc = ifelse(var == 'tregrtm_care',"time to destination from transit in minutes  for care type",desc),
    desc = ifelse(var == 'trphhacc_care',"count of household members on trip for care type",desc),
    desc = ifelse(var == 'trpmilad_care',"trip distance in miles, adjusted for comparability to past surveys for care type",desc),
    desc = ifelse(var == 'trpmiles_care',"trip distance in miles, derived from route geometry returned by google maps api, or from reported loop-trip distance for care type",desc),
    desc = ifelse(var == 'trvlcmin_care',"trip duration in minutes for care type",desc),
    desc = ifelse(var == 'trwaittm_care',"transit wait time in minutes for care type",desc),
    desc = ifelse(var == 'vmt_mile_care',"trip distance in miles for personally driven vehicle trips, derived from route geometry returned by google maps api for care type",desc),
    label = value) %>% 
    mutate_all(tolower)  %>%
    mutate_all(trimws)  

#Check to make sure all values are in the codebook


# Save data to disk (.fst)
fst::write_fst(x = d, path = "survey-processed/NHTS/2017/NHTS_2017_T_summary.fst", compress = 100)
fst::write_fst(x = codebook, path = "survey-processed/NHTS/2017/NHTS_2017_T_codebook_summary.fst", compress = 100)


