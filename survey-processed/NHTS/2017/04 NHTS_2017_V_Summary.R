library(tidyverse)
library(readr) 
library(dplyr)
library(lubridate) 
library(rpart)
library(xts)
source("R/utils.R")
source("R/createDictionary.R")
source("R/imputeMissing.R")
source("R/detectDependence.R")

#This is the script to summarise vehicle data at the household level
#Since all vehicles have the same weight for a given household, no weights are used here

setwd("/Users/karthikakkiraju/Documents/FusionData/")

colClean <- function(x){ colnames(x) <- gsub("\\.", "_", colnames(x)); x } 
names <- c('bestmile','fegempg','gscost','gstotcst','gsyrgal')

#Read vehicle level data summarized at the household level. 

nhts_v <-  fst::read_fst(path = "survey-processed/NHTS/2017/NHTS_2017_V_processed.fst") %>%
                     select(c('nhts_2017_hid','vehid','vehtype','bestmile','fegempg','fueltype',
                              'gscost','gstotcst','gsyrgal')) %>% 
        mutate(fueltype = gsub("Hybrid, electric or alternative fuel","Alt",fueltype),
               fueltype = gsub("Some other fuel","other",fueltype))

#Summarize by vehicle type and fuel type by total 
nhts_v_s1 <- nhts_v %>% select(vehtype, nhts_2017_hid, bestmile,gstotcst,gsyrgal,fueltype) %>%
  group_by(nhts_2017_hid,vehtype,fueltype) %>% 
  summarize_if(is.numeric,sum) 

#Summarize all vehicle types and fuel types at the household level by total    
nhts_v_s2 <- nhts_v %>% select(vehtype, nhts_2017_hid, bestmile,gstotcst,gsyrgal) %>% 
              group_by(nhts_2017_hid) %>% 
              summarize_if(is.numeric,sum)

#Summarize by vehicle type and fuel type by average 
nhts_v_m1 <- nhts_v %>% select(vehtype, nhts_2017_hid, gscost,fueltype) %>%
  group_by(nhts_2017_hid,vehtype,fueltype) %>% 
  summarize_if(is.numeric,mean) 

#Summarize all vehicle types and fuel types at the household level by average
nhts_v_m2 <- nhts_v %>% select(vehtype, nhts_2017_hid, gscost) %>%
             group_by(nhts_2017_hid) %>% 
             summarize_if(is.numeric,mean) 

#Merge all the dataframes
d_0 <-  merge(nhts_v_s1,nhts_v_m1,by = c('nhts_2017_hid','vehtype','fueltype')) %>% 
  mutate(
    vehtype = gsub("/Car/Station Wagon","",vehtype),
    vehtype = gsub("/Motorbike","",vehtype),
    vehtype = gsub("(Santa Fe, Tahoe, Jeep, etc.)","",vehtype),
    vehtype = gsub("\\(Mini/Cargo/Passenger)","",vehtype),
    vehtype = gsub("\\(Recreational Vehicle)","",vehtype),
    vehtype = gsub("([(\\)])","",vehtype),
    vehtype = gsub("[[:blank:]]", "",vehtype)) %>% 
  mutate_all(trimws) %>%
  mutate(vehtype = paste0(vehtype,"_",fueltype)) %>% 
  subset(., select = -c(fueltype))  %>%
  reshape(.,timevar='vehtype',idvar=c("nhts_2017_hid"),direction="wide")  %>% 
  mutate_at(vars(-c(nhts_2017_hid)), ~replace(., is.na(.), 0))  %>%
  colClean()   %>%
  rename_with(tolower) 

d <- merge(nhts_v_s2,nhts_v_m2,by = c('nhts_2017_hid')) %>%
      merge(d_0,by = c('nhts_2017_hid'))

#Create new codebook with column names and values from the summarized data 
codebook_d = data.frame(var = colnames(d)) %>%
                mutate(
                       value_min = apply(d,2,min),
                       value_max = apply(d,2,max),
                        value = paste0(value_min,"-",value_max)) %>%  
                        subset(., select = -c(value_min,value_max))  
                      
 codebook_v <- fst::read_fst(path = "survey-processed/NHTS/2017/NHTS_2017_V_codebook.fst") %>%
              select(c('var','desc','label')) %>%
              mutate(var = tolower(var))  %>%
              filter(var %in% c('vehtype','bestmile','fegempg','fueltype','gscost','gstotcst','gsyrgal')) 
  
  vehtypes <- codebook_v %>% 
  filter(var == 'vehtype') %>% subset(., select = c(label)) %>% rename(vehtype = label)%>%
    mutate(
      vehtype = gsub("/Car/Station Wagon","",vehtype),
      vehtype = gsub("(Santa Fe, Tahoe, Jeep, etc.)","",vehtype),
      vehtype = gsub("\\(Mini/Cargo/Passenger)","",vehtype),
      vehtype = gsub("\\(Recreational Vehicle)","",vehtype),
      vehtype = gsub("/Motorbike","",vehtype),
      vehtype = gsub("([(\\)])","",vehtype),
      vehtype = gsub("[[:blank:]]", "",vehtype)) %>%
    filter(vehtype != "N/A")
  
  fueltype <- codebook_v %>% 
  filter(var == 'fueltype') %>% subset(., select = c(label)) %>% rename(fueltype = label) %>% 
     mutate(
     fueltype = gsub("Hybrid, electric or alternative fuel","Alt",fueltype),
     fueltype = gsub("Some other fuel","Other",fueltype))   %>%
    filter(fueltype != "N/A")

  codebook_v1 <- codebook_v  %>%
              mutate(
                      label = gsub("/Car/Station Wagon","",label),
                      label = gsub("(Santa Fe, Tahoe, Jeep, etc.)","",label),
                      label = gsub("\\(Mini/Cargo/Passenger)","",label),
                      label = gsub("\\(Recreational Vehicle)","",label),
                      label = gsub("Hybrid, electric or alternative fuel","Alt",label),
                      label = gsub("Some other fuel","Other",label),
                      label = gsub("([(\\)])","",label)) %>%
                       filter(label != "N/A") %>%
                      mutate(across(where(is.character), tolower)) %>% 
              mutate(
              desc = ifelse(var == "vehtype", label,desc),
              desc = ifelse(var == "fueltype", label,desc)) %>% 
              add_row(var = "vehage", desc = "Age of vehicle, based on model year", label = NA) %>%
               mutate_all(trimws) %>% 
              filter((var %in% names)) %>% 
              slice(rep(1:n(), each = 4)) %>% cbind(fueltype) %>% 
  mutate(desc = paste(desc,"for"),
         desc = paste(desc,fueltype),
         desc = paste(desc,"type")) %>%  
  slice(rep(1:n(), each = 8)) %>%
         cbind(vehtypes) %>% 
           mutate(desc = paste(desc,vehtype),
                  var = paste(var,vehtype,sep = "_")) %>%
            mutate_all(trimws) %>%
  
         mutate(var = paste(var,fueltype,sep = "_"))  %>%  
      subset(., select = c(var,desc)) %>%
    mutate(var = tolower(var),
           desc = tolower(desc)) %>%
    mutate_all(trimws)
 
#Create new variable description 
codebook <- left_join(codebook_d,codebook_v1, by = 'var') %>%
        mutate(
          desc = ifelse(var == 'nhts_2017_hid',"Household Identifier",desc),
          desc = ifelse(var == 'bestmile',"Total best estimate of annual miles of all household vehicles",desc),
          desc = ifelse(var == 'gstotcst',"Total annual fuel expenditures in US dollars for all household vehicles",desc),
          desc = ifelse(var == 'gsyrgal',"Total annual fuel consumption in US gallons for all household vehicles",desc),
          desc = ifelse(var == 'gscost',"Average annualized fuel cost in US dollars per equivalent gallon",desc),
          label = value)

# Save data to disk (.fst)
fst::write_fst(x = d,"survey-processed/NHTS/2017/NHTS_2017_V_summary.fst",compress = 100)
fst::write_fst(x = codebook, path = "survey-processed/NHTS/2017/NHTS_2017_V_codebook_summary.fst", compress = 100)

      