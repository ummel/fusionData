library(tidyverse)
library(fuzzyjoin)
library(matrixStats)
options(scipen=integer)
setwd("/Users/user/Documents/Yale/FusionData/fusionData")


#This code is similar to the corresponding validation file but with a loop for the implicates 
# Read 50 RECS implicate data for electricity consumption 
for (i in 1:50){
 tempdf <-  fst::read_fst(paste0("recs_sim_imp", i, ".fst"))
 colnames(tempdf) <- c('acs_2015_hid', 'btuel')
 Newname <- paste("recs_sim_imp",i,sep="")
 assign(Newname,tempdf)
 
 }

# Load required ACS microdata
acs <- fst::read_fst("survey-processed/ACS/2015/ACS_2015_H_processed.fst",
                     columns = c("acs_2015_hid", "state", "puma10", "weight"))

# Load required PUMS county map data for NY, MA,CA
pums_county <- fst::read_fst("production/validation/pums_county.fst")

# Load required utilities external data for CA,MA,NY
util_comb <- fst::read_fst("production/validation/util_comb_val.fst")



# Load required hhunits county map data for NY, MA,CA
pop_county <- fst::read_fst("production/validation/pop_county.fst")


#-----

# Compare mean fused utility expenditure, by PUMA, with the external utilities data 
# For NY, aggregating fused electricity consumption at the county level
acs_NY <- acs[acs$state == "36", c("acs_2015_hid","puma10","state","weight")]
acs_NY <- merge(acs_NY, pums_county, by =c("puma10","state"))
pop_county_NY <- pop_county[pop_county$state == "36", c("county","hhunits")]
pop_county_NY$county <- tolower(pop_county_NY$county) 

util_comb_NY <- util_comb[util_comb$state == "36", c("county","totalkwh","averagekwh","hhunits")]
util_comb_NY$totalkwh <- util_comb_NY$totalkwh
trimws(util_comb_NY)

# Combing RECS - ACS fusion data 

for (i in 1:50) {
  recs.sim01_now <- get(paste0("recs_sim_imp",i,sep=""))
  comp_NY_01 <- recs.sim01_now %>%
    left_join(acs_NY, by = "acs_2015_hid")  %>%
    group_by(state, county) %>%
    summarize(county_weight = sum(weight),  # county total household weight
              totalkwh.sim = sum(btuel*0.293*weight),
              averagekwh.sim = (totalkwh.sim/county_weight),
              .groups = "drop") 
  trimws(comp_NY_01) 
  comp_NY_01 <- filter(comp_NY_01, (state != "NA"))
  comp_NY_01$county <- tolower(comp_NY_01$county) 
  Newname <- paste("comp_NY_01_",i,sep="")
  
     assign(Newname, comp_NY_01)
  }

for (i in 1:50) {
  comp_NY <- get(paste0("comp_NY_01_",i,sep=""))
comp_NY <- filter(comp_NY, (state != "NA"))
#Count number of shared counties per PUMA
comp_NY$count <- str_count(comp_NY$county,'&')

comp_NY_sub0 <- filter(comp_NY, (count == "0"))
comp_NY_sub0 <- select(comp_NY_sub0, c('county','county_weight','totalkwh.sim'))

#Divide the total kwh.sim and county_weights  by hhunits among the shared counties
comp_NY_sub <- comp_NY[grep("&", comp_NY$county), ]
comp_NY_sub <- select(comp_NY_sub, c('county','county_weight','totalkwh.sim'))

Newname1 <- paste("comp_NY_sub0_",i,sep="")
Newname2 <- paste("comp_NY_sub_",i,sep="")

assign(Newname1, comp_NY_sub0)
assign(Newname2, comp_NY_sub)
}

#Split each combined entry to separate dataframe

for (i in 1:50) {
  comp_NY_sub <- get(paste0("comp_NY_sub_",i,sep=""))
   n <- length(unique(comp_NY_sub$county))
  eval(parse(text = paste0("comp_NY_", seq(1:n), " <- ", split(comp_NY_sub, comp_NY_sub$county))))
  eval(parse(text = paste0("comp_NY_N", seq(1:n), " <-  as.data.frame(comp_NY_", seq(1:n), ")"))) 

  for(j in 1:n) {
  get(paste("comp_NY_N",j,sep="")) -> comp
  temp <- separate_rows(comp,county, sep = "&")
  temp1 <- temp %>% 
    stringdist_left_join(pop_county_NY, by=c('county'),method='qgram', q=2)
  
  
  temp1 <- select(temp1, c('county.x','county_weight','totalkwh.sim','hhunits'))
  
  temp1 <- temp1 %>% 
    dplyr::mutate(frac=round(hhunits/sum(hhunits),4))
  
  temp1$county_weight <- with(temp1, county_weight * frac)               
  temp1$totalkwh.sim <- with(temp1, totalkwh.sim * frac)               
  temp1 <- select(temp1, c('county.x','county_weight','totalkwh.sim'))
  colnames(temp1) <- c('county','county_weight','totalkwh.sim')
  Newname <- paste("comp_NY_small",j,sep="")
  assign(Newname,temp1)
}

#Merge the newly created county data into one dataframe 
NY_new_list <- list()
for(k in 1:n){
  df.now <- get(paste0("comp_NY_small",k,sep=""))
  NY_new_list[[k]] <- df.now
}

NY_list <- do.call(rbind,NY_new_list) 
Newname1 <- paste("NY_list_",i,sep="")
assign(Newname1, NY_list)

#Merge the newly created county data into one dataframe 

NY_new_list <- list()
for(k in 1:n){
  df.now <- get(paste0("comp_NY_small",k,sep=""))
  NY_new_list[[k]] <- df.now
}

NY_list <- do.call(rbind,NY_new_list) 
Newname1 <- paste("NY_list_",i,sep="")
assign(Newname1, NY_list)
}


for(i in 1:50) {
  get(paste("NY_list_",i,sep="")) -> NY_list
  get(paste("comp_NY_sub0_",i,sep="")) -> comp_NY_sub0
  comp_NY_new <- rbind(NY_list,comp_NY_sub0)
  
  Newname <- paste("comp_NY_new_",i,sep="")
  assign(Newname,comp_NY_new)
}


for(i in 1:50) {
  get(paste("comp_NY_new_",i,sep="")) -> comp_NY_new
  comp_NY <- comp_NY_new  %>%
    
    group_by(county) %>%
    summarize(county_weight = sum(county_weight),  # county total household weight
              totalkwh.sim = sum(totalkwh.sim),
              averagekwh.sim = (totalkwh.sim/county_weight),
              .groups = "drop") 
  trimws(comp_NY) 
  comp_NY$state <- '36' 
  
  eval_NY <- stringdist_left_join(comp_NY, util_comb_NY, by =c("county"),method='qgram', q=2,distance_col = NULL)
  eval_NY  <- select(eval_NY,c("state","county.x","totalkwh.sim","averagekwh.sim","averagekwh", "totalkwh","hhunits"))
  colnames(eval_NY)[colnames(eval_NY) == 'county.x'] <- 'county'
  eval_NY$averagekwh.sim <- round(eval_NY$averagekwh.sim, digits = 2) 
  
  Newname <- paste("eval_NY_",i,sep="")
  assign(Newname,eval_NY)
}

#----------------------------------------------------------------------------------------

#Average Kwh Error bars 

for(i in 1:50) {
  get(paste("eval_NY_",i,sep="")) -> eval_NY
  eval_NY_2 <- eval_NY  %>%
    group_by(averagekwh.sim) %>%
    summarize(
      totalkwh.sim = sum(totalkwh.sim), 
      averagekwh.sim = averagekwh.sim, 
      totalkwh = sum(totalkwh), 
      hhunits = sum(hhunits), 
      county = paste(county, collapse = "-"), 
      .groups = "drop") 
  eval_NY_2 <- distinct(eval_NY_2, .keep_all = FALSE)
  
  eval_NY_2$averagekwh <- eval_NY_2$totalkwh/eval_NY_2$hhunits
  
  Newname <- paste("eval_NY_2_",i,sep="")
  assign(Newname,eval_NY_2)
}


#----------------------------------------------------------------------------------------

#Average Kwh Error bars 

for(i in 1:50) {
  get(paste("eval_NY_",i,sep="")) -> eval_NY
  eval_NY_2 <- eval_NY  %>%
    group_by(averagekwh.sim) %>%
    summarize(
      totalkwh.sim = sum(totalkwh.sim), 
      averagekwh.sim = averagekwh.sim, 
      totalkwh = sum(totalkwh), 
      hhunits = sum(hhunits), 
      county = paste(county, collapse = "-"), 
      .groups = "drop") 
  eval_NY_2 <- distinct(eval_NY_2, .keep_all = FALSE)
  
  eval_NY_2$averagekwh <- eval_NY_2$totalkwh/eval_NY_2$hhunits
  
  Newname <- paste("eval_NY_2_",i,sep="")
  assign(Newname,eval_NY_2)
}

# Extract averagekwh.sim from each implicate set for std calculation 

for(i in 1:50) {
  get(paste("eval_NY_2_",i,sep="")) -> eval_NY
  eval_NY_imp_a <- select(eval_NY, c('averagekwh.sim','county'))
  
  Newname <- paste("eval_NY_imp_a_",i,sep="")
  assign(Newname,eval_NY_imp_a)
}

Check_1 <-c() 
Ds <-eval_NY_imp_a_1

for(i in 2:50){
  get(paste("eval_NY_imp_a_",i,sep="")) -> eval_NY_x
  Ds<-merge(Ds,eval_NY_x, by ='county') 
  names(Ds)[names(Ds) == 'averagekwh.sim.x'] <- 'averagekwh.sim'
  names(Ds)[names(Ds) == 'averagekwh.sim.y'] <- (paste("imp_2_",i,sep=""))
  # Ds<- Ds[, !duplicated(colnames(Ds))]
  Newname <- paste("Check_",i,sep="")
  assign(Newname,Ds)
}

names(Check_50)[names(Check_50) == 'averagekwh.sim'] <- 'imp_2_50'

Check_50$averagekwh.sim_avg <- apply(Check_50[,2:51], 1, mean)
Check_50$averagekwh.sim_std  <- apply(Check_50[,2:51], 1, sd)

Check_51 <- select(Check_50, c('county','averagekwh.sim_avg','averagekwh.sim_std'))

# Aggregating PUMS county information for CA,MA,and NY 
fst::write_fst(Check_51, "production/validation/averagekwh_sim_avg_NY.fst", compress = 100)

#----------------------------------------------------------------------------------------

#Rank Plot for average kwh 
for(i in 1:50) {
  get(paste("eval_NY_2_",i,sep="")) -> eval_NY
  rank.sim_a <- rank(-eval_NY$averagekwh.sim)
  rank.util_a <- rank(-eval_NY$averagekwh)
  eval_NY <- cbind (eval_NY, rank.sim_a,rank.util_a)
  Newname <- paste("eval_NY_2_",i,sep="")
  assign(Newname,eval_NY)
}


# Extract same column from each implicate set 
for(i in 1:50) {
  get(paste("eval_NY_2_",i,sep="")) -> eval_NY
  eval_NY_imp <- select(eval_NY, c('rank.sim_a','county'))
  paste("eval_NY_imp_2",i,sep="")
  
  Newname <- paste("eval_NY_imp_2_",i,sep="")
  assign(Newname,eval_NY_imp)
}



Check_1 <-c() 
Ds <-eval_NY_imp_2_1

for(i in 2:50){
  get(paste("eval_NY_imp_2_",i,sep="")) -> eval_NY_x
  Ds<-merge(Ds,eval_NY_x, by ='county') 
  names(Ds)[names(Ds) == 'rank.sim_a.x'] <- 'rank.sim_a'
  names(Ds)[names(Ds) == 'rank.sim_a.y'] <- (paste("imp_2_",i,sep=""))
  # Ds<- Ds[, !dupliNYted(colnames(Ds))]
  Newname <- paste("Check_",i,sep="")
  assign(Newname,Ds)
}

names(Check_50)[names(Check_50) == 'rank.sim_a'] <- 'imp_2_50'

Check_50$rank.sim_a_avg <- apply(Check_50[,2:51], 1, mean)
Check_50$rank.sim_a_std  <- apply(Check_50[,2:51], 1, sd)

Check_51 <- select(Check_50, c('county','rank.sim_a_avg','rank.sim_a_std'))

# Aggregating PUMS county information for CA,MA,and NY 
fst::write_fst(Check_51, "production/validation/rank_sim_a_avg_NY.fst", compress = 100)










