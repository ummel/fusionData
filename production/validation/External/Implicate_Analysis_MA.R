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
# For CA, aggregating fused electricity consumption at the county level
acs_MA <- acs[acs$state == "25", c("acs_2015_hid","puma10","state","weight")]
acs_MA <- merge(acs_MA, pums_county, by =c("puma10","state"))
pop_county_MA <- pop_county[pop_county$state == "25", c("county","hhunits")]
pop_county_MA$county <- tolower(pop_county_CA$county) 

util_comb_MA <- util_comb[util_comb$state == "25", c("county","totalkwh","averagekwh","hhunits")]
util_comb_MA$totalkwh <- util_comb_CA$totalkwh
trimws(util_comb_MA)

# Combing RECS - ACS fusion data 

for (i in 1:50) {
  recs.sim01_now <- get(paste0("recs_sim_imp",i,sep=""))
  comp_MA_01 <- recs.sim01_now %>%
    left_join(acs_MA, by = "acs_2015_hid")  %>%
    group_by(state, county) %>%
    summarize(county_weight = sum(weight),  # county total household weight
              totalkwh.sim = sum(btuel*0.293*weight),
              averagekwh.sim = (totalkwh.sim/county_weight),
              .groups = "drop") 
  trimws(comp_MA_01) 
  comp_MA_01 <- filter(comp_MA_01, (state != "NA"))
  comp_MA_01$county <- tolower(comp_MA_01$county) 
  Newname <- paste("comp_MA_01_",i,sep="")
  
     assign(Newname, comp_MA_01)
  }

for (i in 1:50) {
  comp_MA <- get(paste0("comp_MA_01_",i,sep=""))
comp_MA <- filter(comp_MA, (state != "NA"))
#Count number of shared counties per PUMA
comp_MA$count <- str_count(comp_MA$county,'&')

comp_MA_sub0 <- filter(comp_MA, (count == "0"))
comp_MA_sub0 <- select(comp_MA_sub0, c('county','county_weight','totalkwh.sim'))

#Divide the total kwh.sim and county_weights  by hhunits among the shared counties
comp_MA_sub <- comp_MA[grep("&", comp_MA$county), ]
comp_MA_sub <- select(comp_MA_sub, c('county','county_weight','totalkwh.sim'))

Newname1 <- paste("comp_MA_sub0_",i,sep="")
Newname2 <- paste("comp_MA_sub_",i,sep="")

assign(Newname1, comp_MA_sub0)
assign(Newname2, comp_MA_sub)
}

#Split each combined entry to separate dataframe

for (i in 1:50) {
  comp_MA_sub <- get(paste0("comp_MA_sub_",i,sep=""))
   n <- length(unique(comp_MA_sub$county))
  eval(parse(text = paste0("comp_MA_", seq(1:n), " <- ", split(comp_MA_sub, comp_MA_sub$county))))
  eval(parse(text = paste0("comp_MA_N", seq(1:n), " <-  as.data.frame(comp_MA_", seq(1:n), ")"))) 

  for(j in 1:n) {
  get(paste("comp_MA_N",j,sep="")) -> comp
  temp <- separate_rows(comp,county, sep = "&")
  temp1 <- temp %>% 
    stringdist_left_join(pop_county_MA, by=c('county'))
  
  
 temp1 <- select(temp1, c('county.x','county_weight','totalkwh.sim','hhunits'))
  
 temp1 <- temp1 %>% 
   dplyr::mutate(frac=round(hhunits/sum(hhunits),4))
  
  temp1$county_weight <- with(temp1, county_weight * frac)               
  temp1$totalkwh.sim <- with(temp1, totalkwh.sim * frac)               
 temp1 <- select(temp1, c('county.x','county_weight','totalkwh.sim'))
 colnames(temp1) <- c('county','county_weight','totalkwh.sim')
  Newname <- paste("comp_MA_small",j,sep="")
  assign(Newname,temp1)
}

#Merge the newly created county data into one dataframe 
MA_new_list <- list()
for(k in 1:n){
  df.now <- get(paste0("comp_MA_small",k,sep=""))
  MA_new_list[[k]] <- df.now
}

MA_list <- do.call(rbind,MA_new_list) 
Newname1 <- paste("MA_list_",i,sep="")
assign(Newname1, MA_list)

}

for(i in 1:50) {
  get(paste("MA_list_",i,sep="")) -> MA_list
  get(paste("comp_MA_sub0_",i,sep="")) -> comp_MA_sub0
  comp_MA_new <- rbind(MA_list,comp_MA_sub0)
  
  Newname <- paste("comp_MA_new_",i,sep="")
  assign(Newname,comp_MA_new)
  }


for(i in 1:50) {
  get(paste("comp_MA_new_",i,sep="")) -> comp_MA_new
  comp_MA <- comp_MA_new  %>%
  
  group_by(county) %>%
  summarize(county_weight = sum(county_weight),  # county total household weight
            totalkwh.sim = sum(totalkwh.sim),
            averagekwh.sim = (totalkwh.sim/county_weight),
            .groups = "drop") 
trimws(comp_MA) 
comp_MA$state <- '25' 

eval_MA <- stringdist_left_join(comp_MA, util_comb_MA, by =c("county"),distance_col = NULL)
eval_MA  <- select(eval_MA,c("state","county.x","totalkwh.sim","averagekwh.sim","averagekwh", "totalkwh","hhunits"))
colnames(eval_MA)[colnames(eval_MA) == 'county.x'] <- 'county'
eval_MA$averagekwh.sim <- round(eval_MA$averagekwh.sim, digits = 2) 
  
Newname <- paste("eval_MA_",i,sep="")
assign(Newname,eval_MA)
}

#----------------------------------------------------------------------------------------------------



#Rank Plot for average kwh 
for(i in 1:50) {
  get(paste("eval_MA_",i,sep="")) -> eval_MA
rank.sim_t <- rank(eval_MA$totalkwh.sim)
rank.util_t <- rank(eval_MA$totalkwh)
eval_MA <- cbind (eval_MA, rank.sim_t,rank.util_t)
Newname <- paste("eval_MA_",i,sep="")
assign(Newname,eval_MA)
}

# Extract same column from each implicate set 

for(i in 1:50) {
  get(paste("eval_MA_",i,sep="")) -> eval_MA
  eval_MA_imp <- select(eval_MA, c('rank.sim_t'))
  paste("eval_MA_imp",i,sep="")
  
  Newname <- paste("eval_MA_imp",i,sep="")
  assign(Newname,eval_MA_imp)
}

lsEOG<-list()

for (j in 1:50){
  z <- j
  sEOG <- paste("eval_MA_imp", z, sep="")
  dEOG <- get(paste("eval_MA_imp", z, sep=""))
  lsEOG[[sEOG]] <-dEOG
}

dfxx  <- as.data.frame(do.call(cbind, lsEOG)) 
colnames(dfxx) <- paste("rank.sim_t",1:ncol(dfxx), sep = "")
dfxx$rank.sim_t_avg  <- (rowMeans(dfxx))  
dfxx$rank.sim_t_std  <- (rowSds(as.matrix(dfxx,c(-51))))  

eval_MA_1xx <- select(eval_MA_1, c('county','rank.util_t','rank.sim_t'))
Final <- cbind(eval_MA_1xx,dfxx)
view(Final)
r <- ggplot(Final, aes(x = rank.util_t, y = rank.sim_t_avg)) + geom_point(size=5, color = "#7ea4b3") + xlim(0,20)+ ylim(0,20)
r + theme(text = element_text(size=18)) + theme(axis.line.x.bottom = element_line(color="black", size = 0.5), axis.line.x.top = element_line(color="black", size = 1.5), axis.line.y.left = element_line(color="black", size = 0.5), axis.line.y.right = element_line(color="black", size = 1.5)) + theme(axis.ticks.length = unit(-0.25, "cm")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+theme(panel.border = element_rect( fill = NA, size = 1))+
  geom_abline(slope = 1, intercept = 0, color = 'grey', linetype = "dashed")  +
  geom_errorbar(aes(ymin=rank.sim_t_avg-rank.sim_t_std , ymax=rank.sim_t_avg+rank.sim_t_std ), width=.2)

#----------------------------------------------------------------------------------------------------

#Average Kwh Error bars 

for(i in 1:50) {
  get(paste("eval_MA_",i,sep="")) -> eval_MA
  eval_MA_2 <- eval_MA  %>%
         group_by(averagekwh.sim) %>%
    summarize(
      totalkwh.sim = sum(totalkwh.sim), 
      averagekwh.sim = averagekwh.sim, 
      totalkwh = sum(totalkwh), 
      hhunits = sum(hhunits), 
      county = paste(county, collapse = "-"), 
      .groups = "drop") 
  eval_MA_2 <- distinct(eval_MA_2, .keep_all = FALSE)
  
  eval_MA_2$averagekwh <- eval_MA_2$totalkwh/eval_MA_2$hhunits
  
     Newname <- paste("eval_MA_2_",i,sep="")
  assign(Newname,eval_MA_2)
}

# Extract averagekwh.sim from each implicate set for std calculation 

for(i in 1:50) {
  get(paste("eval_MA_2_",i,sep="")) -> eval_MA
  eval_MA_imp_a <- select(eval_MA, c('averagekwh.sim','county'))
  
  Newname <- paste("eval_MA_imp_a_",i,sep="")
  assign(Newname,eval_MA_imp_a)
}

Check_1 <-c() 
Ds <-eval_MA_imp_a_1

for(i in 2:50){
  get(paste("eval_MA_imp_a_",i,sep="")) -> eval_MA_x
  Ds<-merge(Ds,eval_MA_x, by ='county') 
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
fst::write_fst(Check_51, "production/validation/averagekwh_sim_avg_MA.fst", compress = 100)


#----------------------------------------------------------------------------------------------------

#Rank Plot for average kwh 
for(i in 1:50) {
  get(paste("eval_MA_2_",i,sep="")) -> eval_MA
  rank.sim_a <- rank(eval_MA$averagekwh.sim)
  rank.util_a <- rank(eval_MA$averagekwh)
  eval_MA <- cbind (eval_MA, rank.sim_a,rank.util_a)
  Newname <- paste("eval_MA_2_",i,sep="")
  assign(Newname,eval_MA)
}


# Extract same column from each implicate set 
for(i in 1:50) {
  get(paste("eval_MA_2_",i,sep="")) -> eval_MA
  eval_MA_imp <- select(eval_MA, c('rank.sim_a','county'))
  paste("eval_MA_imp_2",i,sep="")
  
  Newname <- paste("eval_MA_imp_2_",i,sep="")
  assign(Newname,eval_MA_imp)
}

Check_1 <-c() 
Ds <-eval_MA_imp_2_1

for(i in 2:50){
    get(paste("eval_MA_imp_2_",i,sep="")) -> eval_MA_x
        Ds<-merge(Ds,eval_MA_x, by ='county') 
        names(Ds)[names(Ds) == 'rank.sim_a.x'] <- 'rank.sim_a'
        names(Ds)[names(Ds) == 'rank.sim_a.y'] <- (paste("imp_2_",i,sep=""))
      # Ds<- Ds[, !duplicated(colnames(Ds))]
        Newname <- paste("Check_",i,sep="")
    assign(Newname,Ds)
}

names(Check_50)[names(Check_50) == 'rank.sim_a'] <- 'imp_2_50'

Check_50$rank.sim_a_avg <- apply(Check_50[,2:51], 1, mean)
Check_50$rank.sim_a_std  <- apply(Check_50[,2:51], 1, sd)

Check_51 <- select(Check_50, c('county','rank.sim_a_avg','rank.sim_a_std'))

# Aggregating PUMS county information for CA,MA,and NY 
fst::write_fst(Check_51, "production/validation/rank_sim_a_avg_MA.fst", compress = 100)





