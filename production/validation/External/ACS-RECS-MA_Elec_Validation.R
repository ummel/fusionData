library(tidyverse)
library(fuzzyjoin)
library(dplyr)
options(scipen=integer)

setwd("/Users/user/Documents/Yale/FusionData/fusionData")

# Load fused RECS sim data for electricity consumption
recs.sim <- fst::read_fst("production/fusion/RECS_2015_sim_btuel.fst")

# Load required ACS microdata
acs <- fst::read_fst("survey-processed/ACS/2015/ACS_2015_H_processed.fst",
                     columns = c("acs_2015_hid", "state", "puma10", "weight"))

# Load required PUMS county map data for NY,MA,CA,PA,MD
pums_county <- fst::read_fst("production/validation/pums_county.fst")

# Load required hhunits county map data for NY, MA,CA,PA,MD
pop_county <- fst::read_fst("production/validation/pop_county.fst")

# Load required utilities external data for NY, MA,CA,PA,MD
util_comb <- fst::read_fst("production/validation/util_comb_val.fst")
#______________________________________________________________________________#

# Compare mean fused utility expenditure, by PUMA, with the external utilities data 
# For MA, aggregating fused electricity consumption at the county level

acs_MA <- acs[acs$state == "25", c("acs_2015_hid","puma10","state","weight")]
acs_MA <- merge(acs_MA, pums_county, by =c("puma10","state"))
pop_county_MA <- pop_county[pop_county$state == "25", c("county","hhunits")]
util_comb_MA <- util_comb[util_comb$state == "25", c("county","totalkwh","averagekwh","hhunits")]
util_comb_MA <- filter(util_comb_MA, (county != "NA")) #Remove any NA that might have been created 
trimws(util_comb_MA)

comp_MA <- recs.sim %>%
  left_join(acs_MA, by = "acs_2015_hid")  %>%
  group_by(state, county) %>%
  summarize(county_weight = sum(weight),  # county total household weight
            totalkwh.sim = sum(btuel*0.293*weight),
            averagekwh.sim = (totalkwh.sim/county_weight),
            .groups = "drop") 

trimws(comp_MA) 
comp_MA <- filter(comp_MA, (state != "NA")) #Remove any NA that might have been created 


#Count number of shared counties per PUMA
comp_MA$count <- str_count(comp_MA$county,'&')


comp_MA_sub0 <- filter(comp_MA, (count == "0"))   # Separate out counties with no sharing 
comp_MA_sub0 <- select(comp_MA_sub0, c('county','county_weight','totalkwh.sim'))


#Divide the total kwh.sim and county_weights  by hhunits among the shared counties
comp_MA_sub <- comp_MA[grep("&", comp_MA$county), ]
comp_MA_sub <- select(comp_MA_sub, c('county','county_weight','totalkwh.sim'))


#Split each combined entry to separate dataframe
splitData <- split(comp_MA_sub, comp_MA_sub$county)
n <- length(unique(comp_MA_sub$county))
eval(parse(text = paste0("comp_MA_", seq(1:n), " <- ", split(comp_MA_sub, comp_MA_sub$county))))
eval(parse(text = paste0("comp_MA_", seq(1:n), " <-  as.data.frame(comp_MA_", seq(1:10), ")"))) 

for(i in 1:10) {
  get(paste("comp_MA_",i,sep="")) -> comp
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
    Newname <- paste("comp_MA_small",i,sep="")
    assign(Newname,temp1)
}

#Merge the newly created county data into one dataframe 
MA_new_list <- list()
for(i in 1:10){
  df.now <- get(paste0("comp_MA_small",i,sep=""))
  MA_new_list[[i]] <- df.now
}

MA_list <- do.call(rbind,MA_new_list) 

comp_MA_new <- rbind(MA_list,comp_MA_sub0)   #Recombine untreated and treated counties  


comp_MA <- comp_MA_new  %>%   #Recalculate totals and averages after recombination 
  
  group_by(county) %>%
  summarize(county_weight = sum(county_weight),  # county total household weight
            totalkwh.sim = sum(totalkwh.sim),
            averagekwh.sim = (totalkwh.sim/county_weight),
            .groups = "drop") 
trimws(comp_MA) 
comp_MA$state <- '25' 

#Add utilities data to merged and compiled ACS data 
eval_MA <- stringdist_left_join(comp_MA, util_comb_MA, by =c("county"),distance_col = NULL)
eval_MA  <- select(eval_MA,c("state","county.x","totalkwh.sim","averagekwh.sim","averagekwh", "totalkwh","hhunits"))
colnames(eval_MA)[colnames(eval_MA) == 'county.x'] <- 'county'

#______________________________________________________________________________#

#Average grouping to remove duplicates 

eval_MA$averagekwh.sim <- round(eval_MA$averagekwh.sim, digits = 2)

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

#_________________________________________________________________________________________#

#Plots for MA data 

#Scatter Plot
r <- ggplot(eval_MA, aes(x = totalkwh, y = totalkwh.sim)) + geom_point(size=6, color = "#7ea4b3", alpha = 0.8) + xlim(0,8E+09) + ylim(0,8E+09)
r + theme(text = element_text(size=14)) + theme(axis.line.x.bottom = element_line(color="black", size = 0.5), axis.line.x.top = element_line(color="black", size = 1), axis.line.y.left = element_line(color="black", size = 0.5), axis.line.y.right = element_line(color="black", size = 1)) + theme(axis.ticks.length = unit(-0.25, "cm")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+theme(panel.border = element_rect(color = "black",fill = NA, size =1.5)) +
  geom_abline(slope = 1, intercept = 0, color = 'grey', linetype = "dashed") + theme(plot.margin = margin(1,1,1,1, "cm")) + theme(axis.text=element_text(size=14)) +
  labs(x = "Total Consumption (kWh): Utilities", y = "Total Consumption (kWh): Simulated") +
  geom_smooth(method = "lm", colour= "#7ea4b3", se = FALSE)

#Scatter Plot for hhunits 

r <- ggplot(eval_MA, aes(x = totalkwh, y = totalkwh.sim, color = hhunits)) + geom_point(size=5) + xlim(0,8E+09) + ylim(0,8E+09)
r + theme(text = element_text(size=12)) + theme(axis.line.x.bottom = element_line(color="black", size = 0.5), axis.line.x.top = element_line(color="black", size = 1), axis.line.y.left = element_line(color="black", size = 0.5), axis.line.y.right = element_line(color="black", size = 1)) + theme(axis.ticks.length = unit(-0.25, "cm")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+theme(panel.border = element_rect(color = "black",fill = NA, size =1.5)) +
  geom_abline(slope = 1, intercept = 0, color = 'grey', linetype = "dashed") + theme(plot.margin = margin(1,1,1,1, "cm")) + theme(axis.text=element_text(size=12)) +
  labs(x = "Total Consumption (kWh): Utilities", y = "Total Consumption (kWh): Simulated") +
  geom_smooth(method = "lm", colour="#ff6961", alpha = 0.8, se = FALSE) 

r <- ggplot(eval_MA, aes(x = hhunits, y = totalkwh.sim)) + geom_point(size=5, color = "#7ea4b3") + ylim(0,8E+09)
r + theme(text = element_text(size=12)) + theme(axis.line.x.bottom = element_line(color="black", size = 0.5), axis.line.x.top = element_line(color="black", size = 1), axis.line.y.left = element_line(color="black", size = 0.5), axis.line.y.right = element_line(color="black", size = 1)) + theme(axis.ticks.length = unit(-0.25, "cm")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+theme(panel.border = element_rect(color = "black",fill = NA, size =1.5)) +
  theme(plot.margin = margin(1,1,1,1, "cm")) + theme(axis.text=element_text(size=12)) +
  labs(x = "Household Units (hhunits) ", y = "Total Consumption (kWh): Simulated") +
  geom_smooth(method = "lm", colour="#7ea4b3", alpha = 0.8, se = FALSE) 

r <- ggplot(eval_MA, aes(x = hhunits, y = totalkwh)) + geom_point(size=5, color = "grey")  + ylim(0,8E+09)
r + theme(text = element_text(size=12)) + theme(axis.line.x.bottom = element_line(color="black", size = 0.5), axis.line.x.top = element_line(color="black", size = 1), axis.line.y.left = element_line(color="black", size = 0.5), axis.line.y.right = element_line(color="black", size = 1)) + theme(axis.ticks.length = unit(-0.25, "cm")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+theme(panel.border = element_rect(color = "black",fill = NA, size =1.5)) +
  theme(plot.margin = margin(1,1,1,1, "cm")) + theme(axis.text=element_text(size=12)) +
  labs(x = "Household Units (hhunits) ", y = "Total Consumption (kWh): Utilities") +
  geom_smooth(method = "lm", colour="grey", alpha = 0.8, se = FALSE) 

#Rank Plot for total kwh 
rank.sim_t <- rank(-eval_MA$totalkwh.sim)
rank.util_t <- rank(-eval_MA$totalkwh)
eval_MA <- cbind (eval_MA, rank.sim_t,rank.util_t)

r <- ggplot(eval_MA, aes(x = rank.util_t, y = rank.sim_t)) + geom_point(size=5, color = "#7ea4b3") + xlim(0,20)+ ylim(0,20)
r + theme(text = element_text(size=14)) + theme(axis.line.x.bottom = element_line(color="black", size = 0.5), axis.line.x.top = element_line(color="black", size = 1), axis.line.y.left = element_line(color="black", size = 0.5), axis.line.y.right = element_line(color="black", size = 1)) + theme(axis.ticks.length = unit(-0.25, "cm")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+theme(panel.border = element_rect(color = "black",fill = NA, size =1.5)) +
  geom_abline(slope = 1, intercept = 0, color = 'grey', linetype = "dashed") + theme(plot.margin = margin(1,1,1,1, "cm")) + theme(axis.text=element_text(size=14)) +
  labs(x = "Total Consumption Rank : Utilities", y = "Total Consumption Rank: Simulated")  +
  geom_smooth(method = "lm", colour="#7ea4b3", se = FALSE)

# Distribution for total kwh county wise and overall density plot  

r <- ggplot () + geom_density(data=eval_MA, aes(x = totalkwh), size = 1) + xlim(0,8E+09) + ylim(0,4E-10)
r + theme(text = element_text(size=14)) + theme(axis.line.x.bottom = element_line(color="black", size = 0.5), axis.line.x.top = element_line(color="black", size = 1.25), axis.line.y.left = element_line(color="black", size = 0.5), axis.line.y.right = element_line(color="black", size = 1.25)) + theme(axis.ticks.length = unit(-0.25, "cm")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+theme(panel.border = element_rect( fill = NA, size = 1.25)) +
  theme(plot.margin = margin(1,1,1,1, "cm")) + theme(axis.text=element_text(size=14)) +
  labs(x = "Total Consumption (kWh): Utilities", y = "Density")  

r <- ggplot () + geom_density(data=eval_MA, aes(x = totalkwh.sim), color = "#7ea4b3",size = 1) + xlim(0,8E+9) + ylim(0,4E-10)
r + theme(text = element_text(size=14)) + theme(axis.line.x.bottom = element_line(color="black", size = 0.5), axis.line.x.top = element_line(color="black", size = 1.5), axis.line.y.left = element_line(color="black", size = 0.5), axis.line.y.right = element_line(color="black", size = 1.5)) + theme(axis.ticks.length = unit(-0.25, "cm")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+theme(panel.border = element_rect( fill = NA, size = 1.5)) +
  theme(plot.margin = margin(1,1,1,1, "cm")) + theme(axis.text=element_text(size=14)) +
  labs(x = "Total Consumption (kWh): Simulated", y = "Density")  

# Bar plot  for total kwh county wise 

r <- ggplot () + geom_bar(data=eval_MA, aes(x = reorder(county,-totalkwh), y = totalkwh), stat = 'identity',width = 0.75, size = 0.5, fill = "grey", color ="grey")   + ylim(0,8E+9)
r + theme(text = element_text(size=14)) + theme(axis.line.x.bottom = element_line(color="black", size = 0.5), axis.line.x.top = element_line(color="black", size = 1.25), axis.line.y.left = element_line(color="black", size = 0.5), axis.line.y.right = element_line(color="black", size = 1.25)) + theme(axis.ticks.length = unit(-0.25, "cm")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+theme(panel.border = element_rect( fill = NA, size = 1.25)) +
  theme(plot.margin = margin(1,1,1,1, "cm")) + theme(axis.text=element_text(size=14)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "County", y = "Total Consumption (kWh): Utilities")  

r <- ggplot () + geom_bar(data=eval_MA, aes(x = reorder(county,-totalkwh.sim), y = totalkwh.sim), stat = 'identity',width = 0.75, size = 0.5, fill = "#7ea4b3",color ="#7ea4b3")   + ylim(0,8E+9)
r + theme(text = element_text(size=14)) + theme(axis.line.x.bottom = element_line(color="black", size = 0.5), axis.line.x.top = element_line(color="black", size = 1.25), axis.line.y.left = element_line(color="black", size = 0.5), axis.line.y.right = element_line(color="black", size = 1.25)) + theme(axis.ticks.length = unit(-0.25, "cm")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+theme(panel.border = element_rect( fill = NA, size = 1.25)) +
  theme(plot.margin = margin(1,1,1,1, "cm")) + theme(axis.text=element_text(size=14)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "County", y = "Total Consumption (kWh): Utilities")  


#Scatter plots for averages  

# MA_avg_imp <- read_fst("production/validation/averagekwh_sim_avg_MA.fst")  #Add std dev from implicate runs 
eval_MA_2 <- stringdist_left_join(eval_MA_2, MA_avg_imp, by =c("county"), method='qgram', q=2,distance_col = NULL)

r <- ggplot(eval_MA_2, aes(x = averagekwh, y = averagekwh.sim)) + geom_point(size=6, color = "#7ea4b3",alpha = 0.8) +xlim(4000,10000) +ylim(4000,10000)
r + theme(text = element_text(size=14)) + theme(axis.line.x.bottom = element_line(color="black", size = 0.5), axis.line.x.top = element_line(color="black", size = 1.5), axis.line.y.left = element_line(color="black", size = 0.5), axis.line.y.right = element_line(color="black", size = 1.5)) + theme(axis.ticks.length = unit(-0.25, "cm")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+theme(panel.border = element_rect( fill = NA, size = 1)) + 
  geom_abline(slope = 1, intercept = 0, color = 'grey', linetype = "dashed")  + theme(plot.margin = margin(1,1,1,1, "cm")) + theme(axis.text=element_text(size=14)) +
  geom_smooth(method = "lm", colour="#7ea4b3", se = FALSE, size = 1)  +
  labs(x = "Average Consumption (kWh) : Utilities", y = "Average Consumption (kWh) : Simulated")  #+ 
  # geom_errorbar(aes(ymin = averagekwh.sim - averagekwh.sim_std , ymax = averagekwh.sim + averagekwh.sim_std),colour = "#7ea4b3", size=1)   

eval_MA_2N <- eval_MA_2[-5,]
r <- ggplot(eval_MA_2N, aes(x = averagekwh, y = averagekwh.sim)) + geom_point(size=6, color = "#7ea4b3",alpha = 0.8) +xlim(4000,10000) +ylim(4000,10000)
r + theme(text = element_text(size=14)) + theme(axis.line.x.bottom = element_line(color="black", size = 0.5), axis.line.x.top = element_line(color="black", size = 1.5), axis.line.y.left = element_line(color="black", size = 0.5), axis.line.y.right = element_line(color="black", size = 1.5)) + theme(axis.ticks.length = unit(-0.25, "cm")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+theme(panel.border = element_rect( fill = NA, size = 1)) + 
  geom_abline(slope = 1, intercept = 0, color = 'grey', linetype = "dashed")  + theme(plot.margin = margin(1,1,1,1, "cm")) + theme(axis.text=element_text(size=14)) +
  geom_errorbar(aes(ymin = averagekwh.sim - averagekwh.sim_std , ymax = averagekwh.sim + averagekwh.sim_std),colour = "#7ea4b3", size=1) +
  geom_smooth(method = "lm", colour="#7ea4b3", se = FALSE, size = 1)  +
  labs(x = "Average Consumption (kWh) : Utilities", y = "Average Consumption (kWh) : Simulated")  


# Rank plot 
# MA_rank_avg_imp  <- read_fst("production/validation/rank_sim_a_avg_MA.fst")
eval_MA_2 <- stringdist_left_join(eval_MA_2, MA_rank_avg_imp, by =c("county"), method='qgram', q=2,distance_col = NULL)


rank.sim_a <- rank(-eval_MA_2$averagekwh.sim)
rank.util_a <- rank(-eval_MA_2$averagekwh)
eval_MA_2 <- cbind (eval_MA_2, rank.sim_a,rank.util_a)

r <- ggplot(eval_MA_2, aes(x = rank.util_a, y = rank.sim_a)) + geom_point(size=5, color = "#7ea4b3", alpha = 0.8) + xlim(0,20)+ ylim(0,20)
r + theme(text = element_text(size=14)) + theme(axis.line.x.bottom = element_line(color="black", size = 0.5), axis.line.x.top = element_line(color="black", size = 1), axis.line.y.left = element_line(color="black", size = 0.5), axis.line.y.right = element_line(color="black", size = 1)) + theme(axis.ticks.length = unit(-0.25, "cm")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+theme(panel.border = element_rect(color = "black",fill = NA, size =1.5)) +
  geom_abline(slope = 1, intercept = 0, color = 'grey', linetype = "dashed") + theme(plot.margin = margin(1,1,1,1, "cm")) + theme(axis.text=element_text(size=14)) +
  labs(x = "Average Consumption Rank : Utilities", y = "Average Consumption Rank: Simulated")  +
  geom_smooth(method = "lm", colour="#7ea4b3", se = FALSE) +
  geom_errorbar(aes(ymin = rank.sim_a - rank.sim_a_std, ymax = rank.sim_a + rank.sim_a_std),colour = "#7ea4b3", width =1.5) 


# Distribution for average kwh county wise 
r <- ggplot () + geom_density(data=eval_MA_2, aes(x = averagekwh), size = 1) + xlim(4000,10000) +ylim(0,12E-4)
r + theme(text = element_text(size=14)) + theme(axis.line.x.bottom = element_line(color="black", size = 0.5), axis.line.x.top = element_line(color="black", size = 1.25), axis.line.y.left = element_line(color="black", size = 0.5), axis.line.y.right = element_line(color="black", size = 1.25)) + theme(axis.ticks.length = unit(-0.25, "cm")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+theme(panel.border = element_rect( fill = NA, size = 1.25)) +
  theme(plot.margin = margin(1,1,1,1, "cm")) + theme(axis.text=element_text(size=14)) +
  labs(x = "Average Consumption (kWh): Utilities", y = "Density")  

r <- ggplot () + geom_density(data=eval_MA_2, aes(x = averagekwh.sim), color = "#7ea4b3",size = 1) + xlim(4000,10000) +ylim(0,12E-4)
r + theme(text = element_text(size=14)) + theme(axis.line.x.bottom = element_line(color="black", size = 0.5), axis.line.x.top = element_line(color="black", size = 1.25), axis.line.y.left = element_line(color="black", size = 0.5), axis.line.y.right = element_line(color="black", size = 1.25)) + theme(axis.ticks.length = unit(-0.25, "cm")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+theme(panel.border = element_rect( fill = NA, size = 1.25)) +
  theme(plot.margin = margin(1,1,1,1, "cm")) + theme(axis.text=element_text(size=14)) +
  labs(x = "Average Consumption (kWh): Simulation", y = "Density")  


# Bar lot for averages   
r <- ggplot () + geom_bar(data=eval_MA_2, aes(x = reorder(county.x,-averagekwh), y = averagekwh), stat = 'identity',width = 0.75, size = 0.5, fill = "grey", color ="grey") + ylim(0,10000)
r + theme(text = element_text(size=12)) + theme(axis.line.x.bottom = element_line(color="black", size = 0.5), axis.line.x.top = element_line(color="black", size = 1.25), axis.line.y.left = element_line(color="black", size = 0.5), axis.line.y.right = element_line(color="black", size = 1.25)) + theme(axis.ticks.length = unit(-0.25, "cm")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+theme(panel.border = element_rect( fill = NA, size = 1.25)) +
  theme(plot.margin = margin(1,1,1,1, "cm")) + theme(axis.text=element_text(size=12)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "County", y = "Average Consumption (kWh): Utilities") 

r <- ggplot () + geom_bar(data=eval_MA_2, aes(x = reorder(county.x,-averagekwh.sim), y = averagekwh.sim), stat = 'identity',width = 0.75, size = 0.5, fill = "#7ea4b3",color ="#7ea4b3") + ylim(0,10000)
r + theme(text = element_text(size=12)) + theme(axis.line.x.bottom = element_line(color="black", size = 0.5), axis.line.x.top = element_line(color="black", size = 1.25), axis.line.y.left = element_line(color="black", size = 0.5), axis.line.y.right = element_line(color="black", size = 1.25)) + theme(axis.ticks.length = unit(-0.25, "cm")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+theme(panel.border = element_rect( fill = NA, size = 1.25)) +
  theme(plot.margin = margin(1,1,1,1, "cm")) + theme(axis.text=element_text(size=12)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "County", y = "Average Consumption (kWh): Simulated") 



#County Map for MA
states <- map_data("state")

#select only the Washington information from the states dataset
MA_state <- subset(states, region == "massachusetts")

#name the built-in county dataset in mapdata to 'counties'
counties <- map_data("county")

#select only the california information from the states dataset
MA_county <- subset(counties, region == "massachusetts")

MA_base <- ggplot(data = MA_state, mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) + geom_polygon(color = "black", fill = "gray")

ditch_the_axis <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

MA_base + ditch_the_axis +
  geom_polygon(data = MA_county, fill = NA, color = "white") +
  geom_polygon(color = "black", fill = NA)
MA_county$county <- MA_county$subregion

eval_MA_map_sim_t <- select(eval_MA,c("county","rank.sim_t"))
eval_MA_map_util_t <- select(eval_MA,c("county","rank.util_t"))


rank_map <- stringdist_left_join(MA_county, eval_MA_map_sim_t , by = "county")
rank_map2 <- stringdist_left_join(MA_county, eval_MA_map_util_t , by = "county")

gg1 <- MA_base +
  geom_polygon(data = rank_map, aes(fill = rank.sim_t), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() +
  ditch_the_axis +
  ggtitle("Total electricity consumtion rank")
gg2 <- gg1 + scale_fill_viridis_c(option = "D") 

gg2
