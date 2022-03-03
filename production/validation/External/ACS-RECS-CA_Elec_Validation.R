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

# Load required hhunits county map data for NY,MA,CA,PA,MD
pop_county <- fst::read_fst("production/validation/pop_county.fst")

# Load required utilities external data for NY,MA,CA,PA,MD
util_comb <- fst::read_fst("production/validation/util_comb_val.fst")

#_________________________#

# Compare mean fused utility expenditure, by PUMA, with the external utilities data 
#For CA, aggregating fused electricity consumption at the county level

acs_CA <- acs[acs$state == "06", c("acs_2015_hid","puma10","state","weight")]
acs_CA <- merge(acs_CA, pums_county, by =c("puma10","state"))
pop_county_CA <- pop_county[pop_county$state == "06", c("county","hhunits")]
pop_county_CA$county <- tolower(pop_county_CA$county) 

util_comb_CA <- util_comb[util_comb$state == "06", c("county","totalkwh","averagekwh","hhunits")]
util_comb_CA$totalkwh <- util_comb_CA$totalkwh
trimws(util_comb_CA)

comp_CA <- recs.sim %>%
  left_join(acs_CA, by = "acs_2015_hid")  %>%
  group_by(state, county) %>%
  summarize(county_weight = sum(weight),  # county total household weight
            totalkwh.sim = sum(btuel*0.293*weight),
            averagekwh.sim = (totalkwh.sim/county_weight),
            .groups = "drop") 

trimws(comp_CA) 
comp_CA <- filter(comp_CA, (state != "NA"))
comp_CA$county <- tolower(comp_CA$county) 


#Count number of shared counties per PUMA
comp_CA$count <- str_count(comp_CA$county,'&')

comp_CA_sub0 <- filter(comp_CA, (count == "0"))  # Separate out counties with no sharing 
comp_CA_sub0 <- select(comp_CA_sub0, c('county','county_weight','totalkwh.sim'))

#Divide the total kwh.sim and county_weights  by hhunits among the shared counties
comp_CA_sub <- comp_CA[grep("&", comp_CA$county), ]
comp_CA_sub <- select(comp_CA_sub, c('county','county_weight','totalkwh.sim'))


#Split each combined entry to separate dataframe
n <- length(unique(comp_CA_sub$county))
eval(parse(text = paste0("comp_CA_", seq(1:n), " <- ", split(comp_CA_sub, comp_CA_sub$county))))
eval(parse(text = paste0("comp_CA_N", seq(1:n), " <-  as.data.frame(comp_CA_", seq(1:n), ")"))) 

for(i in 1:n) {
  get(paste("comp_CA_N",i,sep="")) -> comp
     temp <- separate_rows(comp,county, sep = "&")
     temp1 <- temp %>% 
       stringdist_left_join(pop_county_CA, by=c('county'), method='qgram', q=2)
    
     temp1 <- select(temp1, c('county.x','county_weight','totalkwh.sim','hhunits'))
     
     temp1 <- temp1 %>% 
      dplyr::mutate(frac=round(hhunits/sum(hhunits),4))
     
    temp1$county_weight <- with(temp1, county_weight * frac)               
     temp1$totalkwh.sim <- with(temp1, totalkwh.sim * frac)               
    temp1 <- select(temp1, c('county.x','county_weight','totalkwh.sim'))
     colnames(temp1) <- c('county','county_weight','totalkwh.sim')
    Newname <- paste("comp_CA_small",i,sep="")
   assign(Newname,temp1)
}

#Merge the newly created county data into one dataframe 
CA_new_list <- list()
for(i in 1:n){
  df.now <- get(paste0("comp_CA_small",i,sep=""))
  CA_new_list[[i]] <- df.now
}

CA_list <- do.call(rbind,CA_new_list) 

comp_CA_new <- rbind(CA_list,comp_CA_sub0) #Recombine untreated and treated counties  


comp_CA <- comp_CA_new  %>%
  
  group_by(county) %>%
  summarize(county_weight = sum(county_weight),  # county total household weight
            totalkwh.sim = sum(totalkwh.sim),
            averagekwh.sim = (totalkwh.sim/county_weight),
            .groups = "drop") 
trimws(comp_CA) 
comp_CA$state <- '06' 
comp_CA$county <- tolower(comp_CA$county)

eval_CA <- stringdist_left_join(comp_CA, util_comb_CA, by =c("county"), method='qgram', q=2,distance_col = NULL)
eval_CA  <- select(eval_CA,c("state","county.x","totalkwh.sim","averagekwh.sim","averagekwh", "totalkwh","hhunits"))
colnames(eval_CA)[colnames(eval_CA) == 'county.x'] <- 'county'

#----------------------------------------------------------------------------------------------------------------------------------------

eval_CA$averagekwh.sim <- round(eval_CA$averagekwh.sim, digits = 2)

eval_CA_2 <- eval_CA  %>%
  
  group_by(averagekwh.sim) %>%
  summarize(
    totalkwh.sim = sum(totalkwh.sim), 
    averagekwh.sim = averagekwh.sim, 
    totalkwh = sum(totalkwh), 
    hhunits = sum(hhunits), 
    county = paste(county, collapse = "-"), 
    .groups = "drop") 
eval_CA_2 <- distinct(eval_CA_2, .keep_all = FALSE)

eval_CA_2$averagekwh <- eval_CA_2$totalkwh/eval_CA_2$hhunits

#______________________________________________________________________________________________#

#Plot CA data 

#Read averaged total kwh 
#CA_imp <- read_fst("production/validation/totalkwh_sim_avg_CA.fst")

#eval_CA <- stringdist_left_join(eval_CA, CA_imp, by =c("county"), method='qgram', q=2,distance_col = NULL)

#Scatter Plot

#Trends in  CA data 
#Scatter Plot for total and average kwh values. 

r <- ggplot(eval_CA, aes(x = totalkwh, y = totalkwh.sim)) + geom_point(size=6, color = "#ff6961", aplha = 0.7) + xlim(0,2.5E+10) + ylim(0,2.5E+10)
r + theme(text = element_text(size=14)) + theme(axis.line.x.bottom = element_line(color="black", size = 0.5), axis.line.x.top = element_line(color="black", size = 1), axis.line.y.left = element_line(color="black", size = 0.5), axis.line.y.right = element_line(color="black", size = 1)) + theme(axis.ticks.length = unit(-0.25, "cm")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+theme(panel.border = element_rect(color = "black",fill = NA, size =1.5)) +
  geom_abline(slope = 1, intercept = 0, color = 'grey', linetype = "dashed") + theme(plot.margin = margin(1,1,1,1, "cm")) + theme(axis.text=element_text(size=14)) +
  labs(x = "Total Consumption (kWh): Utilities", y = "Total Consumption (kWh): Simulated") +
  geom_smooth(method = "lm", colour="#ff6961", alpha = 0.8, se = FALSE) #+
  #geom_errorbar(aes(ymin = totalkwh.sim_avg-totalkwh.sim_std , ymax = totalkwh.sim_avg+totalkwh.sim_std),colour = "#ff6961", alpha=0.9, size=.6)


#Scatter Plot for hhunits 

r <- ggplot(eval_CA, aes(x = totalkwh, y = totalkwh.sim, color = hhunits)) + geom_point(size=5) 
r + theme(text = element_text(size=12)) + theme(axis.line.x.bottom = element_line(color="black", size = 0.5), axis.line.x.top = element_line(color="black", size = 1), axis.line.y.left = element_line(color="black", size = 0.5), axis.line.y.right = element_line(color="black", size = 1)) + theme(axis.ticks.length = unit(-0.25, "cm")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+theme(panel.border = element_rect(color = "black",fill = NA, size =1.5)) +
  geom_abline(slope = 1, intercept = 0, color = 'grey', linetype = "dashed") + theme(plot.margin = margin(1,1,1,1, "cm")) + theme(axis.text=element_text(size=12)) +
  labs(x = "Total Consumption (kWh): Utilities", y = "Total Consumption (kWh): Simulated") +
  geom_smooth(method = "lm", colour="#ff6961", alpha = 0.8, se = FALSE) 
  

r <- ggplot(eval_CA, aes(x = hhunits, y = totalkwh.sim)) + geom_point(size=5, color = "#ff6961") + ylim (0,2.5E+10)
r + theme(text = element_text(size=12)) + theme(axis.line.x.bottom = element_line(color="black", size = 0.5), axis.line.x.top = element_line(color="black", size = 1), axis.line.y.left = element_line(color="black", size = 0.5), axis.line.y.right = element_line(color="black", size = 1)) + theme(axis.ticks.length = unit(-0.25, "cm")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+theme(panel.border = element_rect(color = "black",fill = NA, size =1.5)) +
 theme(plot.margin = margin(1,1,1,1, "cm")) + theme(axis.text=element_text(size=12)) +
  labs(x = "Household Units (hhunits) ", y = "Total Consumption (kWh): Simulated") +
  geom_smooth(method = "lm", colour="#ff6961", alpha = 0.8, se = FALSE) 


r <- ggplot(eval_CA, aes(x = hhunits, y = totalkwh)) + geom_point(size=5, color = "grey") + ylim (0,2.5E+10)
r + theme(text = element_text(size=12)) + theme(axis.line.x.bottom = element_line(color="black", size = 0.5), axis.line.x.top = element_line(color="black", size = 1), axis.line.y.left = element_line(color="black", size = 0.5), axis.line.y.right = element_line(color="black", size = 1)) + theme(axis.ticks.length = unit(-0.25, "cm")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+theme(panel.border = element_rect(color = "black",fill = NA, size =1.5)) +
  theme(plot.margin = margin(1,1,1,1, "cm")) + theme(axis.text=element_text(size=12)) +
  labs(x = "Household Units (hhunits) ", y = "Total Consumption (kWh): Utilities") +
  geom_smooth(method = "lm", colour="grey", alpha = 0.8, se = FALSE) 


#Rank plots for county kwh consumption 
rank.sim_t <- rank(-eval_CA$totalkwh.sim)
rank.util_t <- rank(-eval_CA$totalkwh)
eval_CA <- cbind (eval_CA, rank.sim_t,rank.util_t)

r <- ggplot(eval_CA, aes(x = rank.util_t, y = rank.sim_t)) + geom_point(size=5, color = "#ff6961",alpha = 0.7) + xlim(0,80)+ ylim(0,80)
r + theme(text = element_text(size=14)) + theme(axis.line.x.bottom = element_line(color="black", size = 0.5), axis.line.x.top = element_line(color="black", size = 1), axis.line.y.left = element_line(color="black", size = 0.5), axis.line.y.right = element_line(color="black", size = 1)) + theme(axis.ticks.length = unit(-0.25, "cm")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+theme(panel.border = element_rect(color = "black",fill = NA, size =1.5)) +
  geom_abline(slope = 1, intercept = 0, color = 'grey', linetype = "dashed") + theme(plot.margin = margin(1,1,1,1, "cm")) + theme(axis.text=element_text(size=14)) +
  labs(x = "Total Consumption Rank : Utilities", y = "Total Consumption Rank: Simulated")  +
  geom_smooth(method = "lm", colour="#ff6961", se = FALSE)

# Distribution for average kwh county wise and overal density plot  
options(scipen  = 0)

r <- ggplot () + geom_density(data=eval_CA, aes(x =totalkwh), size = 1) + xlim(00,2.5e+10) + ylim(0,6E-10)
r + theme(text = element_text(size=14)) + theme(axis.line.x.bottom = element_line(color="black", size = 0.5), axis.line.x.top = element_line(color="black", size = 1.25), axis.line.y.left = element_line(color="black", size = 0.5), axis.line.y.right = element_line(color="black", size = 1.25)) + theme(axis.ticks.length = unit(-0.25, "cm")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+theme(panel.border = element_rect( fill = NA, size = 1.25)) +
  theme(plot.margin = margin(1,1,1,1, "cm")) + theme(axis.text=element_text(size=14)) +
  labs(x = "Total Consumption (kWh): Utilities", y = "Density")  

r <- ggplot () + geom_density(data=eval_CA, aes(x = totalkwh.sim), color = "#ff6961",size = 1) + xlim(0,2.5e+10) + ylim(0,6E-10)
r + theme(text = element_text(size=14)) + theme(axis.line.x.bottom = element_line(color="black", size = 0.5), axis.line.x.top = element_line(color="black", size = 1.5), axis.line.y.left = element_line(color="black", size = 0.5), axis.line.y.right = element_line(color="black", size = 1.5)) + theme(axis.ticks.length = unit(-0.25, "cm")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+theme(panel.border = element_rect( fill = NA, size = 1.5)) +
  theme(plot.margin = margin(1,1,1,1, "cm")) + theme(axis.text=element_text(size=14)) +
  labs(x = "Total Consumption (kWh): Simulated", y = "Density")  

#Bar plots for total kwh 

r <- ggplot () + geom_bar(data=eval_CA, aes(x = reorder(county.x,-totalkwh), y = totalkwh), stat = 'identity',width = 0.75, size = 0.5, fill = "grey", color ="grey") + ylim(0,2.5E+10)
r + theme(text = element_text(size=12)) + theme(axis.line.x.bottom = element_line(color="black", size = 0.5), axis.line.x.top = element_line(color="black", size = 1.25), axis.line.y.left = element_line(color="black", size = 0.5), axis.line.y.right = element_line(color="black", size = 1.25)) + theme(axis.ticks.length = unit(-0.25, "cm")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+theme(panel.border = element_rect( fill = NA, size = 1.25)) +
  theme(plot.margin = margin(1,1,1,1, "cm")) + theme(axis.text=element_text(size=9)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "County", y = "Total Consumption (kWh): Utilities")  

r <- ggplot () + geom_bar(data=eval_CA, aes(x = reorder(county.x,-totalkwh.sim), y = totalkwh.sim), stat = 'identity',width = 0.75, size = 0.5, fill = "#ff6961",color ="#ff6961")  + ylim(0,2.5E+10)
r + theme(text = element_text(size=12)) + theme(axis.line.x.bottom = element_line(color="black", size = 0.5), axis.line.x.top = element_line(color="black", size = 1.25), axis.line.y.left = element_line(color="black", size = 0.5), axis.line.y.right = element_line(color="black", size = 1.25)) + theme(axis.ticks.length = unit(-0.25, "cm")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+theme(panel.border = element_rect( fill = NA, size = 1.25)) +
  theme(plot.margin = margin(1,1,1,1, "cm")) + theme(axis.text=element_text(size=9)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "County", y = "Total Consumption (kWh): Utilities")  

#______________________________________________________________________________________________#

#Read averaged total kwh 
#CA_avg_imp <- read_fst("production/validation/averagekwh_sim_avg_CA.fst")
#eval_CA_2 <- stringdist_left_join(eval_CA_2, CA_avg_imp, by =c("county"), method='qgram', q=2,distance_col = NULL)

r <- ggplot(eval_CA_2, aes(x = averagekwh, y = averagekwh.sim_avg)) + geom_point(size=6, color = "#ff6961", alpha = 0.7) + xlim(2000,15000) + ylim(2000,15000)
r + theme(text = element_text(size=14)) + theme(axis.line.x.bottom = element_line(color="black", size = 0.5), axis.line.x.top = element_line(color="black", size = 1.5), axis.line.y.left = element_line(color="black", size = 0.5), axis.line.y.right = element_line(color="black", size = 1.5)) + theme(axis.ticks.length = unit(-0.25, "cm")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+theme(panel.border = element_rect( fill = NA, size = 1)) + 
  geom_abline(slope = 1, intercept = 0, color = 'grey', linetype = "dashed")  + theme(plot.margin = margin(1,1,1,1, "cm")) + theme(axis.text=element_text(size=14)) +
  geom_errorbar(aes(ymin = averagekwh.sim - averagekwh.sim_std , ymax = averagekwh.sim + averagekwh.sim_std),colour = "#ff6961", size=1) +
  geom_smooth(method = "lm", colour="#ff6961", se = FALSE, size = 1)  +
  labs(x = "Average Consumption (kWh) : Utilities", y = "Average Consumption (kWh) : Simulated")  

#--------------------------------

#CA_rank_avg_imp  <- read_fst("production/validation/rank_sim_a_avg_CA.fst")
#eval_CA_2 <- stringdist_left_join(eval_CA_2, CA_rank_avg_imp, by =c("county"), method='qgram', q=2,distance_col = NULL)

rank.sim_a <- rank(-eval_CA_2$averagekwh.sim)
rank.util_a <- rank(-eval_CA_2$averagekwh)
eval_CA_2 <- cbind (eval_CA_2, rank.sim_a,rank.util_a)

r <- ggplot(eval_CA_2, aes(x = rank.util_a, y = rank.sim_a)) + geom_point(size=5, color = "#ff6961",alpha = 0.8) + xlim(0,60)+ ylim(0,60)
r + theme(text = element_text(size=14)) + theme(axis.line.x.bottom = element_line(color="black", size = 0.5), axis.line.x.top = element_line(color="black", size = 1), axis.line.y.left = element_line(color="black", size = 0.5), axis.line.y.right = element_line(color="black", size = 1)) + theme(axis.ticks.length = unit(-0.25, "cm")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+theme(panel.border = element_rect(color = "black",fill = NA, size =1.5)) +
  geom_abline(slope = 1, intercept = 0, color = 'grey', linetype = "dashed") + theme(plot.margin = margin(1,1,1,1, "cm")) + theme(axis.text=element_text(size=14)) +
  labs(x = "Average Consumption Rank : Utilities", y = "Averagel Consumption Rank: Simulated")  +
  geom_smooth(method = "lm", colour="#ff6961", alpha = 0.8, se = FALSE) #+
#geom_errorbar(aes(ymin = rank.sim_a- rank.sim_a_std, ymax = rank.sim_a + rank.sim_a_std),colour = "#ff6961", width =1.5) 

# Distribution for average kwh county wise and overall density plot  
r <- ggplot () + geom_density(data=eval_CA_2, aes(x = averagekwh), size = 1)  + xlim(0,16000) + ylim(0,4E-4)
r + theme(text = element_text(size=14)) + theme(axis.line.x.bottom = element_line(color="black", size = 0.5), axis.line.x.top = element_line(color="black", size = 1.25), axis.line.y.left = element_line(color="black", size = 0.5), axis.line.y.right = element_line(color="black", size = 1.25)) + theme(axis.ticks.length = unit(-0.25, "cm")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+theme(panel.border = element_rect( fill = NA, size = 1.25)) +
  theme(plot.margin = margin(1,1,1,1, "cm")) + theme(axis.text=element_text(size=14)) +
  labs(x = "Average Consumption (kWh): Utilities", y = "Density")  

r <- ggplot () + geom_density(data=eval_CA_2, aes(x = averagekwh.sim), color = "#ff6961",size = 1) + xlim(0,16000) + ylim(0,4E-4)
r + theme(text = element_text(size=14)) + theme(axis.line.x.bottom = element_line(color="black", size = 0.5), axis.line.x.top = element_line(color="black", size = 1.25), axis.line.y.left = element_line(color="black", size = 0.5), axis.line.y.right = element_line(color="black", size = 1.25)) + theme(axis.ticks.length = unit(-0.25, "cm")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+theme(panel.border = element_rect( fill = NA, size = 1.25)) +
  theme(plot.margin = margin(1,1,1,1, "cm")) + theme(axis.text=element_text(size=14)) +
  labs(x = "Average Consumption (kWh): Simulation", y = "Density")  

#Bar plots 

r <- ggplot () + geom_bar(data=eval_CA_2, aes(x = reorder(county.x,-averagekwh), y = averagekwh), stat = 'identity',width = 0.75, size = 0.5, fill = "grey", color ="grey")  + ylim(0,16000)
r + theme(text = element_text(size=8)) + theme(axis.line.x.bottom = element_line(color="black", size = 0.5), axis.line.x.top = element_line(color="black", size = 1.25), axis.line.y.left = element_line(color="black", size = 0.5), axis.line.y.right = element_line(color="black", size = 1.25)) + theme(axis.ticks.length = unit(-0.25, "cm")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+theme(panel.border = element_rect( fill = NA, size = 1.25)) +
  theme(plot.margin = margin(1,1,1,1, "cm")) + theme(axis.text=element_text(size=5)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "County", y = "Total Consumption (kWh): Utilities")  +  scale_x_discrete(breaks=unique(eval_CA_2$county.x))

r <- ggplot () + geom_bar(data=eval_CA_2, aes(x = reorder(county.x,-averagekwh.sim), y = averagekwh.sim), stat = 'identity',width = 0.75, size = 0.5, fill = "#ff6961", color ="#ff6961")  + ylim(0,16000)
r + theme(text = element_text(size=8)) + theme(axis.line.x.bottom = element_line(color="black", size = 0.5), axis.line.x.top = element_line(color="black", size = 1.25), axis.line.y.left = element_line(color="black", size = 0.5), axis.line.y.right = element_line(color="black", size = 1.25)) + theme(axis.ticks.length = unit(-0.25, "cm")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+theme(panel.border = element_rect( fill = NA, size = 1.25)) +
  theme(plot.margin = margin(1,1,1,1, "cm")) + theme(axis.text=element_text(size=5)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "County", y = "Total Consumption (kWh): Utilities")  +  scale_x_discrete(breaks=unique(eval_CA_2$county.x))

#County maps for CA

states <- map_data("state")

#select only the Washington information from the states dataset
CA_state <- subset(states, region == "california")

#name the built-in county dataset in mapdata to 'counties'
counties <- map_data("county")

#select only the california information from the states dataset
CA_county <- subset(counties, region == "california")

CA_base <- ggplot(data = CA_state, mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) + geom_polygon(color = "black", fill = "gray")
CA_base

ditch_the_axis <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

CA_base + ditch_the_axis +
  geom_polygon(data = CA_county, fill = NA, color = "white") +
  geom_polygon(color = "black", fill = NA)
CA_county$county <- CA_county$subregion

eval_CA_map_sim_t <- select(eval_CA,c("county","rank.sim_t"))
eval_CA_map_util_t <- select(eval_CA,c("county","rank.util_t"))

rank_map <- stringdist_left_join(CA_county, eval_CA_map_sim_t , by = "county", method='qgram', q=2)
rank_map2 <- stringdist_left_join(CA_county, eval_CA_map_util_t , by = "county",method='qgram', q=2)

gg1 <- CA_base +
  geom_polygon(data = rank_map2, aes(fill = rank.util_t), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() +
  ditch_the_axis +
  ggtitle("Total electricity consumtion rank")
gg2 <- gg1 + scale_fill_viridis_c(option = "D") 
gg2
