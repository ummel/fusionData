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

# Load fused RECS sim data for electricity expenditure
# ngboost <- read.csv("production/fusion/sim_NGBoost_elec_only.csv")

# Load required hhunits county map data for NY,MA,CA,PA,MD
pop_county <- fst::read_fst("production/validation/pop_county.fst")


# Load required utilities external data for NY,MA,CA,PA,MD

util_comb <- fst::read_fst("production/validation/util_comb_val.fst")

#_________________________#

# Compare mean fused utility expenditure, by PUMA, with the external utilities data 
#For MA, aggregating fused electricity consumption at the county level

acs_NY <- acs[acs$state == "36", c("acs_2015_hid","puma10","state","weight")]
acs_NY <- merge(acs_NY, pums_county, by =c("puma10","state"))
pop_county_NY <- pop_county[pop_county$state == "36", c("county","hhunits")]
util_comb_NY <- util_comb[util_comb$state == "36", c("county","totalkwh","averagekwh","hhunits")]
util_comb_NY$totalkwh <- util_comb_NY$totalkwh
util_comb_NY <- filter(util_comb_NY, (county!= "NA"))

trimws(util_comb_NY)


comp_NY <- recs.sim %>%
  left_join(acs_NY, by = "acs_2015_hid")  %>%
  group_by(state, county) %>%
  summarize(county_weight = sum(weight),  # county total household weight
            totalkwh.sim = sum(btuel*0.293*weight),
            averagekwh.sim = (totalkwh.sim/county_weight),
            .groups = "drop") 

trimws(comp_NY) 
comp_NY <- filter(comp_NY, (state != "NA"))


#Count number of shared counties per PUMA
comp_NY$count <- str_count(comp_NY$county,'&')

comp_NY_sub0 <- filter(comp_NY, (count == "0"))
comp_NY_sub0 <- select(comp_NY_sub0, c('county','county_weight','totalkwh.sim'))

#Divide the total kwh.sim and county_weights  by hhunits among the shared counties
comp_NY_sub <- comp_NY[grep("&", comp_NY$county), ]
comp_NY_sub <- select(comp_NY_sub, c('county','county_weight','totalkwh.sim'))

#Split each combined entry to separate dataframe
n <- length(unique(comp_NY_sub$county))
eval(parse(text = paste0("comp_NY_", seq(1:n), " <- ", split(comp_NY_sub, comp_NY_sub$county))))
eval(parse(text = paste0("comp_NY_N", seq(1:n), " <-  as.data.frame(comp_NY_", seq(1:n), ")"))) 

for(i in 1:n) {
  get(paste("comp_NY_N",i,sep="")) -> comp
     temp <- separate_rows(comp,county, sep = "&")
     temp1 <- temp %>% 
       stringdist_left_join(pop_county_NY, by=c('county'))
     temp1 <- select(temp1, c('county.x','county_weight','totalkwh.sim','hhunits'))
     
     temp1 <- temp1 %>% 
       dplyr::mutate(frac=round(hhunits/sum(hhunits),4))
     
     temp1$county_weight <- with(temp1, county_weight * frac)               
     temp1$totalkwh.sim <- with(temp1, totalkwh.sim * frac)               
     temp1 <- select(temp1, c('county.x','county_weight','totalkwh.sim'))
     colnames(temp1) <- c('county','county_weight','totalkwh.sim')
    Newname <- paste("comp_NY_small",i,sep="")
    assign(Newname,temp1)
}

#Merge the newly created county data into one dataframe 
NY_new_list <- list()
for(i in 1:n){
  df.now <- get(paste0("comp_NY_small",i,sep=""))
  NY_new_list[[i]] <- df.now
}

NY_list <- do.call(rbind,NY_new_list) 

comp_NY_new <- rbind(NY_list,comp_NY_sub0)


comp_NY <- comp_NY_new  %>%
  
  group_by(county) %>%
  summarize(county_weight = sum(county_weight),  # county total household weight
            totalkwh.sim = sum(totalkwh.sim),
            averagekwh.sim = (totalkwh.sim/county_weight),
            .groups = "drop") 
trimws(comp_NY) 
comp_NY$state <- '36' 
comp_NY$county <- tolower(comp_NY$county)



eval_NY <- stringdist_left_join(comp_NY, util_comb_NY, by =c("county"),distance_col = NULL)
eval_NY  <- select(eval_NY,c("state","county.x","totalkwh.sim","averagekwh.sim","averagekwh", "totalkwh","hhunits"))
colnames(eval_NY)[colnames(eval_NY) == 'county.x'] <- 'county'

#-------------------------------------------------------------------------------------------------------------------------------
#Average grouping to remove duplicates 

eval_NY$averagekwh.sim <- round(eval_NY$averagekwh.sim, digits = 2)

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


#-------------------------------------------------------------------------------------------------------------------------------
#Plot NY data 

#Scatter Plot
#Trends in  NY data 

#NY scatter Plot for total and average kwh values. 

r <- ggplot(eval_NY, aes(x = totalkwh, y = totalkwh.sim)) + geom_point(size=6, color = "#2cb22c", alpha = 0.8)  + xlim(0,6E+09) + ylim(0,6E+09)
r + theme(text = element_text(size=14)) + theme(axis.line.x.bottom = element_line(color="black", size = 0.5), axis.line.x.top = element_line(color="black", size = 1), axis.line.y.left = element_line(color="black", size = 0.5), axis.line.y.right = element_line(color="black", size = 1)) + theme(axis.ticks.length = unit(-0.25, "cm")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+theme(panel.border = element_rect(color = "black",fill = NA, size =1.5)) +
  geom_abline(slope = 1, intercept = 0, color = 'grey', linetype = "dashed") + theme(plot.margin = margin(1,1,1,1, "cm")) + theme(axis.text=element_text(size=14)) +
  labs(x = "Total Consumption (kWh): Utilities", y = "Total Consumption (kWh): Simulated") +
  geom_smooth(method = "lm", colour= "#2cb22c", se = FALSE)

#Scatter Plot for hhunits 

r <- ggplot(eval_NY, aes(x = totalkwh, y = totalkwh.sim, color = hhunits)) + geom_point(size=5)  + xlim(0,6E+09) + ylim(0,6E+09)
r + theme(text = element_text(size=12)) + theme(axis.line.x.bottom = element_line(color="black", size = 0.5), axis.line.x.top = element_line(color="black", size = 1), axis.line.y.left = element_line(color="black", size = 0.5), axis.line.y.right = element_line(color="black", size = 1)) + theme(axis.ticks.length = unit(-0.25, "cm")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+theme(panel.border = element_rect(color = "black",fill = NA, size =1.5)) +
  geom_abline(slope = 1, intercept = 0, color = 'grey', linetype = "dashed") + theme(plot.margin = margin(1,1,1,1, "cm")) + theme(axis.text=element_text(size=12)) +
  labs(x = "Total Consumption (kWh): Utilities", y = "Total Consumption (kWh): Simulated") +
  geom_smooth(method = "lm", colour="#ff6961", alpha = 0.8, se = FALSE) 

r <- ggplot(eval_NY, aes(x = hhunits, y = totalkwh.sim)) + geom_point(size=5, color = "#2cb22c",alpha = 0.8) + ylim(0,6E+09)
r + theme(text = element_text(size=12)) + theme(axis.line.x.bottom = element_line(color="black", size = 0.5), axis.line.x.top = element_line(color="black", size = 1), axis.line.y.left = element_line(color="black", size = 0.5), axis.line.y.right = element_line(color="black", size = 1)) + theme(axis.ticks.length = unit(-0.25, "cm")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+theme(panel.border = element_rect(color = "black",fill = NA, size =1.5)) +
  theme(plot.margin = margin(1,1,1,1, "cm")) + theme(axis.text=element_text(size=12)) +
  labs(x = "Household Units (hhunits) ", y = "Total Consumption (kWh): Simulated") +
  geom_smooth(method = "lm", colour="#2cb22c", se = FALSE) 

r <- ggplot(eval_NY, aes(x = hhunits, y = totalkwh)) + geom_point(size=5, color = "grey",alpha = 0.8) + ylim(0,6E+09)
r + theme(text = element_text(size=12)) + theme(axis.line.x.bottom = element_line(color="black", size = 0.5), axis.line.x.top = element_line(color="black", size = 1), axis.line.y.left = element_line(color="black", size = 0.5), axis.line.y.right = element_line(color="black", size = 1)) + theme(axis.ticks.length = unit(-0.25, "cm")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+theme(panel.border = element_rect(color = "black",fill = NA, size =1.5)) +
  theme(plot.margin = margin(1,1,1,1, "cm")) + theme(axis.text=element_text(size=12)) +
  labs(x = "Household Units (hhunits) ", y = "Total Consumption (kWh): Utilities") +
  geom_smooth(method = "lm", colour="grey",  se = FALSE) 

#Rank plots for county kwh consumption 

rank.sim_t <- rank(-eval_NY$totalkwh.sim)
rank.util_t <- rank(-eval_NY$totalkwh)
eval_NY <- cbind(eval_NY, rank.sim_t,rank.util_t)

r <- ggplot(eval_NY, aes(x = rank.util_t, y = rank.sim_t)) + geom_point(size=5, color = "#2cb22c", alpha = 0.8) + xlim(0,80)+ ylim(0,80)
r + theme(text = element_text(size=14)) + theme(axis.line.x.bottom = element_line(color="black", size = 0.5), axis.line.x.top = element_line(color="black", size = 1), axis.line.y.left = element_line(color="black", size = 0.5), axis.line.y.right = element_line(color="black", size = 1)) + theme(axis.ticks.length = unit(-0.25, "cm")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+theme(panel.border = element_rect(color = "black",fill = NA, size =1.5)) +
  geom_abline(slope = 1, intercept = 0, color = 'grey', linetype = "dashed") + theme(plot.margin = margin(1,1,1,1, "cm")) + theme(axis.text=element_text(size=14)) +
  labs(x = "Total Consumption Rank : Utilities", y = "Total Consumption Rank: Simulated")  +
  geom_smooth(method = "lm", colour="#2cb22c", se = FALSE)


# Distribution for total kwh county wise 

r <- ggplot () + geom_density(data=eval_NY, aes(x =totalkwh), size = 1) + xlim(0,6e+09) + ylim(0,1.4e-09)
r + theme(text = element_text(size=14)) + theme(axis.line.x.bottom = element_line(color="black", size = 0.5), axis.line.x.top = element_line(color="black", size = 1.25), axis.line.y.left = element_line(color="black", size = 0.5), axis.line.y.right = element_line(color="black", size = 1.25)) + theme(axis.ticks.length = unit(-0.25, "cm")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+theme(panel.border = element_rect( fill = NA, size = 1.25)) +
  theme(plot.margin = margin(1,1,1,1, "cm")) + theme(axis.text=element_text(size=14)) +
  labs(x = "Total Consumption (kWh): Utilities", y = "Density")  


r <- ggplot () + geom_density(data=eval_NY, aes(x = totalkwh.sim), color = "#2cb22c",size = 1) + xlim(0,6e+09) + ylim(0,1.4e-09)
r + theme(text = element_text(size=14)) + theme(axis.line.x.bottom = element_line(color="black", size = 0.5), axis.line.x.top = element_line(color="black", size = 1.5), axis.line.y.left = element_line(color="black", size = 0.5), axis.line.y.right = element_line(color="black", size = 1.5)) + theme(axis.ticks.length = unit(-0.25, "cm")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+theme(panel.border = element_rect( fill = NA, size = 1.5)) +
  theme(plot.margin = margin(1,1,1,1, "cm")) + theme(axis.text=element_text(size=14)) +
  labs(x = "Total Consumption (kWh): Simulated", y = "Density")  


# Distribution for total kwh county wise and overal density plot  
r <- ggplot () + geom_bar(data=eval_NY, aes(x = reorder(county,-totalkwh, FUN = sum), y = totalkwh), stat = 'identity',width = 0.75, size = 0.5, fill = "grey", color ="grey")  + ylim(0,6e+09)
r + theme(text = element_text(size=9)) + theme(axis.line.x.bottom = element_line(color="black", size = 0.5), axis.line.x.top = element_line(color="black", size = 1.25), axis.line.y.left = element_line(color="black", size = 0.5), axis.line.y.right = element_line(color="black", size = 1.25)) + theme(axis.ticks.length = unit(-0.25, "cm")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+theme(panel.border = element_rect( fill = NA, size = 1.25)) +
  theme(plot.margin = margin(1,1,1,1, "cm")) + theme(axis.text=element_text(size=9)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "County", y = "Total Consumption (kWh): Utilities")  


r <- ggplot () + geom_bar(data=eval_NY, aes(x = reorder(county,-totalkwh.sim, FUN = sum), y = totalkwh.sim), stat = 'identity',width = 0.75, size = 0.5, fill = "#2cb22c",color ="#2cb22c")  + ylim(0,6e+09)
r + theme(text = element_text(size=9)) + theme(axis.line.x.bottom = element_line(color="black", size = 0.5), axis.line.x.top = element_line(color="black", size = 1.25), axis.line.y.left = element_line(color="black", size = 0.5), axis.line.y.right = element_line(color="black", size = 1.25)) + theme(axis.ticks.length = unit(-0.25, "cm")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+theme(panel.border = element_rect( fill = NA, size = 1.25)) +
  theme(plot.margin = margin(1,1,1,1, "cm")) + theme(axis.text=element_text(size=9)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "County", y = "Total Consumption (kWh): Simulated")  


#-------------------------------------------------------------------------------------------------------------------------------

#Scatter plot for average values 

#NY_avg_imp <- read_fst("production/validation/averagekwh_sim_avg_NY.fst")
#eval_NY_2 <- stringdist_left_join(eval_NY_2,NY_avg_imp, by =c("county"), method='qgram', q=2,distance_col = NULL)

r <- ggplot(eval_NY_2, aes(x = averagekwh, y = averagekwh.sim)) + geom_point(size=6, color = "#2cb22c",alpha = 0.7) +xlim(4000,14000) +ylim(4000,14000)
r + theme(text = element_text(size=14)) + theme(axis.line.x.bottom = element_line(color="black", size = 0.5), axis.line.x.top = element_line(color="black", size = 1.5), axis.line.y.left = element_line(color="black", size = 0.5), axis.line.y.right = element_line(color="black", size = 1.5)) + theme(axis.ticks.length = unit(-0.25, "cm")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+theme(panel.border = element_rect( fill = NA, size = 1)) + 
  geom_abline(slope = 1, intercept = 0, color = 'grey', linetype = "dashed")  + theme(plot.margin = margin(1,1,1,1, "cm")) + theme(axis.text=element_text(size=14)) +
  #geom_errorbar(aes(ymin = averagekwh.sim - averagekwh.sim_std , ymax = averagekwh.sim + averagekwh.sim_std),colour = "#2cb22c", size=1) +
  geom_smooth(method = "lm", colour="#2cb22c", se = FALSE, size = 1)  +
  labs(x = "Average Consumption (kWh) : Utilities", y = "Average Consumption (kWh) : Simulated")  

# Bar Plots

r <- ggplot () + geom_bar(data=eval_NY_2, aes(x = reorder(county.x,-averagekwh, FUN = sum), y = averagekwh), stat = 'identity',width = 0.75, size = 0.5, fill = "grey", color ="grey")  +ylim(0,14000)
r + theme(text = element_text(size=8)) + theme(axis.line.x.bottom = element_line(color="black", size = 0.5), axis.line.x.top = element_line(color="black", size = 1.25), axis.line.y.left = element_line(color="black", size = 0.5), axis.line.y.right = element_line(color="black", size = 1.25)) + theme(axis.ticks.length = unit(-0.25, "cm")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+theme(panel.border = element_rect( fill = NA, size = 1.25)) +
  theme(plot.margin = margin(1,1,1,1, "cm")) + theme(axis.text=element_text(size=8)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "County", y = "Average Consumption (kWh): Utilities") 


r <- ggplot () + geom_bar(data=eval_NY_2, aes(x = reorder(county.x,-averagekwh.sim, FUN = sum), y = averagekwh.sim), stat = 'identity',width = 0.75, size = 0.5, fill = "#2cb22c", color ="#2cb22c")   +ylim(0,14000)
r + theme(text = element_text(size=8)) + theme(axis.line.x.bottom = element_line(color="black", size = 0.5), axis.line.x.top = element_line(color="black", size = 1.25), axis.line.y.left = element_line(color="black", size = 0.5), axis.line.y.right = element_line(color="black", size = 1.25)) + theme(axis.ticks.length = unit(-0.25, "cm")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+theme(panel.border = element_rect( fill = NA, size = 1.25)) +
  theme(plot.margin = margin(1,1,1,1, "cm")) + theme(axis.text=element_text(size=8)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "County", y = "Average Consumption (kWh): Simulated") 


# Rank plot 
NY_rank_avg_imp  <- read_fst("production/validation/rank_sim_a_avg_NY.fst")
eval_NY_2 <- stringdist_left_join(eval_NY_2, NY_rank_avg_imp, by =c("county"), method='qgram', q=2,distance_col = NULL)

rank.sim_a <- rank(-eval_NY_2$averagekwh.sim)
rank.util_a <- rank(-eval_NY_2$averagekwh)
eval_NY_2 <- cbind (eval_NY_2, rank.sim_a,rank.util_a)

r <- ggplot(eval_NY_2, aes(x = rank.util_a, y = rank.sim_a)) + geom_point(size=5, color = "#2cb22c", alpha = 0.8) + xlim(0,60)+ ylim(0,60)
r + theme(text = element_text(size=14)) + theme(axis.line.x.bottom = element_line(color="black", size = 0.5), axis.line.x.top = element_line(color="black", size = 1), axis.line.y.left = element_line(color="black", size = 0.5), axis.line.y.right = element_line(color="black", size = 1)) + theme(axis.ticks.length = unit(-0.25, "cm")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+theme(panel.border = element_rect(color = "black",fill = NA, size =1.5)) +
  geom_abline(slope = 1, intercept = 0, color = 'grey', linetype = "dashed") + theme(plot.margin = margin(1,1,1,1, "cm")) + theme(axis.text=element_text(size=14)) +
  labs(x = "Average Consumption Rank : Utilities", y = "Average Consumption Rank: Simulated")  +
  geom_smooth(method = "lm", colour="#2cb22c", se = FALSE) +
  geom_errorbar(aes(ymin = rank.sim_a - rank.sim_a_std, ymax = rank.sim_a + rank.sim_a_std),colour = "#2cb22c", width =1.5) 


# Distribution for average kwh county wise and overal density plot  
options(scipen  = 0)
r <- ggplot () + geom_density(data=eval_NY_2, aes(x = averagekwh), size = 1)  + xlim(0,14000) + ylim(0,6E-4)
r + theme(text = element_text(size=14)) + theme(axis.line.x.bottom = element_line(color="black", size = 0.5), axis.line.x.top = element_line(color="black", size = 1.25), axis.line.y.left = element_line(color="black", size = 0.5), axis.line.y.right = element_line(color="black", size = 1.25)) + theme(axis.ticks.length = unit(-0.25, "cm")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+theme(panel.border = element_rect( fill = NA, size = 1.25)) +
  theme(plot.margin = margin(1,1,1,1, "cm")) + theme(axis.text=element_text(size=14)) +
  labs(x = "Average Consumption (kWh): Utilities", y = "Density")  


r <- ggplot () + geom_density(data=eval_NY_2, aes(x = averagekwh.sim), color = "#2cb22c",size = 1) + xlim(0,14000) + ylim(0,6E-4)
r + theme(text = element_text(size=14)) + theme(axis.line.x.bottom = element_line(color="black", size = 0.5), axis.line.x.top = element_line(color="black", size = 1.25), axis.line.y.left = element_line(color="black", size = 0.5), axis.line.y.right = element_line(color="black", size = 1.25)) + theme(axis.ticks.length = unit(-0.25, "cm")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+theme(panel.border = element_rect( fill = NA, size = 1.25)) +
  theme(plot.margin = margin(1,1,1,1, "cm")) + theme(axis.text=element_text(size=14)) +
  labs(x = "Average Consumption (kWh): Simulation", y = "Density")  


#County Map for NY
states <- map_data("state")

#select only the Washington information from the states dataset
NY_state <- subset(states, region == "new york")

#name the built-in county dataset in mapdata to 'counties'
counties <- map_data("county")

#select only the california information from the states dataset
NY_county <- subset(counties, region == "new york")

NY_base <- ggplot(data = NY_state, mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) + geom_polygon(color = "black", fill = "gray")

NY_base

ditch_the_axis <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

NY_base + ditch_the_axis +
  geom_polygon(data = NY_county, fill = NA, color = "white") +
  geom_polygon(color = "black", fill = NA)
NY_county$county <- NY_county$subregion

eval_NY_map_sim_t <- select(eval_NY,c("county","rank.sim_t"))
eval_NY_map_util_t <- select(eval_NY,c("county","rank.util_t"))


rank_map <- stringdist_left_join(NY_county, eval_NY_map_sim_t , by = "county")
rank_map2 <- stringdist_left_join(NY_county, eval_NY_map_util_t , by = "county")


gg1 <- NY_base +
  geom_polygon(data = rank_map, aes(fill = rank.sim_t), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() +
  ditch_the_axis +
  ggtitle("Total electricity consumtion rank")
gg2 <- gg1 + scale_fill_viridis_c(option = "D") 

gg2
