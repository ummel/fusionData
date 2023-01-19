library(fusionModel)
library(fusionData)
library(tigris)
library(mapview)
library(tmap)
library(sf)
setwd("/Users/karthikakkiraju/Documents/fusionData")

#Number of housing units (puma_weight) in each PUMA-county intersection
geo <- fst::read_fst("geo-processed/concordance/geo_concordance.fst", columns = c("state", "puma10", "ur12")) %>% distinct()

geo1 <- fst::read_fst("geo-processed/concordance/geo_concordance.fst")
unique(geo1[c('puma10',"cbsatype13")])

#Load 2019 ACS data
# Person observations. Retain race of reference person, derived from original variables 'rac1p' and 'hisp'
acs.p_2019 <- read_fst("survey-processed/ACS/2019/ACS_2019_P_processed.fst", columns = c("acs_2019_hid", "sporder", "rac1p", "hisp")) %>%
  filter(sporder == 1) %>%
  mutate(race = ifelse(rac1p %in% c("White alone", "Black or African American alone"), as.character(rac1p), "Other"),
         race = ifelse(hisp == "Not Spanish / Hispanic / Latino", race, "Latino"),
         race = factor(race)) %>%
        select(acs_2019_hid, race)

# Household observations, including weights, with 'race' merged from person file
acs_2019 <- read_fst("survey-processed/ACS/2019/ACS_2019_H_processed.fst") %>%
  select(acs_2019_hid, state, weight, starts_with("rep_"),hincp,puma10,elep,gasp,fulp) %>%
  left_join(acs.p_2019, by = "acs_2019_hid")

rm(acs.p_2019)

#Load 2015 ACS data
# Person observations. Retain race of reference person, derived from original variables 'rac1p' and 'hisp'
acs.p_2015 <- read_fst("survey-processed/ACS/2015/ACS_2015_P_processed.fst", columns = c("acs_2015_hid", "sporder", "rac1p", "hisp")) %>%
  filter(sporder == 1) %>%
  mutate(race = ifelse(rac1p %in% c("White alone", "Black or African American alone"), as.character(rac1p), "Other"),
         race = ifelse(hisp == "Not Spanish / Hispanic / Latino", race, "Latino"),
         race = factor(race)) %>%
  select(acs_2015_hid, race)

# Household observations, including weights, with 'race' merged from person file
acs_2015 <- read_fst("survey-processed/ACS/2015/ACS_2015_H_processed.fst") %>%
  select(acs_2015_hid, state, weight, starts_with("rep_"),hincp,puma10,elep,gasp,fulp) %>%
  left_join(acs.p_2015, by = "acs_2015_hid")

rm(acs.p_2015)


#Load 2019 AHS data
ahs <- fst::read_fst(path = "survey-processed/AHS/2019/AHS_2019_H_processed.fst") %>% 
        select(c('ahs_2019_hid','cold','weight','division'))

#Load fused AHS data 
ahs_sim <- fst::read_fst(path = "fusion/AHS/2019/2019/output/AHS_2019_2019_fused_merged.fst") %>% 
          select(.,-c('acs_2019_hid'))

# Load 2015 RECS data 
recs <- read_fst(path = "survey-processed/RECS/2015/RECS_2015_H_processed.fst")

# Load fused RECS  data 
recs_sim <- fst::read_fst(path = "fusion/RECS/2015/2015/output/RECS_2015_2015_fused_merged.fst")  %>% 
            select(.,-c('acs_2015_hid'))

#Load 2017 NHTS data
nhts <- fst::read_fst(path = "survey-processed/NHTS/2017/NHTS_2017_H_processed.fst") %>% 
  select(c('nhts_2017_hid','vmt_mile','weight','division','bestmile','gstotcst'))


#Load fused NHTS data 
nhts_sim <- fst::read_fst(path = "fusion/NHTS/2017/2015/output/NHTS_2017_2015_fused_merged.fst") %>% 
            select(.,-c('acs_2015_hid'))

recs_sim1 <- split(recs_sim , f = recs_sim$M)



# To derive estimates at county-level, we randomly assign counties to ACS households based on known PUMA
# The script below generates the random county assignments across 100 implicates
#Make sure to update the files with the correct 'M' value 
source("production/examples/RECS-ACS 2015/Random assignment of urban-rural to ACS 2015 households.R")

# Modify 'sim' by adding random county assignment from 'county.rnd' for each implicate
# This effectively treats county assignment as a simulated variable that varies across implicates
# The advantage is that uncertainty in county assignment is implicitly captured by analyze()
recs_sim <- lapply(1:length(recs_sim1), function(i) mutate(recs_sim1[[i]], ur12 = ur12.rnd[[paste0("V", i)]]))
recs_sim2 <- data.table::rbindlist(recs_sim) 


#Analysis for CONUS maps

#Cold metric at the division level using AHS
ahs_cold <- ahs %>%
  group_by(division,cold) %>%
  summarize(type = sum(weight)) %>%
  summarize(cold = cold,
            type = type,
            total = sum(type),
            prop_cold = 100*(type/total)) %>%
  filter(cold == 'Yes') %>% select(c('cold','prop_cold'))

#Cold metric averaged at the census level using 2019 AHS data
us_div <- divisions(year = 2019) %>% rename(division = NAME) %>%
  left_join(ahs_cold, by = c('division'))  

tm_shape(us_div) + 
  tm_fill(style = "kmeans",col = "prop_cold", palette =  "-RdYlBu",
          alpha = 0.8,legend.show = TRUE,
          legend.format=list(fun=function(x) paste0(formatC(x, digits=2, format="f")))) + 
  tm_borders(lwd = 0) 


#Cold metric averaged at the PUMA level using fused data
ex1 <- analyze(x = list(mean = "cold"),
               implicates = ahs_sim,
               weight = "weight",
               static = acs_2019,
               by = c("state", "puma10"))

cold <- ex1  %>% filter(level == "Yes") %>% 
  select(c('state','puma10','est')) %>% 
  rename(cold = est) %>% mutate(cold = 100*cold)

# Code for making CONUS maps for fused cold metric
tmap_mode("view")
us_pumas <- pumas(cb = TRUE, year = 2019) %>% rename(puma10 = PUMACE10,
                                                     state = STATEFP10) %>%
  left_join(cold, by = c('state','puma10'))  

tm_shape(us_pumas) + 
  tm_fill(style = "kmeans",col = "cold", palette =  "-RdYlBu",
          alpha = 0.8,legend.show = FALSE,
          legend.format=list(fun=function(x) paste0(formatC(x, digits=2, format="f")))) + 
  tm_borders(lwd = 0.0) 

#Create histogram
us_pumas1 <- us_pumas %>% filter(!is.na(cold))
bins <- kmeans(us_pumas1$cold, 10, iter.max = 10, nstart = 1,
               algorithm = c("Hartigan-Wong", "Lloyd", "Forgy",
                             "MacQueen"), trace=FALSE)
cent <- bins$centers

MyColours <- RColorBrewer::brewer.pal(5, "RdBu")
bin <- c(0.005,0.016,0.023,0.032,0.045,0.078)

us_pumas1 <- us_pumas1 %>% mutate(filled = ifelse((cold > 0.005 & cold < 0.016) | cold == 0.016,MyColours[1],
                                                  ifelse((cold > 0.016 & cold < 0.023) | cold == 0.023,MyColours[2],
                                                         ifelse((cold > 0.023 & cold < 0.032)| cold == 0.032,MyColours[3],
                                                                ifelse((cold > 0.032 & cold < 0.045)| cold == 0.045,MyColours[4],
                                                                       ifelse((cold > 0.045 & cold < 0.078),MyColours[5],NA))))))

r <-ggplot(us_pumas1, aes(x=cold, fill = filled)) + 
  geom_histogram(closed= "left", binwidth = 0.001, bins = 5, boundary = 0) +
  geom_rug(aes(cold, y = NULL, color = filled)) + geom_vline(xintercept = bin, colour="black", size = 0.075) +
  scale_fill_manual(values = MyColours , drop = FALSE)  + scale_color_manual(values = MyColours , drop = FALSE) 

ya <- r + theme(text = element_text(size=10)) +
  theme(axis.line.x.bottom = element_line(color="black", size = 0), axis.line.x.top = element_line(color="black", size = 0), axis.line.y.left = element_line(color="black", size = 0), axis.line.y.right = element_line(color="black", size = 0))+
  theme(axis.ticks.length = unit(0, "cm")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+theme(panel.border = element_rect( fill = NA, size = 0)) +
  theme(plot.margin = margin(1,1,1,1, "cm")) + theme(axis.text.x=element_blank()) +labs(y = "PUMAS") +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank()) +  
  theme(axis.text.y = element_text(margin = margin(r = -10))) + theme(legend.position="none") + 
  scale_x_continuous(breaks = bin, labels = bin) +
  theme(axis.text.x = element_text(color = c("black")),axis.ticks.x = element_line(color = c("black")))

ggsave(filename="/Users/karthikakkiraju/Documents/fusionData/production/examples/Slides/AHS.jpg", plot = ya, width = 4.5, height = 3, units="in")


#Cold metric averaged at the PUMA level for race using fused data
ex2 <- analyze(x = list(mean = "cold"),
               implicates = ahs_sim,
               weight = "weight",
               static = acs_2019,
               by = c("state", "puma10",'race'))

cold_white <- ex2  %>% filter(level == "Yes") %>% filter(race == "White alone") %>% 
  select(c('state','puma10','est')) %>% 
  rename(cold_w = est) 

cold_black <- ex2  %>% filter(level == "Yes") %>% filter(race == "Black or African American alone") %>% 
  select(c('state','puma10','est')) %>% 
  rename(cold_b = est) 

cold_latino <- ex2  %>% filter(level == "Yes") %>% filter(race == "Latino") %>% 
  select(c('state','puma10','est')) %>% 
  rename(cold_l = est) 

# Code for making CONUS maps for fused cold metric for different races
tmap_mode("view")
us_pumas <- pumas(cb = TRUE, year = 2019) %>% rename(puma10 = PUMACE10,
                                                     state = STATEFP10) %>%
          left_join(cold_white, by = c('state','puma10'))  %>%
          left_join(cold_latino, by = c('state','puma10'))  %>%
          left_join(cold_black, by = c('state','puma10'))  

tm_shape(us_pumas) + 
  tm_fill(style = "kmeans",col = "est", palette =  "RdYlBu",
          alpha = 0.8,legend.show = TRUE,
          legend.format=list(fun=function(x) paste0(formatC(x, digits=2, format="f")))) + 
  tm_borders(lwd = 0) 

tm_shape(us_pumas) + 
  tm_fill(style = "kmeans",col = "est", palette =  "RdYlBu",
          alpha = 0.8,legend.show = TRUE,
          legend.format=list(fun=function(x) paste0(formatC(x, digits=2, format="f")))) + 
  tm_borders(lwd = 0) 

tm_shape(us_pumas) + 
  tm_fill(style = "kmeans",col = "est", palette =  "RdYlBu",
          alpha = 0.8,legend.show = TRUE,
          legend.format=list(fun=function(x) paste0(formatC(x, digits=2, format="f")))) + 
  tm_borders(lwd = 0) 



#btuel consumption metric using fused RECS 
ex3 <- analyze(x = list(mean = "btuel"),
               implicates = recs_sim2,
               weight = "weight",
               static = acs_2015,
               by = c("state", "puma10"))

recs_puma <- ex3   %>% 
                select(c('state','puma10','est')) %>% 
               rename(btuel = est) 

#Make CONUS-PUM map for btuel metric
recs_puma2 <-pumas(cb = TRUE,year = 2019) %>% rename(state = STATEFP10,
                                                     puma10 = PUMACE10)

puma2 <- recs_puma2 %>% left_join(recs_puma , by = c('state','puma10')) 

tm_shape(puma2) + 
  tm_fill(style = "kmeans",col = "btuel", palette =  "-RdYlBu",
          alpha = 0.8,legend.show = TRUE,
          legend.format=list(fun=function(x) paste0(formatC(x, digits=0, format="f")))) + 
  tm_borders(lwd = 0) 

#Create histogram
us_pumas1 <- puma2 %>% filter(!is.na(btuel))

MyColours <- RColorBrewer::brewer.pal(5, "RdBu")
bin <- c(10517,22971,30291,38199,46249,61693)

us_pumas1 <- us_pumas1 %>% mutate(filled = ifelse((btuel > 10517 & btuel < 22971) | btuel == 22971,MyColours[1],
                                                  ifelse((btuel > 22971 & btuel < 30291) | btuel == 30291,MyColours[2],
                                                         ifelse((btuel > 30291 & btuel < 38199)| btuel == 38199,MyColours[3],
                                                                ifelse((btuel > 38199 & btuel < 46249)| btuel== 46249,MyColours[4],
                                                                       ifelse((btuel > 46249 & btuel< 61693),MyColours[5],NA))))))

r <- ggplot(us_pumas1, aes(x = btuel, fill = filled)) + 
  geom_histogram(closed = "left", bins = 150) +
  geom_rug(aes(btuel, y = NULL, color = filled)) + geom_vline(xintercept = bin, colour="black", size = 0.075) +
  scale_fill_manual(values = MyColours , drop = FALSE)  + scale_color_manual(values = MyColours , drop = FALSE) 

ya <- r + theme(text = element_text(size=10)) +
  theme(axis.line.x.bottom = element_line(color="black", size = 0), axis.line.x.top = element_line(color="black", size = 0), axis.line.y.left = element_line(color="black", size = 0), axis.line.y.right = element_line(color="black", size = 0))+
  theme(axis.ticks.length = unit(0, "cm")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+theme(panel.border = element_rect( fill = NA, size = 0)) +
  theme(plot.margin = margin(1,1,1,1, "cm")) + theme(axis.text.x=element_blank()) +labs(y = "PUMAS") +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank()) +  
  theme(axis.text.y = element_text(margin = margin(r = -10))) + theme(legend.position="none") + 
  scale_x_continuous(breaks = bin, labels = bin) +
  theme(axis.text.x = element_text(color = c("black")),axis.ticks.x = element_line(color = c("black")))

ggsave(filename="/Users/karthikakkiraju/Documents/fusionData/production/examples/Slides/AHS.jpg", plot=ya, width=4.5, height= 3, units="in")

#RECS btuel greater NYC
nhts_ny_pumas <- ex3 %>% 
  filter(state %in% c('36','34','09')) 

ny_pumas <- pumas(cb = TRUE, year = 2019) %>% rename(puma10 = PUMACE10,
                                                     state = STATEFP10) %>%
  merge(nhts_ny_pumas, by = c('state','puma10'))  %>% filter(str_detect(NAME10, 'NYC|Westchester|Bergen|Hudson|Passaic|Putnam|Rockland|
                                                                          Suffolk|Nassau|Middlesex|Monmouth|Ocean|Somerset|Essex|Union|Morris|Sussex|Hunterdon'))

map1 <- tm_shape(ny_pumas) + 
  tm_fill(style = "kmeans",col = "est", palette =  "-RdYlBu",
          alpha = 0.8,legend.show = TRUE,
          legend.format=list(fun=function(x) paste0(formatC(x, digits=2, format="f")))) + 
  tm_borders(lwd = 0.2) 

map2 <- map1 + tm_view(set.view = c(-74.0060, 40.7128, 7.75))

lf <- tmap_leaflet(map2)
mapshot(lf, file = "/Users/karthikakkiraju/Documents/fusionData/production/examples/Slides/Figures/greater_nyc_btuel.pdf")



#btuel using 2015 RECS at the census division level
recs_div <- recs %>% 
  group_by(recs_division) %>% 
  summarise(btuel = weighted.mean(btuel,weight)) %>% 
  mutate(recs_division = gsub("Middle Atlantic","10",recs_division),
         recs_division = gsub("East North Central","6",recs_division),
         recs_division = gsub("West North Central","2",recs_division),
         recs_division = gsub("South Atlantic","1",recs_division),
         recs_division = gsub("East South Central","4",recs_division),
         recs_division = gsub("West South Central","5",recs_division),
         recs_division = gsub("Mountain North","8",recs_division),
         recs_division = gsub("Mountain South","9",recs_division),
         recs_division = gsub("New England","7",recs_division),
         recs_division = gsub("Pacific","3",recs_division))  %>% 
  rename(div = recs_division)  %>% mutate(div = as.numeric(div))

#Make maps for division level using 2015 RECS data
recs_div2 <- states(year = 2019, cb = TRUE) %>%
  filter(!STUSPS %in% c('HI', 'AK', 'PR', 'GU', 'VI', 'AS', 'MP')) %>% 
    mutate(div = ifelse((STUSPS == 'NJ' | STUSPS == 'NY'| STUSPS == 'PA'),10,
                 ifelse((STUSPS == 'AZ' | STUSPS == 'NV'| STUSPS == 'NM'),09,  
                        ifelse((STUSPS == 'CO' | STUSPS == 'ID'| STUSPS == 'MT'  | STUSPS == 'UT'| STUSPS == 'WY'),08,
                               ifelse((STUSPS == 'CT' | STUSPS == 'ME'| STUSPS == 'MA'  | STUSPS == 'NH'| STUSPS == 'VT' | STUSPS == 'RI'),07,
                                      ifelse((STUSPS == 'IL' | STUSPS == 'IN'| STUSPS == 'MI'  | STUSPS == 'OH'| STUSPS == 'WI'),06,
                                             ifelse((STUSPS == 'AR' | STUSPS == 'LA'| STUSPS == 'OK'  | STUSPS == 'TX'),05,
                                                    ifelse((STUSPS == 'AL' | STUSPS == 'KY'| STUSPS == 'MS'  | STUSPS == 'TN'),04,
                                                           ifelse((STUSPS== 'AK' | STUSPS == 'CA'| STUSPS == 'HW'  | STUSPS == 'OR'| STUSPS == 'WA'),03,
                                                                  ifelse((STUSPS == 'IA' | STUSPS == 'KS'| STUSPS == 'MN'  | STUSPS == 'MO'| STUSPS == 'NE'| STUSPS == 'ND'| STUSPS == 'SD'),2,
                                                                         ifelse((STUSPS == 'DE' | STUSPS == 'DC'| STUSPS == 'FL'  | STUSPS == 'GA'| STUSPS == 'MD' | STUSPS == 'NC'| STUSPS == 'SC'  | STUSPS == 'VA'| STUSPS == 'WV'),1,NA)))))))))))
#Make division level map 
div <- recs_div2 %>%
  group_by(div) %>% 
  summarize()

div2 <- div %>% left_join(recs_div , by = 'div') 

map1 <- tm_shape(div2) + 
  tm_fill(style = "kmeans",col = "btuel", palette =  "-RdYlBu",
          alpha = 0.8,legend.show = TRUE,
          legend.format=list(fun=function(x) paste0(formatC(x, digits=0, format="f")))) + 
  tm_borders(lwd = 0) 

map2 <- map1 + tm_view(set.view = c(-98.35,39.50,5))
map2



#btuel consumption metric using fused RECS for urban/rural
ex4 <- analyze(x = list(mean = "btuel"),
               implicates = recs_sim2,
               weight = "weight",
               static = acs_2015,
               by = c('state','puma10',"ur12"))

recs_puma <- ex4   %>% 
  select(c('state','puma10','est')) %>% 
  rename(btuel = est) 

#Make CONUS-PUM map for btuel metric
recs_puma2 <-pumas(cb = TRUE,year = 2019) %>% rename(state = STATEFP10,
                                                     puma10 = PUMACE10)

puma2 <- recs_puma2 %>% left_join(recs_puma , by = c('state','puma10')) 
tm_shape(puma2) + 
  tm_fill(style = "kmeans",col = "est", palette =  "RdYlBu",
          alpha = 0.6,legend.show = TRUE,
          legend.format=list(fun=function(x) paste0(formatC(x, digits=2, format="f")))) + 
  tm_borders(lwd = 0.2) 



#btuel consumption metric using fused RECS for urban/rural
ex5 <- analyze(x = list(mean = "btuel"),
               implicates = recs_sim2,
               weight = "weight",
               static = acs_2015,
               by = c('state','puma10',"ur12"))

recs_puma_u <- ex5   %>% filter(ur12 == 'U') %>%
  select(c('state','puma10','est')) %>% 
  rename(btuel = est) 

recs_puma_r <- ex5   %>% filter(ur12 == 'R') %>%
  select(c('state','puma10','est')) %>% 
  rename(btuel = est) 


r <- ggplot() + 
  geom_histogram(data = recs_puma_u, aes(x = btuel), closed = "left", bins = 30, fill = '#b35806', alpha = 0.7) +
  geom_histogram(data = recs_puma_r, aes(x = btuel), closed = "left", bins = 30, fill = '#542788', alpha = 0.7) 

ya <- r + theme(text = element_text(size=12)) +
  theme(axis.line.x.bottom = element_line(color="black", size = 0), axis.line.x.top = element_line(color="black", size = 0), axis.line.y.left = element_line(color="black", size = 0), axis.line.y.right = element_line(color="black", size = 0))+
  theme(axis.ticks.length = unit(0, "cm")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+theme(panel.border = element_rect( fill = NA, size = 0)) +
  theme(plot.margin = margin(1,1,1,1, "cm")) + theme(axis.text.x=element_blank()) +labs(y = "#PUMAS") +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank()) +  
  theme(axis.text.y = element_text(margin = margin(r = -10))) + theme(legend.position="none") + 
  theme(axis.text.x = element_text(color = c("black")),axis.ticks.x = element_line(color = c("black")))

ggsave(filename="/Users/karthikakkiraju/Documents/fusionData/production/examples/Slides/RECS_U_R.pdf", plot=ya, width=4.5, height= 3, units="in")



#Make CONUS-PUM map for btuel metric
recs_puma2 <-pumas(cb = TRUE,year = 2019) %>% rename(state = STATEFP10,
                                                     puma10 = PUMACE10)

puma2 <- recs_puma2 %>% left_join(recs_puma , by = c('state','puma10')) 
tm_shape(puma2) + 
  tm_fill(style = "kmeans",col = "est", palette =  "RdYlBu",
          alpha = 0.6,legend.show = TRUE,
          legend.format=list(fun=function(x) paste0(formatC(x, digits=2, format="f")))) + 
  tm_borders(lwd = 0.2) 


#gst averaged at the division level using 2017 NHTS
nhts_gst <- nhts %>%
  group_by(division)  %>%
  summarize(gst = weighted.mean(gstotcst,weight))
tmap_mode("view")

nhts_div <- divisions(year = 2019) %>% rename(division = NAME) %>%
  left_join(nhts_gst, by = c('division'))  

tm_shape(nhts_div) + 
  tm_fill(style = "kmeans",col = "gst", palette =  "-RdYlBu",
          alpha = 0.8,legend.show = TRUE,
          legend.format=list(fun=function(x) paste0(formatC(x, digits=0, format="f")))) + 
  tm_borders(lwd = 0) 

#vmt miles averaged at the PUMA level using fused NHTS 
ex6 <- analyze(x = list(mean = "gstotcst"),
               implicates = nhts_sim,
               weight = "weight",
               static = acs_2015,
               by = c("state", "puma10"))

vmt <- ex6  %>%
  select(c('state','puma10','est')) %>% 
  rename(gst = est) 

# Code for making CONUS-PUMA maps
tmap_mode("view")
us_pumas <- pumas(cb = TRUE, year = 2019) %>% rename(puma10 = PUMACE10,
                                                     state = STATEFP10) %>%
  left_join(vmt, by = c('state','puma10'))  

tm_shape(us_pumas) + 
  tm_fill(style = "kmeans",col = "gst", palette =  "-RdYlBu",
          alpha = 0.8,legend.show = FALSE,
          legend.format=list(fun=function(x) paste0(formatC(x, digits=0, format="f")))) + 
  tm_borders(lwd = 0) 



#insec metric using fused RECS 
ex11 <- analyze(x = list(mean = "insec"),
               implicates = recs_sim2,
               weight = "weight",
               static = acs_2015,
               by = c("state","puma10"))

estimate <- ex11  %>% filter(level == 'TRUE') %>% 
            select(c('state','puma10','est')) %>% mutate(est = 100*est)

insec <- pumas(cb = TRUE, year = 2019)  %>% rename(puma10 = PUMACE10,
                                                   state = STATEFP10) %>%
                                         merge(estimate, by = c('state','puma10')) 

map1 <- tm_shape(insec) + 
  tm_fill(style = "kmeans",col = "est", palette =  "-RdYlBu",
          alpha = 0.6,legend.show = TRUE,
          legend.format=list(fun=function(x) paste0(formatC(x, digits=0, format="f")))) + 
  tm_borders(lwd = 0) 
map1


#insec metric using fused RECS by race
ex12 <- analyze(x = list(mean = "insec"),
               implicates = recs_sim2,
               weight = "weight",
               static = acs_2015,
               by = c("state",'puma10','race'))

estimate12 <- ex12 %>% filter(race %in% "White alone") %>% filter(level == "TRUE")
estimate13 <- ex12 %>% filter(race %in% "Black or African American alone") %>% filter(level == "TRUE")
estimate14 <- ex12 %>% filter(race %in% "Latino")  %>% filter(level == "TRUE")

tmap_mode("view")

#Map  for white 
insec_w <- pumas(cb = TRUE, year = 2019)  %>%
  rename(puma10 = PUMACE10,
         state = STATEFP10) %>% merge(estimate12, by = c('state','puma10')) %>% 
          mutate(est = 100*est) 

map1 <- tm_shape(insec_w) + 
  tm_fill(style = "kmeans",col = "est", palette =  "-RdYlBu",
          alpha = 0.6,legend.show = FALSE,
          legend.format=list(fun=function(x) paste0(formatC(x, digits=0, format="f")))) + 
  tm_borders(lwd = 0) 
map1


#Map for black
insec_b <- pumas(cb = TRUE, year = 2019)  %>%
            rename(puma10 = PUMACE10,
                    state = STATEFP10) %>% merge(estimate13, by = c('state','puma10')) %>% 
                    mutate(est = 100*est)

map1 <- tm_shape(insec_b) + 
  tm_fill(style = "kmeans",col = "est", palette =  "-RdYlBu",
          alpha = 0.6,legend.show = FALSE,
          legend.format=list(fun=function(x) paste0(formatC(x, digits=0, format="f")))) + 
  tm_borders(lwd = 0) 
map1


#Map  for latino
  insec_l <- pumas(cb = TRUE, year = 2019) %>% rename(puma10 = PUMACE10,
                                                             state = STATEFP10) %>% 
                merge(estimate14, by = c('state','puma10')) %>%  
                 mutate(est = 100*est)

map1 <- tm_shape(insec_l) + 
  tm_fill(style = "kmeans",col = "est", palette =  "-RdYlBu",
          alpha = 0.6,legend.show = FALSE,
          legend.format=list(fun=function(x) paste0(formatC(x, digits=0, format="f")))) + 
  tm_borders(lwd = 0) 

map1

