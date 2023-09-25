# Program to make maps at the county level comparing HUD and fused estimates of 
# public housing use
# Adapted from Karthik's program Script to make AHS PUMA-County-Zip maps .R 
# Created: HA 8/1/23
# Updated: HA 8/10/23 
#          HA 9/22/23 - 
rm(list = ls())

library(pacman)
p_load(fusionData, fusionModel, tidyverse, tigris, mapview, tmap)
tmap_mode("view")

# Adapted Program ----

# Data ----

# ACS household level observations - used by analyze2()
acs_2019 <- fst::read_fst("survey-processed/ACS/2019/ACS_2019_H_processed.fst") %>%
  select(acs_2019_hid, state, weight, starts_with("rep_"),hincp,puma10,elep,gasp,fulp)

#Load ASEC fused data
asec <- fst::read_fst(path = "fusion/ASEC/2019/2019/H/output/ASEC_2019_2019_fused.fst") %>%
  select(.,-c('acs_2019_hid'))

# Add counties to ASEC data ----
M <- 30
yr <- 2019
source("production/validation/county_assignment_ACS_2019.R")
asec_1 <- split(asec, f = asec$M)
rm(asec)

# add county and state
# --> if puma is all in one county, then V1-V30 counties will be the same
# --> if puma crosses multiple counties, then V1-V30 counties will vary 
asec_2 <- lapply(1:length(asec_1), function(i) mutate(asec_1[[i]], county10 = county.rnd[[paste0("V", i)]],
                                                    state = county.rnd[['state']]))
rm(asec_1)
asec_sim <- data.table::rbindlist(asec_2) 

rm(asec_2)
gc()

# --> now each observations in the fused ASEC data has a county (or range of counties)
names(asec_sim)

# Calculate housing ----

# calculate pubhous by county (share and count)
ex1 <- analyze(x = list(mean = "pubhous",
                        count = "pubhous"),
               implicates = asec_sim,
               weight = "weight",
               static = acs_2019,
               by = c("state","county10"))

# check there are 3143 counties (number of levels * number of counties)
nrow(ex1)/3

fst::write_fst(ex1, "production/validation/ASEC/asec_pubhous_county.fst")

# calculate rent subsidy by county
ex2 <- analyze(x = list(mean = "rentsub"),
               implicates = asec_sim,
               weight = "weight",
               static = acs_2019,
               by = c("state","county10"))

fst::write_fst(ex2, "production/validation/ASEC/asec_rentsub_county.fst")

# Maps ----

## NYC ----

# list of NY counties
ny_counties <- c("005", "047", "061", "081", "085")

ny_county_pubhous <- ex1 %>%
  filter(level == "Yes") %>%
  select("state", "county10", "est") %>%
  filter(state == "36" & county10 %in% ny_counties) %>%
  mutate(est = 100*est)

tmap_mode("view")

ny_pubhous <- counties(year = 2020, state = "NY") %>%
  rename(state = STATEFP,
         county10 = COUNTYFP) %>%
  right_join(ny_county_pubhous, by = c('state', 'county10'))

map1 <- tm_shape(ny_pubhous) + 
  tm_fill(style = "kmeans", col = "est", palette =  "-RdYlBu",
          alpha = 0.6,legend.show = TRUE,
          legend.format=list(fun=function(x) paste0(formatC(x, digits=0, format="f")))) + 
  tm_borders(lwd = 0.8) 

lf <- tmap_leaflet(map1)
lf

mapshot(lf, file = "production/validation/ASEC/nyc_pubhous_county.pdf")





# OLD - Previous Program ----

# Person observations. Retain race of reference person, derived from original variables 'rac1p' and 'hisp'
acs.p_2019 <- fst::read_fst("survey-processed/ACS/2019/ACS_2019_P_processed.fst", columns = c("acs_2019_hid", "sporder", "rac1p", "hisp")) %>%
  filter(sporder == 1) %>%
  mutate(race = ifelse(rac1p %in% c("White alone", "Black or African American alone"), as.character(rac1p), "Other"),
         race = ifelse(hisp == "Not Spanish / Hispanic / Latino", race, "Latino"),
         race = factor(race)) %>%
  select(acs_2019_hid, race)

# Household observations, including weights, with 'race' merged from person file
acs_2019 <- fst::read_fst("survey-processed/ACS/2019/ACS_2019_H_processed.fst") %>%
  select(acs_2019_hid, state, weight, starts_with("rep_"),hincp,puma10,elep,gasp,fulp) %>%
  left_join(acs.p_2019, by = "acs_2019_hid")

rm(acs.p_2019)


#Load AHS fused data 
ahs <- fst::read_fst(path = "fusion/AHS/2019/2019/output/AHS_2019_2019_fused_merged.fst") %>% 
      select(.,-c('acs_2019_hid')) %>% mutate(fss = fsstatus != "High food security among adults")


#Add counties and zipcodes for ACS 2019
M <- 40
yr <- 2019
source("production/validation/county_assignment_ACS_2019.R")
#source("production/examples/AHS-ACS 2019/Random assignment of counties to ACS 2019 households.R")
#source("production/examples/AHS-ACS 2019/Random assignment of zipcodes to ACS 2019 households.R")
ahs_1 <- split(ahs , f = ahs$M)
ahs_2 <- lapply(1:length(ahs_1), function(i) mutate(ahs_1[[i]], county10 = county.rnd[[paste0("V", i)]],
                                                    state = county.rnd[['state']]))
#ahs_3 <- lapply(1:length(ahs_2), function(i) mutate(ahs_2[[i]], zcta10 = zcta.rnd[[paste0("V", i)]]))

ahs_sim <- data.table::rbindlist(ahs_2) 

rm(ahs_1)
rm(ahs_2)
rm(ahs)
gc()

## Calculate cold by PUMA 
ex1 <- analyze(x = list(mean = "cold"),
               implicates = ahs_sim,
               weight = "weight",
               static = acs_2019,
               by = c("state",'puma10'))

fst::write_fst(ex1, "ahs_cold_puma_temp.fst")

#Map for Philly
ph_pumas <- data.frame(c('03201','03202','03203','03204','03205','03206','03207','03208','03209','03210','03211')) 
colnames(ph_pumas) = c('puma10')

ph_pumas_cold <- ex1  %>% filter(level== 'Yes') %>% 
  select(c('state','puma10','est')) %>% filter(state == '42')  %>% mutate(est =100*est)

tmap_mode("view")
ph_cold <- pumas(cb = FALSE, year = 2020,state='PA') %>% 
  rename(puma10= PUMACE10) %>% merge(ph_pumas_cold, by = c('puma10'))  %>% merge(ph_pumas, by ='puma10')

map1 <- tm_shape(ph_cold) + 
  tm_fill(style = "kmeans",col = "est", palette =  "-RdYlBu",
          alpha = 0.6,legend.show = TRUE,
          legend.format=list(fun=function(x) paste0(formatC(x, digits=2, format="f")))) + 
  tm_borders(lwd = 0.8) 

map2 <- map1 + tm_view(set.view = c(-75.1575,39.9509,11))

lf <- tmap_leaflet(map2)
mapshot(lf, file = "ph_cold_temp.pdf")


#Map for NYC
nyc_pumas_cold <- ex1  %>% filter(level== 'Yes') %>% 
  select(c('state','puma10','est')) %>% filter(state == '36') %>% mutate(est =100*est)

tmap_mode("view")
nyc_cold <- pumas(cb = TRUE, year = 2019,state='NY') %>% 
  rename(puma10= PUMACE10) %>% merge(nyc_pumas_cold, by = c('puma10')) %>% filter(str_detect(NAME10, 'NYC'))

map1 <- tm_shape(nyc_cold) + 
  tm_fill(style = "kmeans",col = "est", palette =  "-RdYlBu",
          alpha = 0.6,legend.show = TRUE,
          legend.format=list(fun=function(x) paste0(formatC(x, digits=2, format="f")))) + 
  tm_borders(lwd = 0.8) 

map2 <- map1 + tm_view(set.view = c(-74.0060, 40.7128, 10.25))

lf <- tmap_leaflet(map2)
mapshot(lf, file = "/Users/karthikakkiraju/Documents/fusionData/production/examples/Slides/Figures/nyc_cold.pdf")


## Calculate rating by PUMA 
ex2 <- analyze(x = list(mean = "ratinghs"),
               implicates = ahs_sim,
               weight = "weight",
               static = acs_2019,
               by = c("state",'puma10'))

#Map for Philly
ph_pumas <- data.frame(c('03201','03202','03203','03204','03205','03206','03207','03208','03209','03210','03211')) 
colnames(ph_pumas) = c('puma10')

ph_pumas_rat <- ex2  %>%
  select(c('state','puma10','est')) %>% filter(state == '42') 

tmap_mode("view")
ph_rat <- pumas(cb = FALSE, year = 2020,state='PA') %>% 
  rename(puma10= PUMACE10) %>% merge(ph_pumas_rat, by = c('puma10'))  %>% merge(ph_pumas, by ='puma10')

map1 <- tm_shape(ph_rat) + 
  tm_fill(style = "kmeans",col = "est", palette =  "RdYlBu",
          alpha = 0.6,legend.show = TRUE,
          legend.format=list(fun=function(x) paste0(formatC(x, digits=2, format="f")))) + 
  tm_borders(lwd = 0.8) 

map2 <- map1 + tm_view(set.view = c(-75.1575,39.9509,11))

lf <- tmap_leaflet(map2)
mapshot(lf, file = "/Users/karthikakkiraju/Documents/fusionData/production/examples/Slides/Figures/ph_rating.pdf")



#Map for NYC
nyc_pumas_rat <- ex2  %>% 
  select(c('state','puma10','est')) %>% filter(state == '36') 

tmap_mode("view")
nyc_rat <- pumas(cb = TRUE, year = 2019,state='NY') %>% 
  rename(puma10= PUMACE10) %>% merge(nyc_pumas_rat, by = c('puma10')) %>% filter(str_detect(NAME10, 'NYC'))

map1 <- tm_shape(nyc_rat) + 
  tm_fill(style = "kmeans",col = "est", palette =  "RdYlBu",
          alpha = 0.6,legend.show = TRUE,
          legend.format=list(fun=function(x) paste0(formatC(x, digits=2, format="f")))) + 
  tm_borders(lwd = 0.8) 

map2 <- map1 + tm_view(set.view = c(-74.0060, 40.7128, 10.25))

lf <- tmap_leaflet(map2)
mapshot(lf, file = "/Users/karthikakkiraju/Documents/fusionData/production/examples/Slides/Figures/nyc_rating.pdf")



#Create cold by zipcode
ex3 <- analyze(x = list(mean = "cold"),
               implicates = ahs_sim,
               weight = "weight",
               static = acs_2019,
               by = c("state",'zcta10'))

# calculate by county
ex3_county <- analyze(x = list(mean = "cold"),
               implicates = ahs_sim,
               weight = "weight",
               static = acs_2019,
               by = c("state","county10"))

#Map for Boston 
bos_zip <- ex3  %>% filter(level== 'Yes') %>% 
            select(c('state','zcta10','est')) %>% filter(state == '25') %>% rename(zcta = zcta10) %>% mutate(est =100*est)

tmap_mode("view")
bos_zip <- zctas(cb = FALSE, year = 2010,state='MA') %>% 
  rename(zcta = ZCTA5CE10) %>% merge(bos_zip, by = c('zcta'))  %>% merge(boston_zipcodes, by ='zcta')

map1 <- tm_shape(bos_zip) + 
  tm_fill(style = "kmeans",col = "est", palette =  "-RdYlBu",
          alpha = 0.6,legend.show = TRUE,
          legend.format=list(fun=function(x) paste0(formatC(x, digits=2, format="f")))) + 
  tm_borders(lwd = 0.8) 

map2 <- map1 + tm_view(set.view = c(-71.0589,42.3601,11))

lf <- tmap_leaflet(map2)
mapshot(lf, file = "/Users/karthikakkiraju/Documents/fusionData/production/examples/Slides/Figures/bos_cold.pdf")


#Create rating by zipcode
ex4 <- analyze(x = list(mean = "ratinghs"),
               implicates = ahs_sim,
               weight = "weight",
               static = acs_2019,
               by = c("state",'zcta10'))

#Map for Boston 
bos_zip_rat <- ex4 %>% 
  select(c('state','zcta10','est')) %>% filter(state == '25') %>% rename(zcta = zcta10)

tmap_mode("view")
bos_zip <- zctas(cb = FALSE, year = 2010,state='MA') %>% 
  rename(zcta = ZCTA5CE10) %>% merge(bos_zip_rat, by = c('zcta'))  %>% merge(boston_zipcodes, by ='zcta')

map1 <- tm_shape(bos_zip) + 
  tm_fill(style = "kmeans",col = "est", palette =  "RdYlBu",
          alpha = 0.6,legend.show = TRUE,
          legend.format=list(fun=function(x) paste0(formatC(x, digits=2, format="f")))) + 
  tm_borders(lwd = 0.8) 

map2 <- map1 + tm_view(set.view = c(-71.0589,42.3601,11))

lf <- tmap_leaflet(map2)
mapshot(lf, file = "/Users/karthikakkiraju/Documents/fusionData/production/examples/Slides/Figures/bos_rating.pdf")


#Denver zip code maps
ex4 <- analyze(x = list(mean = "noheat"),
               implicates = recs_sim,
               weight = "weight",
               static = acs_2015,
               by = c("state",'zcta10'))

estimate4 <- ex4  %>% filter(level == 'TRUE') %>%
  select(c('state','zcta10','est')) %>% filter(state == '08') %>% rename(zcta = zcta10)


#Keep zipcodes in Denver 
#Make Denver
tmap_mode("view")
co_zip <- zctas(cb = FALSE, year = 2010,state='CO') %>% 
  rename(zcta = ZCTA5CE10) %>% merge(co_311_zip, by = c('zcta')) %>% merge(estimate4, by = c('zcta')) 

tm_shape(co_zip) + 
  tm_fill(style = "cont",col = "est", palette =  RColorBrewer::brewer.pal(9, "YlOrRd"),
          alpha = 0.6,legend.show = TRUE) + tm_borders() 


#Denver zip code maps
ex6 <- analyze(bur ~ 1,
               implicates = recs_sim,
               donor.N = N_RECS,
               sample_weights = acs_2015$weight,
               static = acs_2015,
               by = c("state",'zcta10','bur_flag'))

estimate6 <- ex6  %>% 
  select(c('state','zcta10','estimate')) %>% filter(state == '08') %>% rename(zcta = zcta10)


#Keep zipcodes in Denver 
#Make Denver
tmap_mode("view")
co_zip <- zctas(cb = FALSE, year = 2010,state='CO') %>% 
  rename(zcta = ZCTA5CE10) %>% merge(co_311_zip, by = c('zcta')) %>% merge(estimate4, by = c('zcta')) 

tm_shape(co_zip) + 
  tm_fill(style = "cont",col = "estimate", palette =  RColorBrewer::brewer.pal(9, "YlOrRd"),
          alpha = 0.6,legend.show = TRUE) + tm_borders() 


#Maps for AHS estimates by zipcode
ex5 <- analyze(cold ~ 1,
               implicates = ahs_sim,
               donor.N = N_AHS,
               sample_weights = acs_2019$weight,
               static = acs_2019,
               by = c("state", "zcta10"))

estimate5 <- ex5  %>% filter(level== 'Yes')  %>% 
                      filter(state %in% c('08')) %>% 
                      filter (estimate < 0.25) %>% 
                      select(c('state','zcta10','estimate')) %>% rename(zcta = zcta10)

#Make Denver counties maps
tmap_mode("view")
co_zip <- zctas(cb = FALSE, year = 2010,state='CO') %>% 
  rename(zcta= ZCTA5CE10) %>% merge(co_311_zip, by = c('zcta')) %>% merge(estimate5, by = c('zcta')) 

tm_shape(co_zip) + 
  tm_fill(style = "cont",col = "estimate", palette =  RColorBrewer::brewer.pal(9, "YlOrRd"),
          alpha = 0.6,legend.show = TRUE) + tm_borders() 




## Calculate cold by PUMA 
ex6 <- analyze(x = list(mean = "fss"),
               implicates = ahs,
               weight = "weight",
               static = acs_2019,
               by = c("state",'puma10'))

#Map for NYC
nyc_pumas_cold <- ex6  %>% filter(level == 'TRUE') %>% 
  select(c('state','puma10','est')) %>% filter(state == '36') %>% mutate(est = 100*est)

tmap_mode("view")
nyc_cold <- pumas(cb = TRUE, year = 2019,state='NY') %>% 
  rename(puma10= PUMACE10) %>% merge(nyc_pumas_cold, by = c('puma10')) %>% filter(str_detect(NAME10, 'NYC'))

map1 <- tm_shape(nyc_cold) + 
  tm_fill(style = "kmeans",col = "est", palette =  "-RdYlBu",
          alpha = 0.6,legend.show = TRUE,
          legend.format=list(fun=function(x) paste0(formatC(x, digits=2, format="f")))) + 
  tm_borders(lwd = 0.8) 

map2 <- map1 + tm_view(set.view = c(-74.0060, 40.7128, 10.25))

lf <- tmap_leaflet(map2)
mapshot(lf, file = "/Users/karthikakkiraju/Documents/fusionData/production/examples/Slides/Figures/nyc_food_insec.pdf")

