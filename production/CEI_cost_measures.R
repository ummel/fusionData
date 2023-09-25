## Program to create cost-burden measures using CEX 
## Created: HA 11/07/22
## Updated: HA 11/07/22

# Set Up ----
rm(list = ls())
options(scipen=999)

library(pacman)
p_load(here, tidyverse, data.table, ipumsr, fusionData, fusionModel, tigris, 
       mapview, tmap, Hmisc)

#nycgeo not available for R 4.2

setwd(here())

# Data ----
# fused ACS and CEI data available on google drive - download locally before running
d <- fst::read_fst(path = "fusion/CEI/2015-2019/2019/post/CEI_2015-2019_2019_calib.fst") 

# limit to 1 implicate per household - take M == 1 
d <- filter(d, M == 1)
d <- as.data.table(d)
gc()

# ACS household data
acs <- fst::read_fst(path = "survey-processed/ACS/2019/ACS_2019_H_processed.fst")

# drop rep flags
acs <- acs %>% select(-starts_with('rep_'))

# rows in CEI and ACS data are ordered the same therefore can cbind rather than merge
d <- cbind(d, acs)
rm(acs)

# Cost Measures ----
# all annual, all household level, all in 2019$

d[ , c_util := elec + ofuel + intphn + ngas + watrsh]
d[ , c_trans_priv := gas + vehins + vehint + vehreg + vehprn + vehmlr + vehprd]
d[ , c_trans_pub := pubtrn]
d[ , c_health := health]
d[ , c_food := eatout + eathome]
d[ , c_house := hinsp + hmtimp + mrtgip + mrtgpp + ohouse + ptaxp + rent]

summary(d$c_house)
summary(d$c_util)
summary(d$c_food)
summary(d$c_trans_priv)
summary(d$c_trans_pub)
summary(d$c_health)

# # check if households that don't own a home have any housing costs - correct 
# table(d$ten)
# d[ten != "Rented" & ten != "Occupied without payment of rent", rent] %>% summary()
# d[ten == "Owned free and clear", mrtgip] %>% summary()
# d[ten == "Owned with mortgage or loan (include home equity loans)", mrtgip] %>% summary()
# 
# # do renters pay for home insurance? no
# d[ten == "Rented", ohouse] %>% summary()
# 
# # see how the CEI costs measure up to the ACS costs 
# summary(d$rent)
# summary(d$grntp)
#   # rent is similar 
# 
# summary(d$fulp)
# summary(d$ofuel)
#   # fuel in CEI much higher
# 
# summary(d$gas)
# summary(d$gasp)
#   # gas in CEI is much much lower - can't be capturing the same thing

# divide all costs by annual household income
vars <- names(select(d, starts_with('c_')))
d[ , (vars) := lapply(.SD, function(x){x/hincp}), .SDcols = vars]

## zero and negative income ----
# in about 1% of observations, household income is 0 or negative
# -> leave burden as missing, but this could be changed to Inf (or 1 or 0?)
mean(d[ , hincp <= 0])
d[hincp <= 0, (vars) := NA]

# check extreme burdens
summary(d$c_house)
summary(d$c_util)
summary(d$c_food)
summary(d$c_trans_priv)
summary(d$c_trans_pub)
summary(d$c_health)

## topcoding ----
# --> there are values greater than 1 
# --> topcode expenses at 200% of total income (burden = 2 or greater)
# --> include a flag for any values that have been topcoded 

for (v in vars){
  
  # flag observations that are being topcoded
  tcv <- paste0(v, '_TC')
  d[ , eval(tcv) := ifelse(get(v) > 2, 1, 0)]
  
  # topcode burdens to be maximum 200% of income
  d[get(v) > 2, eval(v) := 2]
  
}

# report share topcoded 
paste0(vars, '_TC')

d %>% 
  summarise(across(paste0(vars, '_TC'), ~mean(., na.rm = T)*100)) %>%
  pivot_longer(cols = everything(), 
              names_to = 'Expense', 
              values_to = 'Percentage') %>%
  mutate(Expense = str_remove_all(Expense, 'c_|_TC'))
  

# Maps ----

# summarize burden at the puma level 
# - b/c burdens are NA when hincp <= 0, effectively dropping these households 

# median
puma_med <- d[ , by = c('state', 'puma10'), 
               lapply(.SD, function(x) round(wtd.quantile(x, weight, probs = 0.5, na.rm = T)*100, 0)), 
               .SDcols = vars]

# mean
puma_avg <- d[ , by = c('state', 'puma10'), 
               lapply(.SD, function(x) round(weighted.mean(x, weight, na.rm = T)*100,0)),
               .SDcols = vars]

# ipums crosswalk between MSA and puma
puma2msa <- readxl::read_excel("./survey-processed/CEX/MSA2013_PUMA2010_crosswalk.xls") %>% as.data.table()

# list of PUMAs in desired MSAs
msa_list <- c('New York-Newark-Jersey City, NY-NJ-PA', 
              'Philadelphia-Camden-Wilmington, PA-NJ-DE-MD',
              'Boston-Cambridge-Newton, MA-NH')

msa_puma <- puma2msa[`MSA Title` %in% msa_list, c('State FIPS Code', 'PUMA Code', 'MSA Title')]
names(msa_puma) <- c('state', 'puma10', 'msa')

# mapping data for pumas in city MSA
mapd <- pumas(cb = TRUE, year = 2019) %>% 
  rename(puma10 = PUMACE10, state = STATEFP10) %>%
  right_join(msa_puma, by = c('state', 'puma10')) %>%
  mutate(msa = str_remove(msa, ', .*'))

# join to cost burdens - median
mapd <- left_join(mapd, puma_med)

makemap_allcity <- function(v, save = F){

 map <- tm_shape(mapd) + 
    tm_fill(style = "cont", col = v, 
            palette =  RColorBrewer::brewer.pal(9, "YlOrRd"),
            alpha = 0.6, legend.show = TRUE, title = 'Percent of Income') + 
    tm_facets(by = 'msa', nrow = 1) +
    tm_borders() +
    tm_layout(legend.outside = F, legend.position = c('left', 'bottom')) 

  if (save == T) tmap_save(map, 
                           filename = paste0('G:/My Drive/GSR/fusionACS/', v, '.png'),
                           height = 4, width = 8, units = 'in')
 
 return(map)

}

map(vars, ~makemap_allcity(., save = T))

# split housing for renters and owners? - after strike


# function for outputting maps
makemap <- function(v, lbl, city){
  
  # list of PUMAs 
  msa_puma <- puma2msa[`MSA Title` == city, c('State FIPS Code', 'PUMA Code')]
  names(msa_puma) <- c('state', 'puma10')
  
  # mapping data for pumas in city MSA
  mapd <- pumas(cb = TRUE, year = 2019) %>% 
    rename(puma10 = PUMACE10, state = STATEFP10) %>%
    right_join(msa_puma, by = c('state', 'puma10'))
  
  # join to cost burdens - median
  mapd <- left_join(mapd, puma_med)
  
  # --> want the legend to always be on the same scale 
  # --> and maybe break down more categorically, eg. 0-5%, 5-10% etc. 
  
  map <- tm_shape(mapd) + 
    tm_fill(style = "cont", col = v, palette =  RColorBrewer::brewer.pal(9, "YlOrRd"),
            alpha = 0.6, legend.show = TRUE, title = 'Percent of Income') + 
    tm_borders() +
    tm_layout(title = paste0("Median ", lbl, " Burden - \n", city), 
              legend.position = c('left', 'bottom')) +
    tm_credits("Source: ACS 2019 and CEX 2015-2019. \nHouseholds with zero household income are excluded.", 
               align = 'right')
  
  return(map)
}



## NYC ----

makemap('c_util', 'Utilities', "New York-Newark-Jersey City, NY-NJ-PA")

## Philly ----

makemap('c_util', 'Utilities', "Philadelphia-Camden-Wilmington, PA-NJ-DE-MD")

## Boston ----

makemap('c_util', 'Utilities','Boston-Cambridge-Newton, MA-NH')




