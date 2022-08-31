library(fusionModel)
library(fusionData)
library(tigris)
library(mapview)
library(tmap)

#Example code to fuse NHTS(2017) and ACS(2015)
setwd("/Users/karthikakkiraju/Documents/fusionData")

prep <- prepare(donor = "NHTS_2017",
                recipient = "ACS_2015",
                respondent = "household",
                implicates = 1)

data <- assemble(prep,
                 fusion.variables = c("place","price"),
                 spatial.datasets = "all",
                 window = 2)

N <- nrow(data$NHTS_2017)

fusion.vars <- c("place","price")
pred.vars <- unlist(map(c("harmonized.vars", "location.vars", "spatial.vars"), ~ attr(data, .x)))

yorder <- blockchain(data = data$NHTS_2017, 
                     y = fusion.vars, 
                     x = pred.vars,
                     weight = "weight",
                     cores = 3)

# Train fusion model
fit <- train(data = data$NHTS_2017,
             y = yorder,
             x = pred.vars,
             file = "production/v2/NHTS/2017/NHTS_2017.fsn",
             hyper = list(boosting = "gbdt",
                          feature_fraction = 0.8,
                          num_iterations = 1000,
                          learning_rate = 0.05),
             weight = "weight")

#Fuse with 'M' implicates 
sim <- fuse(data = data$ACS_2015,
             file = "production/v2/NHTS/2017/NHTS_2017.fsn",
             k = 5,
             M = 2) 

# Save simulation results to disk
fst::write_fst(x = sim,
               path = "production/v2/NHTS/2017/NHTS_2017-ACS_2017.fst",
               compress = 100)


#Example analysis at the county level 
# Load subset of original ACS PUMS variables
acs <- read_fst(path = "survey-processed/ACS/2015/ACS_2015_H_processed.fst",
                columns = c("acs_2015_hid", "weight", "state", "puma10"))

# Calculate whether house experience price, by PUMA
ex1 <- analyze(price ~ 1,
               implicates = sim,
               donor.N = nrow(data$NHTS_2017),
               sample_weights = acs$weight,
               static = acs,
               by = c("state", "puma10"))

price <- filter(ex1, level == 'Strongly agree') %>% 
  select(c('state','puma10','estimate')) %>% 
  rename(price_yes = estimate)

# Code for making NYC PUMA maps 
tmap_mode("view")

nhts_ny_pumas <- price %>% 
  filter(state == '36')

ny_pumas <- pumas(state = "NY", cb = TRUE, year = 2019) %>% rename(puma10 = PUMACE10) %>%
  left_join(nhts_ny_pumas, by = 'puma10')  %>% filter(str_detect(NAME10, 'NYC'))

tm_shape(ny_pumas) + 
  tm_fill(style = "cont",col = "price_yes", palette =  RColorBrewer::brewer.pal(9, "YlGnBu"),
          alpha = 0.6,legend.show = TRUE) + tm_borders() 



