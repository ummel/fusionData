library(fusionModel)
library(fusionData)

setwd("/Users/karthikakkiraju/Documents/fusionData")

#Keep only data points from the national survey. This needs to be changed during processing 
ahs_national <- fst::read_fst(path = "survey-processed/AHS/2019/AHS_2019_H_processed.fst") %>% filter(sample == "National")
fst::write_fst(ahs_national,path = "survey-processed/AHS/2019/AHS_2019_H_processed.fst", compress = 100) 


prep <- prepare(donor = "AHS_2019",
                recipient = "ACS_2019",
                respondent = "household",
                implicates = 5)

data <- assemble(prep,
                  fusion.variables = c("cold","coldeq","coldeqfreq","coldhtcap","coldcost","coldutil","hotwater"),
                  spatial.datasets = "all",
                  window = 2)

N <- nrow(data$AHS_2019)

# Create custom fusion variables
# coldeqfreq has too many '0'. Changing it to factor 

data$AHS_2019 <- data$AHS_2019 %>%
  mutate(
    coldeqfreq_flag = ifelse(coldeqfreq == '0', "No","Yes"),
    coldeqfreq_flag = as.factor(coldeqfreq_flag)
    )
    
fusion.vars <- c("cold","coldeq","coldeqfreq_flag","coldhtcap","coldcost","coldutil","hotwater")
pred.vars <- unlist(map(c("harmonized.vars", "location.vars", "spatial.vars"), ~ attr(data, .x)))

yorder <- blockchain(data = data$AHS_2019, 
                     y = fusion.vars, 
                     x = pred.vars,
                     weight = "weight",
                     cores = 3) # Change for Windows 

# Train fusion model
fit <- train(data = data$AHS_2019,
             y = yorder,
             x = pred.vars,
             file = "production/v2/AHS/2019/AHS_2019.fsn",
             hyper = list(boosting = "gbdt",
                          feature_fraction = 0.8,
                          num_iterations = 1000,
                          learning_rate = 0.05),
             nfolds = 5,
             threads = 3,
             weight = "weight")

sim <- fuseM(data = data$ACS_2019,
             file = "production/v2/AHS/2019/AHS_2019-ACS.fsn",
             k = 5,
             M = 2)  # Increase more for production 

# Save simulation results to disk
fst::write_fst(x = sim,
               path = "production/v2/AHS/2019/AHS_2019-ACS_2019.fst",
               compress = 100)

sim <- split(sim , f = sim$M)

#Example analysis at the county level 
# Load subset of original ACS PUMS variables
acs <- read_fst(path = "survey-processed/ACS/2019/ACS_2019_H_processed.fst",
                columns = c("acs_2019_hid", "weight", "state", "puma10"))

#Make sure to update the file to the correct M value and vintage
source("production/examples/AHS-ACS 2019/Random assignment of counties to ACS 2019 households.R")
all(acs$acs_2019_hid == county.rnd$acs_2019_hid)  # Safety check to confirm row order is OK

# Modify 'sim' by adding random county assignment from 'county.rnd' for each implicate
# This effectively treats county assignment as a simulated variable that varies across implicates
# The advantage is that uncertainty in county assignment is implicitly captured by analyze()
sim <- lapply(1:length(sim), function(i) mutate(sim[[i]], county10 = county.rnd[[paste0("V", i)]]))


# Safety check on row ordering
stopifnot(all(acs$acs_2019_hid == data$ACS_2019$acs_2019_hid))

sim1 <- do.call(rbind.data.frame, sim)

# Calculate whether house experience cold, by county
ex1 <- analyze(cold ~ 1,
              implicates = sim1,
              donor.N = nrow(data$AHS_2019),
              sample_weights = acs$weight,
              static = acs,
              by = c("state", "county10"))

cold <- filter(ex1, level == 'Yes') %>% 
        select(c('state','county10','estimate')) %>% 
        rename(cold_yes = estimate)

# Code for making county maps 
#Geocodes files in available in the google drive : https://docs.google.com/spreadsheets/d/1xWx_WC-AIPBoKaT9alfrBLU-x2izmwzm/edit?usp=sharing&ouid=116966545729853965083&rtpof=true&sd=true
#Currently not available in geoconcordance as having count names is sometimes preferred when making plots 

fips_county <- readxl::read_xlsx("production/validation/Electricitydata/all-geocodes-v2017.xlsx", skip =4)
fips_county <- select(fips_county,"State Code (FIPS)","County Code (FIPS)","Area Name (including legal/statistical area description)")
colnames(fips_county) <- c('state','county10','county')
fips_county <- fips_county[grepl("\\County", fips_county$county),]
fips_county$county <- gsub("\\County","", fips_county$county)
fips_county$county <- tolower(fips_county$county)

#Manual edits to county names 
fips_county$county <- gsub("st. lawrence","st lawrence",fips_county$county)


#Produce map for NY 
cold <- cold %>% filter(state == '36')  %>%
                 left_join(.,fips_county, by = c('state','county10')) 

#County Map for NY

library(fuzzyjoin)
states <- map_data("state")
NY_state <- subset(states, region == "new york")
counties <- map_data("county")
NY_county <- subset(counties, region == "new york")
NY_base <- ggplot(data = NY_state, mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) + geom_polygon(color = "black", fill = "gray")
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

county_map <- stringdist_left_join(NY_county, cold,by = "county", method='qgram', q=2)

gg1 <- NY_base +
  geom_polygon(data = county_map, aes(fill = cold_yes), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() +
  ditch_the_axis +
  ggtitle(" Population uncomfortable from cold")
gg2 <- gg1 + scale_fill_viridis_c(option = "D", limits = c(0.0,0.06))

gg2





