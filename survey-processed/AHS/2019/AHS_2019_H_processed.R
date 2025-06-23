library(tidyverse)
library(fusionData)
library(fusionModel)
source('R/utils.R')
#-----

# Function to process the labels from the codebook

get_labels <- function(resp_codes) {
  
  labs <- str_split(resp_codes,
                    pattern = "\\|\\|",simplify = T)
  labs <- str_split(labs, pattern = ": ",simplify = T)
  labs <- as.data.frame(labs)
  
  if (dim(labs)[2]>2) {
    not_labs <- str_split(labs$V3, pattern = "to ",simplify = T)
    not_labs <- as.data.frame(not_labs)
    if (dim(not_labs)[2]>1) {
      not_labs$V1 <- str_remove_all(not_labs$V1,"\\$")
      not_labs <- (labs$V1==not_labs$V1)
      labs <- str_split(resp_codes,
                        pattern = "\\|\\|",simplify = T)
      labs <- labs[!not_labs]
      labs <- str_split(labs, pattern = ": ",simplify = T, n=2)
      labs <- as.data.frame(labs)
    } else {
      labs <- str_split(resp_codes,
                        pattern = "\\|\\|",simplify = T)
      labs <- str_split(labs, pattern = ": ",simplify = T, n=2)
      labs <- as.data.frame(labs)
    }
  }
  
  # Not applicable, will be set as the default No
  if (length(grep("or -6",labs$V1)>0)) {
    
    # Find the default No
    if (sum(labs$V2=="No")==1) {
      default_no<-"No"
    } else if (length(grep("No ",labs$V2)==1)) {
      default_no<-labs$V2[grep("No ",labs$V2)]
    } else {
      default_no<-"Not applicable"
    }
    
    labs <- rbind(labs, data.frame("V1"= c("N", "-6"),
                                   "V2"= c(default_no, default_no)))
    
    labs <- labs[-c(grep("or -6",labs$V1)),]
  }
  
  # Actual missing values, set to NA
  if (length(grep("or -9",labs$V1)>0)) {
    
    labs <- rbind(labs, data.frame("V1"= c("M", "-9"),
                                   "V2"= c(NA, NA)))
    
    labs <- labs[-c(grep("or -9",labs$V1)),]
    
  }
  
  return(labs)
}

#-----

# Function to check if the labels are ordered

ordered_labels <- function(labels) {
  
  non_missing_labs <- labels[!labels$V2%in%c("Not reported","Not applicable"),]
  if(length(grep("\\d",non_missing_labs$V2))>=0.5*length(non_missing_labs)) {
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}

#-----

# Load raw AHS 2019 data
d_national <- read_csv("survey-raw/AHS/2019/National/household.csv")
d_metro <- read_csv("survey-raw/AHS/2019/Metropolitan/household.csv")

d_national$Sample <- "1"
d_metro$Sample <- "2"

d <- bind_rows(d_national,d_metro)
rm(d_national,d_metro)
gc()

#-----

# Load and process and codebook

codebook <- read_csv("survey-raw/AHS/2019/ahsdict_06APR22_22_30_02_95.csv")
codebook <- codebook[codebook$`Table Name`%in%c("ALL","HOUSEHOLD"),]
codebook$metro <- 0
codebook$metro[grep("2019 Metro",codebook$`Survey Years`)]<-1
codebook$national <- 0
codebook$national[grep("2019 National",codebook$`Survey Years`)]<-1

codebook <- codebook %>%
  rename(
    var = Variable,
    desc = Description,
    topic = Subtopic,
    resp_codes = `Response Codes`,
  ) %>%
  select(var, national, metro, topic, desc, resp_codes)

# Add the new Sample variable
codebook <- rbind(codebook,
                  data.frame("var" = "Sample",
                             "national" = 1,
                             "metro" = 1,
                             "desc" = "National or Metro Sample",
                             "topic" = "Interview Status",
                             "resp_codes" = "1: National||2: Metro"))

# Here I will remove variables that are too specific
# and not needed for almost any analysis
# such as edit flag variables and split-sample
# replicate weights
to_remove <- names(d)[!names(d)%in%codebook$var]
repl_wgts <- to_remove[grep("WEIGHT",to_remove)]
to_remove <- to_remove[-grep("WEIGHT",to_remove)]

d <- d[!names(d)%in%to_remove]

labeled_vars <- names(d)[names(d)%in%codebook$var]
# Remove OMB13CBSA to keep it as in the geographic concordance...
labeled_vars <- labeled_vars[-grep("OMB13CBSA",labeled_vars)]
# ... and fix the wrong labels
d$OMB13CBSA[d$OMB13CBSA%in%c("'99998'","'99999'")]<-NA

# Add labels
pb <- pbapply::timerProgressBar(min = 0, max = dim(d)[2],
                                char = "+", width = 50, style = 3)

for (var in names(d)) {
  
  d[[var]] <- str_remove_all(d[[var]],"'")
  
  # Check if variable has labels
  if ((var%in%labeled_vars) &
      (length(grep("\\|\\|",codebook$resp_codes[codebook$var==var]))!=0)) {
    
    # Check if labels are the same for metro and national
    # (i.e. one set of labels)
    if (dim(codebook[codebook$var==var,])[1]==1) {
      
      labs <- get_labels(codebook$resp_codes[codebook$var==var])
      
      temp <- data.frame("V1" = d[[var]], "order" = 1:dim(d)[1])
      temp <- merge(temp, labs, by="V1", all.x = T)
      temp <- temp[order(temp$order),]
      temp$V2[is.na(temp$V2)] <- temp$V1[is.na(temp$V2)]
      
    } else {
      
      # For National
      labs <- get_labels(codebook$resp_codes[(codebook$var==var) &
                                               (codebook$national==1)])
      
      temp_nat <- data.frame("V1" = d[[var]], "order" = 1:dim(d)[1])
      temp_nat <- merge(temp_nat, labs, by="V1", all.x = T)
      temp_nat <- temp_nat[order(temp_nat$order),]
      temp_nat$V2[is.na(temp_nat$V2)] <- temp_nat$V1[is.na(temp_nat$V2)]
      
      # For Metro
      labs <- get_labels(codebook$resp_codes[(codebook$var==var) &
                                               (codebook$metro==1)])
      
      temp_met <- data.frame("V1" = d[[var]], "order" = 1:dim(d)[1])
      temp_met <- merge(temp_met, labs, by="V1", all.x = T)
      temp_met <- temp_met[order(temp_met$order),]
      temp_met$V2[is.na(temp_met$V2)] <- temp_met$V1[is.na(temp_met$V2)]
      
      temp <- temp_nat
      temp$V2[d$Sample=="Metro"] <- temp_met$V2[d$Sample=="Metro"]
      
    }
    
    # Final check to see if there is no continuous variable
    # passing up here unintendedly
    if (length(unique(temp$V2[!is.na(temp$V2)]))>length(unique(labs$V2))) {
      d[[var]] <- as.numeric(d[[var]])
      d[[var]][d[[var]]==-6] <- 0 # Not applicable, therefore zero
      d[[var]][d[[var]]==-9] <- NA # Not reported, actual missing
    } else {
      d[[var]] <- factor(temp$V2,
                         levels=unique(labs$V2[labs$V2%in%temp$V2]),
                         ordered=ordered_labels(labs))
    }
    
  } else {
    d[[var]] <- as.numeric(d[[var]])
    d[[var]][d[[var]]==-6] <- 0  # Not applicable, therefore zero
    d[[var]][d[[var]]==-9] <- NA # Not reported, actual missing
  }
  
  pbapply::setTimerProgressBar(pb, match(var,names(d)))
  
}

pbapply::closepb(pb)

gc()

#------------------




# Subset only observations and variables where the dwelling is not vacant
# This is important, because otherwise there is no "household"

d<-d[d$INTSTATUS=="Occupied interview",
     setdiff(names(d),
             codebook$var[codebook$topic%in%
                            c("Months Occupied",
                              "Vacancy Characteristics",
                              "Seasonal and URE Characteristics")])]

# And fix the tenure variable because of this
d$TENURE<-factor(d$TENURE,
                 levels=levels(d$TENURE)[1:3]) 

#------------------
d <- d %>% mutate(
  #HHINUSYR = factor(HHINUSYR),
  HHINUSYR = ifelse(HHINUSYR == 0,"Born in US",HHINUSYR))

d <- d %>%
  mutate(HRATE = gsub("\\<Not applicable\\>","Did not move",HRATE)) %>%
  mutate(NRATE = gsub("\\<Not applicable\\>","Did not move", NRATE),
         MVG3TYPE = gsub("\\<Not applicable\\>","Did not move", MVG3TYPE),
         MVG3TEN = gsub("\\<Not applicable\\>","Did not move",MVG3TEN),
         MVG3STAT = gsub("\\<Not applicable\\>","Did not move",MVG3STAT),
         MVG3COST = gsub("\\<Not applicable\\>","Did not move",MVG3COST),
         MVG2TYPE = gsub("\\<Not applicable\\>","Did not move",MVG2TYPE),
         MVG2TEN = gsub("\\<Not applicable\\>","Did not move",MVG2TEN),
         MVG2STAT = gsub("\\<Not applicable\\>","Did not move",MVG2STAT),
         MVG2COST = gsub("\\<Not applicable\\>","Did not move",MVG2COST), 
         MVG1TYPE = gsub("\\<Not applicable\\>","Did not move",MVG1TYPE),
         MVG1TEN  = gsub("\\<Not applicable\\>","Did not move",MVG1TEN),
         MVG1STAT = gsub("\\<Not applicable\\>","Did not move",MVG1STAT),
         MVG1COST= gsub("\\<Not applicable\\>","Did not move",MVG1COST),
         #LOTSIZE = gsub("\\<Not applicable\\>","Not a single family or mobile home",LOTSIZE),
         SEWUSERS = gsub("\\<Not applicable\\>",NA,SEWUSERS),
         
         COLD = gsub("Household did not live in the unit last winter",NA,COLD),
         
         MHWIDE = gsub("\\<Not applicable\\>",'Not manufactured',MHWIDE),
         FOUNDTYPE = gsub("\\<Not applicable\\>",'Not manufactured',FOUNDTYPE),
         HOWBUY = gsub("\\<Not applicable\\>",'Rented',HOWBUY),
         HUDSUB = gsub("\\<Not applicable\\>",'Owned',HUDSUB),
         HHRACEAS = gsub("\\<Not applicable\\>",'Not Asian',HHRACEAS),
         HHRACEPI = gsub("\\<Not applicable\\>",'Not Pacific Islander',HHRACEPI),
         BATHEXCLU = gsub("\\<Not applicable\\>",'Bathroom is inside unit',BATHEXCLU),
         MOVWHY = gsub("\\<Not applicable\\>","Did not move",MOVWHY)) %>%
  
  mutate(HRATE = as.factor(HRATE),
         NRATE = as.factor(NRATE),
         MVG3TYPE = as.factor(MVG3TYPE), #%>%
         
         MVG3TEN = as.factor(MVG3TEN),
         MVG3STAT = as.factor(MVG3STAT), 
         MVG3COST  = as.factor(MVG3COST),
         MVG2TYPE = as.factor(MVG2TYPE), 
         MVG2TEN = as.factor(MVG2TEN),
         MVG2STAT = as.factor(MVG2STAT), 
         MVG2COST = as.factor(MVG2COST),
         MVG1TYPE = as.factor(MVG1TYPE),
         MVG1TEN   = as.factor(MVG1TEN), 
         MVG1STAT = as.factor(MVG1STAT),
         MVG1COST = as.factor(MVG1COST),
         MOVWHY = as.factor(MOVWHY),
         SEWUSERS = as.factor(SEWUSERS),
         MHWIDE = as.factor(MHWIDE),
         HOWBUY = as.factor(HOWBUY),
         HUDSUB = as.factor(HUDSUB),
         HHRACEAS  = as.factor(HHRACEAS),
         HHRACEPI = as.factor(HHRACEPI),
         BATHEXCLU  = as.factor(BATHEXCLU),
         FOUNDTYPE = as.factor(FOUNDTYPE),
         LOTSIZE = as.factor(LOTSIZE))#

###Adding ordered factor########
d <- d %>% mutate(
  ADEQUACY = factor(ADEQUACY,levels = c('Adequate', 'Moderately inadequate', 'Severely inadequate'), ordered = T),
  
  DWNPAYPCT = factor(DWNPAYPCT, levels = c('No down payment', 'Greater than zero to 2 percent', '3 to 5 percent', '6 to 10 percent', 
                                           '11 to 15 percent', '16 to 20 percent', '21 to 40 percent', '41 to 99 percent', '100 percent'), ordered = T),
  
  NUMCARE = factor(NUMCARE, levels = c('No one has this disability', 'One person has this disability', '2 or more have this disability'), ordered = T),
  NUMERRND = factor(NUMERRND , levels = c('No one has this disability', 'One person has this disability', '2 or more have this disability'), ordered = T),
  NUMHEAR = factor(NUMHEAR, levels = c('No one has this disability', 'One person has this disability', '2 or more have this disability'), ordered = T),
  NUMMEMRY = factor(NUMMEMRY, levels = c('No one has this disability', 'One person has this disability', '2 or more have this disability'), ordered = T),
  NUMSEE = factor(NUMSEE, levels = c('No one has this disability', 'One person has this disability', '2 or more have this disability'), ordered = T),
  NUMWALK = factor(NUMWALK, levels = c('No one has this disability', 'One person has this disability', '2 or more have this disability'), ordered = T),
  
  ROACH = factor(ROACH,levels = c('Seen daily in the last 12 months', 'Seen weekly in the last 12 months', 'Seen monthly in the last 12 months', 
                                  'Seen a few times in the last 12 months', 'No signs in the last 12 months'), ordered = T),
  
  RODENT = factor(RODENT,levels = c('Seen daily in the last 12 months', 'Seen weekly in the last 12 months', 'Seen monthly in the last 12 months', 
                                    'Seen a few times in the last 12 months', 'No signs in the last 12 months'), ordered = T),
  
  SEWBREAK = factor(SEWBREAK, levels = c('No breakdowns in the last 3 months','Sewage system broke down in the last 3 months, but never for 6 hours or more', 
                                         'One breakdown in the last 3 months for 6 hours or more', 'Two breakdowns in the last 3 months for 6 hours or more', 
                                         'Three breakdowns in the last 3 months for 6 hours or more', 'Four or more breakdowns in last three months for 6 hours or more'), ordered = T),
  
  UNITSIZE = factor(UNITSIZE, levels = c('Less than 500 square feet', '500 to 749 square feet', '750 to 999 square feet', 
                                         '1,000 to 1,499 square feet', '1,500 to 1,999 square feet', '2,000 to 2,499 square feet', 
                                         '2,500 to 2,999 square feet', '3,000 to 3,999 square feet', '4,000 square feet or more'), ordered = T),
  
  UPKEEP = factor(UPKEEP, levels = c('Less than 3 upkeep problems', '3 or 4 upkeep problems', '5 or more upkeep problems'), ordered = T),
  
  FSAFFORD = factor(FSAFFORD, levels = c('Often true', 'Sometimes true', 'Never true'), ordered = T),
  FSWORRY = factor(FSWORRY, levels = c('Often true', 'Sometimes true', 'Never true'), ordered = T),
  
  FSLAST = factor(FSLAST,levels = c('Often true', 'Sometimes true', 'Never true'), ordered = T),
  
  HHGRAD = factor(HHGRAD, levels = c('Less than 1st grade', '1st, 2nd, 3rd, or 4th grade', '5th or 6th grade', '7th or 8th grade', '9th grade', 
                                     '10th grade', '11th grade', '12th grade', 'High school graduate - high school diploma or equivalent (for example: GED)', 
                                     'Some college but no degree', 'Diploma or certificate from a vocational, technical, trade business school beyond high school', 
                                     'Associate degree in college - occupational / vocational program', 'Associate degree in college - academic program', 
                                     "Bachelor's degree (for example: BA, AB, BS)", 
                                     'Professional school degree (for example: MD, DDS, DVM, LLB, JD)', 
                                     "Master's degree (for example: MA, MS, MEng, MEd, MSW, MBA)", 
                                     'Doctorate degree (for example: PhD, EdD)'), ordered = T),
  HHINUSYR = as.factor(HHINUSYR),
  COLD = as.factor(COLD)) %>%
  
  mutate(RATINGHS = as.numeric(RATINGHS),
         RATINGNH = as.numeric(RATINGNH)) %>%
  filter(Sample == 'National') #Keep only the national sample

# Which variables have missing values and how frequent are they?
na.count <- colSums(is.na(d))
na.count <- na.count[na.count > 0]
view(na.count)  # See which variables have NA's

d <- fusionModel::impute(data = as.data.table(d),weight = "WEIGHT",
                                ignore = c('INTLANG','INTSTATUS','INTMODE','INTMONTH',
                                           'SP1WEIGHT','SP2WEIGHT','CONTROL',
                                           'OMB13CBSA', #Don't impute OMB13CBSA
                                           grep("REP", colnames(d), value = TRUE)))
gc()

na.count <- colSums(is.na(d))
na.count <- na.count[na.count > 0]
view(na.count)  # See which variables have NA's

# Add/create variables for geographic concordance with variables in 'geo_concordance.fst'
d <-d %>%
  rename(division = DIVISION,
         cbsa13 = OMB13CBSA)

geo_conc <-fst::fst("geo-processed/concordance/geo_concordance.fst")
gnames <- names(geo_conc)
gvars <- intersect(gnames, names(d))

d <- d %>%
  mutate_at(gvars, ~ factor(.x, levels = sort(unique(.x)), ordered = FALSE)) 

d$cbsa13[is.na(d$cbsa13)] <- ""
d$cbsa13 <- as.factor(d$cbsa13)
#---------------

#Custom variable addition
d <- d %>% mutate(INADEQUATE = ADEQUACY != 'Adequate')

# Assemble final output
# NOTE: var_label assignment is done AFTER any manipulation of values/classes, because labels can be lost when classes are changed
h.final <- d %>%
  #mutate(HHINUSYR = as.factor(HHINUSYR)) %>%
  mutate(cbsa13 = as.factor(cbsa13)) %>%
  mutate_if(is.factor, safeCharacters) %>%
  mutate_if(is.numeric, convertInteger) %>%
  mutate_if(is.double, cleanNumeric, tol = 0.001) %>%
  mutate_if(is.factor, droplevels) %>%
  labelled::set_variable_labels(.labels = setNames(as.list(safeCharacters(codebook$desc)), codebook$var), .strict = FALSE) %>%  # Set descriptions for codebook variables
  labelled::set_variable_labels(.labels = setNames(as.list(paste(gvars, "geographic concordance")), gvars)) %>%  # Set descriptions for geo identifiers
  labelled::set_variable_labels(.labels = setNames(as.list(paste("Replicate Weight",str_remove(repl_wgts,"REPWEIGHT"))), repl_wgts)) %>%  # Set descriptions for replicate weights
  rename(
    hid = CONTROL,  # Rename ID and weight variables to standardized names
    weight = WEIGHT
  ) %>%
  rename_with(~ gsub("REPWEIGHT", "REP_", .x, fixed = TRUE), .cols = starts_with("REPWEIGHT")) %>%  # Rename replicate weight columns to standardized names
  rename_with(tolower) %>%  # Convert all variable names to lowercase
  select(hid, sample, weight, everything(), starts_with("rep_"))  # Reorder columns with replicate weights at the end

labelled::var_label(h.final$inadequate) <- "Housing Inadequacy Status"

# Create dictionary and save to disk
dictionary <- createDictionary(data = h.final, survey = "AHS", vintage = 2019, respondent = "H")
saveRDS(object = dictionary, file = "survey-processed/AHS/2019/AHS_2019_H_dictionary.rds")
fusionData::compileDictionary()

#----------------
# Save data to disk (.fst)
fst::write_fst(x = h.final, path = "survey-processed/AHS/2019/AHS_2019_H_processed.fst", compress = 100)
