library(tidyverse)
library(fusionData)
source("R/utils.R")
source("R/createDictionary.R")
source("R/imputeMissing.R")
source("R/detectDependence.R")

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
         
         RATINGHS = factor(RATINGHS, levels = c('1','2','3','4','5','6','7','8','9','10'), ordered = T),
         
        
         
         RATINGNH = factor(RATINGNH, levels = c('0','1','2','3','4','5','6','7','8','9','10'), ordered = T))
  

# Impute SOME missing variables
# Note: this also requires installing the rpart.plot package
exp_vars<-c("WEIGHT", "DIVISION", "OMB13CBSA",
            "HINCP", "NUMPEOPLE", "NUMOLDKIDS", "NUMYNGKIDS","NUMELDERS",
            "HHAGE", "HHMAR", "HHRACE", "HHSEX", "HHSPAN", "HHGRAD",
            "BLD", "YRBUILT", "TENURE",
            "TOTROOMS","BEDROOMS", "STORIES")

selected_vars<-intersect(intersect(labeled_vars,names(d)),
                         codebook$var[!codebook$topic%in%
                                        c("Interview Status","Geography",
                                          "Weighting", "Edit Variables")])

selected_vars <- c("WEIGHT", "DIVISION", "OMB13CBSA", selected_vars)

 imp <- imputeMissing(data = d[selected_vars],
                      N = 1,
                      max_ncats = 10,
                      weight = "WEIGHT",
                      x_exclude = setdiff(selected_vars,exp_vars))

# Replace NA's in 'd' with the imputed values
d[setdiff(names(imp),codebook$var[codebook$topic%in%c("Interview Status","Geography","Weighting")])] <-
  imp[setdiff(names(imp),codebook$var[codebook$topic%in%c("Interview Status","Geography","Weighting")])]
rm(imp)
gc()

anyNA(d)

#-----

# Add/create variables for geographic concordance with variables in 'geo_concordance.fst'

d <- d %>%
  rename(division = DIVISION,
         cbsa13 = OMB13CBSA)

geo_conc<-fst::fst("geo-processed/concordance/geo_concordance.fst")
gnames <- names(geo_conc)
gvars <- intersect(gnames, names(d))

d <- d %>%
  mutate_at(gvars, ~ factor(.x, levels = sort(unique(.x)), ordered = FALSE)) 
#---------------

# Impute division to the missing cbsa13
# For the cases where the cbsa13 may belong to more than one division
# the household is split among the possible divisions, with its weight
# split according to the probability of being in each of them
miss_wgt<-geo_conc[geo_conc$cbsa13%in%unique(d$cbsa13[is.na(d$division)]),
                   c("division","cbsa13","puma_weight")]
miss_wgt<-aggregate(miss_wgt$puma_weight,by=list(miss_wgt$division,miss_wgt$cbsa13),FUN=sum)
names(miss_wgt)<-c("division","cbsa13","puma_weight")
cbsa13wgt<-aggregate(miss_wgt$puma_weight,by=list(miss_wgt$cbsa13),FUN=sum)
names(cbsa13wgt)<-c("cbsa13","cbsa13_weight")
miss_wgt<-merge(miss_wgt,cbsa13wgt,by="cbsa13")
miss_wgt$share<-miss_wgt$puma_weight/miss_wgt$cbsa13_weight

miss_div <- d[is.na(d$division),]
miss_div <- merge(miss_div,miss_wgt,by="cbsa13")
miss_div$division <- miss_div$division.y
for (wgt_var in names(d)[grep("WEIGHT",names(d))]) {
  miss_div[[wgt_var]]<-miss_div[[wgt_var]]*miss_div$share
}
miss_div<-miss_div[names(d)]

d <-rbind(d[!d$CONTROL%in%miss_div$CONTROL,],miss_div)
rm(miss_div,miss_wgt,cbsa13wgt)
gc()

d$cbsa13  <- as.character(d$cbsa13)
d$cbsa13[is.na(d$cbsa13)] <- "None"
d$cbsa13 <- as.factor(d$cbsa13)

anyNA(d)
names(d)[colSums(is.na(d))>0] # Those missing are fine

#----------------

# Assemble final output
# NOTE: var_label assignment is done AFTER any manipulation of values/classes, because labels can be lost when classes are changed

h.final <- d %>%
  mutate(HHINUSYR = as.factor(HHINUSYR)) %>%
  mutate(cbsa13 = as.factor(cbsa13)) %>%
  mutate_if(is.factor, safeCharacters) %>%
  mutate_if(is.numeric, convertInteger) %>%
  mutate_if(is.double, cleanNumeric, tol = 0.001) %>%
  mutate_if(is.factor, droplevels) %>%
  labelled::set_variable_labels(.labels = setNames(as.list(safeCharacters(codebook$desc)), codebook$var), .strict = FALSE) %>%  # Set descriptions for codebook variables
  labelled::set_variable_labels(.labels = setNames(as.list(paste(gvars, "geographic concordance")), gvars)) %>%  # Set descriptions for geo identifiers
  labelled::set_variable_labels(.labels = setNames(as.list(paste("Replicate Weight",str_remove(repl_wgts,"REPWEIGHT"))), repl_wgts)) %>%  # Set descriptions for replicate weights
  rename(
    ahs_2019_hid = CONTROL,  # Rename ID and weight variables to standardized names
    weight = WEIGHT
  ) %>%
  rename_with(~ gsub("REPWEIGHT", "REP_", .x, fixed = TRUE), .cols = starts_with("REPWEIGHT")) %>%  # Rename replicate weight columns to standardized names
  rename_with(tolower) %>%  # Convert all variable names to lowercase
  select(ahs_2019_hid, sample, weight, everything(), -starts_with("rep_"), starts_with("rep_")) %>%   # Reorder columns with replicate weights at the end
  arrange(ahs_2019_hid)  %>% filter(sample == "National")

####Create new HEQI variables##############
#h.final <- h.final %>%
 # mutate(
#  air_1 = ifelse(cookfuel == 'Piped gas' | cookfuel  == 'LP gas (liquid propane)', 1,0),
#   air_2 = ifelse(heatfuel == 'Kerosene or other liquid fuel' | heatfuel   == 'Coal or coke' | heatfuel   == 'Wood', 1,0),
# air_3 = ifelse(heattype == 'Unvented room heater[s]' | heattype   == 'Cooking stove used for heating' | heattype  == 'Fireplace with inserts', 1,0),
 # air_4 = ifelse(fireplace == 'Has usable fireplace, considers it heating equipment', 1,0),
#  air_HEQI = air_1  + air_2 + air_3 + air_4,
  
 # mold_1 = ifelse(moldbath == 'Yes', 1,0),
#  mold_2 = ifelse(moldbedrm == 'Yes', 1,0), 
#  mold_3 = ifelse(moldkitch == 'Yes', 1,0),
 # mold_4 = ifelse(moldlroom == 'Yes', 1,0), 
  #mold_5 = ifelse(moldother == 'Yes', 1,0), 
  #mold_HEQI = ifelse(mold_1 + mold_2 + mold_3 + mold_4 + mold_5>1,1,0),
  
 # leak_1 = ifelse(leakoroof == 'Yes', 1,0),
#  leak_2 = ifelse(leakowall == 'Yes',1,0),
 # leak_3 = ifelse(leakobase == 'Yes',1,0),
#  leak_4 = ifelse(leakidk == 'Yes',1,0),
 # leak_5 = ifelse(leakiwath == 'Yes',1,0),
#  leak_6 = ifelse(leakooth == 'Yes',1,0),
 # leak_7 = ifelse(leakipipe == 'Yes',1,0),
  #leak_8 = ifelse(leakiplum == 'Yes',1,0),
  #leak_9 = ifelse(leakioth == 'Yes',1,0),
  #leak_HEQI = ifelse(leak_1 + leak_2 +leak_3 +leak_4 +leak_5 +leak_6 +leak_7 +leak_8 +leak_9>1,1,0),
  
  #pest_1  = ifelse(rodent == 'Seen daily in the last 12 months' | rodent == 'Seen weekly in the last 12 months', 1,0),
  #pest_2 = ifelse(roach  == 'Seen daily in the last 12 months' | roach == 'Seen weekly in the last 12 months', 1,0),
  #pest_HEQI =  ifelse(pest_1 + pest_2>1,1,0),
  
#  paint_HEQI = ifelse(paintpeel == 'Yes' & yrbuilt < 1980,1,0),
  
 # crowd_HEQI = ifelse(numpeople/totrooms > 1.5,1,0),
  
  #elec_1 = ifelse(nowire == 'Exposed'| nowire == 'No electrical wiring',1,0),
  #elec_2 = ifelse(plugs == 'No',1,0),
  #elec_3 = ifelse(fuseblow == '3 fuses / breakers blown in the last 3 months' |fuseblow == '4 or more fuses / breakers blown in the last 3 months',1,0),
  #floor_1 = ifelse(floorhole == 'Yes',1,0),
  #wall_1 =  ifelse(wallcrack == 'Yes',1,0),
  #inj_HEQI = ifelse(elec_1 + elec_2 + elec_3 + floor_1 + wall_1>1,1,0),
  
#  san_1 = ifelse(notoilfreq  > 0,1,0),
 # san_2 = ifelse(sewbreak == 'Four or more breakdowns in last three months for 6 hours or more',1,0),
  #san_3 = ifelse(bathrooms == 'No full bath: sink and toilet present' | bathrooms == 'No full bath: tub and toilet present' | bathrooms == 'No full bath: toilet only'| bathrooms == 'No full bath: no sink, tub or toilet',1,0),
  #san_4 = ifelse(kitchsink == "No",1,0),
  
  #wat_1 = ifelse(hotwater == 'No hot and cold running water',1,0),
  #wat_2 = ifelse(nowat  == 'Yes',1,0),
  #wat_3 = ifelse(watsource == 'Well'| watsource == 'Other',1,0),
  #watsan_HEQI = ifelse(san_1 + san_2 + san_3 + san_4 + wat_1 + wat_2 + wat_3>1,1,0),
  
 
 # area = ifelse(unitsize == 'Less than 500 square feet', 23.22,
          #      ifelse(unitsize == '500 to 749 square feet', 58.06,
         #       ifelse(unitsize == '750 to 999 square feet', 81.29,
        #        ifelse(unitsize == '1,000 to 1,499 square feet', 116.12,
       #         ifelse(unitsize == '1,500 to 1,999 square feet', 162.58,
      #          ifelse(unitsize == '2,000 to 2,499 square feet', 209.13,
     #            ifelse(unitsize == '2,500 to 2,999 square feet', 255.48,
    #            ifelse(unitsize == '3,000 to 3,999 square feet', 325.16,
   #             ifelse(unitsize == '4,000 square feet or more', 418.06,NA))))))))),
                
  #  div_coeff = ifelse(division == 'East North Central',0.112,
               #   ifelse(division == 'Middle Atlantic',0.112,
                #         ifelse(division == 'Pacific' | division =='Mountain',0.156,
                 #   ifelse(division == 'New England',0.000,
                  #    ifelse(division == 'South Atlantic' | division ==  'East South Central',0.253,
                   #   ifelse(division == 'West North Central',0.112,
                    #  ifelse(division == 'West South Central',-0.008,NA))))))),              
                
#    duct_coeff = ifelse(division == 'East North Central',-0.091	+ 0.015	+ 0.009,
 #                      ifelse(division == 'Middle Atlantic',-0.022 + 	0.056 + 	0.005,
  #                            ifelse(division == 'Pacific' | division =='Mountain',-0.030	+ 0.038	+ 0.041,
   #                                  ifelse(division == 'New England',-0.017 + 	0.060	+  0.003,
    #                                        ifelse(division == 'South Atlantic' | division ==  'East South Central',-0.482+ 	0.442	+ 0.360,
     #                                              ifelse(division == 'West North Central',-0.101	+ 0.011+ 	0.005,
      #                                                    ifelse(division == 'West South Central',-0.020+ 	0.204+ 	0.038,NA))))))),       
    
       #      height = unitfloors*3,
                
  #   year_coeff = ifelse(yrbuilt <1960 | yrbuilt  == 1960,-0.25,
   #                  ifelse(yrbuilt == 1970,0.433,
    #                        ifelse(yrbuilt == 1980,-0.452,
     #                              ifelse(yrbuilt == 1990,-0.654,
      #                                    ifelse(yrbuilt == 2000,-0.915,
       #                                          ifelse(yrbuilt > 2000,-1.06,NA)))))),
 
  #    pov = ifelse(perpovlvl>150,4.20E-01,0),
 #     NL = exp(-0.00208*area + 0.0638*height + year_coeff + div_coeff + duct_coeff + pov -0.037 + 0.1),
   #   NL_HEQI = ifelse(NL > 2.51,1,0),
 
# heqi = air_HEQI + mold_HEQI + leak_HEQI + pest_HEQI + inj_HEQI + watsan_HEQI + crowd_HEQI + paint_HEQI + NL_HEQI) %>% 
#  select(-air_1,-air_2,-air_3,-air_4 ,-air_HEQI, 
        #   -mold_1 ,-mold_2 , -mold_3 ,-mold_4 , -mold_5, -mold_HEQI,
       #    -leak_1 ,-leak_2 ,-leak_3 ,-leak_4 ,-leak_5,-leak_6 ,-leak_7 ,-leak_8 ,-leak_9 ,-leak_HEQI,
      #      -pest_1  ,-pest_2 ,-pest_HEQI,
     #       -paint_HEQI ,
    #        -crowd_HEQI, 
   #          -elec_1 ,-elec_2,-elec_3 ,-floor_1 ,-wall_1 ,-inj_HEQI,
  #           -san_1 ,-san_2 ,-san_3,-san_4, -wat_1 ,-wat_2,-wat_3,-watsan_HEQI,
 #          -area,-div_coeff,-duct_coeff,-height,-year_coeff,-pov,-NL,-NL_HEQI)

#labelled::var_label(h.final$heqi) <- "Multi-dimensional house quality indicator"

# Create dictionary and save to disk
dictionary <- createDictionary(data = h.final, survey = "AHS", vintage = 2019, respondent = "H")
saveRDS(object = dictionary, file = "survey-processed/AHS/2019/AHS_2019_H_dictionary.rds")

#----------------
compileDictionary()

# Save data to disk (.fst)
fst::write_fst(x = h.final, path = "survey-processed/AHS/2019/AHS_2019_H_processed.fst", compress = 100)

