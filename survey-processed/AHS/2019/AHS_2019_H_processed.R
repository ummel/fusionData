library(tidyverse)
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

# NOTE: I ran this in the cluster, then read
# imp <- imputeMissing(data = d[selected_vars],
#                      N = 1,
#                      max_ncats = 10,
#                      weight = "WEIGHT",
#                      x_exclude = setdiff(selected_vars,exp_vars))
# fst::write_fst(imp,"~/ahs_imp.fst")
imp <- fst::read_fst("survey-raw/AHS/2019/ahs_imp.fst")

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
         cbsa13 = OMB13CBSA
  )

geo_conc<-fst::fst("geo-processed/concordance/geo_concordance.fst")
gnames <- names(geo_conc)
gvars <- intersect(gnames, names(d))

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

miss_div<-d[is.na(d$division),]
miss_div<-merge(miss_div,miss_wgt,by="cbsa13")
miss_div$division<-miss_div$division.y
for (wgt_var in names(d)[grep("WEIGHT",names(d))]) {
  miss_div[[wgt_var]]<-miss_div[[wgt_var]]*miss_div$share
}
miss_div<-miss_div[names(d)]

d<-rbind(d[!d$CONTROL%in%miss_div$CONTROL,],miss_div)
rm(miss_div,miss_wgt,cbsa13wgt)
gc()

anyNA(d)

names(d)[colSums(is.na(d))>0] # Those missing are fine

#----------------

# Assemble final output
# NOTE: var_label assignment is done AFTER any manipulation of values/classes, because labels can be lost when classes are changed

h.final <- d %>%
  mutate_if(is.factor, safeCharacters) %>%
  mutate_if(is.numeric, convertInteger) %>%
  mutate_if(is.double, cleanNumeric, tol = 0.001) %>%
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
  arrange(ahs_2019_hid)

# Create dictionary and save to disk
dictionary <- createDictionary(data = h.final, survey = "AHS", vintage = 2019, respondent = "H")
saveRDS(object = dictionary, file = "survey-processed/AHS/2019/AHS_2019_H_dictionary.rds")

#----------------

# Save data to disk (.fst)
fst::write_fst(x = h.final, path = "survey-processed/AHS/2019/AHS_2019_H_processed.fst", compress = 100)