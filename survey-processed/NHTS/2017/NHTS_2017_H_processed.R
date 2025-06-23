library(fusionModel)
library(fusionData)
library(tidyverse)
source("R/utils.R")

#This is the script to process household along with the summarized vehicle and trip level data
#Make sure that the corresponding summarized files have been generated before running this

setwd("/Users/karthikakkiraju/Documents/fusionData")

#Read household raw data
d_01 <- read_csv("survey-raw/NHTS/2017/hhpub.csv") # Load raw NHTS 2017 household level data

# Load house weight file and attach to hpub at the end. This file needs to be downloaded separately from the website.
wt <- read_csv("survey-raw/NHTS/2017/hhwgt.csv")
wt <- within(wt, rm("WTHHFIN"))
d_02 <- merge(d_01,wt,by="HOUSEID")

#Read vehicle-level data summarized at the household level
d_v <- fst::read_fst("survey-processed/NHTS/2017/NHTS_2017_V_summary.fst") %>% rename(HOUSEID = hid)

#Read trip-level data summarized at the household level
d_t <- fst::read_fst("survey-processed/NHTS/2017/NHTS_2017_T_summary.fst") %>% rename(HOUSEID = hid)

#Read person-level file and summarise it
d_p <- fst::read_fst("survey-processed/NHTS/2017/NHTS_2017_P_processed.fst") %>% select(hid,deliver,weight) %>%
        mutate(deliver = ifelse(deliver == "No online delivery",0,deliver)) %>%
        group_by(hid) %>% summarise(deliver_home = sum(deliver)) %>% rename(HOUSEID = hid)

d_p_hlt <- fst::read_fst("survey-processed/NHTS/2017/NHTS_2017_P_processed.fst") %>% select(hid,health,pid) %>%
        filter(pid == 1) %>% 
        rename(HOUSEID = hid) %>%# mutate(hlt = as.factor(ifelse(health %in% c('Fair','Poor'),'FALSE','TRUE'))) %>% 
        select(-pid)#, -health) 


#Merge household data and summarized vehicle data
d_03 <- merge(d_02,d_v, by = c('HOUSEID'),  all.x=TRUE)

#Merge household data and summarized trip-level data
d_04 <- merge(d_03,d_t, by = c('HOUSEID'),  all.x=TRUE)

#Merge household data and summarized person-level data
d <- merge(d_04,d_p, by = c('HOUSEID'),  all.x=TRUE) 

rm(d_01,d_02,d_03,d_04)

#Replace NA arising from trip-level merger with O. This could be due to household members not traveling
#for (v in names(d_t)) {
 # d[[v]] <- ifelse(is.na(d[[v]]),0, d[[v]])}

# Load and process household codebook
codebook_h <- readxl::read_excel("survey-raw/NHTS/2017/codebook_v1.2.xlsx", sheet ="CODEBOOK_HH") %>%

   setNames(c('var', 'desc', 'type', 'length', 'valuelabel', 'frequency','weighted') )%>%  #value and label are in the same cell. Need to separate them into differnt columns
   select(var, desc, valuelabel) %>%

  fill(var, desc) %>% # Replacing NA with the appropriate  entry

  mutate(
   valuelabel= gsub("Responses=","",valuelabel, fixed = TRUE),  #separating the valuelabel cell at the "=" sign
    )%>%

       mutate(
    value = (str_extract(valuelabel, "[^=]+")),
    label = (str_extract(valuelabel, "[^=]+$")),
    valuelabel = (NULL),
     ) %>%

    unnest(cols = c(label)) %>%

  # Replacing "Not ascertained", "I don't know", "Don't know" labels with NA
  mutate(
    label = ifelse(grepl("Not ascertained", label) | grepl("I don't know", label) | grepl("Don't know", label) | grepl("I prefer not to answer",label) | grepl("Refused", label), NA, label)   # Set label to NA if value is "I don't know", "Not ascertained",  (these observations are to be imputed eventually)
    ) %>%
  mutate(
    label = ifelse(var == 'HHSTFIPS', value,label),
    label = ifelse(var == 'HH_CBSA', value,label) ,
    label = ifelse(value == 'XXXXX',"",label),
    label =  ifelse((var == 'TAXI'|var == 'PARA'|var == 'BUS'|var == 'BIKE'|var == 'TRAIN') & (is.na(label)),"Appropriate skip",label)) %>%  #For these variables NA values are replaced instead of imputing 
  mutate_all(trimws)


#Load vehicle-level summarized codebook
codebook_v <- fst::read_fst(path = "survey-processed/NHTS/2017/NHTS_2017_V_codebook_summary.fst")

#Load vtrip-level summarized codebook
codebook_t <- fst::read_fst(path = "survey-processed/NHTS/2017/NHTS_2017_T_codebook_summary.fst")

#Merge trip,vehicle, and household codebooks
codebook <-   bind_rows(codebook_h,codebook_v,codebook_t) %>% distinct(var,value,label, .keep_all = T) %>%
              add_row(var = "deliver_home",desc = "Online deliveries made to home",value = NA, label = NA) 

rm(codebook_h,codebook_v,codebook_t)

# Add replicate household weight variables to codebook

rep_var <-colnames(wt)
rep_var <- rep_var[-c(1)]

rep_desc <- str_extract(rep_var,"\\d+$")
rep_desc <- paste0('Replicate Weight', rep_desc)

replicate_weight_labels <- data.frame(c(rep_var),c(rep_desc), NA ,NA)
names(replicate_weight_labels) <- c("var", "desc","value","label")
common_cols <- intersect(colnames(codebook), colnames(replicate_weight_labels))
codebook <- rbind(codebook[common_cols], replicate_weight_labels[common_cols])


# Variables with  "Appropriate skip" values
# These are the variables for which suitable replacement values must be specified below
as.vars <- codebook %>%
  filter(label == "Appropriate skip") %>%
  pull(var) %>%
  unique()

as.values <- list(
  CAR = "No personal car use",
  BUS = "No bus available",
  BIKE = "No bike available",
  TAXI = "No taxi available",
  TRAIN = "No train available",
  PARA = "No para-transit available"
)

  # These are cases where the variable measures a continuous/numeric concept,
  # but inserting a zero-value for "Not applicable" entries does not make sense.
  # In this case, a categorical value is assigned to override the default zero
  # and the variable should be treated as an ordered factor.

# Safety check for missing entries in 'as.values'
miss <- noquote(setdiff(as.vars, names(as.values)))
extras <- noquote(setdiff(names(as.values), as.vars))
stopifnot(length(miss) == 0 & length(extras) == 0)

extras
#-----

# Update 'codebook' with the manually specified override values for "Appropriate skip"

# Drop variables set to NULL in 'ap.skip'
codebook <- filter(codebook, !var %in% names(which(map_lgl(as.values, is.null))))

# Update "Appropriate skip" values for each variable
for (v in as.vars) {
  if (!is.null(as.values[[v]])) {
    ind <- which(codebook$var == v & codebook$label == "Appropriate skip")  # Update the "Appropriate skip" label to specified label
    codebook$label[ind] <- as.values[[v]]
  }
}

# Safety check
# Ensure there are not "Appropriate skip entries remaining in the codebook
stopifnot(!any(codebook$label == "Appropriate skip", na.rm = TRUE))

#-----

# Assign levels for ordered factors
# In general, we want to coerce unordered factor to ordered factors whenever feasible
# There is some judgement involved

ordered.factors <- list(

  BIKE = c("No bike available","Never","A few times a year","A few times a month","A few times a week","Daily"),
  BIKE2SAVE = c("Strongly disagree","Disagree","Neither Agree or Disagree","Agree","Strongly agree"),
  BUS = c("No bus available","Never","A few times a year","A few times a month","A few times a week","Daily"),
  CAR = c("No personal car use","Never","A few times a year","A few times a month","A few times a week","Daily"),
  HBHTNRNT = c("0-4%","5-14%","15-24%","25-34%","35-44%","45-54%","55-64%","65-74%","75-84%","85-94%","95-100%"),
  HBHUR = c("Second City","Rural","Suburban","Small Town","Urban"),
  HBPPOPDN =c("0-99","100-499","500-999","1,000-1,999","2,000-3,999","4,000-9,999","10,000-24,999","25,000-999,999"),
  HBRESDN =c("0-99","100-499","500-999","1,000-1,999","2,000-3,999","4,000-9,999","10,000-24,999","25,000-999,999"),
  HHFAMINC = c("Less than $10,000","$10,000 to $14,999","$15,000 to $24,999","$25,000 to $34,999","$35,000 to $49,999","$50,000 to $74,999","$75,000 to $99,999","$100,000 to $124,999","$125,000 to $149,999","$150,000 to $199,999","$200,000 or more"),
  HTEEMPDN = c("0-49","50-99","100-249","250-499","500-999","1,000-1,999","2,000-3,999","4,000-999,999"),
  HTHTNRNT = c("0-4%","5-14%","15-24%","25-34%","35-44%","45-54%","55-64%","65-74%","75-84%","85-94%","95-100%"),
  HTPPOPDN = c("0-99","100-499","500-999","1,000-1,999","2,000-3,999","4,000-9,999","10,000-24,999","25,000-999,999"),
  HTRESDN = c("0-99","100-499","500-999","1,000-1,999","2,000-3,999","4,000-9,999","10,000-24,999","25,000-999,999"),
  LIF_CYC = c("one adult, no children","2+ adults, no children","one adult, youngest child 0-5","2+ adults, youngest child 0-5","one adult, youngest child 6-15","2+ adults, youngest child 6-15","one adult, youngest child 16-21","2+ adults, youngest child 16-21","one adult, retired, no children","2+ adults, retired, no children"),
  MSACAT = c("Not in MSA","MSA less than 1 million","MSA of 1 million or more, and not in 1","MSA of 1 million or more, with rail"),
  MSASIZE = c("Not in MSA or CMSA","In an MSA of Less than 250,000","In an MSA of 250,000 - 499,999","In an MSA of 500,000 - 999,999","In an MSA or CMSA of 1,000,000 - 2,999,999","In an MSA or CMSA of 3 million or more","Not in MSA or CMSA"),
  PARA =  c("No para-transit available","Never","A few times a year","A few times a month","A few times a week","Daily"),
  PC =  c("Never","A few times a year","A few times a month","A few times a week","Daily"),
  PLACE = c("Strongly disagree","Disagree","Neither Agree or Disagree","Agree","Strongly agree"),
  PRICE =  c("Strongly disagree","Disagree","Neither Agree or Disagree","Agree","Strongly agree"),
  PTRANS =  c("Strongly disagree","Disagree","Neither Agree or Disagree","Agree","Strongly agree"),
  RAIL = c("MSA does not have rail, or hh not in an MSA","MSA has rail"),
  SPHONE = c("Never","A few times a year","A few times a month","A few times a week","Daily"),
  TAB = c("Never","A few times a year","A few times a month","A few times a week","Daily"),
  TAXI = c("No taxi available","Never","A few times a year","A few times a month","A few times a week","Daily"),
  TRAIN = c("No train available","Never","A few times a year","A few times a month","A few times a week","Daily"),
  URBAN = c("Not in urban area","In an area surrounded by urban areas", "In an urban area","In an Urban cluster"),
  URBANSIZE = c("Not in an urbanized area","50,000 - 199,999","200,000 - 499,999","500,000 - 999,999","1 million or more without heavy rail","1 million or more with heavy rail"),
  WALK = c("Never","A few times a year","A few times a month","A few times a week","Daily"),
  WALK2SAVE = c("Strongly disagree","Disagree","Neither Agree or Disagree","Agree","Strongly agree"),
  WEBUSE17 = c("Never","A few times a year","A few times a month","A few times a week","Daily")
  )

# Safety check
# Detect any variables in 'ordered.factors' that are NOT in the codebook
extras <- noquote(setdiff(names(ordered.factors), codebook$var))
stopifnot(length(extras) == 0)

# Safety check
# Check for a precise match between levels specified in 'ordered.factors' and the codebook labels
# Will return helpful message if a discrepancy is detected; some discrepancies may be allowable
# NOTE: The "TEMP*" variables will be flagged here, but that's OK: They are an odd case where we have a numeric mixed with a categorical
# for (v in names(ordered.factors)) {
#   of <- sort(ordered.factors[[v]])
#   cb <- sort(unique(filter(codebook, var == v)$label))
#   if (!identical(of, cb)) warning("Have a closer look at ", v, "\n-- Supplied levels:\n", paste(of, collapse = "\n"), "\n--Codebook levels:\n", paste(cb, collapse = "\n"))
# }

#-----

# Only retain variables remaining in the codebook
d <- d[intersect(names(d), codebook$var)]

options(scipen = 999) #Removes scientific notation. Scientific notation creeps in during new label creation and creates NA values upon comparison with ordered.factors.

for (v in names(d)) {

  cb <- filter(codebook, var == v )
  x <- d[[v]]
  y <- unlist(cb$value)

  z <- unlist(cb$label)
  m <- match(x, y)
  new.labels <- z[na.omit(m)]

  # Update 'x' with new value labels

  x[!is.na(m)] <- new.labels

  # Coerce result to ordered factor, if specified
  # Note that levels are restricted to those actually present in the data
  if (v %in% names(ordered.factors)) {

    num.na <- sum(is.na(x))
    y <- ordered.factors[[v]]
    x <- factor(x, levels = intersect(y, x), ordered = TRUE)
    stopifnot(sum(is.na(x)) == num.na)  # This is a final safety check to ensure no NA's introduced inadvertently
  }

  # Apply type.convert() to 'x'; leave ordered factors unchanged
  x <- if (is.ordered(x)) x else type.convert(x, as.is = FALSE)

  #Ensure unordered factor levels are sorted according to codebook order of levels
  #Originally, unordered factors were sorted alphabetically -- but there is often useful information in the codebook ordering
  #This retains a valid codebook ordering if one exists; otherwise sort levels alphabetically
  if (is.factor(x) & !is.ordered(x)) {
    num.na <- sum(is.na(x))
    if (all(x %in% cb$label)) {
      x <- factor(x, levels = intersect(cb$label, unique(x)))
    } else {
      x <- factor(x, levels = sort(unique(x)))
    }
    stopifnot(sum(is.na(x)) == num.na)  # This is a final safety check to ensure no NA's introduced inadvertently
  }

  # Update column in 'd'
  d[[v]] <- x

}

#-----
#Replace vehicle level data NA values with 0 if household has no vehicle
for(v in names(d_v))
  if(v != "HOUSEID")
{d[[v]]= ifelse(d$HHVEHCNT == 0,0,d[[v]])}

###Make NA times which are >200 min#####
d <- d %>% mutate(trwaittm = ifelse(trwaittm > 200, NA, trwaittm)) %>% 
            left_join(.,d_p_hlt, by = 'HOUSEID')

# See which variables have NA's
# Which variables have missing values and how frequent are they?
na.count <- colSums(is.na(d))
na.count <- na.count[na.count > 0]
na.count

d <- fusionModel::impute(as.data.table(d), 
                          weight = 'WTHHFIN',
             ignore = c('HOUSEID','SMPLSRCE','TRAVDAY','HHRELATD','TDAYDATE','SCRESP','RESP_CNT','HH_CBSA',
                        grep("WTHH", colnames(d), value = TRUE)),
             cores = 3)

na.count <- colSums(is.na(d))
na.count <- na.count[na.count > 0]
na.count

# Add/create variables for geographic concordance with variables in 'geo_concordance.fst'
d1 <- d %>%
  rename(
    nhts_region = CENSUS_R,
    nhts_division = CENSUS_D,
    cbsa13 = (HH_CBSA),
    state = HHSTFIPS) %>% 
  
  mutate(
    ur12 = ifelse(URBRUR == 'Rural','R','U'),
    state = str_pad(state, 2, pad ="0"),
    cbsa13 = as.factor(cbsa13),
    state = as.factor(state)) 

#Flag entries at the state-division level. 
#This could be due to the response being from a non-home location
#This was noted in the BTS LATCH model
geo2 <- fst::read_fst("geo-processed/concordance/geo_concordance.fst")  %>%
  select('state','division','region') %>% unique()

d1 <-  d1  %>%
  merge(.,geo2, by ='state') %>%
  mutate(travel_flag = ifelse(nhts_division == division, 'No','Yes')) %>%
  filter(travel_flag == "No")  %>% 
  select(-c('division','region','travel_flag')) %>%
  rename(
    region = nhts_region,
    division = nhts_division)

# See which variables in 'dxx' are also in 'geo_concordance' and
gnames <- names(fst::fst("geo-processed/concordance/geo_concordance.fst"))
gvars <- intersect(gnames, names(d1))

# Class new/added geo identifiers as unordered factors
d1 <- d1 %>%

  #CBSA correction: There are some CBSA which have been suppressed in NHTS but are available in 'geoconcordance.fst'
  #We could drop these entries altogether, randomly assign CBSA to the households, or just drop the CBSA variable. Choose #3
  mutate(
    cbsa13 = data.table::fifelse((state ==  '09'|state ==  '10'|state ==  '15'|state ==  '34') & is.na(cbsa13), NA, as.factor(cbsa13))) %>%
    mutate_at(gvars, ~ factor(.x, levels = sort(unique(.x)), ordered = FALSE))  %>%
  
  mutate(
    
#Add custom variables and descriptions
placeinsecurity = PLACE == "Strongly agree"|PLACE == "Agree",
priceinsecurity = PRICE == "Strongly agree"|PRICE == "Agree",
travelinsecurity =  WALK2SAVE == "Strongly agree"| WALK2SAVE == "Agree" |BIKE2SAVE == "Strongly agree"|BIKE2SAVE == "Agree",
travelinsecurity_intense =  WALK2SAVE == "Strongly agree" |BIKE2SAVE == "Strongly agree")

d1 = expss::apply_labels(d1, placeinsecurity = "Travel insecurity due to financial burden")
d1 = expss::apply_labels(d1,priceinsecurity = "Travel insecurity due to price")
d1 = expss::apply_labels(d1,travelinsecurity = "Travel insecurity overall due to biking and walking")
d1 = expss::apply_labels(d1,travelinsecurity_intense = "Travel insecurity overall due to biking and walking")
d1 = expss::apply_labels(d1, health = "Health of respondent")

#----------------
d1$cbsa13[is.na(d1$cbsa13)] <- ""

# Assemble final output
# NOTE: var_label assignment is done AFTER any manipulation of values/classes, because labels can be lost when classes are changed

h.final <- d1 %>%
  mutate_if(is.factor, safeCharacters) %>%
  mutate_if(is.numeric, convertInteger) %>%
  mutate_if(is.double, cleanNumeric, tol = 0.001) %>%
  labelled::set_variable_labels(.labels = setNames(as.list(safeCharacters(codebook$desc)), codebook$var), .strict = FALSE) %>%  # Set descriptions for codebook variables
  labelled::set_variable_labels(.labels = setNames(as.list(paste(gvars, "geographic concordance")), gvars)) %>%  # Set descriptions for geo identifiers
    rename(hid = HOUSEID,
           weight = WTHHFIN ) %>%   # Rename ID and weight variables to standardized names
  rename_with(~ gsub("WTHHFIN", "REP_", .x, fixed = TRUE), .cols = starts_with("WTHHFIN")) %>%  # Rename replicate weight columns to standardized names
  rename_with(tolower) %>%  # Convert all variable names to lowercase
  select(hid, everything(), starts_with("rep_")) %>%   # Reorder columns with replicate weights at the end
  arrange(hid) 

# Create dictionary and save to disk
dictionary <- createDictionary(data = h.final, survey = "NHTS", vintage = 2017, respondent = "H")
saveRDS(object = dictionary, file = "survey-processed/NHTS/2017/NHTS_2017_H_dictionary.rds")
fusionData::compileDictionary()
#----------------

# Save data to disk (.fst)
fst::write_fst(x = h.final, path = "survey-processed/NHTS/2017/NHTS_2017_H_processed.fst", compress = 100)
