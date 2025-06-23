  library(tidyverse)
  library(fusionData)
  library(readxl)
  
  source("R/utils.R")

  #######################
  d <- read.csv('survey-raw/FAPS/2013/faps_individual_puf.csv') %>%
    mutate_all(~replace(., . == 'V', -996)) %>% 
    mutate_all(~replace(., . == 'R', -998)) %>%
    mutate_all(~replace(., . == 'D', -997)) %>%
    mutate_all(~replace(., . == 'M', -997)) %>%
    mutate_all(~replace(., . == 'E', '.')) %>%
    
  rename_all(toupper) %>% select(-SCHBREAK_FLAG,-SCHBREAKSTARTYR,-SCHBREAKENDYR) %>%
                          select(-contains('FLAG')) %>%
    
    mutate(USBORN = ifelse(USBORN == "",".",USBORN),
           BMICAT = ifelse(BMICAT == "",".",BMICAT),
           HOMETENURE = ifelse(HOMETENURE == "",".",HOMETENURE),
           WORKCOMMUTETIME_R = ifelse(WORKCOMMUTETIME_R == "",".",WORKCOMMUTETIME_R),
           USCITIZEN = ifelse(USCITIZEN == "",".",USCITIZEN))
  
  colnames(d) <- trimws(colnames(d))
  
  #Create codebook for demographics
  codebook <- read_excel('survey-raw/FAPS/2013/2_Individual Codebook PUF.xlsx', skip = 25) %>%
   mutate(blocks = NA_integer_) %>%
    rename(var = "4.  Variable-by-Variable Descriptions") %>%
    mutate(blocks = if_else(!is.na(var) & is.na(lag(var)), cumsum(!is.na(var)), blocks)) %>%
    fill(blocks) %>% select(var,blocks,everything()) %>% 
  
      mutate(var = gsub("Variable:","",var)) %>%
    mutate(var = ifelse(grepl('Interview',var), lag(var), var)) %>% 
    mutate(var = ifelse(grepl('Feedback Form',var), lag(var), var)) %>% 
    
    
    mutate(desc = ifelse(grepl("Definition:",...6),...6,NA)) %>% rename(value = '...6') %>%
    mutate(value = ifelse(blocks == 53,...4,value),
           value = ifelse(blocks == 196,...7,value)) %>%
    select(-c("...2",   "...3"  , "...4"  , "...5")) %>%
             
      mutate(range = ifelse(grepl("Range:",value),value,NA),
           missing = ifelse(grepl("Missing observations:",value),value,NA),
           unique = ifelse(grepl("Unique values:",value),value,NA)) %>%
  
    mutate(value = ifelse(grepl("Range:|Missing observations:|Unique values:|Definition:|Note:|Source:",value),NA,value)) %>%
    rename(count = '...10') %>%
    mutate(value = gsub('Value|Valu e',NA,value),
           count = gsub('Count',NA,count)) %>%
      rename(label = '...19') %>% 
    mutate(label = ifelse(blocks == 32,...17,label),
           label = ifelse(blocks == 24,...18,label),
           label = ifelse(blocks == 53,...14,label)) %>%
    select(var,blocks,value,count,label,desc,range,missing,unique) %>%
    
    mutate(label  = gsub('Value description',"",label),
           desc = gsub('Definition:',"",desc),
           range = gsub('Range:','',range),
           unique = gsub('Unique values:','',unique),
           missing = gsub('Missing observations:','',missing)) %>%
      fill(var) %>% mutate(var = trimws(var)) %>%
    
    mutate(value = ifelse((var == 'HOMETENURE'|var == 'USBORN'|var == 'USCITIZEN') & label == "No",0,
                   ifelse((var == 'HOMETENURE'|var == 'USBORN'|var == 'USCITIZEN') & label == "Yes",1,
                   ifelse((var == 'HOMETENURE'|var == 'USBORN'|var == 'USCITIZEN') & (label == "Missing but applicable"|label == "Missing"),'.', 
                   ifelse((var == 'HOMETENURE'|var == 'USBORN'|var == 'USCITIZEN') & grepl("know$",label),-997, 
                   ifelse((var == 'HOMETENURE'|var == 'USBORN'|var == 'USCITIZEN') & label == "Refused",-998,
                   ifelse((var == 'HOMETENURE'|var == 'USBORN'|var == 'USCITIZEN') & label == "Valid skip",-996, value))))))) %>%
    
    mutate(desc = ifelse(blocks == 196,"Years in current address (recoded)",desc)) %>% 
    mutate(desc = ifelse(var == 'HOMETENURE', "Individual has always lived at this address",desc),
           desc = ifelse(var == 'USBORN', "Whether born in the United States",desc),
           desc = ifelse(var == 'USCITIZEN', "Individual is a U.S. citizen (Y/N)",desc)) %>% 

    group_by(var) %>% fill(desc) %>%
    
    group_by(var) %>%
    filter(!(value == 'NA' & is.na(range))) %>%
    mutate(count = ifelse(lag(value == 'N'), value, count)) %>% 
    mutate(value = ifelse(is.na(value),range,value)) %>% filter(value != 'N') %>% 
    group_by(var) %>% fill(desc) %>% 
    select(var,value,label,desc) %>% 
                
    mutate(var = trimws(var))  %>%
    
    mutate(desc = str_replace(desc, "Universe:*", ""),
           desc = ifelse(var == "SCHLEVEL_R","Level of school that student attends, or reason not attending",desc)) %>%
    
    filter(!grepl("_FLAG",var)) %>%
    
    filter(!is.na(desc)) %>%
    
   mutate(label = ifelse(grepl("AGE_R|NDINNERSOUT|SCHBRKFSTDAYS|SCHLUNCHDAYS|CHILDCAREMEAL|CHILDCARESNACK" ,var) & is.na(label),value,label)) %>% ungroup() %>%
    
##Add manual entries    
    add_row(.,var = 'INCRETDISINDAVG_R', desc = "Individual has RETDIS income last month, reported or average imputed (Y/N)",label = 'No', value = '0') %>%
    add_row(.,var = 'INCRETDISINDAVG_R', desc = "Individual has RETDIS income last month, reported or average imputed (Y/N)",label = 'Yes', value = '1')  %>%
    add_row(.,var = 'INCRETDISINDAVG_R', desc = "Individual has RETDIS income last month, reported or average imputed (Y/N)",label = 'Valid skip', value = '-996')  %>%    
                   
    add_row(.,var = 'INCEARNINDAVG_R', desc = "Individual has gross earnings last month, reported or average imputed (Y/N)",label = 'No', value = '0') %>%
    add_row(.,var = 'INCEARNINDAVG_R', desc = "Individual has gross earnings last month, reported or average imputed (Y/N)",label = 'Yes', value = '1')  %>%
    add_row(.,var = 'INCEARNINDAVG_R', desc = "Individual has gross earnings last month, reported or average imputed (Y/N)",label = 'Valid skip', value = '-996')  %>%    
    
    
      
       mutate(label = ifelse(value %in% c('-997','-998','-994'),"Not applicable",
                             ifelse(value %in% c('-996'),"Valid skip",  label))) %>%
    
    mutate(label = ifelse(var == "HHNUM",'Unique household indentifier',label)) %>%
  
    mutate(label = ifelse(  grepl("^Don.t know$",label) |  grepl("Missing",label) | 
                            grepl("Refused",label) | grepl("^Don.t Know$", label), "Not applicable", label)) 
  
  skip.vars <- codebook %>% 
    filter(label == "Valid skip")  %>%
    pull(var) %>%
    unique() %>% trimws()
  
  skip.values <- list(
  
  EDUCCAT = "10th grade or less",                  
  MARITAL   = "Never married",                                 
  SPOUSEHHMEM    = "No",                    
  EMPLOYMENT    =    "Less than 16 years old",                      
  WORKCOMMUTETIME_R     = 0,         
  REASONNOWORK    = "Already working",                         
  WORKGETFOOD     = "No",  
  SCHLEVEL_R     =  "Older",                          
  SCHTYPE        =  "Not in school",      
  SCHOPEN     = "Not in school",
  SCHBREAKSTARTMO  = "Not in school",
  SCHBREAKSTARTDY = "Not in school",
  SCHBREAKSTARTYR = "Not in school",
  SCHBREAKENDMO = "Not in school",
  SCHBREAKENDDY = "Not in school",
  SCHBREAKENDYR = "Not in school",
  SCHLUNCH = "Not in school",
  SCHLUNCHDAYS = "Not in school",
  SCHLUNCHCOST = "Not in school",
  SCHBRKFSTRECVD = "Not in school",
  SCHBRKFSTDAYS = "Not in school",
  SCHBRKFSTCOST = "Not in school",
  AFTERSCHOOLCARE = "Not in school",
  AFTERSCHOOLSNACK = "Not in school",
  SUMMERPGMMEAL = "Not in school",
  CHILDCARE_R = "No childcare",
  CHILDCAREMEAL = "No childcare",
  CHILDCARESNACK = "No childcare",
  BREASTFEEDING = "No children",
  WICIND = "No WIC",
  WICTYPE = "No WIC",
  WICTIME = "No WIC",
  WICTIMEUNIT = "No WIC",
  BMI =  "Not relevant",
  BMIPCT = "Not relevant age group",
  ANYINCOMEIND  =  'Not working',
  INCUNEMPINDAVG_R = "No imputation",
  INCTRANSFERINDAVG_R = "No imputation",
  INCINVESTINDAVG_R = "No imputation",
  INCOTHERINDAVG_R = "No imputation",
  BMICAT = "Not relevant",
  SCHLEVEL_R = "Older",
  YEARSRESIDENCE =  "Age less than 18",
  HOMETENURE =  "Age less than 18",
  USBORN =  "Age less than 18",
  USCITIZEN = "Not in the US",
  INCRETDISINDAVG_R =  "Age less than 18 or guest",
  INCEARNINDAVG_R =  "Age less than 18 or guest")
  
  # Safety check for missing entries in 'skip.values'
  miss <- noquote(setdiff(skip.vars, names(skip.values)))
  extras <- noquote(setdiff(names(skip.values), skip.vars))
  stopifnot(length(miss) == 0 & length(extras) == 0)
  
  # Update "Not applicable" values for each variable
  for (v in skip.vars) {
    if (!is.null(skip.values[[v]])) {
      ind <- which(codebook$var == v & codebook$label == "Valid skip")  # Update the "Valid skip" label to specified label
      codebook$label[ind] <- skip.values[[v]]
    }
  }
  
  stopifnot(!any(codebook$label == "Valid skip", na.rm = TRUE))
  
  
  na.vars <- codebook %>% 
    filter(label == "Not applicable")  %>%
    pull(var) %>%
    unique() %>% trimws()
  
  na.values <- list(
    
    AGE_R  = NA,
  #  SPOUSEHHMEM = NA,
    WORKCOMMUTETIME_R  = NA,
    REASONNOWORK = NA,
    WORKGETFOOD  = NA,
 #   SCHLEVEL_R   = NA,
  #  SCHTYPE = NA,
  #  SCHBRKFSTRECVD = NA,
  # BREASTFEEDING       = NA,
    BMI  = NA,
    BMIPCT = NA,
    BMICAT = NA,
    #INCUNEMPINDAVG_R = NA,
    #INCTRANSFERINDAVG_R = NA,
    #INCINVESTINDAVG_R   = NA,
    #INCOTHERINDAVG_R = NA,
  
  SEX = NA,
  HISPANIC  = NA,
  EDUCCAT = NA,
  RELATION_R = NA,
  RACECAT_R = NA,
  MARITAL = NA,
  EMPLOYMENT = NA,
  SCHOPEN = NA,
  SCHBREAKSTARTMO  = NA,
  SCHBREAKSTARTDY = NA,
  SCHBREAKSTARTYR = NA,
  SCHBREAKENDMO = NA,
  SCHBREAKENDDY = NA,
  SCHBREAKENDYR = NA,
  SCHLUNCH = "Not in school",
  SCHLUNCHDAYS = "Not in school",
  SCHLUNCHCOST = NA,
  SCHBRKFSTDAYS = NA,
  SCHBRKFSTCOST = NA,
  AFTERSCHOOLCARE = NA,
  AFTERSCHOOLSNACK = NA,
  SUMMERPGMMEAL = NA,
  CHILDCARE_R = "No childcare",
  CHILDCAREMEAL = "No childcare",
  CHILDCARESNACK = "No childcare",
  WICIND = "No WIC",
  WICTYPE = "No WIC",
  WICTIME = "No WIC",
  WICTIMEUNIT = "No WIC",
  NDINNERSOUT = NA,
  LACTOSEINTOL = "No",
  FOODALLERGY = "No",
  HEALTHSTATUS = NA,
  TOBACCO = "No",
  ANYINCOMEIND  =  NA,
  YEARSRESIDENCE = NA,
 USCITIZEN = NA,
 USBORN = NA,
 HOMETENURE = NA)
  
  
  # Safety check for missing entries in 'na.values'
  miss <- noquote(setdiff(na.vars, names(na.values)))
  extras <- noquote(setdiff(names(na.values), na.vars))
  stopifnot(length(miss) == 0 & length(extras) == 0)
  
  
  # Update "Not applicable" values for each variable
  for (v in na.vars) {
    if (!is.null(na.values[[v]])) {
      ind <- which(codebook$var == v & codebook$label == "Not applicable")  # Update the "Not applicable" label to specified label
      codebook$label[ind] <- na.values[[v]]
    }
  }
  
  stopifnot(!any(codebook$label == "Not applicable", na.rm = TRUE))
  
  
  ordered.factors <- list(
    
    EDUCCAT = c("10th grade or less",'11th or 12th grade, no diploma', 'H.S. diploma, GED or equivalent',
               'Some college or associate’s degree','Bachelor’s degree',"Master's degree and above"),
               
    MARITAL = c('Never married','Separated','Divorced','Widowed','Married'),
    
    SCHLEVEL_R = c('Not old enough','Kindergarten','Elementary school','Primary school','Middle school or junior high',
                   'High school','Some other school','On school break','Summer vacation','Home-schooled','Dropped out, disabled, or other reason not in school','Older'),
    
    SCHLUNCH = c('Not in school','No','Yes'),
    SCHLUNCHDAYS = c('Not in school','0','1','2','3','4','5'),
    SCHLUNCHCOST = c('Not in school','Free','Reduced price','Full price'),
    SCHBRKFSTRECVD = c('Not in school','Child’s school does not serve breakfast','Child receives breakfast at school'),
    SCHBRKFSTDAYS = c('Not in school','0','1','2','3','4','5'),
    
    SCHBRKFSTCOST = c('Not in school','Free','Reduced price','Full price'),
    AFTERSCHOOLSNACK = c('Not in school','No','Yes'),
    
    SUMMERPGMMEAL = c('Not in school','Yes, gets free meal','No, gets food but it’s not free','No, does not get food','Does not attend a summer program'),
    CHILDCAREMEAL = c('No childcare','0','1','2','3','4','5','6','7','8','10','15','16','21'),
    CHILDCARESNACK = c('No childcare','0','1','2','3','4','5','6','7','8','10','12','15'),
    
    HEALTHSTATUS = c('Excellent','Very good','Good','Fair','Poor'),
    
    BMICAT = c("Not relevant",'Not overweight','Overweight','Obese'),
    
    YEARSRESIDENCE = c('Age less than 18','0 to 2 yrs','3 to 5 yrs','6 to 10 yrs','11 to 15 yrs','16 to 20 yrs',
                       '21 or more yrs'))
    
    
  # Detect any variables in 'ordered.factors' that are NOT in the codebook
  extras <- noquote(setdiff(names(ordered.factors), codebook$var))
  stopifnot(length(extras) == 0)
  

  # Update variable values with associated labels from 'codebook'
  # Loop through each variable in 'd', assigning labels when applicable
  for (v in names(d)) {
    
    cb <- filter(codebook, var == v)
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
        x <- factor(x, levels = intersect(ordered.factors[[v]], x), ordered = TRUE)
        stopifnot(sum(is.na(x)) == num.na)  # This is a final safety check to ensure no NA's introduced inadvertently
    }
    
    # Apply type.convert() to 'x'; leave ordered factors unchanged
    x <- if (is.ordered(x)) x else type.convert(x, as.is = FALSE)
    
    # Ensure unordered factor levels are sorted according to codebook order of levels
    # Originally, unordered factors were sorted alphabetically -- but there is often useful information in the codebook ordering
    # This retains a valid codebook ordering if one exists; otherwise sort levels alphabetically
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
  # Impute NA values in 'd'
  # Detect structural dependencies
  # Which variables have missing values and how frequent are they?
  na.count <- colSums(is.na(d))
  na.count <- na.count[na.count > 0]
  na.count  # See which variables have NA's
  
  
 d <- fusionModel::impute(data = as.data.table(d),
                       weight = "HHWGT",
                       ignore = c('BMI','BMIPCT'))

  codebook <- codebook %>% mutate(var = tolower(var),
                                  var = trimws(var))
  
  # Add/create variables for geographic concordance with variables in 'geo_concordance.fst'
  # Assemble final output
  # NOTE: var_label assignment is done AFTER any manipulation of values/classes, because labels can be lost when classes are changed
  h.final <- d %>%
    rename_with(tolower) %>% 
    mutate_if(is.factor, safeCharacters) %>%
    mutate_if(is.numeric, convertInteger) %>%
    mutate_if(is.double, cleanNumeric, tol = 0.001) %>%
    labelled::set_variable_labels(.labels = setNames(as.list(safeCharacters(codebook$desc)), codebook$var), .strict = FALSE) %>%  # Set descriptions for codebook variables
    rename(
      hid = hhnum,
      pid = pnum,# Rename ID and weight variables to standardized names
      weight = hhwgt
    ) %>%
     # Convert all variable names to lowercase
    select(hid,pid,everything()) %>%   # Reorder columns with replicate weights at the end
    arrange(hid) 
  #----------------
  
  #rm(d)
  labelled::var_label(h.final$incearnindavg_r) <- "Individual has gross earnings last month, reported or average imputed"
  labelled::var_label(h.final$incretdisindavg_r) <- "Individual has RETDIS income last month, reported or average imputed (Y/N)"

  # Create dictionary and save to disk
  dictionary <- createDictionary(data = h.final, survey = "FAPS", vintage = 2013, respondent = "P")
  saveRDS(object = dictionary, file = "survey-processed/FAPS/2013/FAPS_2013_P_dictionary.rds")
  
  #----------------
  
  # Save data to disk (.fst)
  fst::write_fst(x = h.final, path = "survey-processed/FAPS/2013/FAPS_2013_P_processed.fst", compress = 100)
  fst::write_fst(x = codebook, path = "survey-processed/FAPS/2013/FAPS_2013_P_codebook.fst", compress = 100)
  
  compileDictionary()

d_r <- d %>% filter(RELATION_R == "Respondent") %>% select(HISPANIC,RACECAT_R,SEX,MARITAL,EDUCCAT,SCHLEVEL_R,SCHTYPE,
                                                           EMPLOYMENT,WORKCOMMUTETIME_R,YEARSRESIDENCE,
                                                           USBORN,USCITIZEN,HOMETENURE,HHNUM) %>% rename_all(tolower)

codebook_r <- codebook %>% filter(var %in% colnames(d_r)) %>%
              mutate(desc = paste0(desc," for the household respondent"),
                     var = paste0(var,'_R')) 
  
fst::write_fst(x = codebook_r , path = "survey-processed/FAPS/2013/FAPS_2013_HR_codebook.fst", compress = 100)

d_r1 <- d_r %>%
  rename_with(~ paste0(., "_R"), !matches("hhnum"))

fst::write_fst(x = d_r1, path = "survey-processed/FAPS/2013/FAPS_2013_HR_processed.fst", compress = 100)


