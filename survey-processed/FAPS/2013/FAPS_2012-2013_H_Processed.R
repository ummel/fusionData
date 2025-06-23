library(tidyverse)
library(fusionData)
library(readxl)

source("R/utils.R")

#######################
d_0 <- read.csv('survey-raw/FAPS/2013/faps_household_puf.csv') %>%
  rename_all(toupper) %>% rename(HHNUM = HHNUM) %>%
  
  mutate(INCHHREPORTED_R = round(12* INCHHREPORTED_R, digits = 0),      #Convert monthly amount to annual
         INCFAMREPORTED_R = round(12* INCFAMREPORTED_R, digits = 0)) %>% 
    select(-contains('_U'))# %>% mutate(INCHHREPORTED_R = ifelse(INCHHREPORTED_R == '291200', "More than 291000",INCHHREPORTED_R),
                                  #     INCFAMREPORTED_R = ifelse(INCFAMREPORTED_R == '280678', "More than 280000",INCFAMREPORTED_R))

#Read nutrient files
d_fafh <- fst::read_fst(path = "survey-processed/FAPS/2013/FAPS_2013_sumFAFHN_processed.fst") %>%
  rename(HHNUM  = 'hid') %>%
  rename_all(toupper) 

d_fah <- fst::read_fst(path = "survey-processed/FAPS/2013/FAPS_2013_sumFAHN_processed.fst") %>%
  rename(HHNUM  = 'hid') %>%
  rename_all(toupper) 

d_hr <- fst::read_fst(path = "survey-processed/FAPS/2013/FAPS_2013_HR_processed.fst") %>%
  rename_all(toupper) 

codebook_fafh <- fst::read_fst(path = "survey-processed/FAPS/2013/FAPS_2013_sumFAHN_codebook.fst")
codebook_fah <- fst::read_fst(path = "survey-processed/FAPS/2013/FAPS_2013_sumFAHN_codebook.fst")
codebook_hr <- fst::read_fst(path = "survey-processed/FAPS/2013/FAPS_2013_HR_codebook.fst")


#Create codebook for demographics
codebook_h <- read_excel('survey-raw/FAPS/2013/1_Household Codebook PUF.xlsx', skip = 325) %>%
  mutate(blocks = NA_integer_) %>%
  rename(var = "4. Variable-by-Variable Codebook") %>%
  mutate(blocks = if_else(!is.na(var) & is.na(lag(var)), cumsum(!is.na(var)), blocks)) %>%
  fill(blocks) %>% select(var,blocks,everything()) %>% select(-c("...2",   "...3"  , "...4"  , "...5" ,  "...6"  ,
                                                                 "...7" ,  "...8"  , "...9"   ,"...10" , "...11" ,
                                                                 "...12" , "...13" ,"...14",'...16','...17',
                                                                 "...19" , "...20" ,"...21",'...22','...23','...24',
                                                                 "...25" , "...26" ,"...27",'...28','...30',
                                                                 "...32" , "...33" ,"...34",'...35','...36',
                                                                 "...37" , "...38" ,"...39",'...40','...41',
                                                                 "...42" , "...43" ,"...44",'...45')) %>%
  
  mutate(...15 = ifelse(grepl('BENEST',var), "Definition: Sum of estimated monthly SNAP benefits for all eligible SNAP units in household in model run", ...15)) %>% 
  
  filter(!(is.na(...15))) %>% mutate(var = gsub("Variable:","",var)) %>%
  mutate(var = ifelse(grepl('Interview',var), lag(var), var)) %>% 
  mutate(var = ifelse(grepl('Feedback Form',var), lag(var), var)) %>% 
  
  mutate(desc = ifelse(grepl("Definition:",...15),...15,NA)) %>% rename(value = '...15') %>%
  mutate(value = ifelse(value %in% c('Range:','Unique values:','Missing observations:'), paste0(value,...29),value)) %>%
  mutate(range = ifelse(grepl("Range:",value),value,NA),
         missing = ifelse(grepl("Missing observations:",value),value,NA),
         unique = ifelse(grepl("Unique values:",value),value,NA)) %>%
  
  mutate(value = ifelse(grepl("Range:|Missing observations:|Unique values:|Definition:|Note:|Source:",value),NA,value)) %>%
  rename(count = '...18') %>%
  mutate(value = gsub('Value',NA,value),
         count = gsub('Count',NA,count)) %>%
  rename(labelx = '...31') %>% select(var,blocks,value,count,labelx,desc,range,missing,unique) %>%
  
  mutate(labelx  = gsub('Value description',"",labelx),
         desc = gsub('Definition:',"",desc),
         range = gsub('Range:','',range),
         unique = gsub('Unique values:','',unique),
         missing = gsub('Missing observations:','',missing)) %>%
  fill(var) %>% 
  group_by(var) %>% fill(desc) %>%
  
  group_by(var) %>%
  filter(!(value == 'NA' & is.na(range))) %>%
  mutate(count = ifelse(lag(value == 'N'), value, count)) %>% 
  mutate(value = ifelse(is.na(value),range,value)) %>% filter(value != 'N') %>% 
  group_by(var) %>% fill(desc) %>% rename(label = labelx) %>%
  select(var,value,label,desc) %>%
  mutate(var = trimws(var),
         value = trimws(value),
         label = trimws(label)) %>% 
  filter(!grepl('Results from match of household members with SNAP administrative data.',value)) %>%
  filter(!grepl('See note to MEALGUESTANY',value)) %>%
  
  mutate(label = ifelse(grepl("NGUEST|MEALGUEST|FOODSTORENUM|NDINNERSOUTHH",var) & is.na(label),value,label)) %>%
  
  mutate(label = ifelse((var == 'PRIMSTOREEDIT_TYPE'|  var == 'PRIMSTOREEDIT_FILLTYPE'|  var == 'ALTSTOREEDIT_TYPE'|  var == 'ALTSTOREEDIT_FILLTYPE') & value == '0','edit made to primstore name',label),
         label = ifelse((var == 'PRIMSTOREEDIT_TYPE'|  var == 'PRIMSTOREEDIT_FILLTYPE'|  var == 'ALTSTOREEDIT_TYPE'|  var == 'ALTSTOREEDIT_FILLTYPE') & value == '.','no edit to primstore name',label),
         value = ifelse((var == 'PRIMSTOREEDIT_TYPE'|  var == 'PRIMSTOREEDIT_FILLTYPE'|  var == 'ALTSTOREEDIT_TYPE'|  var == 'ALTSTOREEDIT_FILLTYPE') & value == '.',NA,value)) %>%
  
  mutate(desc = str_replace_all(desc,"(primary store was selected from drop-down & PRIMSTORESNAPTYPE ≠ \"LG,\"\"SM,\"\"SS\") or (primary store was open- ended text & PRIMSTORETYPEREPORT ≠ 1). Note: PRIMSTORETYPEREPORT is available in the restricted use file only","")) %>%
  mutate(desc = str_replace_all(desc,"(primary store was selected from drop-down & PRIMSTORESNAPTYPE ≠ \"LG,\",\"SM,\"\"SS\") or (primary store was open- ended text & PRIMSTORETYPEREPORT ≠ 1). Note: PRIMSTORETYPEREPORT is available in the restricted use file only","")) %>%
  
  mutate(desc = str_replace(desc, "Universe:*", ""),
         desc = str_replace(desc, "primary store was selected*",""),
         label = str_replace(label,"\\(Consent for data matching not given\\)","")) %>%
  
  mutate(label = ifelse(var %in% c('FEEDBACK4_8','FEEDBACK4_7','FEEDBACK4_6',
                                   'FEEDBACK4_5','FEEDBACK4_4','FEEDBACK4_3','FEEDBACK4_2','FEEDBACK4_1') & (value == "0"),'Not checked',label),
         
         label = ifelse(var %in% c('FEEDBACK4_8','FEEDBACK4_7','FEEDBACK4_6',
                                   'FEEDBACK4_5','FEEDBACK4_4','FEEDBACK4_3','FEEDBACK4_2','FEEDBACK4_1') & (value == "1"),'Checked',label),
         
         label = ifelse(var %in% c('FEEDBACK4_8','FEEDBACK4_7','FEEDBACK4_6',
                                   'FEEDBACK4_5','FEEDBACK4_4','FEEDBACK4_3','FEEDBACK4_2','FEEDBACK4_1') & (value == "-999"),'Did not complete the Feedback Form',label),
         
         label = ifelse(var %in% c('SNAPNOWADMIN') & (value == "-996"),'Valid skip',label)) %>% ungroup() %>%
  
  mutate(label = ifelse(var == "WHYNOTSUPERMKT1" & value == '0','Not identified as reason primary store is not a supermarket',label),
         label = ifelse(     var == "WHYNOTSUPERMKT1" & value == '1','Identified as reason primary store is not a supermarket', label),
         label = ifelse(   var == "WHYNOTSUPERMKT1" & value == '-996','Valid skip', label),
         label = ifelse(   var == "WHYNOTSUPERMKT1" & value == '-997',"Don't know", label)) %>%
  
  add_row(.,var = 'INCHHREPORTED_R', desc = "Total monthly household income, excluding imputed amounts (top-coded)", label = 'More than 291000', value = 'More than 291000') %>%
  add_row(.,var = 'INCFAMREPORTED_R', desc = "Total monthly family income, excluding imputed amounts (top-coded)", label = 'More than 286000', value = 'More than 286000') %>%
  
  
  add_row(.,var = 'INCWORKSHEET', desc = "Income worksheet was completed prior to final interview (Y/N)", label = NA, value = '-997') %>%
  add_row(.,var = 'INCWORKSHEET', desc = "Income worksheet was completed prior to final interview (Y/N)", label = NA, value = '-998') %>%
  add_row(.,var = 'EXPRENTMRTG_R', desc = "Household's monthly rent/mortgage expense (top-coded)", label = '0', value = '-998') %>%
  add_row(.,var = 'EXPRENTMRTG_R', desc = "Household's monthly rent/mortgage expense (top-coded)", label = '0', value = '-996') %>%
  add_row(.,var = 'EXPRENTMRTG_R', desc = "Household's monthly rent/mortgage expense (top-coded)", label = '0', value = '-997') %>%
  
  add_row(.,var = 'EXPHOMEINS_R', desc = "Household's monthly rent/mortgage expense (top-coded)", label = NA, value = '-998') %>%
  add_row(.,var = 'EXPHOMEINS_R', desc = "Household's monthly rent/mortgage expense (top-coded)", label = NA, value = '-997') %>%
  
  add_row(.,var = 'EXPPROPTAX_R', desc = "Household's monthly property taxes (top-coded)", label = NA, value = '-998') %>%
  add_row(.,var = 'EXPPROPTAX_R', desc = "Household's monthly property taxes (top-coded)", label = NA, value = '-997') %>%
  
  add_row(.,var = 'EXPPUBTRANS_R', desc = "Household's monthly public transport expense (top-coded)", label = NA, value = '-998') %>%
  add_row(.,var = 'EXPPUBTRANS_R', desc = "Household's monthly public transport expense (top-coded)", label = NA, value = '-997') %>%
  
  add_row(.,var = 'EXPELECTRIC_R', desc = "Household's monthly electricity expense (top-coded)", label = NA, value = '-998') %>%
  add_row(.,var = 'EXPELECTRIC_R', desc = "Household's monthly electricity expense (top-coded)", label = NA, value = '-997') %>%
  
  add_row(.,var = 'EXPADULTCARE_R', desc = "Household's monthly adult care expense (top-coded)", label = NA, value = '-998') %>%
  add_row(.,var = 'EXPADULTCARE_R', desc = "Household's monthly adult care expense (top-coded)", label = NA, value = '-997') %>%
  
  add_row(.,var = 'EXPCHILDSUPPORT_R', desc = "Household's monthly child support expense (top-coded)", label = NA, value = '-998') %>%
  add_row(.,var = 'EXPCHILDSUPPORT_R', desc = "Household's monthly child support expense (top-coded)", label = NA, value = '-997') %>%
  
  add_row(.,var = 'EXPCHILDCARE_R', desc = "Household's monthly child care expense (top-coded)", label = NA, value = '-998') %>%
  add_row(.,var = 'EXPCHILDCARE_R', desc = "Household's monthly child care expense (top-coded)", label = NA, value = '-997') %>%
  
  add_row(.,var = 'EXPRX_R', desc = "Household's monthly prescription drug expense (top-coded)", label = NA, value = '-998') %>%
  add_row(.,var = 'EXPRX_R', desc = "Household's monthly prescription drug expense (top-coded)", label = NA, value = '-997') %>%
  
  add_row(.,var = 'EXPCOPAY_R', desc = "Household's monthly health insurance copays (top-coded)", label = NA, value = '-998') %>%
  add_row(.,var = 'EXPCOPAY_R', desc = "Household's monthly health insurance copays (top-coded)", label = NA, value = '-997') %>%
  
  add_row(.,var = 'EXPDOCTOR_R', desc = "Household's monthly doctor/hospital bills (top-coded)", label = NA, value = '-998') %>%
  add_row(.,var = 'EXPDOCTOR_R', desc = "Household's monthly doctor/hospital bills (top-coded)", label = NA, value = '-997') %>%
  
  add_row(.,var = 'EXPHEALTHINS_R', desc = "Household's monthly health insurance expense (top-coded)", label = NA, value = '-998') %>%
  add_row(.,var = 'EXPHEALTHINS_R', desc = "Household's monthly health insurance expense (top-coded)", label = NA, value = '-997') %>%
  
  add_row(.,var = 'EXPWASTEDISP_R', desc = "Household's monthly sewer/garbage removal expense (top-coded)", label = NA, value = '-998') %>%
  add_row(.,var = 'EXPWASTEDISP_R', desc = "Household's monthly sewer/garbage removal expense (top-coded)", label = NA, value = '-997') %>%
  
  add_row(.,var = 'EXPHEATFUEL_R', desc = "Household's monthly heating fuel expense (top-coded)", label = NA, value = '-998') %>%
  add_row(.,var = 'EXPHEATFUEL_R', desc = "Household's monthly heating fuel expense (top-coded)", label = NA, value = '-997') %>%
  
  add_row(.,var = 'SNAPDAYS_INITIAL', desc = "Days since last SNAP received, at Initial Interview; 1st, 2nd, 3rd, 4th, 5th, 6th, 7th days of the food reporting week; and at the Final Interview", label = 'Valid skip', value = '-996') %>%
  add_row(.,var = 'SNAPDAYS1', desc = "Days since last SNAP received, at Initial Interview; 1st, 2nd, 3rd, 4th, 5th, 6th, 7th days of the food reporting week; and at the Final Interview", label = 'Valid skip', value = '-996') %>%
  add_row(.,var = 'SNAPDAYS2', desc = "Days since last SNAP received, at Initial Interview; 1st, 2nd, 3rd, 4th, 5th, 6th, 7th days of the food reporting week; and at the Final Interview", label = 'Valid skip', value = '-996') %>%
  add_row(.,var = 'SNAPDAYS3', desc = "Days since last SNAP received, at Initial Interview; 1st, 2nd, 3rd, 4th, 5th, 6th, 7th days of the food reporting week; and at the Final Interview", label = 'Valid skip', value = '-996') %>%
  add_row(.,var = 'SNAPDAYS4', desc = "Days since last SNAP received, at Initial Interview; 1st, 2nd, 3rd, 4th, 5th, 6th, 7th days of the food reporting week; and at the Final Interview", label = 'Valid skip', value = '-996') %>%
  add_row(.,var = 'SNAPDAYS5', desc = "Days since last SNAP received, at Initial Interview; 1st, 2nd, 3rd, 4th, 5th, 6th, 7th days of the food reporting week; and at the Final Interview", label = 'Valid skip', value = '-996') %>%
  add_row(.,var = 'SNAPDAYS6', desc = "Days since last SNAP received, at Initial Interview; 1st, 2nd, 3rd, 4th, 5th, 6th, 7th days of the food reporting week; and at the Final Interview", label = 'Valid skip', value = '-996') %>%
  add_row(.,var = 'SNAPDAYS7', desc = "Days since last SNAP received, at Initial Interview; 1st, 2nd, 3rd, 4th, 5th, 6th, 7th days of the food reporting week; and at the Final Interview", label = 'Valid skip', value = '-996') %>%
  add_row(.,var = 'SNAPDAYS_FINAL', desc = "Days since last SNAP received, at Initial Interview; 1st, 2nd, 3rd, 4th, 5th, 6th, 7th days of the food reporting week; and at the Final Interview", label = 'Valid skip', value = '-996') %>%
  
  add_row(.,var = 'SNAPLASTAMT', desc = "Last reported SNAP amount compared to usual amount", label = 'Valid skip', value = '-996') %>%
  add_row(.,var = 'SNAPLASTAMT_R', desc = "Last reported SNAP amount compared to usual amount", label = 'Valid skip', value = '-996') %>%
  add_row(.,var = 'SNAPLASTAMT_R', desc = "Last reported SNAP amount compared to usual amount", label = 'Valid skip', value = '-997') %>%
  add_row(.,var = 'SNAPLASTAMT_R', desc = "Last reported SNAP amount compared to usual amount", label = 'Valid skip', value = '-998') %>%
  
  
  add_row(.,var = 'BENEST1_HH', desc = "Sum of estimated monthly SNAP benefits for all eligible SNAP units in household in model run 1", label = 'Valid skip', value = '-995') %>%
  add_row(.,var = 'BENEST2_HH', desc = "Sum of estimated monthly SNAP benefits for all eligible SNAP units in household in model run 2", label = 'Valid skip', value = '-995') %>%
  add_row(.,var = 'BENEST3_HH', desc = "Sum of estimated monthly SNAP benefits for all eligible SNAP units in household in model run 3", label = 'Valid skip', value = '-995') %>%
  add_row(.,var = 'BENEST4_HH', desc = "Sum of estimated monthly SNAP benefits for all eligible SNAP units in household in model run 4", label = 'Valid skip', value = '-995') %>%
  
  add_row(.,var = 'PRIMSTOREPLACEID', desc = "FoodAPS identifier for household's primary food store", label = 'Valid skip', value = '-996') %>%
  add_row(.,var = 'PRIMSTOREDIST_S', desc = "Straight-line distance, in miles, between residence and primary food store", label = NA, value = '-996') %>%
  add_row(.,var = 'PRIMSTOREDIST_D', desc = "Driving distance, in miles, between residence and primary food store", label = NA, value = '-996') %>%
  add_row(.,var = 'PRIMSTORETIME_D', desc = "Driving time, in minutes, between residence and primary food store", label = NA, value = '-996') %>%
  
  add_row(.,var = 'PRIMSTOREDIST_W', desc = "Walking distance, in miles, between residence and nearby primary food store", label = 'Valid skip', value = '-996') %>%
  add_row(.,var = 'PRIMSTORETIME_W', desc = "Walking time, in minutes, between residence and nearby primary food store", label = 'Valid skip', value = '-996') %>%
  
  add_row(.,var = 'PRIMSTORETRAVELCOST', desc = "One-way travel cost for getting to primary food store, in dollars", label = 'Non bus/taxi means to get to store', value = '-996') %>%
  add_row(.,var = 'PRIMSTORETRAVELTIME', desc = "One-way travel time to primary food store, in minutes", label = NA, value = '-997') %>%
  
  add_row(.,var = 'HEALTHYTIME', desc = "Respondent is too busy to take time to prepare healthy foods", label = NA, value = '-997') %>%
  add_row(.,var = 'NMEALSTOGETHER', desc = "During past 7 days, number of times family ate dinner together, at home or away", label = 'Single person household', value = '-996') %>%
  add_row(.,var = 'NMEALSTOGETHER', desc = "During past 7 days, number of times family ate dinner together, at home or away", label = 'Single person household', value = '-997') %>%
  
  add_row(.,var = 'NGUESTSNACKMON_R', desc = "Number of guests for a snack on Monday (top-coded)", label = 'No guests', value = '-996') %>%
  add_row(.,var = 'NGUESTSNACKMON_R', desc = "Number of guests for a snack on Monday (top-coded)", label = 'No guests', value = '-997') %>%
  
  add_row(.,var = 'ALTSTOREPLACEID', desc = "FoodAPS identifier for household's alternate food store", label = 'Valid skip', value = '-996') %>%
  add_row(.,var = 'ALTSTOREDIST_S', desc = "Straight-line distance, in miles, between residence and alternate food store", label = 'Valid skip', value = '-996') %>%
  
  add_row(.,var = 'ALTSTOREDIST_D', desc = "Driving distance, in miles, between residence and alternate food store", label = 'Valid skip', value = '-996') %>%
  
  add_row(.,var = 'ALTSTORETIME_D', desc = "Driving time, in minutes, between residence and alternate food store", label = 'Valid skip', value = '-996') %>%
  
  add_row(.,var = 'ALTSTOREDIST_W', desc = "Walking distance, in miles, between residence and nearby alternate food store", label = 'Valid skip', value = '-996') %>%
  
  add_row(.,var = 'ALTSTORETIME_W', desc = "Walking time, in minutes, between residence and nearby alternate food store", label = 'Valid skip', value = '-996') %>%
  
  mutate(label = ifelse(grepl("^Don.t know$",label) | grepl("Refused",label) |
                          grepl("^Don.t Know$", label), "Not applicable", label)) %>%
  
  mutate(label = ifelse(grepl("Missing but applicable",label) |grepl("Missing",label), "Valid skip", label)) 

###Read nutrient codebook
codebook_fah <- fst::read_fst(path = "survey-processed/FAPS/2013/FAPS_2013_sumFAHN_codebook.fst")
codebook_fafh <- fst::read_fst(path = "survey-processed/FAPS/2013/FAPS_2013_sumFAFHN_codebook.fst")
codebook_hr <- fst::read_fst(path = "survey-processed/FAPS/2013/FAPS_2013_HR_codebook.fst")


na.skip <- codebook_h %>% 
  filter(grepl("Valid skip",label))  %>%
  pull(var) %>%
  unique() %>% trimws()

skip.values <- list(
  
  HHSIZECHILD = 'No',
  HHSIZEMOVE = 'No',
  HHSIZECHANGEOTH = 'No',
  SELFEMPLOYFOODHH = 'No',
  JOBCHANGECAT = 'No',
  EARNLESSNUM_R = 'None',
  EARNMORENUM_R = 'None',
  EARNSAMENUM = 'None',
  HOUSINGPUB = 'Own house',
  HOUSINGSUB = 'Own house',
  VEHICLENUM = 0,
  CARACCESS = "Own car",
  SNAPEVER = "Yes, currently receiving",
  SNAP12MOS = 'No',
  SNAPLASTAMT = "No SNAP benefits",
  SNAPUSUALAMT = "No SNAP benefits",
  SCHSERVEBRKFST = "No children",
  ANYPREGNANT = "No",
  WICHH = "No",
  FOODSECUREQ4 = 'No',
  FOODSECUREQ5 = 0,
  FOODSECUREQ6 = 'No',
  FOODSECUREQ7 = 'No',
  FOODSECUREQ8 = 'No',
  FOODSECUREQ9 = 'No',
  FOODSECUREQ10 = 0,
  
  WHYNOTSUPERMKT2 ="Does not go to supermarket",
  WHYNOTSUPERMKT3 ="Does not go to supermarket",
  WHYNOTSUPERMKT4 ="Does not go to supermarket",
  WHYNOTSUPERMKT5 ="Does not go to supermarket",
  WHYNOTSUPERMKT6 ="Does not go to supermarket",
  WHYNOTSUPERMKT7 ="Does not go to supermarket",
  WHYNOTSUPERMKT8 ="Does not go to supermarket",
  WHYNOTSUPERMKT9 ="Does not go to supermarket",
  WHYNOTSUPERMKTOTH ="Does not go to supermarket",
  
  ALTSTOREREASON = "No alternate supermarket",
  FOODSTORENUM = 1,
  DIETSTATUSHH = 'Single person household',
  HEALTHYTASTEHH = "Single person hosuehold",
  MYPLATEFOLLOW = 'No',
  FOODPYRAMID = 'Other food pyramids',
  MYPYRAMIDSEARCH = 'No',
  MYPYRAMIDFOLLOW = 'No',
  BILLPAYPROB6MOS = "No",
  UTILNOTPAID6MOS = 'No',
  EVICTED6MOS = 'No',
  CASHADV6MOS = 'No',
  PAYDAYLOAN6MOS = 'No',
  NDINNERSOUTHH = 'Single person household',
  
  MEALGUESTDAYS = 0,
  MEALGUESTSUN = 'No',
  MEALGUESTMON = 'No',
  MEALGUESTTUE  = 'No',
  MEALGUESTWED = 'No',
  MEALGUESTTHU = 'No',
  MEALGUESTFRI = 'No',
  MEALGUESTSAT = 'No',
  
  BENEST1_HH = 0,
  BENEST2_HH = 0,
  BENEST3_HH = 0,
  BENEST4_HH = 0,
  
  NGUESTBRKFSTSUN_R = '0',
  NGUESTBRKFSTMON_R = '0',
  NGUESTBRKFSTTUE_R = '0',
  NGUESTBRKFSTWED_R = '0',
  NGUESTBRKFSTTHU_R = '0',
  NGUESTBRKFSTFRI_R = '0',
  NGUESTBRKFSTSAT_R = '0',
  NGUESTLUNCHSUN_R = '0',
  NGUESTLUNCHMON_R = '0',
  NGUESTLUNCHTUE_R = '0',
  NGUESTLUNCHWED_R = '0',
  NGUESTLUNCHTHU_R = '0',
  NGUESTLUNCHFRI_R = '0',
  NGUESTLUNCHSAT_R = '0',
  NGUESTDINNERSUN_R = '0',
  NGUESTDINNERMON_R = '0',
  NGUESTDINNERMON_R = '0',
  NGUESTDINNERTUE_R = '0',
  
  NGUESTDINNERWED_R = '0',
  NGUESTDINNERTHU_R = '0',
  NGUESTDINNERFRI_R = '0',
  NGUESTDINNERSAT_R = '0',
  NGUESTSNACKSUN_R = '0',
  NGUESTSNACKTUE_R = '0',
  NGUESTSNACKWED_R = '0',
  NGUESTSNACKTHU_R = '0',
  NGUESTSNACKFRI_R = '0',
  NGUESTSNACKSAT_R = '0',
  WHYNOTSUPERMKT1 = "Selected primary store as LG,SM,SS",
  SNAPNOWADMIN = "No consent given",
  PRIMSTORETYPE = NA,
  ALTSTORETYPE = NA,
  FEEDBACK1 = NA,
  FEEDBACK2 = NA,
  FEEDBACK3 = NA,
  FEEDBACK4_1 = NA,
  SNAPDAYS_INITIAL = "No SNAP benefits",
  SNAPDAYS1 = "No SNAP benefits",
  SNAPDAYS2 = "No SNAP benefits",
  SNAPDAYS3 = "No SNAP benefits",
  SNAPDAYS4 = "No SNAP benefits",
  SNAPDAYS5 = "No SNAP benefits",
  SNAPDAYS6 = "No SNAP benefits",
  SNAPDAYS7 = "No SNAP benefits",
  SNAPDAYS_FINAL = "No SNAP benefits",
  SNAPLASTAMT = "No SNAP benefits",
  SNAPLASTAMT_R = "No SNAP benefits",
  PRIMSTOREPLACEID = "No option chosen",
  ALTSTOREPLACEID = "No option chosen",
  PRIMSTOREDIST_W = 0,
  PRIMSTORETIME_W = 0,
  ALTSTOREDIST_W = 0,
  ALTSTORETIME_W = 0,
  ALTSTOREDIST_S = 0,
  ALTSTOREDIST_D = 0,
  ALTSTORETIME_D = 0) 

# Safety check for missing entries in 'na.values'

miss <- noquote(setdiff(na.skip, names(skip.values)))
extras <- noquote(setdiff(names(skip.values), na.skip))
stopifnot(length(miss) == 0 & length(extras) == 0)

# Update "Not applicable" values for each variable
for (v in na.skip) {
  if (!is.null(skip.values[[v]])) {
    ind <- which(codebook_h$var == v & codebook_h$label == "Valid skip")  # Update the "Valid skip" label to specified label
    codebook_h$label[ind] <- skip.values[[v]]
  }
}

stopifnot(!any(codebook_h$label == "Valid skip", na.rm = TRUE))

na.vars <- codebook_h %>% 
  filter(label == "Not applicable")  %>%
  pull(var) %>%
  unique() %>% trimws()

na.values <- list(
  
  HHSIZECHANGE = NA,
  SELFEMPLOYHH = NA,
  SELFEMPLOYFOODHH = NA,
  JOBCHANGEANY = NA,
  HOUSINGOWN = NA,
  HOUSINGPUB =  NA,
  HOUSINGSUB = NA,
  LIQASSETS = NA,
  ANYVEHICLE  = NA, 
  LARGEEXP  = NA, 
  SNAPNOWHH = NA,
  SNAPNOWREPORT = NA,
  SNAPEVER = NA,
  SNAPLASTAMT = NA,
  SNAPUSUALAMT = NA,
  USDAFOODS = NA,
  SCHSERVEBRKFST = NA,
  ANYPREGNANT = "No",
  WICHH = NA,
  MEALFACILITY   = NA,
  
  FOODSECUREQ3 = NA,
  FOODSECUREQ4 = NA,
  FOODSECUREQ5 = NA,
  FOODSECUREQ6 = NA,
  FOODSECUREQ8 = NA,
  FOODSECUREQ10 = NA,
  
  GROCERYLISTFREQ = NA,
  PRIMSTOREPRICES = NA,
  PRIMSTOREPRODUCE = NA,
  PRIMSTOREMEAT = NA,
  PRIMSTOREQUALITY = NA,
  PRIMSTOREVARIETY = NA,
  PRIMSTORESPECIAL = NA,
  PRIMSTORECLOSE = NA,
  PRIMSTORELOYALTY = NA,
  PRIMSTOREOTHREASON = NA,
  
  WHYNOTSUPERMKT1 = NA,
  WHYNOTSUPERMKT2 = NA,
  WHYNOTSUPERMKT3 = NA,
  WHYNOTSUPERMKT4 = NA,
  WHYNOTSUPERMKT5 = NA,
  WHYNOTSUPERMKT6 = NA,
  WHYNOTSUPERMKT7 = NA,
  WHYNOTSUPERMKT8 = NA,
  WHYNOTSUPERMKT9 = NA,
  WHYNOTSUPERMKTOTH = NA,
  
  #ALTSTORETYPE = NA,
  EVERSHOPOTHER = "No",
  FOODSTORENUM = NA,
  
  SHOPCONV = NA,
  SHOPBIGBOX = NA,
  SHOPCLUB = NA,
  SHOPDOLLAR = NA,
  SHOPBAKERY = NA,
  SHOPMEATFISH = NA,
  SHOPVEGSTAND = NA,
  SHOPANYOTHER = NA,
  SHOPOTHNONE = NA,
  
  FOODPANTRY = NA,
  FARMERSMARKET = NA,
  
  DIETSTATUSHH = NA,
  HEALTHYCOST = NA,
  HEALTHYTASTEPR = NA,
  EATHEALTHYHH = NA,
  
  MYPYRAMID = 'No',
  MYPLATE = 'No',
  FOODPYRAMID = 'No',
  MYPYRAMIDFOLLOW = 'No',
  
  FRUITSVEG = NA,
  NUTRITIONFACTS = 'Never seen',
  ANYLACTOSEINTOL = 'No',
  ANYFOODALLERGY = 'No',
  ANYTOBACCO = 'No',
  ANYILLNESS  = 'No',
  FINCONDITION = NA,
  
  BILLREVFREQ = "No bills received",
  BILLSONTIMEFREQ = "No bills received",
  PAYABOVEMINFREQ = "No bills received",
  BILLSONTIMEFREQ = 'No',
  NMEALSHOME = NA,
  
  BILLPAYPROB6MOS = NA,
  UTILNOTPAID6MOS = NA,
  EVICTED6MOS = NA,
  CASHADV6MOS = NA,
  PAYDAYLOAN6MOS = NA,
  
  FEEDBACK2 = NA,
  #FEEDBACK1 = NA,
  #FEEDBACK3 = NA,
  #FEEDBACK4_1 = NA,
  
  #PRIMSTORETYPE = NA,
  
  VEHICLENUM = NA,
  ALTSTOREREASON    = NA,
  HEALTHYTASTEHH   = NA,
  NDINNERSOUTHH    = NA,
  MEALGUESTANY = NA,    
  NGUESTDINNERMON_R = NA)

# Safety check for missing entries in 'na.values'
miss <- noquote(setdiff(na.vars, names(na.values)))
extras <- noquote(setdiff(names(na.values), na.vars))
stopifnot(length(miss) == 0 & length(extras) == 0)


# Update "Not applicable" values for each variable
for (v in na.vars) {
  if (!is.null(na.values[[v]])) {
    ind <- which(codebook_h$var == v & codebook_h$label == "Not applicable")  # Update the "Not applicable" label to specified label
    codebook_h$label[ind] <- na.values[[v]]
  }
}

stopifnot(!any(codebook_h$label == "Not applicable", na.rm = TRUE))
rm(skip.values,na.values)

ordered.factors <- list(
  NUMGUESTS = c('None','One','More than one'),

  HOUSINGOWN = c('Rent','Own','Other, do not pay for housing'),
  
  LIQASSETS = c('Less than $2,000',"At or above $2,000 and below\n$3,000",'At least $2,000','Equal to or greater than $3,000'),
  
  SNAPNOWADMIN = c('No consent given','No match','Match confirms SNAP non- participation','Match confirms SNAP participation'),
  
  SNAPLASTAMT = c("No SNAP benefits",'0<amount<=16','16<amount<50','50<=amount<100','100<=amount<150','150<=amount<200','200<=amount<250',
                  '250<=amount<300','300<=amount<350','350<=amount<400','400<=amount<450','450<=amount<500','500<=amount<550',
                  '550<=amount<600','600<=amount<650','650<=amount<700','700<=amount<750','750<=amount<800','800<=amount<1200'),
  SNAPUSUALAMT = c("No SNAP benefits",'The usual amount','More than the usual amount','Less than the usual amount'),
  
  ADLTFSCAT =c('High food security','Marginal food security','Low food security','Very low food security'),
  
  FOODSUFFICIENT = c('Enough of the kinds of food we want to eat','Enough, but not always the kinds of food we want to eat',
                     'Sometimes not enough to eat','Often not enough to eat'),
  
  FOODSECUREQ1 = c('Often true','Sometimes true','Never true'),
  
  FOODSECUREQ2 = c('Often true','Sometimes true','Never true'),
  
  FOODSECUREQ3 = c('Often true','Sometimes true','Never true'),
  
  GROCERYLISTFREQ = c('Never','Seldom','Sometimes','Most of the time','Almost always'),
  
  FOODSTORENUM = c(1:20),
  
  DIETSTATUSPR = c('Excellent','Very good','Good','Fair','Poor'),
  
  DIETSTATUSHH = c('Single person household','Excellent','Very good','Good','Fair','Poor'),
  
  FRUITSVEG = c('Eat right amount','Should eat more','Should eat less'),
  
  NUTRITIONFACTS = c('Always','Most of the time','Sometimes','Rarely','Never','Never seen'),
  
  FINCONDITION = c('Very comfortable and secure','Able to make ends meet without much difficulty','Occasionally have some difficulty making ends meet',
                   'Tough to make ends meet but keeping your head above water','In over your head'),
  
  BILLREVFREQ = c('No bills received','Never','Rarely','Sometimes','Usually','Always'),
  BILLSONTIMEFREQ = c('No bills received','Never','Rarely','Sometimes','Usually','Always'),
  PAYABOVEMINFREQ = c('No bills received','Never','Rarely','Sometimes','Usually','Always'),
  NDINNERSOUTHH = c('Single person household','0','1','2','3','4','5','6','7'),

  MEALGUESTDAYS = c('0','1','2','3','4','5','6','7'),
  NGUESTBRKFSTSUN_R = c('0','1','2','3','4 or more'),
  NGUESTBRKFSTMON_R = c('0','1','2','3','4 or more'),
  NGUESTBRKFSTTUE_R = c('0','1','2 or more'),
  NGUESTBRKFSTWED_R   = c('0','1','2','3 or more'),
  NGUESTBRKFSTTHU_R  = c('0','1','2','3 or more'),
  NGUESTBRKFSTFRI_R = c('0','1','2','3','4 or more'),
  NGUESTBRKFSTSAT_R = c('0','1','2','3','4 or more'),
  VEHICLENUM = c('0','1','2','3','4 or more'),
  NGUESTLUNCHSUN_R  = c('0','1','2','3','4','5','6 or more'),
  NGUESTLUNCHMON_R = c('0','1','2 or more'),
  NGUESTLUNCHTUE_R = c('0','1','2 or more'),
  
  NGUESTLUNCHWED_R = c('0','1','2','3','4 or more'),
  NGUESTLUNCHTHU_R  = c('0','1','2','3','4 or more'),
  NGUESTLUNCHFRI_R = c('0','1','2','3','4 or more'),
  NGUESTLUNCHSAT_R  = c('0','1','2','3','4 or more'),
  
  NGUESTDINNERSUN_R  = c('0','1','2','3','4','5','6','7','8','9','10','11','12','13 or more'),
  NGUESTDINNERMON_R = c('0','1','2','3','4 or more'),
  NGUESTDINNERTUE_R = c('0','1','2','3','4 or more'),
  NGUESTDINNERWED_R  = c('0','1','2','3','4','5 or more'),
  NGUESTDINNERTHU_R = c('0','1','2','3','4','5 or more'),
  NGUESTDINNERFRI_R = c('0','1','2','3','4','5 or more'),
  NGUESTDINNERSAT_R = c('0','1','2','3','4','5','6','7','8','9','10','11 or more'),
  
  NGUESTSNACKSUN_R = c('0','1','2','3','4 or more'),
  NGUESTSNACKTUE_R = c('0','1','2','3 or more'),
  NGUESTSNACKWED_R  = c('0','1','2','3','4 or more'),
  NGUESTSNACKTHU_R  = c('0','1','2','3','4 or more'),
  NGUESTSNACKFRI_R =  c('0','1','2','3','4','5 or more'),
  NGUESTSNACKSAT_R = c('0','1','2','3','4','5 or more')) 

# Detect any variables in 'ordered.factors' that are NOT in the codebook
extras <- noquote(setdiff(names(ordered.factors), codebook_h$var))
stopifnot(length(extras) == 0)

#miss
#xx <- setdiff(colnames(d), codebook$var)

# Update variable values with associated labels from 'codebook'
# Loop through each variable in 'd', assigning labels when applicable

for (v in names(d_0)) {
  
  cb <- filter(codebook_h, var == v)
  x <- d_0[[v]]
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
    zz <- factor(x, levels = intersect(ordered.factors[[v]], x), ordered = TRUE)
    stopifnot(sum(is.na(zz)) == num.na)  # This is a final safety check to ensure no NA's introduced inadvertently
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
  d_0[[v]] <- x
  
}

#-----
d <- d_0 %>% left_join(.,d_fah, by = c('HHNUM')) %>% left_join(.,d_fafh, by = 'HHNUM') %>% left_join(.,d_hr, by = 'HHNUM') 
rm(d_0,d_fah,d_fafh)

codebook <- rbind(codebook_h,codebook_fafh,codebook_fah,codebook_hr) %>% mutate(desc = str_replace(desc,"\\(Y/N\\)*", ""),
                                                                                desc = str_replace(desc,"SELFEMPLOYHH ≠ 0",""),
                                                                                desc = str_replace(desc,"\\( from drop-down \\& PRIMSTORESNAPTYPE.*",""),
                                                                                desc = str_replace(desc,"No supermarkets close by.*","")) %>%
  
  add_row(.,var = 'F_WHOLE_CUPS', desc = "Total only whole fruit cups ",label = NA, value = NA) %>%
  add_row(.,var = 'F_TOTAL_CUPS', desc = "Total all fruit cups ",label = NA, value = NA)  %>%
  add_row(.,var = 'V_TOTAL_CUPS', desc = "Total vegetable cups ",label = NA, value = NA)  %>%
  add_row(.,var = 'V_DRKGR_LEG_CUPS', desc = "Total dark green vegetables and legumes cups ",label = NA, value = NA)  %>%
  add_row(.,var = 'G_WHOLE_OZ', desc = "Total whole grain (oz) ",label = NA, value = NA)  %>%
  add_row(.,var = 'D_TOTAL_CUPS', desc = "Total dairy cups ",label = NA, value = NA)  %>%
  add_row(.,var = 'PROTEIN_OZ', desc = "Total protein (oz) ",label = NA, value = NA)  %>%
  add_row(.,var = 'PF_SEAFOOD_OZ', desc = "Total seafood and plant protein cups ",label = NA, value = NA)  %>%
  add_row(.,var = 'TOT_ENG', desc = "Total energy (kcal)  ",label = NA, value = NA)  %>%
  add_row(.,var = 'G_REFINED_OZ ', desc = "Total refined grains (oz) ",label = NA, value = NA)  %>%
  add_row(.,var = 'SODIUM_G', desc = "Total sodium (g)  ",label = NA, value = NA)  %>%
  add_row(.,var = 'SUGAR_ENG', desc = "Total added sugar energy (kcal) ",label = NA, value = NA)  %>%
  add_row(.,var = 'SATFAT_ENG', desc = "Total saturated fat energy (kcal) ",label = NA, value = NA)  %>%
  add_row(.,var = 'HEI_GRAMS', desc = "Total food ",label = NA, value = NA) 

rm(codebook_h,codebook_fafh,codebook_fah,codebook_hr)

# Detect structural dependencies
# Which variables have missing values and how frequent are they?
na.count <- colSums(is.na(d))
na.count <- na.count[na.count > 0]
na.count 

# Impute NA values in 'd'
d <- fusionModel::impute(data = data.table::as.data.table(d),
                         weight = "HHWGT",
                         ignore = c("HHNUM",'ALTSTOREPLACEID'))

# Replace NA's in 'd' with the imputed values
na.count <- colSums(is.na(d))
check <- na.count[na.count > 0]
check 

#Add/create variables for geographic concordance with variables in 'geo_concordance.fst'
d <- d %>%
  mutate(
    ur12 = as.factor(ifelse(RURAL == "Yes",'R','U')),
    cbsatype13 = as.factor(ifelse(NONMETRO == "In a CBSA",'Metro','Micro'))) %>%
  select(-RURAL,-NONMETRO)  %>%   rename(region = REGION) 

# See which variables in 'd' are also in 'geo_concordance' and
gnames <- names(fst::fst("geo-processed/concordance/geo_concordance.fst"))
gvars <- intersect(gnames, names(d))

# Class new/added geo identifiers as unordered factors
d <- d %>%
  mutate_at(gvars, ~ factor(.x, levels = sort(unique(.x)), ordered = FALSE)) 

##Define HEI scoring system########
d <- d %>% 

    ##HEI at home 
  mutate (
    
    TOTALFRUIT_SCORE_FAH = ifelse((F_TOTAL_CUPS_FAH/TOT_ENG_FAH)*1000 == 0, 0, 
                                  ifelse((F_TOTAL_CUPS_FAH/TOT_ENG_FAH)*1000 <= 0.8, 5 * ((F_TOTAL_CUPS_FAH/TOT_ENG_FAH)*1000 / 0.8), 5)),
    
    WHOLEFRUIT_SCORE_FAH = ifelse((F_WHOLE_CUPS_FAH/TOT_ENG_FAH)*1000 == 0, 0, 
                                ifelse((F_WHOLE_CUPS_FAH/TOT_ENG_FAH)*1000 <= 0.4, 5 * ((F_WHOLE_CUPS_FAH/TOT_ENG_FAH)*1000 / 0.4), 5)),
         
          TOTVEG_SCORE_FAH = ifelse((V_TOTAL_CUPS_FAH/TOT_ENG_FAH)*1000 == 0, 0, 
                              ifelse((V_TOTAL_CUPS_FAH/TOT_ENG_FAH)*1000 <= 1.1, 5 * ((V_TOTAL_CUPS_FAH/TOT_ENG_FAH)*1000 / 1.1), 5)),
          
          DARKVEG_SCORE_FAH = ifelse((V_DRKGR_LEG_CUPS_FAH/TOT_ENG_FAH)*1000 == 0, 0,
                              ifelse((V_DRKGR_LEG_CUPS_FAH/TOT_ENG_FAH)*1000 <= 0.2, 5 * ((V_DRKGR_LEG_CUPS_FAH/TOT_ENG_FAH)*1000 / 0.2), 5)),
          
          GRAIN_SCORE_FAH = ifelse((G_WHOLE_OZ_FAH/TOT_ENG_FAH)*1000 == 0, 0, 
                                   ifelse((G_WHOLE_OZ_FAH/TOT_ENG_FAH)*1000 <= 1.5, 10 * ((G_WHOLE_OZ_FAH/TOT_ENG_FAH)*1000 / 1.5), 10)),
          
          DAIRY_SCORE_FAH = ifelse((D_TOTAL_CUPS_FAH/TOT_ENG_FAH)*1000 == 0, 0, 
                                   ifelse((D_TOTAL_CUPS_FAH/TOT_ENG_FAH)*1000 <= 1.3, 10 * ((D_TOTAL_CUPS_FAH/TOT_ENG_FAH)*1000 / 1.3), 10)),
          
          PROTEIN_SCORE_FAH = ifelse((PROTEIN_OZ_FAH/TOT_ENG_FAH)*1000 == 0, 0, 
                                     ifelse((PROTEIN_OZ_FAH/TOT_ENG_FAH)*1000 <= 2.5, 5 * ((PROTEIN_OZ_FAH/TOT_ENG_FAH)*1000 / 2.5), 5)),
          
          SEAFOOD_SCORE_FAH = ifelse((PF_SEAFOOD_OZ_FAH/TOT_ENG_FAH)*1000 == 0, 0, 
                                     ifelse((PF_SEAFOOD_OZ_FAH/TOT_ENG_FAH)*1000 <= 0.8, 5 * ((PF_SEAFOOD_OZ_FAH/TOT_ENG_FAH)*1000 / 0.8), 5)),
          
          FAT_RATIO_FAH = ifelse(SATFAT_FAH == 0,0,((as.numeric(POLYFAT_FAH) + as.numeric(MONOFAT_FAH))/(as.numeric(SATFAT_FAH)))),
          
          FAT_SCORE_FAH = ifelse(FAT_RATIO_FAH <= 1.2, 0, 
                          ifelse(FAT_RATIO_FAH <= 2.5, (FAT_RATIO_FAH - 1.2) * (10 / (2.5 - 1.2)),10)),
    
    
          REFINED_SCORE_FAH = ifelse((G_REFINED_OZ_FAH/TOT_ENG_FAH)*1000 < 1.8 |  (G_REFINED_OZ_FAH/TOT_ENG_FAH)*1000 == 1.8, 10, 
                            ifelse((G_REFINED_OZ_FAH/TOT_ENG_FAH)*1000 <= 4.25, 10 - ((G_REFINED_OZ_FAH/TOT_ENG_FAH)*1000- 1.8) * (10 / (4.25 - 1.8)), 0)),
          
          SODIUM_SCORE_FAH = ifelse((SODIUM_G_FAH/TOT_ENG_FAH)*1000 < 1.1 |  (SODIUM_G_FAH/TOT_ENG_FAH)*1000 == 1.1, 10, 
                                  ifelse((SODIUM_G_FAH/TOT_ENG_FAH)*1000 <= 2.2, 10 - ((SODIUM_G_FAH/TOT_ENG_FAH)*1000 - 1.1) * (10 / (2.2 - 1.1)), 0)),
          
          ADDSUGAR_SCORE_FAH = ifelse((SUGAR_ENG_FAH/TOT_ENG_FAH) < 0.065 |  (SUGAR_ENG_FAH/TOT_ENG_FAH) == 0.065, 10, 
                                ifelse((SUGAR_ENG_FAH/TOT_ENG_FAH) <= 0.26, 10 - ((SUGAR_ENG_FAH/TOT_ENG_FAH) - 0.065) * (10 / (0.26 - 0.065)), 0)),
    

          SATFAT_SCORE_FAH = ifelse((SATFAT_ENG_FAH/TOT_ENG_FAH) < 0.08 |  (SATFAT_ENG_FAH/TOT_ENG_FAH) == 0.08, 10, 
                              ifelse((SATFAT_ENG_FAH/TOT_ENG_FAH) <= 0.16, 10 - ((SATFAT_ENG_FAH/TOT_ENG_FAH) - 0.08) * (10 / (0.16 - 0.08)), 0))) %>%
  
  mutate(across(ends_with("_SCORE_FAH"),
                ~ if_else(TOT_ENG_FAH == 0, 0, .))) %>%
  
  mutate(HEI_INDEX_FAH = WHOLEFRUIT_SCORE_FAH + TOTALFRUIT_SCORE_FAH + TOTVEG_SCORE_FAH + DARKVEG_SCORE_FAH + GRAIN_SCORE_FAH + DAIRY_SCORE_FAH  + PROTEIN_SCORE_FAH + 
           SEAFOOD_SCORE_FAH + FAT_SCORE_FAH + REFINED_SCORE_FAH +SODIUM_SCORE_FAH+ ADDSUGAR_SCORE_FAH+SATFAT_SCORE_FAH) %>% 
  
  ##HEI away from home 
  mutate (WHOLEFRUIT_SCORE_FAFH = ifelse((F_WHOLE_CUPS_FAFH/TOT_ENG_FAFH)*1000 == 0, 0, 
                                         ifelse((F_WHOLE_CUPS_FAFH/TOT_ENG_FAFH)*1000 <= 0.4, 5 * ((F_WHOLE_CUPS_FAFH/TOT_ENG_FAFH)*1000 / 0.4), 5)),
          
          TOTALFRUIT_SCORE_FAFH = ifelse((F_TOTAL_CUPS_FAFH/TOT_ENG_FAFH)*1000 == 0, 0, 
                                        ifelse((F_TOTAL_CUPS_FAFH/TOT_ENG_FAFH)*1000 <= 0.8, 5 * ((F_TOTAL_CUPS_FAFH/TOT_ENG_FAFH)*1000 / 0.8), 5)),
          
          TOTVEG_SCORE_FAFH = ifelse((V_TOTAL_CUPS_FAFH/TOT_ENG_FAFH)*1000 == 0, 0,
                                     ifelse((V_TOTAL_CUPS_FAFH/TOT_ENG_FAFH)*1000 <= 1.1, 5 * ((V_TOTAL_CUPS_FAFH/TOT_ENG_FAFH)*1000 / 1.1), 5)),
          
          DARKVEG_SCORE_FAFH = ifelse((V_DRKGR_LEG_CUPS_FAFH/TOT_ENG_FAFH)*1000 == 0, 0,
                                      ifelse((V_DRKGR_LEG_CUPS_FAFH/TOT_ENG_FAFH)*1000 <= 0.2, 5 * ((V_DRKGR_LEG_CUPS_FAFH/TOT_ENG_FAFH)*1000 / 0.2), 5)),
          
          GRAIN_SCORE_FAFH = ifelse((G_WHOLE_OZ_FAFH/TOT_ENG_FAFH)*1000 == 0, 0, 
                              ifelse((G_WHOLE_OZ_FAFH/TOT_ENG_FAFH)*1000 <= 1.5, 10 * ((G_WHOLE_OZ_FAFH/TOT_ENG_FAFH)*1000 / 1.5), 10)),
          
          DAIRY_SCORE_FAFH = ifelse((D_TOTAL_CUPS_FAFH/TOT_ENG_FAFH)*1000 == 0, 0, 
                            ifelse((D_TOTAL_CUPS_FAFH/TOT_ENG_FAFH)*1000 <= 1.3, 10 * ((D_TOTAL_CUPS_FAFH/TOT_ENG_FAFH)*1000 / 1.3), 10)),
          
          PROTEIN_SCORE_FAFH = ifelse((PROTEIN_OZ_FAFH/TOT_ENG_FAFH)*1000 == 0, 0, 
                              ifelse((PROTEIN_OZ_FAFH/TOT_ENG_FAFH)*1000 <= 2.5, 5 * ((PROTEIN_OZ_FAFH/TOT_ENG_FAFH)*1000 / 2.5), 5)),
          
          SEAFOOD_SCORE_FAFH = ifelse((PF_SEAFOOD_OZ_FAFH/TOT_ENG_FAFH)*1000 == 0, 0, 
                                ifelse((PF_SEAFOOD_OZ_FAFH/TOT_ENG_FAFH)*1000 <= 0.8, 5 * ((PF_SEAFOOD_OZ_FAFH/TOT_ENG_FAFH)*1000 / 0.8), 5)),
          
          FAT_RATIO_FAFH = ifelse(SATFAT_FAFH == 0,0,(as.numeric(POLYFAT_FAFH) + as.numeric(MONOFAT_FAFH))/(as.numeric(SATFAT_FAFH))),
         
          FAT_SCORE_FAFH = ifelse(FAT_RATIO_FAFH <= 1.2, 0, 
                                 ifelse(FAT_RATIO_FAFH <= 2.5, (FAT_RATIO_FAFH - 1.2) * (10 / (2.5 - 1.2)),10)),
          
          REFINED_SCORE_FAFH = ifelse((G_REFINED_OZ_FAFH/TOT_ENG_FAFH)*1000 < 1.8 |  (G_REFINED_OZ_FAFH/TOT_ENG_FAFH)*1000 == 1.8, 10, ifelse((G_REFINED_OZ_FAFH/TOT_ENG_FAFH)*1000 <= 4.25, 10 - ((G_REFINED_OZ_FAFH/TOT_ENG_FAFH)*1000- 1.8) * (10 / (4.25 - 1.8)), 0)),
          
          SODIUM_SCORE_FAFH = ifelse((SODIUM_G_FAFH/TOT_ENG_FAFH)*1000 < 1.1 |  (SODIUM_G_FAFH/TOT_ENG_FAFH)*1000 == 1.1, 10, 
                                  ifelse((SODIUM_G_FAFH/TOT_ENG_FAFH)*1000 <= 2.2, 10 - ((SODIUM_G_FAFH/TOT_ENG_FAFH)*1000 - 1.1) * (10 / (2.2 - 1.1)), 0)),
          
          ADDSUGAR_SCORE_FAFH = ifelse((SUGAR_ENG_FAFH/TOT_ENG_FAFH) < 0.065 |  (SUGAR_ENG_FAFH/TOT_ENG_FAFH) == 0.065, 10, 
                                ifelse((SUGAR_ENG_FAFH/TOT_ENG_FAFH) <= 0.26, 10 - ((SUGAR_ENG_FAFH/TOT_ENG_FAFH) - 0.065) * (10 / (0.26 - 0.065)), 0)),
          
          SATFAT_SCORE_FAFH = ifelse((SATFAT_ENG_FAFH/TOT_ENG_FAFH) < 0.08 |  (SATFAT_ENG_FAFH/TOT_ENG_FAFH) == 0.08, 10,
                              ifelse((SATFAT_ENG_FAFH/TOT_ENG_FAFH) <= 0.16, 10 - ((SATFAT_ENG_FAFH/TOT_ENG_FAFH) - 0.08) * (10 / (0.16 - 0.08)), 0))) %>%
  
 mutate(across(ends_with("_SCORE_FAFH"),
    ~ if_else(TOT_ENG_FAFH == 0, 0, .))) %>%
  
  mutate(HEI_INDEX_FAFH = WHOLEFRUIT_SCORE_FAFH + TOTALFRUIT_SCORE_FAFH + TOTVEG_SCORE_FAFH + DARKVEG_SCORE_FAFH + GRAIN_SCORE_FAFH + DAIRY_SCORE_FAFH  + PROTEIN_SCORE_FAFH + 
           SEAFOOD_SCORE_FAFH + FAT_SCORE_FAFH + REFINED_SCORE_FAFH +SODIUM_SCORE_FAFH + ADDSUGAR_SCORE_FAFH+SATFAT_SCORE_FAFH) %>%
  
  ##HEI overall 
  mutate (F_WHOLE_CUPS = F_WHOLE_CUPS_FAH + F_WHOLE_CUPS_FAFH,
          TOT_ENG  = TOT_ENG_FAH  + TOT_ENG_FAFH ,
          F_TOTAL_CUPS = F_TOTAL_CUPS_FAH + F_TOTAL_CUPS_FAFH,
          V_TOTAL_CUPS = V_TOTAL_CUPS_FAH + V_TOTAL_CUPS_FAFH,  
          V_DRKGR_LEG_CUPS = V_DRKGR_LEG_CUPS_FAH + V_DRKGR_LEG_CUPS_FAFH,
          
          HEI_GRAMS =  HEI_GRAMS_FAH +  HEI_GRAMS_FAFH,
          
          D_TOTAL_CUPS = D_TOTAL_CUPS_FAH + D_TOTAL_CUPS_FAFH,
          PROTEIN_OZ = PROTEIN_OZ_FAH + PROTEIN_OZ_FAFH,
          PF_SEAFOOD_OZ = PF_SEAFOOD_OZ_FAH + PF_SEAFOOD_OZ_FAFH,
          G_WHOLE_OZ = G_WHOLE_OZ_FAH + G_WHOLE_OZ_FAFH,
          
          FAT_RATIO = (((as.numeric(POLYFAT_FAH)) + as.numeric(POLYFAT_FAFH))+ (as.numeric(MONOFAT_FAH)) + (as.numeric(MONOFAT_FAFH)))/(as.numeric(SATFAT_FAH) + as.numeric(SATFAT_FAFH)),
          
          G_REFINED_OZ = G_REFINED_OZ_FAH + G_REFINED_OZ_FAFH,
          SODIUM_G = SODIUM_G_FAH + SODIUM_G_FAFH,
          SUGAR_ENG =  SUGAR_ENG_FAH +  SUGAR_ENG_FAFH,
          SATFAT_ENG = SATFAT_ENG_FAH + SATFAT_ENG_FAFH,
          
          WHOLEFRUIT_SCORE = ifelse((F_WHOLE_CUPS/TOT_ENG)*1000 == 0, 0, 
                                ifelse((F_WHOLE_CUPS/TOT_ENG)*1000 <= 0.4, 5 * ((F_WHOLE_CUPS/TOT_ENG)*1000 / 0.4), 5)),
          
          TOTALFRUIT_SCORE = ifelse((F_TOTAL_CUPS/TOT_ENG)*1000 == 0, 0, 
                                   ifelse((F_TOTAL_CUPS/TOT_ENG)*1000 <= 0.8, 5 * ((F_TOTAL_CUPS/TOT_ENG)*1000 / 0.8), 5)),
          
          TOTVEG_SCORE = ifelse((V_TOTAL_CUPS/TOT_ENG)*1000 == 0, 0, 
                                ifelse((V_TOTAL_CUPS/TOT_ENG)*1000 <= 1.1, 5 * ((V_TOTAL_CUPS/TOT_ENG)*1000 / 1.1), 5)),
          
          DARKVEG_SCORE = ifelse((V_DRKGR_LEG_CUPS/TOT_ENG)*1000 == 0, 0, 
                                 ifelse((V_DRKGR_LEG_CUPS/TOT_ENG)*1000 <= 0.2, 5 * ((V_DRKGR_LEG_CUPS/TOT_ENG)*1000 / 0.2), 5)),
          
          GRAIN_SCORE = ifelse((G_WHOLE_OZ/TOT_ENG)*1000 == 0, 0,
                               ifelse((G_WHOLE_OZ/TOT_ENG)*1000 <= 1.5, 10 * ((G_WHOLE_OZ/TOT_ENG)*1000 / 1.5), 10)),
          
          DAIRY_SCORE = ifelse((D_TOTAL_CUPS/TOT_ENG)*1000 == 0, 0, 
                               ifelse((D_TOTAL_CUPS/TOT_ENG)*1000 <= 1.3, 10 * ((D_TOTAL_CUPS/TOT_ENG)*1000 / 1.3), 10)),
          
          PROTEIN_SCORE = ifelse((PROTEIN_OZ/TOT_ENG)*1000 == 0, 0, 
                                 ifelse((PROTEIN_OZ/TOT_ENG)*1000 <= 2.5, 5 * ((PROTEIN_OZ/TOT_ENG)*1000 / 2.5), 5)),
          
          SEAFOOD_SCORE = ifelse((PF_SEAFOOD_OZ/TOT_ENG)*1000 == 0, 0, 
                                 ifelse((PF_SEAFOOD_OZ/TOT_ENG)*1000 <= 0.8, 5 * ((PF_SEAFOOD_OZ/TOT_ENG)*1000 / 0.8), 5)),
          
          FAT_SCORE = ifelse(FAT_RATIO <= 1.2, 0, 
                             ifelse(FAT_RATIO <= 2.5, (FAT_RATIO - 1.2) * (10 / (2.5 - 1.2)),10)),
          
          REFINED_SCORE = ifelse((G_REFINED_OZ/TOT_ENG)*1000 < 1.8 |  (G_REFINED_OZ/TOT_ENG)*1000 == 1.8, 10, 
                                 ifelse((G_REFINED_OZ/TOT_ENG)*1000 <= 4.25, 10 - ((G_REFINED_OZ/TOT_ENG)*1000- 1.8) * (10 / (4.25 - 1.8)), 0)),
         
           SODIUM_SCORE = ifelse((SODIUM_G/TOT_ENG)*1000 < 1.1 |  (SODIUM_G/TOT_ENG)*1000 == 1.1, 10, 
                                 ifelse((SODIUM_G/TOT_ENG)*1000 <= 2.2, 10 - ((SODIUM_G/TOT_ENG)*1000 - 1.1) * (10 / (2.2 - 1.1)), 0)),
          
           ADDSUGAR_SCORE = ifelse((SUGAR_ENG/TOT_ENG) < 0.065 |  (SUGAR_ENG/TOT_ENG) == 0.065, 10, 
                                   ifelse((SUGAR_ENG/TOT_ENG) <= 0.26, 10 - ((SUGAR_ENG/TOT_ENG) - 0.065) * (10 / (0.26 - 0.065)), 0)),

          SATFAT_SCORE = ifelse((SATFAT_ENG/TOT_ENG) < 0.08 |  (SATFAT_ENG/TOT_ENG) == 0.08, 10, 
                                ifelse((SATFAT_ENG/TOT_ENG) <= 0.16, 10 - ((SATFAT_ENG/TOT_ENG) - 0.08) * (10 / (0.16 - 0.08)), 0))) %>%
  
  mutate(across(ends_with("_SCORE"),
                ~ if_else(TOT_ENG == 0, 0, .))) %>%
  
  mutate(HEI_INDEX = WHOLEFRUIT_SCORE + TOTALFRUIT_SCORE + TOTVEG_SCORE + DARKVEG_SCORE + GRAIN_SCORE + DAIRY_SCORE  + PROTEIN_SCORE + 
           SEAFOOD_SCORE + FAT_SCORE + REFINED_SCORE + SODIUM_SCORE + ADDSUGAR_SCORE + SATFAT_SCORE) %>%
  
select(-contains('_SCORE'))
  
codebook <- codebook %>% mutate(label = ifelse(is.na(label),desc,label)) %>%
  
  add_row(.,var = 'FAT_RATIO_FAH', desc = "Fat ratio at home",label= NA, value = NA)  %>%
  add_row(.,var = 'FAT_RATIO_FAFH', desc = "Fat ratio away from home",label= NA, value = NA)  %>%
  add_row(.,var = 'FAT_RATIO', desc = "Fat ratio overall",label= NA, value = NA)  %>%
  
  add_row(.,var = 'G_REFINED_OZ', desc = "Refined grains overall",label= NA, value = NA)  %>%
  add_row(.,var = 'SODIUM_G', desc = "Soidum content overall",label= NA, value = NA)  %>%
  add_row(.,var = 'SATFAT_ENG', desc = "Saturated fat overall",label= NA, value = NA)  %>%
  add_row(.,var = 'HEI_INDEX', desc = "Healthy eating index overall",label= NA, value = NA) 

codebook <- codebook %>% mutate(var = toupper(var))

# Add/create variables for geographic concordance with variables in 'geo_concordance.fst'
# Assemble final output
# NOTE: var_label assignment is done AFTER any manipulation of values/classes, because labels can be lost when classes are changed
h.final <- d %>%
  mutate_if(is.factor, safeCharacters) %>%
  mutate_if(is.numeric, convertInteger) %>%
  mutate_if(is.double, cleanNumeric, tol = 0.001) %>%
  labelled::set_variable_labels(.labels = setNames(as.list(safeCharacters(codebook$desc)), codebook$var), .strict = FALSE) %>%  # Set descriptions for codebook variables
  labelled::set_variable_labels(.labels = setNames(as.list(paste(gvars, "geographic concordance")), gvars)) %>%  # Set descriptions for geo identifiers
  rename(
    hid = HHNUM,  # Rename ID and weight variables to standardized names
    weight = HHWGT
  ) %>%
  rename_with(tolower) %>%  # Convert all variable names to lowercase
  select(hid,everything()) %>%   # Reorder columns with replicate weights at the end
  arrange(hid) 
#----------------
#rm(d)

labelled::var_label(h.final$snaplastamt_r) <- "Last benefit amount received by current and recent participants"
labelled::var_label(h.final$nguestsnackmon_r) <- "Number of guests for a snack on Monday"
labelled::var_label(h.final$hei_grams) <- "Total weight of edible food" 

# Create dictionary and save to disk
dictionary <- createDictionary(data = h.final, survey = "FAPS", vintage = 2013, respondent = "H")
saveRDS(object = dictionary, file = "survey-processed/FAPS/2013/FAPS_2013_H_dictionary.rds")
compileDictionary()
#----------------

# Save data to disk (.fst)
fst::write_fst(x = h.final, path = "survey-processed/FAPS/2013/FAPS_2013_H_processed.fst", compress = 100)
fst::write_fst(x = codebook, path = "survey-processed/FAPS/2013/FAPS_2013_H_codebook.fst", compress = 100)

