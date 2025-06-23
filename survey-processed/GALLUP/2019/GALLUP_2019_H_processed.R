library(fusionData)
library(fst)
library(tidyverse)
library(fusionModel)
library(reshape2)
library(haven)
library(readxl)
library(stringi)

source("R/utils.R")


gc()
setwd("/Users/karthikakkiraju/Documents/fusionData")

#Read cross walk between zipcodes and zcta from : 
zip_zcta <- read_excel("survey-raw/gallup/2015/ZIPCodetoZCTACrosswalk2021UDS.xlsx") %>% 
  select('ZIP_CODE','ZCTA') %>% rename(zcta10 = ZCTA,
                                       zipcode = ZIP_CODE)
#-----
# Load raw GALLUP 2019 data for the well-being track
d <- read_dta("survey-raw/GALLUP/2019/US_WB_DAILY_2019_DATA.dta") %>% 
  select(.,-c('D1','HWB6_2','EDUCATION',
              "RACE_WHITE","RACE_BLACK","RACE_ASIAN","RACE_AMINDIAN","RACE_NTVHAWAIIAN",
              'UCLA2','UCLA3A','UCLA3B','UCLA1_1','UCLA1_2','UCLA1_3','UCLA1_4','UCLA1_5','UCLA1_6')) %>%
  
  #Increasing the resolution of RACE 
  
  rename(WP16 = LAD1, #Renaming variables based on the 2008-2017 codebook
         WP18 = LAD2,
         WB_WEIGHT = weights,
         D4_1 = D4,
         SC7 = D7,
         D2I = D2_INCHES,
         D2F = D2_FEET)   %>%
  mutate(MSANAME = gsub('\\(NON-MSA COUNTIES\\)','NON-MSA COUNTIES', MSANAME))
  
#Load codebook after saving codebook pdf as xlsx
codebook_11 <- readxl::read_excel("survey-raw/gallup/2015/Work_Gallup_Daily_Methodology 2008_2017.xlsx",
                                  sheet = 2) %>% 
  fill(Question,.direction = "down") %>% # To fix merged cells
  group_by(Question) %>% 
  fill(Item,.direction = "down")  %>% 
  setNames(c('var','desc','value','label')) %>% 
  select(var, desc, value, label) %>% ungroup() %>% unique()

#Variables which have NA values in codebook1
vars <- codebook_11 %>% pull(var) %>% unique()
dpp <- d[,names(d) %in% vars] 

codebook_12 <- as.data.frame(colnames(is.na(dpp))) %>% mutate(value = NA,label = NA,desc=NA)
colnames(codebook_12) = c("var","value","label","desc")

codebook1 <- rbind(codebook_11,codebook_12)
rm(codebook_11,codebook_12)

#Fix NA values in  for HWB variables in codebook1
#Make these ordinal as these question are asked on a 5-scale with distinct responses
hwb = c('HWB1','HWB2','HWB3','HWB4','HWB5','HWB6','HWB7','HWB8','HWB9','HWB10',
        'HWB11','HWB12','HWB13','HWB14','HWB15','HWB16','HWB17','HWB18','HWB19',
        'HWB20','HWB21','HWB22','HWB23')

codebook1 <- codebook1 %>% 
  mutate(label = ifelse((var == 'WP16' |var == 'WP18'|var %in% hwb) & is.na(label), value,label),
         label = ifelse((var %in% hwb) & (value == '2'), 'Disagree' ,label),
         label = ifelse((var %in% hwb) & (value == '3'), 'Neutral' ,label),
         label = ifelse((var %in% hwb) & (value == '4'), 'Agree' ,label)) %>% filter(!is.na(var))

#Make list of var in codebook1
codebook_a <- codebook1 %>% select('var') %>% distinct() 

#Second list of variables from the codebook
codebook2 <- readxl::read_excel("survey-raw/gallup/2015/Work_Gallup_Daily_Methodology 2008_2017.xlsx",
                                sheet = 3) %>% 
  select(Qtag, Label) %>% 
  setNames(c('var','desc')) %>% distinct() %>% mutate(desc = ifelse(var %in%"STATE_NAME","State Name",desc))

#These are variables not present in codebook1 but are described in codebook2 
#Extract values  for these variables from the dataset
codebook3 <- anti_join(codebook2,codebook_a, by = 'var') %>% 
  filter(!(var %in%c('RACE_AMINDIAN','RACE_ASIAN','RACE_BLACK',
                     'RACE_NTVHAWAIIAN','RACE_WHITE','PartyR','PartyRR')))  %>%
  add_row(.,var = "FIPS_CODE",desc = "FIPS code (identifies counties)")  #Dropping these variable because they are present as RACE and P1 already

var3 <- as.vector(codebook3['var']) %>% unique()
#var <- dcast(var3,var~var)
var <- as.data.frame(var3) 
colnames(var) = 'Var'
var <- spread(var, key = Var, value = Var ,fill = NA)

d3 <- d[, names(d) %in% names(var)]   

#Make codebook entries for weight variables in d3      
d3a <- d3 %>% select(c('WB_WEIGHT')) 

codebook_d3a = data.frame(var = colnames(d3a)) %>%  
  mutate(
    value_min = apply(d3a,2,min,na.rm=T),
    value_max = apply(d3a,2,max,na.rm=T),
    value = paste0(value_min,"-",value_max)) %>%  
  subset(., select = -c(value_min,value_max))  %>% mutate_all(trimws) %>% mutate(label = NA)

#Codebook for geographical variables in d3 
d3b <- d3 %>% select(c('ZIPCODE',"INT_DATE","MOTHERLODE_ID","FIPS_CODE",
                       'ZIPSTATE','STATE_NAME','MSACODE','MSANAME','CONGDIST')) 

codebook_d3b <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(codebook_d3b) <- c("var", "value")

for (v in colnames(d3b)) 
{test <- d3b[v] 
new <- test %>% summarise_all(funs(paste(unique(.), collapse=",")))
colnames(new) = c('value')
new <- new %>% mutate(var = v)
codebook_d3b <- rbind(new,codebook_d3b)}

codebook_d3b <- codebook_d3b %>% mutate(label = NA)

#Codebook values for numerical variables in d3
d3c <- d3 %>% select(c('BMI','FINANCIAL','HEIGHT','COMMUNITY',"MONTH",
                       'PHYSICAL','PURPOSE','SOCIAL','WELL_BEING_INDEX','YEAR')) 

#Get numerical range for the variables
codebook_d3c = data.frame(var = colnames(d3c)) %>%  
  mutate(
    value_min = apply(d3c,2,min,na.rm=T),
    value_max = apply(d3c,2,max,na.rm=T),
    value = paste0(value_min,"-",value_max)) %>%  
  subset(., select = -c(value_min,value_max))  %>% mutate_all(trimws) %>% mutate(label = NA)

#Need to manually enter values
d3d <- d3 %>% select(c('RACE','SC7','OBESE',"H4F","H4_1F","HISPANIC","D40","WEIGHT_GROUP")) 

codebook_d3d <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(codebook_d3d) <- c("var", "value")

for (v in colnames(d3d)) 
{test <- d3d[v] 
new <- test %>% summarise_all(funs(paste(unique(.))))
colnames(new) = c('value')
new <- new %>% mutate(var = v)
codebook_d3d <- rbind(new,codebook_d3d)
}

#Manual entries for codebook values
codebook_d3d <- codebook_d3d %>% 
  mutate(label = ifelse((var == 'RACE' & value == 1),'White',
                  ifelse(var == 'RACE' & value == 2,'Other',
                  ifelse(var == 'RACE' & value == 3,'Black',
                  ifelse(var == 'RACE' & value == 4,'Asian American',
                  ifelse(var == 'RACE' & value == 5,'Hispanic',
                  ifelse((var == 'SC7' & value == 1),'Male',
                   ifelse(var == 'SC7'  & value == 2,'Female',
                                                                  
                  ifelse((var == 'WP70'|var == 'WP6878'|var == 'WP83'|var == 'WP6906'|var == "H4F"| var == "H4_1F"|var == "M20"| var == "M20"| var =="HISPANIC"|var =="D26"|var =="EXER1"|var =="TOB1A"|var =="TOB1B"|var =="TOB1C"|var =="TOB1D"|var =="TOB1E"| var == "D40"| var == "H16"| var == "H41"| var == "WP6906") & value == 1,'Yes',
                  ifelse((var == 'WP70'|var == 'WP6878'|var == 'WP83'|var == 'WP6906'|var == "H4F"| var == "H4_1F"|var == "M20"| var == "M20"| var =="HISPANIC"|var =="D26"|var =="EXER1"|var =="TOB1A"|var =="TOB1B"|var =="TOB1C"|var =="TOB1D"|var =="TOB1E"| var == "D40"| var == "H16"| var == "H41"| var == "WP6906") & value == 2,'No',
                  ifelse((var == 'WP70'|var == 'WP6878'|var == 'WP83'|var == 'WP6906'|var == "H4F"| var == "H4_1F"|var == "M20"| var == "M20"|var =="HISPANIC"|var =="D26"|var =="EXER1"|var =="TOB1A"|var =="TOB1B"|var =="TOB1C"|var =="TOB1D"|var =="TOB1E"| var == "D40"| var == "H16"| var == "H41"| var == "WP6906") & value == 3,'DK',
                  ifelse((var == 'WP70'|var == 'WP6878'|var == 'WP83'|var == 'WP6906'|var == "H4F"| var == "H4_1F"|var == "M20"| var == "M20"|var =="HISPANIC"|var =="D26"|var =="EXER1"|var =="TOB1A"|var =="TOB1B"|var =="TOB1C"|var =="TOB1D"|var =="TOB1E"| var == "D40"| var == "H16"| var == "H41"| var == "WP6906") & value == 4,'Refused',
                                                                                              
                  ifelse(var == 'OBESE' & value == 0,'Yes',
                  ifelse(var == 'OBESE' & value == 1,'No',
                  ifelse(var == "H46" & value == 1, "Never", 
                  ifelse(var == "H46" & value == 2, "Rarely", 
                  ifelse(var == "H46" & value == 3, "Sometimes", 
                  ifelse(var == "H46" & value == 4, "Nearly every day", 
                  ifelse(var == "H46" & value == 8, "(DK)", 
                  ifelse(var == "H46" & value == 9, "(Refused)",
                                                                                                                                                      
                  ifelse(var == "WEIGHT_GROUP" & value == 1, "Under Weight", 
                  ifelse(var == "WEIGHT_GROUP" & value == 2, "Normal Weight", 
                  ifelse(var == "WEIGHT_GROUP" & value == 3, "Overweight", 
                  ifelse(var == "WEIGHT_GROUP" & value == 4, "Obese Class I", 
                  ifelse(var == "WEIGHT_GROUP" & value == 5, "Obese Class II", 
                  ifelse(var == "WEIGHT_GROUP" & value == 6, "Obese Class III", NA))))))))))))))))))))))))))

codebook_d3e <- rbind(codebook_d3a,codebook_d3b,codebook_d3c,codebook_d3d) 
codebook_add <- left_join(codebook_d3e,codebook3, by = 'var')  %>% 
                 mutate(label = ifelse(is.na(label) & (value != 'NA') ,desc,label))

rm(codebook_d3a,codebook_d3b,codebook_d3c,codebook_d3d,codebook_d3e)

#Further manual additions. There are more variables that are not described in the 2008-2017 methodology
# but are described in the 2018 methodology pdf 
codebook4 <- codebook1 %>% ungroup() %>%
  add_row(var = "INCOME_SUMMARY",desc = "Annual Income (Official)", value = 01,label = "Under $720") %>% 
  add_row(.,var = "INCOME_SUMMARY",desc = "Annual Income (Official)", value = 02,label = "$720 to $5999") %>% 
  add_row(.,var = "INCOME_SUMMARY",desc = "Annual Income (Official)", value = 03,label = "$6000 to $11999") %>% 
  add_row(.,var = "INCOME_SUMMARY",desc = "Annual Income (Official)", value = 04,label = "$12000 to $23999") %>% 
  add_row(.,var = "INCOME_SUMMARY",desc = "Annual Income (Official)", value = 05,label = "$24000 to $35999") %>% 
  add_row(.,var = "INCOME_SUMMARY",desc = "Annual Income (Official)", value = 06,label = "$36000 to $47999") %>%
  add_row(.,var = "INCOME_SUMMARY",desc = "Annual Income (Official)", value = 07,label = "$48000 to $59999") %>% 
  add_row(.,var = "INCOME_SUMMARY",desc = "Annual Income (Official)", value = 08,label = "$60000 to $89999") %>% 
  add_row(.,var = "INCOME_SUMMARY",desc = "Annual Income (Official)", value = 09,label = "$90000 to $119999") %>% 
  add_row(.,var = "INCOME_SUMMARY",desc = "Annual Income (Official)", value = 10,label = "$120000 and over") %>% 
  add_row(.,var = "INCOME_SUMMARY",desc = "Annual Income (Official)", value = 98,label = NA) %>% 
  add_row(.,var = "INCOME_SUMMARY",desc = "Annual Income (Official)", value = 99,label = NA) %>% 
  
  add_row(.,var = "EMP1",desc = "Thinking about your work situation, over the past seven days, were you employed, even minimally like for an hour or more?", value = 1, label = "Yes, employed for an employer") %>% 
  add_row(.,var = "EMP1",desc = "Thinking about your work situation, over the past seven days, were you employed, even minimally like for an hour or more?", value = 2, label = "Yes, self-employed") %>% 
  add_row(.,var = "EMP1",desc = "Thinking about your work situation, over the past seven days, were you employed, even minimally like for an hour or more?", value = 3, label = "Yes, employed for an employer AND self-employed") %>% 
  add_row(.,var = "EMP1",desc = "Thinking about your work situation, over the past seven days, were you employed, even minimally like for an hour or more?", value = 4, label = "No, not employed") %>% 
  
  add_row(.,var = "HWB13",desc = "A doctor would say that I do a great job of managing my health", value = 1, label = "1") %>% 
  add_row(.,var = "HWB13",desc = "A doctor would say that I do a great job of managing my health", value = 2, label = "2") %>% 
  add_row(.,var = "HWB13",desc = "A doctor would say that I do a great job of managing my health", value = 3, label = "3") %>% 
  add_row(.,var = "HWB13",desc = "A doctor would say that I do a great job of managing my health", value = 4, label = "4") %>% 
  add_row(.,var = "HWB13",desc = "A doctor would say that I do a great job of managing my health", value = 5, label = "5") %>% 
  add_row(.,var = "HWB13",desc = "A doctor would say that I do a great job of managing my health", value = 8, label = NA) %>% 
  add_row(.,var = "HWB13",desc = "A doctor would say that I do a great job of managing my health", value = 9, label = NA)  %>% 
  
  add_row(.,var = "H4_1F",desc = "Asthma", value = NA, label = NA)  %>%
  
  add_row(.,var = "H4_1A",desc = "Currently being treated for high blood pressure", value = NA, label = NA)  %>%
  add_row(.,var = "H4_1B",desc = "Currently being treated for high cholesterol", value = NA, label = NA)  %>%
  add_row(.,var = "H4_1D",desc = "Currently being treated for depression", value = NA, label = NA)  %>%
  add_row(.,var = "H4_1G",desc = "Currently being treated for cancer", value = NA, label = NA)  %>%
  
  add_row(.,var = "H4A",desc = "High blood pressure", value = NA, label = NA)  %>%
  add_row(.,var = "H4B",desc = "High cholesterol", value = NA, label = NA)  %>%
  add_row(.,var = "H4C",desc = "Diabetes", value = NA, label = NA)  %>%
  add_row(.,var = "H4D",desc = "Depression", value = NA, label = NA)  %>%
  add_row(.,var = "H4E",desc = "Heart attack", value = NA, label = NA)  %>%
  add_row(.,var = "H4F",desc = "Asthma", value = NA, label = NA)  %>%
  add_row(.,var = "H4G",desc = "Cancer", value = NA, label = NA)  %>%
  add_row(.,var = "H47",desc = "Thinking about this secondary health insurance coverage, is it", value = 77, label = NA)  %>%
  
  add_row(.,var = "M21",desc = "Cold", value = NA, label = NA)  %>%
  add_row(.,var = "M20",desc = "Flu", value = NA, label = NA)  %>%
  add_row(.,var = "H16",desc = "Care for Elderly or Disabled Person", value = NA, label = NA)  %>%
  add_row(.,var = "WP119",desc = "Religion Important", value = NA, label = NA)  

#Final merged codebook
codebook00 <- rbind(codebook4,codebook_add) %>% distinct() %>%
  mutate(
    label = ifelse(grepl("(DK)", label) | grepl("(Refused)", label)| grepl("(Does not apply)", label), "Not applicable", label)) %>%
  
  mutate(
    label = ifelse((var == "H36" & value == 4), "Fair", label)) %>%
  
  mutate(label = ifelse((var == "P20" & value == 2), "Conservative", label),
         label = ifelse((var == "P20" & value == 4), "Liberal", label)) %>%
  
  mutate(
    label = ifelse((var == "WP18" | var == "WP16") & label == "Worst possible", 0, label),
    label = ifelse((var == "P18" | var == "WP16") & label == "Best possible", 10, label),
    label = ifelse((var == "H48" | var == "H47") & value == 7, "Something else", label)) %>%
  mutate(  
    label = ifelse((var == "H3"  & label == "Zero days-"), 0, label),
    label = ifelse(((var == "D65" | var == "D66") & label == "None-"), 0, label),
    label = ifelse((var == "H12B"  & label == "Every day"),7, label)) %>%
  
  mutate(  
    label = ifelse(((var == "D2F" |var == "D2I") & value == 0), NA, label),
    label = ifelse((var == "D3" & value == -1), NA, label),
    label = ifelse((var == "D9" & value == 1), 1, label),
    label = ifelse((var == "D63" & value == 4), NA, label),
    label = ifelse((var == "H17" & value == 0), 0, label),
    label = ifelse((var == "M25A" & value == 0), 0, label)) %>%
  
  mutate(  
    label = ifelse((var == "WP1220"  & value == 18), "Less than 18", label),
    label = ifelse((var == "WP1220"  & value == 99), "Greater than 99", label),
    label = ifelse((var == "WP1220" & value == 100), NA, label),
    label = ifelse((var == "WP18" & value == 10), 10, label)) %>% 
  
  mutate(  
    desc = ifelse((var == "H14"), "Do you have health insurance coverage?", desc),
    desc = ifelse((var == "county"), "Identifies counties by state", desc),
    desc = ifelse((var == "H47"), "Thinking about this secondary health insurance coverage, is it", desc),
    
    desc = ifelse((var == "WP1223"), "What is your current marital status?", desc),
    desc = ifelse((var == "WP1220"), "Please tell me your age", desc),
    desc = gsub("(read 1-5)?)","",desc),
    
    desc = ifelse((var == "H4_1A"), "Currently being treated for high blood pressure", desc),
    desc = ifelse((var == "H4_1B"), "Currently being treated for high cholesterol", desc),
  desc = ifelse((var == "H4_1D"), "Currently being treated for for depression", desc), 
  desc = ifelse((var == "H4_1G"), "Currently being treated for cancer", desc)) %>%

  mutate(  
    label = gsub("(Do NOT list)", "", label)) %>%
  add_row(.,var = "H47", desc = "Thinking about this secondary health insurance coverage, is it", value = NA, label = NA)  

#More fixes to bring in for adjusted values
#Codebook values for numerical variables in d3
d3f <- d %>% select(.,contains("_ADJUSTED"))

#Get numerical range for the variables
codebook_d3f1 = data.frame(var = colnames(d3f)) %>%  
  mutate(
    value_min = apply(d3f,2,min,na.rm=T),
    value_max = apply(d3f,2,max,na.rm=T),
    value = paste0(value_min,"-",value_max)) %>%  
  subset(., select = -c(value_min,value_max))  %>% mutate_all(trimws) %>% 
  separate(var, into = c('var1', 'var2'), sep = '_ADJUSTED',remove = FALSE) %>% select(.,-c('var2')) %>% rename(temp = var) %>%
  merge(.,codebook00, by.x=c('var1'),
        by.y=c('var')) %>% select(.,c('temp','value.x','desc')) %>% 
  rename(var = temp,
         value = value.x) %>% distinct() %>% filter(!is.na(desc)) %>% 
  mutate(desc = paste('Adjusted_',desc)) %>% mutate(label = value)

codebook <- rbind(codebook_d3f1, codebook00) %>% filter(!is.na(desc)) %>% #Dropping variables that are not present in the 2019 dataset
  filter(!var %in% c('SURVEY',"SB","CENREG","RPTRGID","SA","FORMCD","FORMEFG","S1","SL1B","D1", "D1A" ,    
                     "H4","H4_1","H12", "H13","H16_FLT", "WP10200","WP10215","WP10202", "WP10216", 
                     "WP10229","WP10230" ,"WP10208","WP10209","M9","WP63","HWB18","HWB21","P8","M30", 
                     "WP148","P858_4","D69","D33","D34","QND12","D74","D75","D76","D15C","P1","P2", 
                     "ANN_INC", "ANN_INC2","D63","P128","P128F", "D65", "D66A","H17A","D66","D66_2","M91_FLT",
                     "M91", "M92","P20","WP9000","HWB13","M21","M20","H16")) %>% mutate(value = as.factor(value))  %>%
  mutate(desc = gsub("Skip:.*$", "",desc),
         desc = gsub("\\'\\(\\?", "'?",desc))


rm(codebook_d3f1, codebook00,codebook1,codebook2, codebook3, codebook4)
rm(codebook_add, codebook_a)
rm(d3,d3a,d3b,d3c,d3d,d3f,dpp,var,var3,new,test)

# Variables with "Not applicable" values
# These are the variables for which suitable replacement values must be specified below
na.vars <- codebook %>%
  filter(label == "Not applicable" | is.na(value)) %>%
  pull(var) %>%
  unique()

# Manually constructed...
# What value should "Not applicable" take for the following variables?
na.values <- list(
  D25A = "No",
  D5 = "No",
  D8B = "No Religion/Atheist/ Agnostic",
  H11 = "No",
  H12B = 0,
  H14 = "No",
  H15A = "No secondary coverage",
  H17 = 0,
  H3 = 0,
  H45 = "Never",
  H47 = "No secondary coverage",
  H48 = "No primary coverage",
  H4A = "No",
  H4B = "No",
  H4C = "No",
  H4D = "No",
  H4E = "No",
  H4F = "No",
  H4G = "No",
  H4_1A = "No",
  H4_1B = "No",
  H4_1D = "No",
  H4_1G = "No",
  H4_1F = "No",
  H7 = "No",
  WP119 = "No",
  WP1225 = "Not working",
  WP23 = "No",
  D5 = NULL
  )
  
# Update 'codebook' with the manually specified override values for "Not applicable"
# Drop variables set to NULL in 'na.values'
codebook <- filter(codebook, !var %in% names(which(map_lgl(na.values, is.null))))

# Update "Not applicable" values for each variable
for (v in na.vars) {
  if (!is.null(na.values[[v]])) {
    ind <- which(codebook$var == v & (codebook$label == "Not applicable" | is.na(codebook$value)))  # Update the "Not applicable" label to specified label
    codebook$label[ind] <- na.values[[v]]
  }
}

# Update "Not applicable" values for each variable
codebook <- codebook %>% mutate(label = ifelse(label == "Not applicable",NA,label)) %>%
            mutate(label = gsub('\\(The same\\)','The same', label),
                   desc = gsub('\\(Official','', desc)) 

#-----

#Create ordered factors
ordered.factors <- list(
  
  D4_1 = c("Less than a high school diploma (Grades 1 through 11 or no schooling","High school graduate (Grade 12 with diploma or GED certificate)","Technical, trade, vocational or business school or program after high school.","Some college – college, university or community college -- but no degree",
           "Two year associate degree from a college, university, or community college","Four year bachelor’s degree from a college or university (e.g., BS, BA, AB)","Some postgraduate or professional schooling after graduating college, but no postgraduate degree (e.g. some graduate school)",
           "Postgraduate or professional degree, including master’s, doctorate, medical or law degree (e.g., MA, MS, PhD, MD, JD)"),
  
  INCOME_SUMMARY = c("Under  $720", "$720 to $5,999","$6,000 to $11,999","$12,000 to $23,999","$24,000 to $35,999","$36,000 to $47,999",
                     "$48,000 to $59,999","$60,000 to $89,999","$90,000 to $119,999","$120000 and over"),
  
  H36 = c("Excellent","Very good","Good","Only fair","Poor"),
  
  HWB1 = c("Strongly Disagree","Disagree","Neutral",'Agree','Strongly Agree'),
  HWB2 = c("Strongly Disagree","Disagree","Neutral",'Agree','Strongly Agree'),
  HWB3 = c("Strongly Disagree","Disagree","Neutral",'Agree','Strongly Agree'),
  HWB4 = c("Strongly Disagree","Disagree","Neutral",'Agree','Strongly Agree'),
  HWB5 = c("Strongly Disagree","Disagree","Neutral",'Agree','Strongly Agree'),
  HWB6 = c("Strongly Disagree","Disagree","Neutral",'Agree','Strongly Agree'),
  HWB7 = c("Strongly Disagree","Disagree","Neutral",'Agree','Strongly Agree'),
  HWB8 = c("Strongly Disagree","Disagree","Neutral",'Agree','Strongly Agree'),
  HWB9 = c("Strongly Disagree","Disagree","Neutral",'Agree','Strongly Agree'),
  HWB10 = c("Strongly Disagree","Disagree","Neutral",'Agree','Strongly Agree'),
  HWB11 = c("Strongly Disagree","Disagree","Neutral",'Agree','Strongly Agree'),
  HWB14 = c("Strongly Disagree","Disagree","Neutral",'Agree','Strongly Agree'),
  HWB15 = c("Strongly Disagree","Disagree","Neutral",'Agree','Strongly Agree'),
  HWB16 = c("Strongly Disagree","Disagree","Neutral",'Agree','Strongly Agree'),
  HWB17 = c("Strongly Disagree","Disagree","Neutral",'Agree','Strongly Agree'),
  HWB19 = c("Strongly Disagree","Disagree","Neutral",'Agree','Strongly Agree'),
  HWB20 = c("Strongly Disagree","Disagree","Neutral",'Agree','Strongly Agree'),
  HWB22 = c("Strongly Disagree","Disagree","Neutral",'Agree','Strongly Agree'),
  HWB23 = c("Strongly Disagree","Disagree","Neutral",'Agree','Strongly Agree'),
  
  H45 = c("Every day","Nearly every day","Sometimes","Rarely","Never"),
  
  WP16 = c('0','1','2','3','4','5','6','7','8','9','10'),
  WP18 =  c('0','1','2','3','4','5','6','7','8','9','10'),
  
  WEIGHT_GROUP = c("Under Weight","Normal Weight","Overweight","Obese Class I","Obese Class II","Obese Class III"),
  
  WP31 = c("Getting better","The same","Getting worse"),
  
  WP1220 = c("Less than 18",18:97,"Greater than 99"))

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
#-----
# Only retain variables remaining in the codebook
d1 <- d[intersect(names(d), codebook$var)]
#-----
#Convert columns type to numeric for the non-double type
d01 <- d1 %>% select(.,-c('BMI','PURPOSE','COMMUNITY','PHYSICAL','FINANCIAL','SOCIAL','WELL_BEING_INDEX','WB_WEIGHT',
                          'STATE_NAME','MSANAME','CONGDIST','INT_DATE')) %>% select(.,-contains("_ADJUSTED"))
d01 <- sapply(d01, as.factor)

d02 <- d1 %>% select(.,c('BMI','PURPOSE','COMMUNITY','PHYSICAL','FINANCIAL','SOCIAL','WELL_BEING_INDEX','WB_WEIGHT',
                         'STATE_NAME','MSANAME','CONGDIST','INT_DATE'),contains("_ADJUSTED"))  %>%  
                          select(.,-contains("_ADJUSTED_BOX"),-c('OBESE_ADJUSTED'))

d03 <- d1 %>%  select(.,contains("_ADJUSTED_BOX"),'OBESE_ADJUSTED')
d03 <- sapply(d03, as.factor)

d <- cbind(d01,d02,d03)  %>% mutate(H12B_ADJUSTED = as.numeric(H12B_ADJUSTED),
                                    H12A_ADJUSTED  = as.numeric(H12A_ADJUSTED),
                                    HWB11_ADJUSTED   = as.numeric(HWB11_ADJUSTED),
                                    HWB2_ADJUSTED  = as.numeric(HWB2_ADJUSTED),
                                    HWB16_ADJUSTED  = as.numeric(HWB16_ADJUSTED),
                                    HWB22_ADJUSTED  = as.numeric(HWB22_ADJUSTED),
                                    HWB10_ADJUSTED   = as.numeric( HWB10_ADJUSTED),
                                    HWB7_ADJUSTED  = as.numeric(HWB7_ADJUSTED),
                                    HWB3_ADJUSTED  = as.numeric(HWB3_ADJUSTED), 
                                    HWB4_ADJUSTED  = as.numeric(HWB4_ADJUSTED),
                                    HWB20_ADJUSTED  = as.numeric(HWB20_ADJUSTED),
                                    HWB23_ADJUSTED = as.numeric(HWB23_ADJUSTED),
                                    HWB9_ADJUSTED = as.numeric(HWB9_ADJUSTED),
                                    HWB19_ADJUSTED  = as.numeric(HWB19_ADJUSTED), 
                                    H11_ADJUSTED  = as.numeric(H11_ADJUSTED), 
                                    H4_1A_ADJUSTED = as.numeric( H4_1A_ADJUSTED),
                                    H4_1B_ADJUSTED  = as.numeric(H4_1B_ADJUSTED),
                                    OBESE_ADJUSTED = as.numeric(OBESE_ADJUSTED))
                       
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
  # x <- as.factor(x)
  x[!is.na(m)] <- new.labels
  
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

#rm(d01,d02,d03)


# # Imputation:
# # Which variables have missing values and how frequent are they?
na.count <- colSums(is.na(d))
na.count <- na.count[na.count > 0]
na.count  

d <- fusionModel::impute(data = as.data.table(d), weight = 'WB_WEIGHT', 
                          ignore = c('MOTHERLODE_ID','YEAR','MONTH','INT_DATE'), cores = 2)

# # Which variables have missing values and how frequent are they?
na.count <- colSums(is.na(d))
na.count <- na.count[na.count > 0]
na.count  #

# Add/create variables for geographic concordance with variables in 'geo_concordance.fst'
d1 <- d %>% mutate(FIPS_CODE = sprintf("%05d",FIPS_CODE)) %>% separate(FIPS_CODE, into = c('STATE', 'COUNTY'), sep = 2) %>% 
  rename(
    zipcode = ZIPCODE,
    state_postal = STATE_NAME,
     cbsa15 = MSACODE,
    state = STATE) %>% 
  mutate(cbsa15 = ifelse(cbsa15 == 0,NA,cbsa15)) %>%
  mutate(state = as.factor(state)) %>% 
  mutate(state = stri_pad_left(state, 2, 0),
         zipcode  = sprintf("%05d",zipcode)) %>%
  left_join(zip_zcta, by = 'zipcode') %>% filter(!is.na(zcta10))  %>% 
  mutate(cbsa15 = ifelse(state == '15',NA,cbsa15),
         cbsa15 = ifelse(state == '09',NA,cbsa15)) %>%
  mutate(zcta10 = as.factor(zcta10)) %>% select(.,-c('zipcode','COUNTY')) 

rm(zip_zcta)

#d111 <- d1 %>% filter(state == '09')

#There are number of 2 zcta which have a different state associated with them compared to geo-concordance
#Fix the states for these as zipcode is the smallest geographic entity in GALLUP
#State names are derived from this as mentioned in the codebook
#Using geo-concordance to fix these variables

zipcode_cor <- fst::read_fst("survey-processed/GALLUP/2019/Zipcode-State_Correction_2019.fst") 

#Split d1 into d01 and d02 for zipcode correction
d01 <- d1 %>% select(.,-c('state','state_postal')) %>% merge(zipcode_cor, by = ('zcta10'))
d02 <- d1  %>% anti_join(zipcode_cor, by = ('zcta10'))

dxx <- rbind(d01,d02) %>% unique() #%>% mutate(zcta10 = as.factor(zcta10))
stopifnot(nrow(dxx) == nrow(d1))  


geo_conc <- fst::read_fst("geo-processed/concordance/geo_concordance.fst", 
                          column = c("division","state_postal",'region')) %>% unique() 

# See which variables in 'd' are also in 'geo_concordance' and
d1 <- dxx %>% 
      #select(.,-c('zcta10')) %>% 
      left_join(.,geo_conc, by = 'state_postal')# %>%
# mutate(#division = as.factor(division),
#       region = as.factor(region)) 
rm(dxx)
gnames <- names(fst::fst("geo-processed/concordance/geo_concordance.fst"))
gvars <- intersect(gnames, names(d1))
gvars

# Class new/added geo identifiers as unordered factors
d1 <- d1  %>% 
  mutate_at(gvars, ~ factor(.x, levels = sort(unique(.x)), ordered = FALSE)) 

# Assemble final output
# NOTE: var_label assignment is done AFTER any manipulation of values/classes, because labels can be lost when classes are changed
h.final <- d1 %>%
  mutate_if(is.factor, safeCharacters) %>%
  mutate_if(is.numeric, convertInteger) %>%
  mutate_if(is.double, cleanNumeric, tol = 0.001) %>%
  labelled::set_variable_labels(.labels = setNames(as.list(safeCharacters(codebook$desc)), codebook$var), .strict = FALSE) %>%  # Set descriptions for codebook variables
  rename_with(tolower) %>%
  labelled::set_variable_labels(.labels = setNames(as.list(paste(gvars, "geographic concordance")), gvars)) %>%  # Set descriptions for geo identifiers
  
  rename(weight = wb_weight,
         hid = motherlode_id)  %>%
  select(-state,-state_postal,-cbsa15,-region,-division) # Rename weight variables to standardized names 

#----------------

# CreateDictionary needs label for variables
labelled::var_label(h.final$hid) <- "Unique household identifier"
#labelled::var_label(h.final$division) <- "Census division"
#labelled::var_label(h.final$region) <- "Census region"

# Create dictionary and save to disk
dictionary <- createDictionary(data = h.final, survey = "GALLUP", vintage = 2019, respondent = "H")
saveRDS(object = dictionary, file = "survey-processed/GALLUP/2019/GALLUP_2019_H_dictionary.rds")

#----------------
# Save data to disk (.fst)
fst::write_fst(x = h.final, path = "survey-processed/GALLUP/2019/GALLUP_2019_H_processed.fst", compress = 100)
fusionData::compileDictionary()
