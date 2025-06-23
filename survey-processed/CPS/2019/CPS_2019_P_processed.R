# Clear the workspace
rm(list = ls())
gc()

library("readxl")
library('fusionModel')
library('tidyverse')
library('fusionData')
library('data.table')

source('R/utils.R')
options(scipen = 999)

#Use this only for the food module analysis
#You have to re-compile the dataset if you want to analyze labor markets

#Read Raw Data from CPS data
d <- read_csv('/Users/karthikakkiraju/Documents/fusionData/survey-raw/CPS/2019/dec19pub.csv') %>%
  select(-HUTYPEA,-HUTYPB,-HUTYPC,-HUINTTYP,-FILLER,-HUBUSL1,-HUBUSL2,-HUBUSL3,-HUBUSL4,-PEPARENT,
         -PRTFAGE,-PESPOUSE,
         -PUJHCK1,-PUJHCK2,-HXPHONEO,
         -PUJHCK3,-PUJHCK4,-PUJHCK5,
         -PUIOCK1,-PUIOCK2,-PUIOCK3,
         -PRIOELG,-PXNLFRET,-PXSCHLVL,
         -PXDISEAR, -PXDISEYE, -PXDISREM, -PXDISPHY, -PXDISDRS, -PXDISOUT, -PRDASIAN,
         -PXCERT1,-PXCERT2, -PXCERT3,-PXRACE1,
         -PUNLFCK1, -PUNLFCK2,-PRHERNAL, 
         -PULAYCK1,-PULAYCK2,-PULAYCK3,-PRERELG,
         -PUBUSCK1,-PUBUSCK2,-PUBUSCK3,-PUBUSCK4,
         -PUDWCK1,-PUDWCK2,-PUDWCK3,-PUDWCK4,-PUDWCK5,
         -PUHRCK1,-PUHRCK2,-PUHRCK3,-PUHRCK4,-PUHRCK5,-PUHRCK6,-PUHRCK7,-PUHRCK12,
         -PTHR,-PTOT,-PXCOHAB,-HXHOUSUT,-HXTENURE,-PXSEX,-PRCITFLG,
         -PXINUSYR,-PXAFWHN1,-PXNATVTY, -PXMNTVTY, -PXFNTVTY ,-PXNMEMP1 ,-PXMLR, -PXRET1, -PXABSRSN, 
         -PXABSPDO,-PXMJOT , -PXMJNUM ,-PXHRUSL1 ,-PXHRUSL2 ,-PXHRFTPT ,-PXHRUSLT ,-PXHRWANT ,
         -PXHRRSN1 ,-PXHRRSN2 ,-PXHRACT1 ,-PXHRACT2 ,-PXHRACTT ,-PXHRRSN3 ,-PXHRAVL ,-PXLAYAVL ,
         -PXLAYLK ,-PXLAYDUR ,-PXLAYFTO ,-PXLKM1 ,-PXLKAVL ,-PXLKLL1O ,-PXLKLL2O ,-PXLKLWO ,
         -PXLKDUR ,-PXLKFTO ,-PXDWWNTO ,-PXDWRSN ,-PXDWLKO ,-PXDWWK ,-PXDW4WK ,-PXDWLKWK ,-PXDWAVL ,-PXDWAVR ,
         -PXJHWKO ,-PXJHRSN ,-PXJHWANT ,-PXIO1COW ,-PXIO1ICD ,-PXIO1OCD ,-PXIO2COW ,-PXIO2ICD ,-PXIO2OCD ,-PXERNUOT  ,
         -PXERNPER ,-PXERNH1O ,-PXERNHRO ,-PXERN ,-PXPDEMP2 ,-PXNMEMP2 ,-PXERNWKP ,-PXERNRT ,
         -PXERNHRY ,-PXERNH2 ,-PXERNLAB ,-PXERNCOV ,-PXNLFJH ,-PXNLFACT ,-PXSCHENR,-PXSCHFT,              -PXPDEMP1, -PRWERNAL,
         -HESP3OTC,-HETSP9) %>%
  rename(GESTFIPS = GCFIP,
         GTCBSA = GCTCB,
         GTCSA = GCTCS,
         GTCO = GCTCO,
         PRINUSYR = PRINUYER) %>% select(-GTCO) %>% 
  mutate(GTINDVPC = paste0(GTCBSA,GTINDVPC)) %>% 
  filter(HRINTSTA == 1) %>%  # Keep only households who were interviewed in person
  filter(PWSUPWGT > 0)

#Columns with -1 values. These are from the Edited Universe value
cols_with_neg <- as.data.frame(colnames(d)[sapply(d, function(col) any(col == -1, na.rm = TRUE))])
colnames(cols_with_neg) <- 'var'

#Columns with -2 values. 
cols_with_mi2 <- as.data.frame(colnames(d)[sapply(d, function(col) any(col == -2, na.rm = TRUE))])
colnames(cols_with_mi2) <- 'var'

#Columns with -3 values. 
cols_with_mi3 <- as.data.frame(colnames(d)[sapply(d, function(col) any(col == -3, na.rm = TRUE))])
colnames(cols_with_mi3) <- 'var'

#Columns with -4 values. 
cols_with_mi4 <- as.data.frame(colnames(d)[sapply(d, function(col) any(col == -4, na.rm = TRUE))])
colnames(cols_with_mi4) <- 'var'

#Columns with -5 values. 
cols_with_mi5 <- as.data.frame(colnames(d)[sapply(d, function(col) any(col == -5, na.rm = TRUE))])
colnames(cols_with_mi5) <- 'var'

#Columns with -6 values. 
cols_with_mi6 <- as.data.frame(colnames(d)[sapply(d, function(col) any(col == -6, na.rm = TRUE))])
colnames(cols_with_mi6) <- 'var'

#Columns with -9 values. 
cols_with_mi9 <- as.data.frame(colnames(d)[sapply(d, function(col) any(col == -9, na.rm = TRUE))])
colnames(cols_with_mi9) <- 'var'

#Columns with 1 values. 
cols_with_val1 <- as.data.frame(colnames(d)[sapply(d, function(col) any(col == 1, na.rm = TRUE))])
colnames(cols_with_val1) <- 'var'

#Columns with 2 values. 
cols_with_val2 <- as.data.frame(colnames(d)[sapply(d, function(col) any(col == 2, na.rm = TRUE))])
colnames(cols_with_val2) <- 'var'

#Columns with 3 values. 
cols_with_val3 <- as.data.frame(colnames(d)[sapply(d, function(col) any(col == 3, na.rm = TRUE))])
colnames(cols_with_val3) <- 'var'

#-----
codebook_a <- read_csv('/Users/karthikakkiraju/Documents/fusionData/survey-raw/CPS/2019/tabula-cpsdec19.csv')  %>%
  mutate(NAME = ifelse(NAME == 'NAME',NA,NAME)) %>%
  # filter(SIZE != 'NAME') %>%
  fill(NAME, .direction = "down") %>% filter(!is.na(NAME)) %>% 
  mutate(DESCRIPTION = ifelse(NAME %in% c('GEREG','GEDIV','GESTFIPS','PEMLR','PRINUSYR','GTCBSASZ','PUWK') & !is.na(SIZE), paste0(SIZE,'',DESCRIPTION),DESCRIPTION)) %>%
  mutate(DESCRIPTION = ifelse(NAME %in% c('PULKDK2','PULKDK5','PULKM3','PULKM4','PULKM5','PULKM6','PULKPS1','PULKPS2','PULKPS3') & is.na(DESCRIPTION), LOCATION,DESCRIPTION)) %>%
  
  select(-SIZE,-LOCATION) %>% filter(!is.na(DESCRIPTION)) %>%
  mutate(EDITED_UNIVERSE = str_extract(DESCRIPTION, "(?<=: ).*")) %>%
  group_by(NAME) %>%
  mutate(
    valid_group = cumsum(DESCRIPTION %in% c("VALID ENTRIES","VALID ENTRIES:",'VALID ENTRIESNA')),  # Number each VALID ENTRIES group
    is_valid = (valid_group > 0 & DESCRIPTION != "VALID ENTRIES") | (is_valid = valid_group > 0 & DESCRIPTION != "VALID ENTRIES:"),  # Keep only rows after VALID ENTRIES
    ,  # Keep only rows after VALID ENTRIES
    value = ifelse(is_valid, str_extract(DESCRIPTION, "^\\d+"), NA),  # Extract first number
    code = ifelse(is_valid, str_trim(str_remove(DESCRIPTION, "-?\\d+")), NA))  %>% ungroup() %>%
  filter(DESCRIPTION != 'DESCRIPTION LOCATION')   %>%   
  fill(EDITED_UNIVERSE, .direction = "down") %>%
  group_by(NAME) %>%
  
  mutate(
    start_flag = cumsum(DESCRIPTION %in% c("EDITED UNIVERSE","EDITED UNIVERSE:")),  # Mark EDITED UNIVERSE occurrences
    end_flag = cumsum(DESCRIPTION == "VALID ENTRIES:"),  # Mark VALID ENTRIES occurrences
    within_range = start_flag > 0 & start_flag > lag(end_flag, default = 0) & 
      DESCRIPTION != "EDITED UNIVERSE:" & DESCRIPTION != "VALID ENTRIES:"  # Extract between
  ) %>%
  group_by(NAME, start_flag) %>%  # Group by NAME and EDITED UNIVERSE occurrence
  mutate(EDITED_UNIVERSE2 = paste(DESCRIPTION[within_range], collapse = ", "), .groups = "drop") %>%
  
  mutate(value = ifelse(code == 'Don\'t Know',NA,value),
         value = ifelse(code == 'Refused',NA,value),
         value = ifelse(code == 'No Response',NA,value)) %>% ungroup() %>%
  
  select(NAME,DESCRIPTION, code,value,EDITED_UNIVERSE) %>% rename(variable = NAME)  %>%
  group_by(variable) %>%
  mutate(
    # Create a flag to stop accumulating after "VALID ENTRIES"
    accumulate_desc = cumsum(DESCRIPTION %in% c("VALID ENTRIES","VALID ENTRIES:")) == 0,  
    # Concatenate descriptions until "VALID ENTRIES" is encountered
    concatenated_desc = ifelse(accumulate_desc, DESCRIPTION, NA)
  )  %>% mutate(description = paste(na.omit(concatenated_desc), collapse = " "), .groups = "drop") %>% ungroup() %>%
  select(-concatenated_desc) %>%
  select(variable, description, code,value) %>% group_by(variable) %>% #filter(code != 'VALID ENTRIES:') %>%
  
  mutate(description = gsub("41 - 42|43 - 44|45 - 46|24 - 26|365 - 366|373 - 374|444 - 445|460 - 461|462 - 463|468 - 469|466 - 467|474 - 475|516 - 519|789-790|202 - 203|1237 -1240|1235 -1236|1221 -1222|174 - 175| 
                            29 - 30|31 - 32|33 - 34|428 - 429|426 - 427|430 - 431|432 - 433|434 - 435 |442 - 443|520 - 523|524 - 524|525 - 526|556 - 556|569 - 570|681-682|353 - 354|834 - 835|836 - 837|838 - 839|846 - 855|856 - 859| 
                            35 - 36|39 - 40|37 - 38|296 - 297|298 - 299|300 - 301|349 - 350|351 - 352|367 - 368|369 - 370|371 - 372|385 - 386|405 - 406|412 - 413|407 - 409|414 - 415|416 - 417|464 - 465|470 - 471|
                            35 - 36|120 - 121|124 - 124|139 – 140|166 - 168|169 - 171|172 - 173|198 - 199|137 - 138|204 - 205|206 - 207|214 - 215|216 - 217|1263 -1266|1261 -1262|1259 -1260|1233 -1234|1223 -1224|1219 -1220|
                            261 - 262|263 - 264|265 – 266|267 - 268|269 - 270|271 – 272|218 - 219|220 - 221|222 - 223|224 - 226|227 - 228|229 - 230|231 - 232|233 - 234|245 - 246|247 - 249|250 - 251|257 - 258|259 - 260|261 - 262|273 - 274|
                            787-788 |637-638|886 - 887|882 - 883|874 - 875|826 - 827|573 - 574|567 - 568|548 - 555|540 - 547|527 - 534|35 - 36|29 - 30|122 - 123|135 - 136|200 - 201","",description))  %>%
  
  unique() %>% filter(code != 'VALID ENTRIES' | is.na(code)) %>% ungroup() %>%
  group_by(variable) %>% mutate(n = n()) %>%
  
  mutate(value = ifelse(code == 'Don\'t Know',-2,
                        ifelse(code == 'Refused',-3,
                               ifelse(code == 'No Response',-9,value)))) %>%
  
  filter((!is.na(code) & !is.na(value) & n != 1)| n == 1) %>%
  mutate(
    MIN_VALUE = ifelse(any(code == "MIN VALUE"), value[code == "MIN VALUE"], NA),  # Extract MIN
    MAX_VALUE = ifelse(any(code == "MAX VALUE"), value[code == "MAX VALUE"], NA)) %>% 
  
  mutate(Concatenated_Value = paste0(MIN_VALUE, "-", MAX_VALUE),
         value = ifelse(code %in% c('MIN VALUE','MAX VALUE'),Concatenated_Value,value),
         code = gsub('MIN VALUE',description,code),
         code = gsub('MAX VALUE',description,code)) %>% unique() %>%
  select(variable,description,code,value) %>% rename(label = code,
                                                     var = variable)  

codebook_st <- codebook_a %>% filter(var == 'GESTFIPS')  %>%
  mutate(new_column = str_sub(label, 3),
         label = str_sub(label, 1, 2) )  # Extract everything after the first two characters

codebook_st_B <- codebook_st %>% select(var, description,new_column)  %>%
  mutate(label = str_sub(new_column, 4),
         value = str_sub(new_column, 1, 3)) %>% select(-new_column) %>% mutate(label = trimws(label),
                                                                               value = trimws(value)) %>% filter(label != "")

codebook_b <- codebook_st %>% select(-new_column) %>% rbind(codebook_st_B) %>% rbind(codebook_a) %>%
  mutate(label = ifelse(var == 'PRNMCHLD', NA,label),
         value = ifelse(var == 'PRNMCHLD', NA,value)) %>%
  
  mutate(label = ifelse(var == 'HRFS30D6' & label == ':8 Number of affirmative Responses', NA,label),
         value = ifelse(var == 'HRFS30D6' & label == ':8 Number of affirmative Responses', NA,value),
         value = ifelse(var == 'HRFS30D6' & label == 'No Affirmative Responses or Did not pass initial screen', '-1',value)) #%>%

###Adding country codes from Appendix#####
codebook_country <- read_csv('/Users/karthikakkiraju/Documents/fusionData/survey-raw/CPS/2019/tabula-cpsdec19_appendix.csv') %>%
  mutate(var = 'PENATVTY',
         desc = 'COUNTRY OF BIRTH')

codebook_country2 <- read_csv('/Users/karthikakkiraju/Documents/fusionData/survey-raw/CPS/2019/tabula-cpsdec19_appendix.csv') %>%
  mutate(var = 'PEFNTVTY',
         desc = 'FATHER\'S COUNTRY OF BIRTH EDITED UNIVERSE: PRPERTYP = 1, 2, 0R 3')

codebook_country3 <- read_csv('/Users/karthikakkiraju/Documents/fusionData/survey-raw/CPS/2019/tabula-cpsdec19_appendix.csv') %>%
  mutate(var = 'PEMNTVTY',
         desc = 'MOTHER\'S COUNTRY OF BIRTH EDITED UNIVERSE: PRPERTYP = 1, 2, 0R 3')


codebook_city <- read_csv('/Users/karthikakkiraju/Documents/fusionData/survey-raw/CPS/2019/tabula-cpsdec19_city_appendix.csv', skip = 1) %>%
  fill(Code) %>% mutate(City = ifelse(is.na(City),Check,City),
                        test =  gsub("[^[:digit:]]","",Check),
                        GTINDVPC = ifelse(is.na(GTINDVPC),test,GTINDVPC),
                        GTINDVPC = ifelse(is.na(GTINDVPC),0,GTINDVPC))  %>% #filter(is.na(City))
  
  filter(!is.na(GTINDVPC)) %>% mutate(value = paste0(Code,GTINDVPC)) %>% rename(label = City) %>% select(label,value) %>%
  mutate(var = 'GTINDVPC',
         desc = 'INDIVIDUAL PRINCIPAL CITY EDITED UNIVERSE: ALL HHLD\'s IN SAMPLE') %>% select(var,desc,value,label) %>%
  add_row(.,var = "GTINDVPC",desc = "INDIVIDUAL PRINCIPAL CITY EDITED UNIVERSE: ALL HHLD\'s IN SAMPLE", value = '00000', label = 'NOT IDENTIFIED, NONMETROPOLITAN') 


#-----
codebook_indus_1 <- read.csv('/Users/karthikakkiraju/Documents/fusionData/survey-raw/CPS/2019/tabula-cpsdec19_industry_codes1.csv', header = FALSE) 


codebook_indus_1a <- codebook_indus_1 %>%
  mutate(
    V2 = if_else(str_length(V1) > 4, str_sub(V1, 5), V2),
    V1 = if_else(str_length(V1) > 4, str_sub(V1, 1, 4), V1)
  )  %>%
  filter(V1 != "" & V2 != "") %>% select(V1,V2)  %>%  mutate(V2 = gsub("[0-9]", "", V2),
                                                             V1 = str_replace(V1, "^0+", "")) %>%
  mutate(var = 'PEIO1ICD',
         
         desc = 'INDUSTRY CODE FOR PRIMARY JOB') %>% rename(label = V2,
                                                            value = V1) 


codebook_indus_1b <- codebook_indus_1 %>%
  mutate(
    V2 = if_else(str_length(V1) > 4, str_sub(V1, 5), V2),
    V1 = if_else(str_length(V1) > 4, str_sub(V1, 1, 4), V1)
  )  %>%
  filter(V1 != "" & V2 != "") %>% select(V1,V2)  %>%  mutate(V2 = gsub("[0-9]", "", V2),
                                                             V1 = str_replace(V1, "^0+", "")) %>%
  mutate(var = 'PEIO2ICD',
         desc = 'INDUSTRY CODE FOR PRIMARY JOB') %>% rename(label = V2,
                                                            value = V1) 



codebook_indus_2 <- read.csv('/Users/karthikakkiraju/Documents/fusionData/survey-raw/CPS/2019/tabula-cpsdec19_industry_codes2.csv', header = FALSE) 


codebook_indus_2a <- codebook_indus_2 %>%
  mutate(
    V2 = if_else(str_length(V1) > 4, str_sub(V1, 5), V2),
    V1 = if_else(str_length(V1) > 4, str_sub(V1, 1, 4), V1)
  )  %>%
  filter(V1 != "" & V2 != "") %>% select(V1,V2)  %>%  mutate(V2 = gsub("[0-9]", "", V2),
                                                             V1 = str_replace(V1, "^0+", "")) %>%
  mutate(var = 'PEIO1OCD',
         
         desc = 'INDUSTRY CODE FOR PRIMARY JOB') %>% rename(label = V2,
                                                            value = V1) 


codebook_indus_2b <- codebook_indus_2 %>%
  mutate(
    V2 = if_else(str_length(V1) > 4, str_sub(V1, 5), V2),
    V1 = if_else(str_length(V1) > 4, str_sub(V1, 1, 4), V1)
  )  %>%
  filter(V1 != "" & V2 != "") %>% select(V1,V2)  %>%  mutate(V2 = gsub("[0-9]", "", V2),
                                                             V1 = str_replace(V1, "^0+", "")) %>%
  mutate(var = 'PEIO2OCD',
         desc = 'INDUSTRY CODE FOR PRIMARY JOB') %>% rename(label = V2,
                                                            value = V1) 

codebook_indus <- rbind(codebook_indus_1b,codebook_indus_1a, 
                        codebook_indus_2b,codebook_indus_2a)

rm(codebook_st,codebook_st_B,codebook3)

###Replace Edited Universe values###
# These are the variables for which suitable replacement values must be specified below

# Manually constructed...
# What value should "-1" take for the following variables?
eduni.values <- list(
  
  HURESPLI = NA,
  HETELAVL  = 'NO',
  PEMARITL = 'NEVER MARRIED',
  PEAFEVER = 'NO',
  PEAFNOW = 'NO',
  PEEDUCA = 'CHILD HOUSEHOLDER',
  PRDTHSP = 'Not Hispanic',
  PUCHINHH = 'No change',
  PRMARSTA = 'Not in armed forces',
  PUSLFPRX = 'CHILD',
  PEMLR = 'NOT IN LABOR FORCE-CHILD',
  PUWK = 'NOT IN LABOR FORCE-CHILD',
  PUBUS1 = 'No family business/farm',
  PUBUS2OT = 'No family business/farm',
  PURETOT = 'Not retired last month',
  
  PUDIS = 'No disability',
  PERET1 = 'Not old',
  PUABSOT = 'NO',
  PULAY = 'NO',
  PEABSRSN = 'AT WORK',
  PEABSPDO = 'NOT WORKING',
  PEMJOT = 'NO',
  PEMJNUM = 'Not more than one job',
  
  PEHRUSL1 = 0,
  PEHRUSL2 = 0,
  
  PEHRFTPT = 'Fixed hours',
  PEHRUSLT = 'Fixed hours',
  PEHRWANT = 'Already working  more than 35 hours',
  
  PEHRRSN1 = 'Dont want to work full-time',
  PEHRRSN2 = 'Want to work full-time',
  PEHRRSN3 = 'Did not work less than 35 hours',
  PUHROFF1 = 'NO',
  PUHROFF2 = 0,
  
  PUHROT1 = 'NO',
  PUHROT2 = 0,
  PEHRACT1 = 'Not working' ,
  PEHRACT2 = 'No 2 jobs',
  PEHRACTT = 'Not working',
  PEHRAVL = 'Already working full-time',
  PULAYDT = 'Not working',
  PULAY6M = 'Not working',
  
  PELAYAVL = 'Not working',
  PULAYAVR = 'OTHER',
  PELAYLK = 'Already working',
  PELAYDUR = 0,
  PELAYFTO = 'NO',
  PULK = 'Already working',
  PELKM1 = 'Already working',
  PELKM2 = 'Already working',
  PELKM3 = 'Already working',
  PELKM1 = 'Already working',
  PULKM2  = 'Already working',
  PULKM3 = 'Already working',
  PULKM4 = 'Already working',
  PULKM5 = 'Already working',
  PULKM6 = 'Already working',
  PULKDK1 = 'Already working',
  PULKDK2 = 'Already working',
  PULKDK3 = 'Already working',
  PULKDK4 = 'Already working',
  PULKDK5 = 'Already working',
  PULKDK6 = 'Already working',
  PULKPS1 = 'Already working',
  PULKPS2 = 'Already working',
  PULKPS3 = 'Already working',
  PULKPS4 = 'Already working',
  PULKPS5 = 'Already working',
  PULKPS6 = 'Already working',
  PELKAVL = 'Already working',
  PULKAVR = 'Already working',
  
  PELKLL1O = 'SOMETHING ELSE',
  PELKLL2O =  'Already working',
  PELKLWO =  'SOMETHING ELSE',
  PELKDUR =  0,
  PELKFTO =  'DOESN\'T MATTER',
  PEDWRSN = 'OTHER - SPECIFY',
  PEDWLKO = 'NO',
  PEDWWK = 'NO',
  PEDW4WK = 'No',
  PEDWLKWK = 'No',
  PEDWAVL = 'Already working',
  PEDWAVR =  'Already working',
  PEDWWNTO = 'NO',
  PEJHWKO = 'NO',
  PUJHDP1O = 'No',
  PEJHRSN = 'Did not leave',
  PEJHWANT = 'Already working',
  PRABSREA = 'At work',
  PRCIVLF = 'NOT IN CIVILIAN LABOR FORCE',
  PRDISC = 'Not seeking jobs',
  PREMPHRS = 'Retired',
  PREMPNOT = 'NOT IN LABOR FORCE (NILF)-other',
  PREXPLF = 'NOT IN LABOR FORCE',
  PRFTLF  = 'NOT IN LABOR FORCE',
  PRHRUSL = 0,
  PRJOBSEA = 'Not looking for job',
  PRPTHRS = 'Full-time already or retired',
  PRPTREA = 'Full-time already or retired',
  PRUNEDUR = 0,
  PRUNTYPE = 'Not unemployed',
  PRWKSCH = 'NOT IN LABOR FORCE',
  PRWKSTAT = 'NOT IN LABOR FORCE',
  PRWNTJOB = 'OTHER NOT IN LABOR FORCE',
  
  PUIODP1 = 'NOT WORKED LAST MONTH',
  PUIODP2 =  'NOT WORKED LAST MONTH',
  PUIODP3 = 'NOT WORKED LAST MONTH',
  
  PRAGNA =  'NOT IN LABOR FORCE',
  PRCOW1 = 'NOT IN LABOR FORCE',
  PRCOW2 = 'NOT IN LABOR FORCE',
  PRCOWPG = 'NOT IN LABOR FORCE',
  PRDTCOW1 = 'NOT IN LABOR FORCE',
  PRDTCOW2 = 'NOT IN LABOR FORCE',
  PRDTIND1 =  'NOT IN LABOR FORCE',
  PRDTIND2 = 'No second job',
  PRDTOCC1 = 'NOT IN LABOR FORCE',
  PRDTOCC2 = 'NOT IN LABOR FORCE',
  PREMP = 'NOT IN LABOR FORCE',
  PRMJIND1 = 'NOT IN LABOR FORCE',
  PRMJIND2 = 'NOT IN LABOR FORCE',
  PRMJOCC1 = 'NOT IN LABOR FORCE',
  PRMJOCC2 = 'NOT IN LABOR FORCE',
  PRMJOCGR = 'NOT IN LABOR FORCE',
  PRNAGPWS = 'NOT IN LABOR FORCE',
  PRNAGWS = 'NOT IN LABOR FORCE',
  PRSJMJ = 'NOT IN LABOR FORCE',
  PEERNUOT = 'No',
  PEERNPER = 'NOT IN LABOR FORCE',
  PEERNRT = 'NOT IN LABOR FORCE',
  PEERNHRY = 'NOT IN LABOR FORCE',
  PUERNH1C = 'NOT IN LABOR FORCE',
  PEERNH2 = 'NOT IN LABOR FORCE',
  PEERNH1O = 'NOT IN LABOR FORCE',
  PRERNHLY = 0,
  PEERNHRO = 0,
  PRERNWA = 0,
  PEERN = 'NOT IN LABOR FORCE',
  PUERN2 =  0,
  PEERNWKP = 0,
  PEERNLAB = 'NOT IN LABOR FORCE',
  PEERNCOV = 'NOT IN LABOR FORCE',
  PENLFJH = 'IN LABOR FORCE',
  PENLFRET  = 'IN LABOR FORCE',
  PENLFACT = 'SOMETHING ELSE/OTHER',
  
  PESCHENR = 'No',
  PESCHFT = 'No',
  PRNLFSCH = 'NOT IN SCHOOL',
  
  PEDIPGED = 'Did not graduate high school',
  PEHGCOMP = 'Received GED or higher',
  PEIO1ICD = 'Not in industry', 
  PEIO1OCD ='Not in industry',
  PEIO2ICD = 'No second job',
  PEIO2OCD = 'No second job',
  PRIMIND1 =   'Not in industry',
  PRIMIND2 = 'Not in industry',
  PEAFWHN1 = 'Not in army',
  PEAFWHN2 = 'Not in army', 
  PEAFWHN3 = 'Not in army',
  PEAFWHN4 = 'Not in army',
  PELNDAD = 'NO FATHER PRESENT',
  PELNMOM = 'NO MOTHER PRESENT',
  PEDADTYP = 'NO FATHER PRESENT',
  PEMOMTYP = 'NO MOTHER PRESENT',
  PECOHAB = 'NO PARTNER PRESENT',
  PEDISEAR = 'Child or person in armed force',
  PEDISEYE = 'Child or person in armed force',
  PEDISREM = 'Child or person in armed force',
  PEDISPHY = 'Child or person in armed force',
  PEDISDRS = 'Child or person in armed force',
  PEDISOUT = 'Child or person in armed force',
  PRDISFLG = 'No',
  
  PEPDEMP1 = 'Not self-employed',
  PTNMEMP1 = 0, 
  PEPDEMP2 = 'NO',
  PTNMEMP2 = 'Not self-employed',
  PECERT1 = 'Not adult',
  PECERT2 =  'No certifications',
  PECERT3 = 'No certifications',
  
  
  PEIO1COW = 'NOT IN LABOR FORCE',
  PUIO1MFG = 'NOT IN LABOR FORCE',
  PEIO2COW = 'NOT IN LABOR FORCE',
  PUIO2MFG = 'NOT IN LABOR FORCE',
  PUDIS1 = 'No disability',
  PUDIS2 = 'No disability',
  
  HES1A = NA,
  HES1B = NA,
  HES1C = NA,
  HES1D = NA, 
  HETS2O =NA,
  HETS3O =  NA,
  HETS4O = NA,
  HETS5O = NA, 
  HETS6O = NA,
  HETS7O = NA,
  HETS8O = NA,
  HETS8OU = NA,
  HES8B = NA, 
  HETS8CO = NA,
  HETS8DO = NA,
  
  PESCHLVL = 'Not student',
  PECYC = 'Not in college',
  HESS2 = 'Not poor',
  
  
  HESP1 = 'Not poor',
  HESP21 = 'Did not receive SNAP at all',
  HESP22 = 'Did not receive SNAP at all',
  HESP23 = 'Did not receive SNAP at all',
  HESP24 = 'Did not receive SNAP at all',
  HESP25 = 'Did not receive SNAP at all',
  HESP26 = 'Did not receive SNAP at all',
  HESP27 = 'Did not receive SNAP at all',
  HESP28 = 'Did not receive SNAP at all',
  HESP29 = 'Did not receive SNAP at all',
  HESP210 = 'Did not receive SNAP at all',
  HESP211 = 'Did not receive SNAP at all',
  HESP212 = 'Did not receive SNAP at all',
  
  HETSP2D  = 'Did not receive SNAP at all',
  HETSP3O  = 0,
  
  HESP6 = 'Not poor/No child',
  HESP7 = 'Not received free or reduced-cost lunches at school',
  HESP7A = 'Not poor/No child',
  HESP8 = 'Not poor/No child/Not women',
  HETSP9 = 'Not poor/No child/Not women',
  
  HETSS2  = 'Not poor',
  HESSM2  = 'No',
  HESS3 = 'Not poor',
  HESSM3  = 'No',
  HESS4 = 'Never true',
  HESSM4  = 'No',
  HESH2 = 'No',
  HESHF2 = 'Never',
  HESHM2 = 'No',
  HETSHMF2 = 0,
  HESH3  = 'No',
  HESHF3 = 'Never',
  HESHM3 = 'No',
  HETSHMF3 = 0,
  HESH4 = 'No',
  HESHF4  = 'Never',
  HESHM4 = 'No',
  HETSHMF4 =  0,
  HESH5 = 'No',
  HESHM5 = 'No',
  
  HESSH1  = 'No',
  HESSHF1 = 'Never',
  HESSHM1  = 'No',
  HETSSHMF1 =  0,
  HESS5 = 'Never true',
  HESSM5 = 'No',
  HESS6 = 'No children',
  HESSM6   = 'No',
  
  HESH1 =  'No children',
  HESHM1 = 'No',
  HESSH2 =  'No children',
  HESSHF2 =  'No children',
  HESSHM2 = 'No',
  
  HETSSHMF2 = 0,
  HESSH3 =  'No children',
  HESSHF3 =  'No children',
  HESSHM3  =  'No children',
  HETSSHMF3 = 0,
  
  HESSH4 = 'No children',
  HESSHF4 = 'Never',
  HESSHM4 = 'No children',
  HETSSHMF4 = 0,
  HESSH5 =  'No children',
  
  HESC1 = 'Not old/poor',  
  HESC2 = 'Not old/poor',  
  HESC3 = 'Not old/poor',  
  
  HESCF3 = 'Not old/poor',
  HESCM3 = 'Did not meet criteria',
  HESC3A = 'No',
  HESC4 = 'Did not meet criteria',
  HESCF4 =  'Never ate at soup kitchen',
  HESCM4 =  'Did not meet criteria',
  
  
  HRFS12MC = 'No child',
  HRFS12M6 = 'No child',
  HRFS12M7 = 'No child',
  HRFS12M4 = 'No child',
  
  HRFS30DE = 'No score assisgned',
  HRFS30D5 = 'No children',
  HRFS30D6 = 'No children',
  HRFS30D7 = 'No children',
  HESSM6 = 'Children have balanced meal'
)

df <- tibble(var = names(eduni.values), label = unlist(eduni.values))
cols_with_neg2 <- cols_with_neg %>% left_join(df, by = 'var') %>% mutate(value = '-1',
                                                                         description = NA)

codebook_c <- codebook_b %>% group_by(var) %>% rbind(cols_with_neg2) %>%
  fill(description, .direction = "downup") %>% ungroup()

# Manually constructed...
# What value should "-6" take for the following variables?
na.values <- list(
  
  HRFS12M4  = '0',
  HRFS12M7 = '0',
  HRFS12ME = '0',
  HRFS30D4 = '0',
  HRFS30D7 = '0',
  HRFS30DE  = '0'
)

df <- tibble(var = names(na.values), label = unlist(na.values))
cols_with_mi6a <- cols_with_mi6 %>% left_join(df, by = 'var') %>% mutate(value = '-6',
                                                                         description = NA)

# What value should "-3" take for the following variables?
na.min3 <- list(
  
  HETELAVL = NA,
  HUBUS = NA, 
  PUWK = NA,
  PUBUS1 = 'No family business/farm',
  PUDIS = NA,
  PUDIS2 = 'No disability',
  PUABSOT= NA,
  PULAY = NA,
  PUHROFF1 = NA,
  PUHROFF2 = 0,
  HESSH2 = 'No children',
  PUHROT1 = NA,
  PULK = NA,
  PULKPS1 = 'Already working',
  PUIODP1 = 'NOT WORKED LAST MONTH',
  PUIODP3 = 'NOT WORKED LAST MONTH',
  PUIO1MFG = NA,
  PEERNRT = NA,
  HES1A = NA,
  HES1D = NA,
  HESP6 = NA,
  HESP8 = NA,
  HESS1 = NA,
  HESS2 = NA,
  HESSM2 = NA,
  HESH2 = NA,
  HESHM2 = NA,
  HESH3 = NA,
  HESHM4 = NA,
  HESSH1 = NA,
  HESSHF1 = 'Never',
  HESHM1  = 'No',
  HESSHF3 = 'No children',
  HESS5 = NA,
  HESH1 = NA,
  HESSH4 = NA,
  HESCF4 = 'Never ate at soup kitchen',
  HESC1 = NA,
  HESC4 = NA)

df <- tibble(var = names(na.min3), label = unlist(na.min3))
cols_with_mi3a <- cols_with_mi3 %>% left_join(df, by = 'var') %>% mutate(value = '-3',
                                                                         description = NA)

# What value should "-4" take for the following variables?
na.min4 <- list(
  
  PEHRUSL1 = 'HOURS VARY',
  PEHRUSL2= 'HOURS VARY', 
  PEHRUSLT= 'HOURS VARY')

df <- tibble(var = names(na.min4), label = unlist(na.min4))
cols_with_mi4a <- cols_with_mi4 %>% left_join(df, by = 'var') %>% mutate(value = '-4',
                                                                         description = NA)

# What value should "-4" take for the following variables?
na.min5 <- list(
  HRFS12CX = 'Missing No Valid Scale Items')

df <- tibble(var = names(na.min5), label = unlist(na.min5))
cols_with_mi5a <- cols_with_mi5 %>% left_join(df, by = 'var') %>% mutate(value = '-5',
                                                                         description = NA)
na.min9 <- list(
  HESP6 = NA,
  HRFS12M1 = NA,
  HESP7A = NA,
  HESP8 = NA,
  HESS2 = NA,
  HESSM2 = NA,
  HESS3 = NA,
  HESSM3 = NA,
  HESS4 = NA,
  HESH2 = NA,
  HESHF2 = 'Never',
  HESHM2 = NA,
  HESH3 = NA,
  HESHF3 = NA,
  HESHM3 = NA,
  HETSHMF3 = NA,
  HESH4 = NA,
  HESHM4 = NA,
  HESH5 = NA,
  HESSH1 = NA,
  HESS5 = NA,
  HESS6 = NA,
  HESH1 = NA,
  HESC1 = NA,
  HESC2 = NA,
  HESC3 = NA,
  HESCM3 = NA,
  HESC3A = 'No',
  HESHM1  = 'No',
  HESC4 = NA,
  HRFS12M1 = NA,
  HRFS12MD = NA,
  HRFS12MD = NA,
  HRFS12M3 = NA,
  HRFS12M4 = NA,
  HRFS12MC = NA,
  HRFS12M6 = NA,
  HRFS12M7 = NA,
  HRFS12M8 = NA,
  HRFS12M9 = NA,
  HRFS12ME = NA,
  HRFS30D1 = NA,
  HESSHF3 = 'No children',
  HRFS30D2 = NA,
  HRFS30D3 = NA,
  HRFS30D4 = NA,
  HRFS30D5 = NA,
  HRFS30D6 = NA,
  HRFS30D7 = NA,
  HRFS30D8 = NA,
  HESCF4 = 'Never ate at soup kitchen',
  HRFS30D9 = NA,
  HRFS30DE = NA)

df <- tibble(var = names(na.min9), label = unlist(na.min9))
cols_with_mi9a <- cols_with_mi9 %>% left_join(df, by = 'var') %>% mutate(value = '-9',
                                                                         description = NA)
na.min2 <- list(
  HETELAVL = NA,
  HUBUS  = NA,
  PUWK  = NA,
  PUBUS1  = 'No family business/farm',
  PUDIS  = NA,
  PERET1  = 'Not old',
  PUDIS1  = NA,
  PUDIS2  = 'No disability',
  PUABSOT  = NA,
  PULAY  = NA,
  PUHROFF1  = NA,
  PUHROFF2  = 0,
  PUHROT1  = NA,
  PUHROT2  = NA,
  PULAYDT  = 'Not working',
  PULAY6M  = 'Not working',
  PULK  = NA,
  PELKM1  = 'Already working',
  PULKDK1  = 'Already working',
  PULKPS1  = 'Already working',
  PUIODP1  = 'NOT WORKED LAST MONTH',
  PUIODP2  = 'NOT WORKED LAST MONTH',
  PUIO1MFG  = NA,
  PUIO2MFG  = 'NOT IN LABOR FORCE',
  PEERNRT  = NA,
  PUERNH1C  = NA,
  PENLFJH  = NA,
  HES1A  = NA,
  HES1B  = NA,
  HES1C  = NA,
  HES1D  = NA,
  HES8B  = NA,
  HES9  = NA,
  HESP1  = NA,
  HESP21  = NA,
  HESP6  = NA,
  HESP7  = NA,
  HESP7A  = NA,
  HESP8  = NA,
  HESS1  = NA,
  HESS2  = NA,
  HESSM2  = NA,
  HESS3  = NA,
  HESSM3  = NA,
  HESS4  = NA,
  HESSM4  = NA,
  HESH2  = NA,
  HESHF2  = 'Never',
  HESHM2  = NA,
  HETSHMF2  = NA,
  HESH3  = NA,
  HESHF3  = NA,
  HESHM3  = NA,
  HETSHMF3  = NA,
  HESH4  = NA,
  HESHF4  = NA,
  HESHM4  = NA,
  HETSHMF4  = NA,
  HESH5  = NA,
  HESHM5  = NA,
  HESSH1  = NA,
  HESSHF1  = 'Never',
  HESSHM1  = 'No',
  HETSSHMF1  = NA,
  HESS5  = NA,
  HESSM5  = 'No',
  HESS6  = NA,
  HESSM6  = 'No',
  HESH1  = NA,
  HESHM1  = 'No',
  HETSSHMF2  = 0,
  HESSH3  = 'No children',
  HESSHF3 = 'No children',
  HESSHM3  = NA,
  HETSSHMF3  = NA,
  HESSH4  = NA,
  HETSSHMF4  = 0,
  HESC1  = NA,
  HESC2  = NA,
  HESC3  = NA,
  HESCF4 = 'Never ate at soup kitchen',
  HESCM3  = NA,
  HESC3A  = NA,
  HESC4  = NA,
  HESCF4  =  'Never ate at soup kitchen',
  HESCM4  = NA)

df <- tibble(var = names(na.min2), label = unlist(na.min2))
cols_with_mi2a <- cols_with_mi2 %>% left_join(df, by = 'var') %>% mutate(value = '-2',
                                                                         description = NA)
value_1 <-list(
  HES1D = 'Yes',
  PRNLFSCH = 'IN SCHOOL',
  HETSP2D = 'Thirty-one days or more prior to interview.',
  HESSM2 = 'Yes',
  HESC1 = 'Yes',
  HESC4 = 'Yes',
  PEDWAVL = 'Yes',
  HESCM4 = 'Yes',
  HESH2 = 'Yes',
  HESH3 = 'Yes',
  HESH5 = 'Yes',
  PEERNCOV = 'Yes',
  HESHM2 = 'Yes',
  HESHM4 = 'Yes',
  HESSHM3  =  'Yes',
  HESP210 = 'Yes',
  PEDADTYP = 'BIOLOGICAL',
  HESP24 = 'Yes',
  PEJHWANT = 'YES, OR IT DEPENDS',
  
  PUJHDP1O = 'Yes',
  HESP6 = 'Yes',
  HESP8 = 'Yes',
  HESSH1 = 'Yes',
  HESM2 = 'Yes',
  PEDW4WK = 'Yes',
  PEDWLKWK = 'Yes',
  PEERNLAB = 'Yes',
  PEERNRT = 'Yes',
  PEERNHRY = 'HOURLY WORKER',
  PESCHLVL = 'HIGH SCHOOL',
  PELKLWO = 'WITHIN THE LAST 12 MONTHS',
  PESCHENR = 'Yes',
  PEMOMTYP  = 'BIOLOGICAL',
  PESCHFT = 'Yes',
  PRNAGPWS = 'NON-AG PRIV WAGE & SALARY',
  
  PELKFTO = 'Yes',
  PEDWAVR = 'OWN TEMPORARY ILLNESS',
  PREMP = 'EMPLOYED PERSONS (EXC. FARM & PRIV HH)',
  PRCIVLF = 'IN CIVILIAN LABOR FORCE',
  PRNAGWS = 'NON-AG PRIV WAGE & SALARY',
  PRNAGWS = 'NON-AG PRIV WAGE & SALARY',
  PENLFJH  = 'WITHIN THE LAST 12 MONTHS',
  PELKFTA = 'Yes')

df <- tibble(var = names(value_1), label = unlist(value_1))
cols_with_val1a <- cols_with_val1 %>% merge(df, by = 'var') %>% mutate(value = '1',
                                                                       description = NA)

value_2 <-list(
  HES1D = 'No',
  PESCHLVL = 'COLLEGE OR UNIVERSITY',
  HESSHM3  = 'No',
  PEJHWANT = 'No',
  PELKFTO = 'No',
  
  PEDADTYP = 'STEP',
  PRNLFSCH = 'NOT IN SCHOOL',
  HETSP2D = 'Within 30 days of interview.',
  PEDWAVL = 'No',
  PESCHFT = 'No',
  
  PEDWAVR = 'GOING TO SCHOOL',
  PRCIVLF = 'NOT IN CIVILIAN LABOR FORCE',
  HESC1 = 'No',
  HESSM2 = 'No',
  HESC4 = 'No',
  HESCM4 = 'No',
  HESH2 = 'No',
  HESH3 = 'No',
  HESH5 = 'No',
  HESHM2 = 'No',
  PEERNCOV = 'No',
  PEERNHRY = 'NONHOURLY WORKER',
  PUJHDP1O = 'No',
  HESHM4 = 'No',
  HESP210 = 'No',
  HESP24 = 'No',
  HESP6 = 'No',
  HESP8 = 'No',
  HESSH1 = 'No',
  HESM2 = 'No',
  PEDW4WK = 'No',
  PEDWLKWK = 'No',
  PELKLWO = 'MORE THAN 12 MONTHS AGO',
  PEMOMTYP  = 'STEP',
  PEERNLAB = 'No',
  PEERNRT = 'No',
  PESCHENR = 'No',
  PENLFJH  = 'MORE THAN 12 MONTHS AGO',
  PELKFTA = 'No')

df <- tibble(var = names(value_2), label = unlist(value_2))
cols_with_val2a <- cols_with_val2 %>% merge(df, by = 'var') %>% mutate(value = '2',
                                                                       description = NA)

value_3a <-list(
  
  HESS2 = 'Often true',
  HESH1 = 'Often true',
  HESS5 =   'Often true',
  HESSHF1 =  'Often true',
  HESSHF2 = 'Often true')
df1 <- tibble(var = names(value_3a), label = unlist(value_3a))

value_3b <-list(
  HESS2 = 'Sometimes true',
  HESH1 = 'Sometimes true',
  HESS5 = 'Sometimes true',
  HESSHF1 = 'Sometimes true',
  HESSHF2 = 'Sometimes true')
df2 <- tibble(var = names(value_3b), label = unlist(value_3b))

value_3c <-list(
  HESS2 = 'Never true',
  HESH1 = 'Never true',
  HESS5 = 'Never true',
  HESSHF1 = 'Never true',
  HESSHF2 = 'Never true',
  PEDWAVR = 'OTHER',
  PEDADTYP = 'ADOPTED',
  PELKFTO = 'DOESN\'T MATTER',
  PELKLWO = 'NEVER WORKED',
  PEMOMTYP  = 'ADOPTED',
  PENLFJH = 'NEVER WORKED')

df3 <- tibble(var = names(value_3c), label = unlist(value_3c))

cols_with_val1a1 <- cols_with_val1 %>% merge(df1, by = 'var') %>% mutate(value = '1',description = NA)
cols_with_val2a1 <- cols_with_val2 %>% merge(df2, by = 'var') %>% mutate(value = '2',description = NA)
cols_with_val3a1 <- cols_with_val3 %>% merge(df3, by = 'var') %>% mutate(value = '3',description = NA)


value_3d <-list(
  
  HESHF2 = 'Almost every month',
  HESHF4 = 'Almost every month')
df4 <- tibble(var = names(value_3d), label = unlist(value_3d))

value_3e <-list(
  HESHF2 = 'Some months but not every month',
  HESHF4 = 'Some months but not every month')
df5 <- tibble(var = names(value_3e), label = unlist(value_3e))

value_3f <-list(
  HESHF2 = 'Only 1 or 2 months',
  HESHF4 = 'Only 1 or 2 months')
df6 <- tibble(var = names(value_3f), label = unlist(value_3f))

cols_with_val4a1 <- cols_with_val1 %>% merge(df4, by = 'var') %>% mutate(value = '1',description = NA)
cols_with_val5a1 <- cols_with_val2 %>% merge(df5, by = 'var') %>% mutate(value = '2',description = NA)
cols_with_val6a1 <- cols_with_val3 %>% merge(df6, by = 'var') %>% mutate(value = '3',description = NA)

codebook <- codebook_c %>% group_by(var) %>% rbind(cols_with_mi6a ) %>% rbind(cols_with_mi3a ) %>% rbind(cols_with_mi9a) %>% rbind(cols_with_mi2a) %>% 
  rbind(cols_with_val1a) %>% rbind(cols_with_val2a) %>% rbind(cols_with_val1a1) %>%  rbind(cols_with_val2a1) %>%  rbind(cols_with_val3a1) %>% 
  rbind(cols_with_val4a1) %>%  rbind(cols_with_val5a1) %>%  rbind(cols_with_val6a1) %>%  rbind(cols_with_mi4a) %>%   rbind(cols_with_mi5a) %>% 
  fill(description, .direction = "downup") %>% ungroup() %>%
  
  #Correct geographic variables####
mutate(label =  ifelse(var == 'GEREG',gsub('WESTNA','West',label),label),
       label =         ifelse(var == 'GEREG',gsub('SOUTHNA','South',label),label),
       label =           ifelse(var == 'GEREG',gsub('NORTHEASTNA','Northeast',label),label),
       label =        ifelse(var == 'GEREG',gsub('MIDWEST \\(FORMERLY NORTH CENTRAL\\)NA','Midwest',label),label)) %>%
  
  #Correct geographic variables####
mutate(label =  ifelse(var == 'GTMETSTA',gsub('NOT IDENTIFIED','NA',label),label),
       label =  ifelse(var == 'GTMETSTA',gsub('METROPOLITAN','Metro',label),label),
       label =        ifelse(var == 'GTMETSTA',gsub('NONMetro','Micro',label),label)) %>%
  
  #Correct geographic variables####
mutate(label =  ifelse(var == 'GEDIV',gsub('NEW ENGLANDNA','New England',label),label),
       label =         ifelse(var == 'GEDIV',gsub('MIDDLE ATLANTICNA','Middle Atlantic',label),label),
       label =           ifelse(var == 'GEDIV',gsub('EAST NORTH CENTRALNA','East North Central',label),label),
       label =  ifelse(var == 'GEDIV',gsub('WEST NORTH CENTRALNA','West North Central',label),label),
       label =         ifelse(var == 'GEDIV',gsub('SOUTH ATLANTICNA','South Atlantic',label),label),
       label =           ifelse(var == 'GEDIV',gsub('EAST SOUTH CENTRALNA','East South Central',label),label),
       label =           ifelse(var == 'GEDIV',gsub('MOUNTAINNA','Mountain',label),label),
       label =           ifelse(var == 'GEDIV',gsub('PACIFICNA','Pacific',label),label),
       label =        ifelse(var == 'GEDIV',gsub('WEST SOUTH CENTRALNA','West South Central',label),label))  %>%
  
  #Correct geographic variables####
mutate(label =  ifelse(var == 'GTCBSA',gsub('NOT IDENTIFIED OR NONMETROPOLITAN','',label),label),
       label =  ifelse(var == 'GTCSA',gsub('NOT IDENTIFIED OR NONMETROPOLITAN','',label),label)) %>%
  
  rename(desc = description) %>%
  filter(var != 'FILLER') %>%
  mutate(
    value =         ifelse(var == 'PTDTRACE',as.numeric(value),value)) %>%
  mutate(label =  ifelse(var == 'PRINUSYR',str_remove(label,'NA'),label),
         label =  ifelse(var == 'PRINUSYR',gsub('-2005','2004-2005',label),label),
         value =  ifelse((var == 'PRINUSYR' & label == '2004-2005'),19,value),
         value =  ifelse(var == 'PRINUSYR',as.numeric(value),value)) %>% filter(var != 'PENATVTY') %>% 
  rbind(codebook_country) %>%   rbind(codebook_country2) %>% rbind(codebook_country3) %>% rbind(codebook_city) %>% 
  filter(!(var == "HRFS12M1")) %>%
  
  add_row(.,var = "HESS1",desc = "Which of these statements best describes the food eaten in your household -- enough of the kinds of food (I/we) want to eat, enough but not always the kinds of food (I/we) want to eat, sometimes not enough to eat, or often not enough to eat?", value = '1', 
          label = 'Enough of the kinds of food we want to eat')  %>%
  add_row(.,var = "HESS1",desc = "Which of these statements best describes the food eaten in your household -- enough of the kinds of food (I/we) want to eat, enough but not always the kinds of food (I/we) want to eat, sometimes not enough to eat, or often not enough to eat?", value = '2', 
          label = 'Enough but not always the kinds of food we want to eat')  %>%
  add_row(.,var = "HESS1",desc = "Which of these statements best describes the food eaten in your household -- enough of the kinds of food (I/we) want to eat, enough but not always the kinds of food (I/we) want to eat, sometimes not enough to eat, or often not enough to eat?", value = '3', 
          label = 'Sometimes not enough to eat')  %>%
  add_row(.,var = "HESS1",desc = "Which of these statements best describes the food eaten in your household -- enough of the kinds of food (I/we) want to eat, enough but not always the kinds of food (I/we) want to eat, sometimes not enough to eat, or often not enough to eat?", value = '4', 
          label = 'Often not enough to eat')  %>%
  
  add_row(.,var = "HRFS12M1",desc = "Detailed Food Security Status, 12-Month Recall (Recode of HRFS12M4) EDITED UNIVERSE: EDITED UNIVERSE: HRSUPINT=1", value = '1', label = 'Food Secure High or Marginal Food Security')  %>%
  add_row(.,var = "HRFS12M1",desc = "Detailed Food Security Status, 12-Month Recall (Recode of HRFS12M4) EDITED UNIVERSE: EDITED UNIVERSE: HRSUPINT=1", value = '2', label = 'Low Food Security')  %>%
  add_row(.,var = "HRFS12M1",desc = "Detailed Food Security Status, 12-Month Recall (Recode of HRFS12M4) EDITED UNIVERSE: EDITED UNIVERSE: HRSUPINT=1", value = '3', label = 'Very Low Food Security')  %>%
  add_row(.,var = "HRFS12M1",desc = "Detailed Food Security Status, 12-Month Recall (Recode of HRFS12M4) EDITED UNIVERSE: EDITED UNIVERSE: HRSUPINT=1", value = '-9', label = NA)  %>%
  
  
  add_row(.,var = "PEERNPER",desc = "PERIODICITY EDITED UNIVERSE: PRERELG = 1", value = '4', label = 'TWICE MONTHLY')  %>%
  add_row(.,var = "PEERNPER",desc = "PERIODICITY EDITED UNIVERSE: PRERELG = 1", value = '5', label = 'MONTHLY')  %>%
  add_row(.,var = "PEERNPER",desc = "PERIODICITY EDITED UNIVERSE: PRERELG = 1", value = '6', label = 'ANNUALLY')  %>%
  add_row(.,var = "PEERNPER",desc = "PERIODICITY EDITED UNIVERSE: PRERELG = 1", value = '7', label = 'OTHER – SPECIFY')  %>%
  
  add_row(.,var = "PEDWWNTO",desc = "PERIODICITY EDITED UNIVERSE: PRERELG = 1", value = '1', label = 'YES, OR MAYBE, IT DEPENDS')  %>%
  add_row(.,var = "PEDWWNTO",desc = "PERIODICITY EDITED UNIVERSE: PRERELG = 1", value = '2', label = 'NO')  %>%
  add_row(.,var = "PEDWWNTO",desc = "PERIODICITY EDITED UNIVERSE: PRERELG = 1", value = '3', label = 'RETIRED')  %>%
  add_row(.,var = "PEDWWNTO",desc = "PERIODICITY EDITED UNIVERSE: PRERELG = 1", value = '4', label = 'DISABLED')  %>%
  add_row(.,var = "PEDWWNTO",desc = "PERIODICITY EDITED UNIVERSE: PRERELG = 1", value = '5', label = 'UNABLE')  %>%
  
  
  add_row(.,var = 'PRCHLD' ,desc = "PRESENCE OF OWN CHILDREN <18 YEARS OF AGE BY SELECTED AGE GROUP", value = '1',label = 'No own children under 18 years of age') %>%
  add_row(.,var = 'PRCHLD' ,desc = "PRESENCE OF OWN CHILDREN <18 YEARS OF AGE BY SELECTED AGE GROUP", value = '0',label = 'No own children under 18 years of age') %>%
  
  add_row(.,var ='PRCHLD' , desc = "PRESENCE OF OWN CHILDREN <18 YEARS OF AGE BY SELECTED AGE GROUP",value = '2', label ='All own children  0- 2 years of age')%>%
  add_row(.,var ='PRCHLD' , desc = "PRESENCE OF OWN CHILDREN <18 YEARS OF AGE BY SELECTED AGE GROUP",value = '3',label ='All own children  3- 5 years of age')%>%
  add_row(.,var = 'PRCHLD' , desc = "PRESENCE OF OWN CHILDREN <18 YEARS OF AGE BY SELECTED AGE GROUP",value = '4',label ='All own children  6-13 years of age')%>%
  add_row(.,var = 'PRCHLD' ,desc = "PRESENCE OF OWN CHILDREN <18 YEARS OF AGE BY SELECTED AGE GROUP", value = '5',label ='All own children 14-17 years of age') %>%
  add_row(.,var = 'PRCHLD' ,desc = "PRESENCE OF OWN CHILDREN <18 YEARS OF AGE BY SELECTED AGE GROUP", value = '6',label ='Own children  0- 2 and  3- 5 years of age (none  6-17)')%>%
  add_row(.,var = 'PRCHLD' ,desc = "PRESENCE OF OWN CHILDREN <18 YEARS OF AGE BY SELECTED AGE GROUP", value = '7',label ='Own children  0- 2 and 14-17 years of age (none  3-13) ')%>%
  add_row(.,var = 'PRCHLD' ,desc = "PRESENCE OF OWN CHILDREN <18 YEARS OF AGE BY SELECTED AGE GROUP", value = '8',label ='Own children  3- 5 and  6-13 years of age (none  0- 2 or 14-17)') %>%
  add_row(.,var = 'PRCHLD' ,desc = "PRESENCE OF OWN CHILDREN <18 YEARS OF AGE BY SELECTED AGE GROUP", value = '9',label ='Own children  3- 5 and 14-17 years of age (none  0- 2 or  6-13)') %>%
  add_row(.,var = 'PRCHLD' ,desc = "PRESENCE OF OWN CHILDREN <18 YEARS OF AGE BY SELECTED AGE GROUP", value = '10',label = 'Own children  6-13 and 14-17 years of age (none  0- 5)') %>%
  add_row(.,var ='PRCHLD' , desc = "PRESENCE OF OWN CHILDREN <18 YEARS OF AGE BY SELECTED AGE GROUP",value = '11', label ='Own children  0- 2,  3- 5, and  6-13 years of age (none 14-17)')%>%
  add_row(.,var ='PRCHLD' , desc = "PRESENCE OF OWN CHILDREN <18 YEARS OF AGE BY SELECTED AGE GROUP",value = '12',label ='Own children  0- 2,  3- 5, and 14-17 years of age (none 6-13)')%>%
  add_row(.,var = 'PRCHLD' , desc = "PRESENCE OF OWN CHILDREN <18 YEARS OF AGE BY SELECTED AGE GROUP",value = '13',label ='Own children  0- 2,  6-13, and 14-17 years of age (none 3- 5)')%>%
  add_row(.,var = 'PRCHLD' ,desc = "PRESENCE OF OWN CHILDREN <18 YEARS OF AGE BY SELECTED AGE GROUP", value = '14',label ='Own children  3- 5,  6-13, and 14-17 years of age (none  0- 2)') %>%
  add_row(.,var = 'PRCHLD' ,desc = "PRESENCE OF OWN CHILDREN <18 YEARS OF AGE BY SELECTED AGE GROUP", value = '15',label ='Own children  from all age groups')%>%
  
  
  add_row(.,var = 'PEAFWHN1' ,desc = "WHEN DID YOU SERVE?", value = '1',label = 'SEPTEMBER 2001 OR LATER') %>%
  add_row(.,var ='PEAFWHN1' , desc = "WHEN DID YOU SERVE?",value = '2', label ='AUGUST 1990 TO AUGUST 2001')%>%
  add_row(.,var ='PEAFWHN1' , desc = "WHEN DID YOU SERVE?",value = '3',label ='MAY 1975 TO JULY 1990')%>%
  add_row(.,var = 'PEAFWHN1' , desc = "WHEN DID YOU SERVE?",value = '4',label ='VIETNAM ERA (AUGUST 1964 TO APRIL 1975)')%>%
  add_row(.,var = 'PEAFWHN1' ,desc = "WHEN DID YOU SERVE?", value = '5',label ='FEBRUARY 1955 TO JULY 1964') %>%
  add_row(.,var = 'PEAFWHN1' ,desc = "WHEN DID YOU SERVE?", value = '6',label ='KOREAN WAR (JULY 1950 TO JANUARY 1955)')%>%
  add_row(.,var = 'PEAFWHN1' ,desc = "WHEN DID YOU SERVE?", value = '7',label ='JANUARY 1947 TO JUNE 1950')%>%
  add_row(.,var = 'PEAFWHN1' ,desc = "WHEN DID YOU SERVE?", value = '8',label ='WORLD WAR II (DECEMBER 1941 TO DECEMBER 1946)') %>%
  add_row(.,var = 'PEAFWHN1' ,desc = "WHEN DID YOU SERVE?", value = '9',label ='NOVEMBER 1941 OR EARLIER') %>%
  
  add_row(.,var  ='PEAFWHN2' , desc = "WHEN DID YOU SERVE?",value = '1',label ='SEPTEMBER 2001 OR LATER') %>%
  add_row(.,var = 'PEAFWHN2' ,desc = "WHEN DID YOU SERVE?", value = '2',label = 'AUGUST 1990 TO AUGUST 2001') %>%
  add_row(.,var = 'PEAFWHN2' ,desc = "WHEN DID YOU SERVE?", value = '3',label ='MAY 1975 TO JULY 1990') %>%
  add_row(.,var = 'PEAFWHN2' , desc = "WHEN DID YOU SERVE?",value = '4',label ='VIETNAM ERA (AUGUST 1964 TO APRIL 1975)') %>%
  add_row(.,var = 'PEAFWHN2' , desc = "WHEN DID YOU SERVE?",value = '5',label ='FEBRUARY 1955 TO JULY 1964') %>%
  add_row(.,var  ='PEAFWHN2' , desc = "WHEN DID YOU SERVE?",value = '6',label ='KOREAN WAR (JULY 1950 TO JANUARY 1955)') %>%
  add_row(.,var  ='PEAFWHN2' ,desc = "WHEN DID YOU SERVE?", value = '7',label ='JANUARY 1947 TO JUNE 1950') %>%
  add_row(.,var = 'PEAFWHN2' ,desc = "WHEN DID YOU SERVE?", value = '8',label ='WORLD WAR II (DECEMBER 1941 TO DECEMBER 1946)') %>%
  add_row(.,var  ='PEAFWHN2' , desc = "WHEN DID YOU SERVE?",value = '9',label ='NOVEMBER 1941 OR EARLIER') %>%
  
  add_row(.,var = 'PEAFWHN3' , desc = "WHEN DID YOU SERVE?", value = '1',label ='SEPTEMBER 2001 OR LATER') %>%
  add_row(.,var = 'PEAFWHN3' , desc = "WHEN DID YOU SERVE?",value = '2',label = 'AUGUST 1990 TO AUGUST 2001') %>%
  add_row(.,var = 'PEAFWHN3' , desc = "WHEN DID YOU SERVE?",value = '3',label ='MAY 1975 TO JULY 1990') %>%
  add_row(.,var = 'PEAFWHN3' , desc = "WHEN DID YOU SERVE?",value = '4',label ='VIETNAM ERA (AUGUST 1964 TO APRIL 1975)') %>%
  add_row(.,var = 'PEAFWHN3' ,desc = "WHEN DID YOU SERVE?", value = '5',label ='FEBRUARY 1955 TO JULY 1964') %>%
  add_row(.,var = 'PEAFWHN3' ,desc = "WHEN DID YOU SERVE?", value = '6',label ='KOREAN WAR (JULY 1950 TO JANUARY 1955)') %>%
  add_row(.,var = 'PEAFWHN3' , desc = "WHEN DID YOU SERVE?",value = '7',label ='JANUARY 1947 TO JUNE 1950') %>%
  add_row(.,var  = 'PEAFWHN3' ,desc = "WHEN DID YOU SERVE?", value = '8',label ='WORLD WAR II (DECEMBER 1941 TO DECEMBER 1946)') %>%
  add_row(.,var = 'PEAFWHN3' , desc = "WHEN DID YOU SERVE?",value = '9',label ='NOVEMBER 1941 OR EARLIER') %>%
  add_row(.,var =  'HESSHF3' , desc = "How often did this happen -- almost every month some months but not every month, or in only 1 or 2 months? EDITED UNIVERSE: HESSH3 = 1", value = '3',label ='Only 1 or 2 months') %>%
  
  add_row(.,var = 'PRMJOCC1' , desc = "MAJOR OCCUPATION RECODE - JOB 1 EDITED UNIVERSE: PRDTOCC1 = 1-46",value = '10',label ='Transportation and material moving occupations') %>%
  add_row(.,var =  'PRMJOCC1' , desc = "MAJOR OCCUPATION RECODE - JOB 1 EDITED UNIVERSE: PRDTOCC1 = 1-46", value = '11',label ='Armed Forces') %>%
  
  add_row(.,var = 'PRDTOCC2' , desc = "DETAILED OCCUPATION RECODE EDITED UNIVERSE: PRIOELG = 1 AND PEMJOT = 1 AND HRMIS = 4 OR 8",value = '10',label ='Transportation and material moving occupations') %>%
  add_row(.,var =  'PRDTOCC2' , desc = "DETAILED OCCUPATION RECODE EDITED UNIVERSE: PRIOELG = 1 AND PEMJOT = 1 AND HRMIS = 4 OR 8", value = '11',label ='Armed Forces') %>%
  add_row(.,var =  'PRDTOCC2' , desc = "DETAILED OCCUPATION RECODE EDITED UNIVERSE: PRIOELG = 1 AND PEMJOT = 1 AND HRMIS = 4 OR 8", value = '12',label ='Protective service occupations') %>%
  
  add_row(.,var =  'HRFS12MD' , desc = "Detailed Food Security Status, 12-Month Recall (Recode of HRFS12M4) EDITED UNIVERSE: EDITED UNIVERSE: HRSUPINT=1", value = '1',label ='High Food Security') %>%
  add_row(.,var ='HRFS12MD' , desc = "Detailed Food Security Status, 12-Month Recall (Recode of HRFS12M4) EDITED UNIVERSE: EDITED UNIVERSE: HRSUPINT=1",value = '2', label ='Marginal Food Security') %>%
  add_row(.,var ='HRFS12MD' , desc = "Detailed Food Security Status, 12-Month Recall (Recode of HRFS12M4) EDITED UNIVERSE: EDITED UNIVERSE: HRSUPINT=1", value = '3',label ='Low Food Security')   %>%
  add_row(.,var = 'HRFS12MD' ,desc ="Detailed Food Security Status, 12-Month Recall (Recode of HRFS12M4) EDITED UNIVERSE: EDITED UNIVERSE: HRSUPINT=1",  value = '4',label ='Very Low Food Security') %>% 
  
  add_row(.,var =  'GTCBSASZ' , desc = "Metropolitan Area (CBSA) SIZE", value = '0',label ='NOT IDENTIFIED OR NONMETROPOLITAN') %>%
  add_row(.,var ='GTCBSASZ' , desc = "Metropolitan Area (CBSA) SIZE",value = '2', label ='100,000 - 249,999') %>%
  add_row(.,var ='GTCBSASZ' , desc = "Metropolitan Area (CBSA) SIZE", value = '3',label ='250,000 - 499,999')   %>%
  add_row(.,var = 'GTCBSASZ' ,desc ="Metropolitan Area (CBSA) SIZE",  value = '4',label ='500,000 - 999,999') %>% 
  add_row(.,var ='GTCBSASZ' , desc = "Metropolitan Area (CBSA) SIZE",value = '5', label ='1,000,000 - 2,499,999') %>%
  add_row(.,var ='GTCBSASZ' , desc = "Metropolitan Area (CBSA) SIZE", value = '6',label ='2,500,000 - 4,999,999')   %>%
  add_row(.,var = 'GTCBSASZ' ,desc ="Metropolitan Area (CBSA) SIZE",  value = '7',label ='5,000,000+') %>% 
  
  add_row(.,var = 'PEJHRSN' , desc = "WHAT IS THE MAIN REASON YOU LEFT YOUR LAST JOB? ", value = '1',label ='PERSONAL/FAMILY (INCLUDING PREGNANCY)') %>%
  add_row(.,var ='PEJHRSN' , desc = "WHAT IS THE MAIN REASON YOU LEFT YOUR LAST JOB? ",value = '2', label ='RETURN TO SCHOOL') %>%
  add_row(.,var ='PEJHRSN' , desc = "WHAT IS THE MAIN REASON YOU LEFT YOUR LAST JOB? ",value = '3', label ='HEALTH') %>%
  add_row(.,var ='PEJHRSN' , desc = "WHAT IS THE MAIN REASON YOU LEFT YOUR LAST JOB? ", value = '4',label ='RETIREMENT OR OLD AGE')   %>%
  add_row(.,var = 'PEJHRSN' ,desc ="WHAT IS THE MAIN REASON YOU LEFT YOUR LAST JOB? ",  value = '5',label ='TEMP, SEASONAL OR INTERMITTENT JOB COMPLETE') %>% 
  add_row(.,var ='PEJHRSN' , desc = "WHAT IS THE MAIN REASON YOU LEFT YOUR LAST JOB? ",value = '6', label ='SLACK WORK/BUSINESS CONDITIONS') %>%
  add_row(.,var ='PEJHRSN' , desc = "WHAT IS THE MAIN REASON YOU LEFT YOUR LAST JOB? ", value = '7',label ='UNSATISFACTORY WORK ARRANGEMENTS (HRS, PAY, ETC.)')   %>%
  add_row(.,var = 'PEJHRSN' ,desc ="WHAT IS THE MAIN REASON YOU LEFT YOUR LAST JOB? ",  value = '8',label ='OTHER – SPECIFY') %>% 
  
  
  
  add_row(.,var =  'PRDTIND1' , desc = "DETAILED INDUSTRY RECODE - JOB 1 EDITED UNIVERSE: PRIOELG = 1", value = '2',label ='Forestry, logging, fishing, hunting, and trapping') %>%
  add_row(.,var ='PRDTIND1' , desc = "DETAILED INDUSTRY RECODE - JOB 1 EDITED UNIVERSE: PRIOELG = 1",value = '3', label ='Mining') %>%
  add_row(.,var ='PRDTIND1' , desc = "DETAILED INDUSTRY RECODE - JOB 1 EDITED UNIVERSE: PRIOELG = 1",value = '4', label ='Construction') %>%
  add_row(.,var ='PRDTIND1' , desc = "DETAILED INDUSTRY RECODE - JOB 1 EDITED UNIVERSE: PRIOELG = 1", value = '5',label ='Nonmetallic mineral product manufacturing')   %>%
  add_row(.,var = 'PRDTIND1' ,desc ="DETAILED INDUSTRY RECODE - JOB 1 EDITED UNIVERSE: PRIOELG = 1",  value = '6',label ='Primary metals and fabricated metal products') %>% 
  add_row(.,var ='PRDTIND1' , desc = "DETAILED INDUSTRY RECODE - JOB 1 EDITED UNIVERSE: PRIOELG = 1",value = '7', label ='Machinery  manufacturing') %>%
  add_row(.,var ='PRDTIND1' , desc = "DETAILED INDUSTRY RECODE - JOB 1 EDITED UNIVERSE: PRIOELG = 1", value = '8',label ='Computer and electronic product manufacturing')   %>%
  add_row(.,var = 'PRDTIND1' ,desc ="DETAILED INDUSTRY RECODE - JOB 1 EDITED UNIVERSE: PRIOELG = 1",  value = '9',label ='Electrical equipment, appliance manufacturing') %>% 
  add_row(.,var =  'PRDTIND1' , desc = "DETAILED INDUSTRY RECODE - JOB 1 EDITED UNIVERSE: PRIOELG = 1", value = '10',label ='Transportation equipment manufacturing') %>%
  add_row(.,var ='PRDTIND1' , desc = "DETAILED INDUSTRY RECODE - JOB 1 EDITED UNIVERSE: PRIOELG = 1",value = '12', label ='Furniture and fixtures manufacturing') %>%
  add_row(.,var ='PRDTIND1' , desc = "DETAILED INDUSTRY RECODE - JOB 1 EDITED UNIVERSE: PRIOELG = 1",value = '13', label ='Miscellaneous and not specified manufacturing') %>%
  add_row(.,var ='PRDTIND1' , desc = "DETAILED INDUSTRY RECODE - JOB 1 EDITED UNIVERSE: PRIOELG = 1", value = '15',label ='Beverage and tobacco products')   %>%
  add_row(.,var = 'PRDTIND1' ,desc ="DETAILED INDUSTRY RECODE - JOB 1 EDITED UNIVERSE: PRIOELG = 1",  value = '16',label ='Textile, apparel, and leather manufacturing') %>% 
  add_row(.,var ='PRDTIND1' , desc = "DETAILED INDUSTRY RECODE - JOB 1 EDITED UNIVERSE: PRIOELG = 1",value = '24', label ='Utilities') %>%
  add_row(.,var ='PRDTIND1' , desc = "DETAILED INDUSTRY RECODE - JOB 1 EDITED UNIVERSE: PRIOELG = 1", value = '27',label ='Broadcasting (except internet)')   %>%
  add_row(.,var = 'PRDTIND1' ,desc ="DETAILED INDUSTRY RECODE - JOB 1 EDITED UNIVERSE: PRIOELG = 1",  value = '28',label ='Internet publishing and broadcasting') %>% 
  add_row(.,var ='PRDTIND1' , desc = "DETAILED INDUSTRY RECODE - JOB 1 EDITED UNIVERSE: PRIOELG = 1",value = '30', label ='Internet service providers and data processing services') %>%
  add_row(.,var ='PRDTIND1' , desc = "DETAILED INDUSTRY RECODE - JOB 1 EDITED UNIVERSE: PRIOELG = 1", value = '38',label ='Administrative and support services')   %>%
  add_row(.,var = 'PRDTIND1' ,desc ="DETAILED INDUSTRY RECODE - JOB 1 EDITED UNIVERSE: PRIOELG = 1",  value = '39',label ='Waste management and remediation services ') %>% 
  add_row(.,var = 'PRDTIND1' ,desc ="DETAILED INDUSTRY RECODE - JOB 1 EDITED UNIVERSE: PRIOELG = 1",  value = '44',label ='Waste management and remediation services ') %>% 
  add_row(.,var = 'PRDTIND1' ,desc ="DETAILED INDUSTRY RECODE - JOB 1 EDITED UNIVERSE: PRIOELG = 1",  value = '52',label ='Armed forces') %>% 
  
  add_row(.,var = 'PRIMIND2' , desc = "INTERMEDIATE INDUSTRY RECODE (JOB 2) EDITED UNIVERSE: PRIOELG = 1 AND PEMJOT = 1 AND HRMIS = 4 OR 8", value = '17',label ='ARTS, ENTERTAINMENT, AND RECREATION') %>%
  add_row(.,var ='PRIMIND2' , desc = "INTERMEDIATE INDUSTRY RECODE (JOB 2) EDITED UNIVERSE: PRIOELG = 1 AND PEMJOT = 1 AND HRMIS = 4 OR 8",value = '18', label ='ACCOMMODATION AND FOOD SERVICES') %>%
  add_row(.,var ='PRIMIND2' , desc = "INTERMEDIATE INDUSTRY RECODE (JOB 2) EDITED UNIVERSE: PRIOELG = 1 AND PEMJOT = 1 AND HRMIS = 4 OR 8",value = '19', label ='PRIVATE HOUSEHOLDS ') %>%
  add_row(.,var ='PRIMIND2' , desc = "INTERMEDIATE INDUSTRY RECODE (JOB 2) EDITED UNIVERSE: PRIOELG = 1 AND PEMJOT = 1 AND HRMIS = 4 OR 8",value = '20', label ='OTHER SERVICES, EXCEPT PRIVATE HOUSEHOLDS ') %>%
  add_row(.,var ='PRIMIND2' , desc = "INTERMEDIATE INDUSTRY RECODE (JOB 2) EDITED UNIVERSE: PRIOELG = 1 AND PEMJOT = 1 AND HRMIS = 4 OR 8",value = '21', label ='PUBLIC ADMINISTRATION') %>%
  add_row(.,var ='PRIMIND1' , desc = "INTERMEDIATE INDUSTRY RECODE (JOB 2) EDITED UNIVERSE: PRIOELG = 1 AND PEMJOT = 1 AND HRMIS = 4 OR 8",value = '22', label ='ARMED FORCES') %>%
  
  add_row(.,var = 'PUBUS1' , desc = "LAST WEEK, DID YOU DO ANY UNPAID WORK IN THE FAMILY BUSINESS OR FARM?", value = '1',label ='Yes') %>%
  add_row(.,var = 'PUBUS1' , desc = "'LAST WEEK, DID YOU DO ANY UNPAID WORK IN THE FAMILY BUSINESS OR FARM?'", value = '2',label ='No') %>%
  
  
  add_row(.,var = 'PULKM3' , desc = "SAME AS PULKM2 (THIRD METHOD)", value = '12',label ='NOTHING') %>%
  add_row(.,var = 'PULKM4' , desc = "SAME AS PULKM2 (FOURTH METHOD)", value = '12',label ='NOTHING') %>%
  add_row(.,var = 'PULKM5' , desc = "SAME AS PULKM2 (FIFTH METHOD)", value = '12',label ='NOTHING') %>%
  add_row(.,var = 'PULKM6' , desc = "SAME AS PULKM2 (SIXTH METHOD)", value = '12',label ='NOTHING') %>%
  
  
  add_row(.,var = "PUSLFPRX",desc = "LABOR FORCE INFORMATION COLLECTED BY SELF OR PROXY RESPONSE", value = '1', label = 'SELF')  %>%
  add_row(.,var = "PUSLFPRX",desc = "LABOR FORCE INFORMATION COLLECTED BY SELF OR PROXY RESPONSE", value = '2', label = 'PROXY')  %>%
  add_row(.,var = "PUSLFPRX",desc = "LABOR FORCE INFORMATION COLLECTED BY SELF OR PROXY RESPONSE", value = '3', label = 'BOTH SELF AND PROXY')  %>%
  add_row(.,var = "HESSHM4",desc = "Now think about the last 30 days. Did (the child/any of the children) ever skip a meal during that time because there wasn't enough money for food? EDITED UNIVERSE: HESSH4 =1", value = '2', label = 'No')  %>%
  
  mutate(label = ifelse(var == 'HESSHM4' & value == '1','Yes',label),
         label = ifelse(var == 'HETS3O' & value == '0','0',label),
         label = ifelse(var == 'HETS5O' & value == '0','0',label)) %>%
  
  mutate(label = ifelse(var == 'HESSHF3' & value == '1','Almost every month',label),
         label = ifelse(var == 'HESSHF3' & value == '2','Some months but not every month',label),
         label = ifelse(var == 'HESSHF3' & value == '3','Only 1 or 2 months',label),
         
         
         label = ifelse(var == 'PEMLR', str_remove(label,'NA$'), label),
         label = ifelse(var == 'PUWK', str_remove(label,'NA$'), label),
         
         
         
         
         label = ifelse(var == 'PRDTHSP',gsub('. ','',label),label),
         
         label = ifelse(var == 'PEFNTVTY' & value == '100','Albania',label),
         label = ifelse(var == 'PEMNTVTY' & value == '100','Albania',label),
         
         label = ifelse(var == 'HETSHMF2' & value == '1','1',label),
         
         label = ifelse(var == 'PEERNH2' & value == '9999','9999',label),
         label = ifelse(var == 'PEERNH1O' & value == '9999','9999',label),
         label = ifelse(var == 'HETSHMF3' & value == '1','1',label),
         
         label = ifelse(var == 'HETSHMF4' & value == '1','1',label),
         label = ifelse(var == 'HETSHMF4' & value == '3','3',label),
         label = ifelse(var == 'HETSHMF4' & value == '7','1',label),
         
         label = ifelse(var == 'HRFS12M9' & value == '1','1',label),
         label = ifelse(var == 'HRFS12MD' & value == '1','High Food Security',label),
         label = ifelse(var == 'HRFS30D9' & value == '1','1',label)) %>%
  
  
  mutate(desc = ifelse(var == 'HES1D','Did (you/anyone in your household) buy food from any other kind of place LAST WEEK?',desc),
         
         desc = ifelse(var == 'PUBUS1','LAST WEEK, DID YOU DO ANY UNPAID WORK IN THE FAMILY BUSINESS OR FARM?',desc),
         
         desc = ifelse(var == 'PRINUSYR','IMMIGRANT\'S YEAR OF ENTRY',desc),
         
         desc = ifelse(var == 'HESC1','During the past 30 days, did (you/anyone in this household) receive any meals delivered to the home from community programs, Meals on Wheels, or any other programs?',desc),
         desc = ifelse(var == 'HESC4','In the last 12 months, did (you/you or other adults in your household) ever eat any meals at a soup kitchen or shelter?',desc),
         desc = ifelse(var == 'HESCM4','Did this happen in the last 30 days?',desc),
         desc = ifelse(var == 'HESH1','(The child in (my/our) household was/The children were) not eating enough because (I/we) just couldn\'t afford enough food. Was that OFTEN, SOMETIMES, or NEVER true for (you/your household) in the last 12 months?',desc),
         desc = ifelse(var == 'HESH2','In the last 12 months did (you/you or other adults in your household) ever cut the size of your meals or skip meals because there wasn\'t enough money for food?',desc),
         desc = ifelse(var == 'HESHM2',' Now think about the last 30 days. During that time did (you/you or other adults in your household) ever cut the size of your meals or skip meals because there wasn\'t enough
                       money for food? ',desc),
         desc = ifelse(var == 'HESHM4',' Did this happen in the last 30 days?',desc),
         desc = ifelse(var == 'HESP210',' In which months of 2019 were SNAP or food stamp benefits received? October?',desc),
         desc = ifelse(var == 'HESP8','During the past 30 days, did any (women/women or children/children) in this household get food through the WIC program?',desc),
         desc = ifelse(var == 'HESS2','Now I\'m going to read you several statements that people have made about their food situation. For these statements, please tell me whether the statement was OFTEN true, SOMETIMES true, or NEVER true for (you/your household) in the last 12 months.
                       The first statement is (I/We) worried whether (my/our) food would run out before (I/we) got money to buy more. Was that OFTEN true, SOMETIMES true, or NEVER true for (you/your household) in the last 12 months?',desc),
         desc = ifelse(var == 'HESS5',' (I/We) relied on only a few kinds of low-cost food to feed (the child in (my/our) household/the children) because (I was/we were) running out of money to buy food.
                                         Was that OFTEN, SOMETIMES or NEVER true for (you/your household) in the last 12 months?',desc),
         
         desc = ifelse(var == 'HESSH1','(In the last 12 months, did (you/you or other adults in your household) ever not eat for a whole day because there wasn\'t enough money for food?',desc),
         desc = ifelse(var == 'HESSHF1','How often did this happen--almost every month, some months but not every month, or in only 1 or 2 months?',desc),
         desc = ifelse(var == 'HESSHF2','How often did this happen--almost every month, some months but not every month, or in only 1 or 2 months?',desc),
         desc = ifelse(var == 'HESSHM3','Did this happen in the last 30 days?',desc),
         desc = ifelse(var == 'HESSM2','Did this happen in the last 30 days?',desc),
         desc = ifelse(var == 'HETSP2D','On what date in November did (you/your household) receive SNAP or food stamp benefits?Answer is reformatted as within 30 days of interview or not.',desc),
         desc = ifelse(var == 'HETSP3O','Out variable that represents the dollaramount of food stamps received per month.Created from HUSP3 or HUSP3COR.',desc),
         desc = ifelse(var == 'HETSSHMF2','In the last 30 days, how many days did you cut the size of (the child\'sany of thechildren\'s) meals because there wasn\'t enough money for food?',desc),
         desc = ifelse(var == 'HETSSHMF4','How many days did this happen in the last 30 days?',desc),
         desc = ifelse(var == 'HRFS30D4','Food Security Rasch Scale Score, 30-Day (2 implied decimals) (Based on Raw Score (HRFS30D3) AND presence or absence of children in household)',desc),
         desc = ifelse(var == 'PEAFWHN1','WHEN DID YOU SERVE?',desc),
         desc = ifelse(var == 'PEAFWHN2','WHEN DID YOU SERVE?',desc),
         desc = ifelse(var == 'PEAFWHN3','WHEN DID YOU SERVE?',desc),
         desc = ifelse(var == 'PEDWAVL','LAST WEEK, COULD YOU HAVE STARTED A JOB IF ONE HAD BEEN OFFERED?',desc),
         desc = ifelse(var == 'PEDWAVR','WHY IS THAT?',desc),
         desc = ifelse(var == 'PEDWLKWK','SINCE YOU LEFT THAT JOB OR BUSINESS HAVE YOU LOOKED FOR WORK?',desc),
         desc = ifelse(var == 'PEDWWNTO','DO YOU CURRENTLY WANT A JOB EITHER FULL OR PART TIME?',desc),
         
         desc = ifelse(var == 'PEERNCOV','ON THIS JOB ARE YOU COVERED BY A UNION OR EMPLOYEE ASSOCIATION CONTRACT?',desc),
         desc = ifelse(var == 'PEERNH2','(EXCLUDING OVERTIME PAY, TIPS AND COMMISSIONS) WHAT IS YOUR HOURLY RATEbOF PAY ON YOUR (MAIN/THIS) JOB? DOLLAR AMOUNT - 2 IMPLIED DECIMALS',desc),
         desc = ifelse(var == 'PEERNHRY','HOURLY/NONHOURLY STATUS',desc),
         desc = ifelse(var == 'PEERNLAB','ON THIS JOB, ARE YOU A MEMBER OF A LABOR UNION OR OF AN EMPLOYEE ASSOCIATION SIMILAR TO A UNION?',desc),
         desc = ifelse(var == 'PEERNRT','(EVEN THOUGH YOU TOLD ME IT IS EASIER TO REPORT YOUR EARNINGS (PERIODICITY);ARE YOU PAID AT AN HOURLY RATE ON YOUR (MAIN/THIS) JOB?',desc),
         desc = ifelse(var == 'PEERNWKP','HOW MANY WEEKS A YEAR DO YOU GET PAID FOR?',desc),
         desc = ifelse(var == 'PEIO1OCD','OCCUPATION CODE FOR PRIMARY JOB',desc),
         desc = ifelse(var == 'PEIO2ICD','INDUSTRY CODE FOR SECOND JOB.',desc),
         desc = ifelse(var == 'PEIO2OCD','OCCUPATION CODE FOR SECOND JOB.',desc),
         desc = ifelse(var == 'PEJHRSN','WHAT IS THE MAIN REASON YOU LEFT  YOUR LAST JOB?',desc),
         desc = ifelse(var == 'PEJHWANT','DO YOU INTEND TO LOOK FOR WORK DURING THE NEXT 12 MONTHS?',desc),
         
         desc = ifelse(var == 'PELKDUR','DURATION OF JOB SEEKING ',desc),
         desc = ifelse(var == 'PELKFTO','FT/PT STATUS OF JOBSEEKER',desc),
         desc = ifelse(var == 'PELKLWO','WHEN LAST WORKED',desc),
         desc = ifelse(var == 'PENLFJH','WHEN DID YOU LAST WORK AT A JOB OR BUSINESS?',desc),
         desc = ifelse(var == 'PRCHLD','PRESENCE OF OWN CHILDREN <18 YEARS OF AGE BY SELECTED AGE GROUP',desc),
         
         desc = ifelse(var == 'PESCHFT','ARE YOU ENROLLED IN SCHOOL AS A FULL-TIME OR PART-TIME STUDENT? ',desc),
         desc = ifelse(var == 'PESCHLVL','WOULD THAT BE HIGH SCHOOL, COLLEGE, OR UNIVERSITY?',desc),
         desc = ifelse(var == 'PRHRUSL','USUAL HOURS WORKED WEEKLY',desc),
         desc = ifelse(var == 'PRNLFSCH','NLF ACTIVITY - IN SCHOOL OR NOT IN SCHOOL',desc),
         
         desc = ifelse(var == 'PUERNH1C','WHAT IS YOUR HOURLY RATE OF PAY ON THIS JOB, EXCLUDING OVERTIME PAY, TIPS OR COMMISSION? DOLLAR AMOUNT - 2 IMPLIED DECIMALS ',desc),
         desc = ifelse(var == 'PUJHDP1O','DID YOU DO ANY OF THIS WORK IN THE LAST 4 WEEKS?',desc),
         desc = ifelse(var == 'PULKDK1','YOU SAID YOU HAVE BEEN TRYING TO FIND WORK. HOW DID YOU GO ABOUT LOOKING? (FIRST METHOD)',desc),
         
         desc = ifelse(var == 'PWFMWGT','FAMILY WEIGHT ',desc),
         desc = ifelse(var == 'PWLGWGT','LONGITUDINAL WEIGHT ',desc),
         desc = ifelse(var == 'PWORWGT','OUTGOING ROTATION WEIGHT',desc),
         desc = ifelse(var == 'PWSSWGT','FINAL WEIGHT ',desc),
         
         desc = ifelse(var == 'PUSLFPRX','LABOR FORCE INFORMATION COLLECTED BY SELF OR PROXY RESPONSE',desc),
         desc = ifelse(var == 'PULKDK2','ANYTHING ELSE? (SECOND METHOD)',desc),
         
         desc = ifelse(var == 'PULKDK6','SAME AS PULKDK2 (SIXTH METHOD)',desc),
         desc = ifelse(var == 'PULKDK5','SAME AS PULKDK2 (FIFTH METHOD)',desc),
         desc = ifelse(var == 'PULKPS6','SAME AS PULKPS2 (SIXTH METHOD)',desc),
         
         desc = ifelse(var == 'PUWK','LAST WEEK, DID YOU DO ANY WORK FOR (EITHER) PAY (OR PROFIT)?',desc),
         
         desc = ifelse(var == 'PULAYAVR','COULD YOU HAVE RETURNED TO WORK LAST WEEK IF YOU HAD BEEN RECALLED? WHY IS THAT?',desc),
         
         desc = ifelse(var == 'HESP6','During the past 30 days, did any children in the household (between 5 and 18 years old) receive free or reduced-cost lunches at school?',desc),
         
         desc = ifelse(var == 'PULKM4','SAME AS PULKM2 (FOURTH METHOD)',desc),
         desc = ifelse(var == 'PULKPS5','SAME AS PULKPS2 (FIFTH METHOD)',desc),
         desc = ifelse(var == 'PULKPS3','SAME AS PULKPS2 (THIRD METHOD)',desc),
         desc = ifelse(var == 'PULKPS2','ANYTHING ELSE? (SECOND METHOD)',desc),
         desc = ifelse(var == 'PULKPS4','SAME AS PULKPS2 (FOURTH METHOD)',desc),
         desc = ifelse(var == 'PULKM6','SAME AS PULKM2 (SIXTH METHOD)',desc),
         
         desc = ifelse(var == 'PULKPS1','CAN YOU TELL ME MORE ABOUT WHAT YOU DID TO SEARCH FOR WORK? (FIRST METHOD)',desc),
         desc = ifelse(var == 'PULKM5','SAME AS PULKM2 (FIFTH METHOD) ',desc),
         label = str_replace(label, ": Coded as 0", "0"),
         label = str_replace(label, ":8 Number of affirmative Responses", "1")) %>%
  
  ##Replace all 'Don't Know', 'Refused', 'No response' entries to NA
  mutate(label = ifelse(label %in% c('Refused','No Response','Don\'t Know'),NA,label))

codebook_add <- codebook %>% filter(var == 'PRIMIND2') %>% 
  mutate(var =  gsub('PRIMIND2','PRIMIND1',var))

codebook <- codebook %>% rbind(codebook_add) %>% rbind(codebook_indus)  %>%
  filter(!(var %in% c("HESHM1",'HESSHF3','HESSHM3',"HETSSHMF3" ,"HESSH5",    "HESCM4",
                      'HESCF4','HESHF2','HESSM6','HESSH2')  & is.na(label)))  %>%
  
  add_row(.,var = 'HESCM4', desc = 'Did this happen in the last 30 days?',value = '-2', label = NA) %>%
  add_row(.,var = 'HESSHM3', desc = 'Did this happen in the last 30 days?',value = '-2', label = NA) %>%
  add_row(.,var = 'HESSH5', desc = '  In the last 12 months, did (the child/any of the children) ever not eat for a whole day because there wasn\'t enough money for food? EDITED UNIVERSE: HESSH4 = 1, 2, -2, -3 or -9',value = '-3', label = NA) 


# Update 'codebook' with the manually specified override values for "Not applicable"
# Drop variables set to NULL in 'na.values'


#Create ordered factors
custom.ordered  <- list(
  
  PEEDUCA = c('LESS THAN 1ST GRADE','1ST, 2ND, 3RD OR 4TH GRADE','5TH OR 6TH GRADE',
              '7TH OR 8TH GRADE','9TH GRADE','10TH GRADE','11TH GRADE','12TH GRADE NO DIPLOMA',
              'HIGH SCHOOL GRAD-DIPLOMA OR EQUIV (GED)','SOME COLLEGE BUT NO DEGREE','ASSOCIATE DEGREE-OCCUPATIONAL/VOCATIONAL',
              'ASSOCIATE DEGREE-ACADEMIC PROGRAM','BACHELOR\'S DEGREE (EX: BA, AB, BS)','MASTER\'S DEGREE (EX: MA, MS, MEng, MEd, MSW)',
              'PROFESSIONAL SCHOOL DEG (EX: MD, DDS, DVM)','DOCTORATE DEGREE (EX: PhD, EdD)'),
  
  PELAYDUR = c('0','1', '2' ,'3', '4', '5', '6' ,'7' ,'8', '9','10', '11' ,'12' ,'13' ,'14', '15' ,'16', '17', '18','21', '22', '23' ,'26' , '35', '43' , '52 weeks or more'),
  PEMJNUM = c('Not more than one job','2', '3' ,'4'),
  PTNMEMP1 = c('Not self-employed','1','2','3', '4', '5', '6' ,'7' , '8', '9', '10', '11' ,'12' ,'13' ,'14' ,'15' ,'18' , '20', '22' ,'26'  ,'30', '32', '36' , '40' ,'45' , '50' ,'60' ,'67',  '75 or more employees'),
  
  HEFAMINC = c("Under  $720",'5,000 TO 7,499','7,500 TO 9,999','10,000 TO 12,499','12,500 TO 14,999',
               '15,000 TO 19,999','20,000 TO 24,999','25,000 TO 29,999','30,000 TO 34,999','35,000 TO 39,999',
               '40,000 TO 49,999','50,000 TO 59,999','60,000 TO 74,999','75,000 TO 99,999','100,000 TO 149,999',
               '150,000 OR MORE'),
  PULKDK1 = c('Already working','1','4','6', '9','10', '12','13'),
  
  HRFS30D8 = c("High Food Security among Adults",'Marginal Food Security among Adults','Low Food Security among Adults','Very Low Food Security among Adults'),
  HRFS30D2 = c("High Food Security",'Marginal Food Security','Low Food Security','Very Low Food Security'),
  HRFS30D1  = c("Food Secure - High or Marginal Food Security",'Low Food Security','Very Low Food Security'),
  HRFS12M8 = c("High Food Security among Adults",'Marginal Food Security among Adults','Low Food Security among Adults','Very Low Food Security among Adults'),
  HRFS12MC = c("Children Food Secure, High or Marginal Food Security among Children",'Low Food Security among Children','Very Low Food Security among Children','No child'),
  # HESCF4 = c('Almost every month','Some months but not every month','Only 1 or 2 months'),
  HESCF3  = c('Almost every month','Some months but not every month','Only 1 or 2 months','Not old/poor'),
  HESSHF4 = c('Almost every month','Some months but not every month','Only 1 or 2 months','Never'),
  HESSH2 = c('Almost every month','Some months but not every month','Only 1 or 2 months','No children'),
  
  HESS6 = c('Often true','Sometimes true','Never true','No children'),
  HETSSHMF1 = c('0', '7', '14', '21', '30'),
  HESS4 = c('Often true','Sometimes true','Never true'),
  HESS3 = c('Often true','Sometimes true','Never true','Not poor'),
  HES8B = c('More','Same','Less'),
  
  PECYC = c('Not in college','Less than 1 year (includes 0 years completed)','The first or Freshman year','The second or Sophomore year',
            'The third or Junior year','Four or more years'),
  
  PEHRACT1 = c('Not working','0', '1', '2','3','4','5','6', '7','8', '9',
               '10', '11', '12', '13', '14', '15', '16', '17', '18', '19',  '20', '21', '22', 
               '23', '24', '25', '26', '27', '28', '29',  '30', '31', '32', '33', '34', '35', '36', '37', '38', 
               '39',  '40', '41', '42', '43', '44', '45', '46', '47', '48', '49',  '50', '51', '52', '53', '54', '55', 
               '56', '57', '58', '59',  '60', '61', '62', '63', '64', '65', '66', '67', '68', '69', '70', '71', '72', 
               '73', '74', '75', '76', 
               '77', '78', '79', '80', '82', '83', '84', '85', '86', '87', '88',  '90', '91', '95', '96', '98', '99'),
  
  PEHRACT2 = c('No 2 jobs', '0', '1','2','3','4', '5','6', '7', '8', '9',
               '10', '11', '12', '13', '14', '15', '16', '17', '18', '19',  '20', 
               '21', '22', '23', '24', '25', '26', '27', '28', '29', '30', '31', '32', '33', '34', '35', 
               '36', '37',  '40', '41', '42', '44', '45', '48', '49',  '50', '52', '60', '67', '68'),
  
  PEHRACTT = c('Not working','1','2',  '3','4','5',  '6','7', '8', '9',
               '10',  '11', '115', '119', '12', '13', 
               '14', '15', '16', '17', '18', '19', '20', '21', '22', '23', '24', 
               '25', '26', '27', '28', '29', '30', '31', '32', '33', '34', '35', '36', '37', '38', '39', 
               '40', '41', '42', '43', '44', '45', '46', '47', '48', '49', '50', '51', '52', '53', '54', '55', '56',
               '57', '58', '59', '60', '61', '62', '63', '64', '65', '66', '67', '68', '69', 
               '70', '71', '72', '73', '74', '75', '76', '77', '78', '79', 
               '80', '82', '83', '84', '85', '86', '87', '88', '89', '90', '91', '92', 
               '94', '95', '96', '97', '98', '99','100', '104', '105', '106', '134', '136', '139'),
  
  
  PTNMEMP2 = c('Not self-employed','1', '2', '3', '4', '5', '9', '10 or more employees'),
  PEHGCOMP = c('Less than 1st grade','1st, 2nd, 3rd, or 4th grade','5th or 6th grade','7th or 8th grade,9th grade','Received GED or higher'),
  PEDIPGED = c('Graduation from high school','GED or other equivalent','Did not graduate high school'),
  
  PEERNPER = c('HOURLY','WEEKLY','BI-WEEKLY'),
  
  PRTAGE = c(0,1,2,3,4,5,6,7,8,9,10, 
             11,12,13, 14,15, 16,17,18,19,  
             20,21,22,23,24,25,26,27,28.29,  
             30,31,32,33,34,35,36,37,38,39,40,                                                                  
             41,42,43,44,45,46,47,48,49,  
             50,51,52 ,53,54,55,56,57,58,59,60  ,                              
             61,62,63,64,65,66,67,68,69,                                                                         
             70,71,72,73,74,75,76,77,78,79,'80-84 Years Old','85+ Years Old'),     
  
  HRFS30D5 = c("Children Food Secure High or Marginal Food Security among Children",'Low Food Security among Children','Very Low Food Security among Children','No children'),
  HRFS12M1 = c("Food Secure High or Marginal Food Security",'Low Food Security','Very Low Food Security'))

# Safety check
# Detect any variables in 'ordered.factors' that are NOT in the codebook
extras <- noquote(setdiff(names(custom.ordered), codebook$var))
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


for (v in names(custom.ordered)) {
  d[[v]] <- factor(d[[v]], levels = custom.ordered[[v]])
}

#Keep only rows required for the household-level file
d_p <- d #%>% filter(PERRP == '1' | PERRP  == '2')

#Imputation:
#Which variables have missing values and how frequent are they?
na.count <- colSums(is.na(d_p))
na.count <- na.count[na.count > 0]
na.count

## Impute NA values in 'd_cps'
d_p <- fusionModel::impute(data = as.data.table(d_p), 
                           weight = 'PWSUPWGT',
                           ignore = c('HRHHID','HRHHID2','HRMONTH','HRYEAR4','HRINTSTA','HHSUPWGT',
                                      'PWFMWGT','PWLGWGT','PWORWGT','GTCBSA', 'GTMETSTA','GTCSA',
                                      'PWSSWGT','PWVETWGT'))

###Correct geographic variables for compatability with geoconcordance.fst
d1 <- d_p %>% rename(region = GEREG,
                     division = GEDIV,
                     cbsatype13 =  GTMETSTA,
                     state_postal = GESTFIPS,
                     cbsa13 = GTCBSA,
                     csa13 = GTCSA)

geo_conc <- fst::read_fst("geo-processed/concordance/geo_concordance.fst", 
                          column = c("division",'region')) %>% unique() 

# See which variables in 'd' are also in 'geo_concordance' and
gnames <- names(fst::fst("geo-processed/concordance/geo_concordance.fst"))
gvars <- intersect(gnames, names(d1))
gvars 

# Class new/added geo identifiers as unordered factors
d1 <- d1 %>% 
  mutate_at(gvars, ~ factor(.x, levels = sort(unique(.x)), ordered = FALSE)) %>%
  mutate( division = as.factor(division),#,
          region = as.factor(region),
          state_postal = as.factor(state_postal),
          cbsatype13 = as.factor(cbsatype13),
          cbsa13 = as.factor(cbsa13),
          csa13 = as.factor(csa13),
          HRHHID = as.factor(HRHHID)) 

# NOTE: var_label assignment is done AFTER any manipulation of values/classes, because labels can be lost when classes are changed
p.final <- d1 %>%
  mutate_if(is.factor, safeCharacters) %>%
  mutate_if(is.numeric, convertInteger) %>%
  mutate_if(is.double, cleanNumeric, tol = 0.001) %>%
  mutate_if(is.factor, droplevels) %>%
  mutate(hid = as.factor(paste0(HRHHID,HRHHID2)),
         pid = OCCURNUM) %>%
  labelled::set_variable_labels(.labels = setNames(as.list(safeCharacters(codebook$desc)), codebook$var), .strict = FALSE) %>%  # Set descriptions for codebook variables
  labelled::set_variable_labels(.labels = setNames(as.list(paste(gvars, "geographic concordance")), gvars)) %>%  # Set descriptions for geo identifiers
  rename(
    weight = PWSUPWGT) %>%
  rename_with(tolower) %>%  # Convert all variable names to lowercase
  select(hid,pid, weight, everything())  

# Create Dictionary needs label for variables
labelled::var_label(p.final$hid) <- "Unique household identifier"
labelled::var_label(p.final$perrp) <- "RELATIONSHIP TO REFERENCE"
labelled::var_label(p.final$hrfs12m3) <- "Food Security Raw Score, 12-Month Recall"
labelled::var_label(p.final$hulangcode) <- "Census region" 
labelled::var_label(p.final$pwvetwgt) <- "Veteran's Weight" 
labelled::var_label(p.final$weight) <- "Person-level Food Supplement Weight" 
labelled::var_label(p.final$pid) <- "Unique person identifier"
labelled::var_label(p.final$hhsupwgt) <- "Household-level Food Supplement Weight" 

# Create dictionary and save to disk
dictionary <- createDictionary(data = p.final, survey = "CPS", vintage = 2019, respondent = "P")
saveRDS(object = dictionary, file = "survey-processed/CPS/2019/CPS_2019_P_dictionary.rds")

#----------------
compileDictionary()

# Save data to disk (.fst)
fst::write_fst(x = p.final, path = "survey-processed/CPS/2019/CPS_2019_P_processed.fst", compress = 100)

