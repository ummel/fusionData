library(tidyverse)
library(fusionData)
library(readxl)

source("R/utils.R")

#######################
d_0 <- read.csv('survey-raw/FAPS/2013/faps_fahnutrients.csv')  

hei_g <- haven::read_dta('survey-raw/FAPS/2013/faps_eiq_fahitem_puf.dta') %>% select(-imp_gram_flag)

d <- merge(d_0,hei_g, by = c('hhnum','eventid','itemnum')) %>% rename_all(toupper) %>%
  filter(FOODCODETYPE != 0) %>%
  select(-TOTGRAMSUNADJ,TOTGRAMSEDIBLE,TOTGRAMSUNADJIMP,TOTGRAMSEDIBLEIMP,-FOODCODEMPR_FLAG) #%>%
 # mutate(SATFAT = gsub("Total saturated fatty acids (g), per 100g",0,SATFAT))

remove(d_0,hei_g)

#Create codebook for demographics
codebook <- read_excel('survey-raw/FAPS/2013/9_FAH Nutrient Codebook PUF.xlsx', skip = 16) %>%
 mutate(blocks = NA_integer_) %>%
  rename(var = "4.1.  Identifying Variables") %>%
  mutate(blocks = if_else(!is.na(var) & is.na(lag(var)), cumsum(!is.na(var)), blocks)) %>%
  fill(blocks) %>% select(var,blocks,everything()) %>% 
  mutate(var = gsub("Variable:","",var)) %>%

  mutate(desc = ifelse(grepl("Definition:",...3),...3,NA)) %>% rename(value = '...3') %>%
  #mutate(value = ifelse(blocks == 53,...4,value),
   #      value = ifelse(blocks == 196,...7,value)) %>%
  select(-c("...2","...4",   "...5"  , "...6"  , "...7",
            "...9",   "...10"  , "...11"  , "...12")) %>%
  filter(!grepl("Unique values:|Range|Missing",var)) %>%

    mutate(value = ifelse(grepl("Definition|Value|Note:",value),NA,value)) %>% 
  rename(label = '...8') %>% 
 # mutate(label = ifelse(blocks == 32,...17,label),
  #       label = ifelse(blocks == 24,...18,label),
   #      label = ifelse(blocks == 53,...14,label)) %>%
  select(var,blocks,value,label,desc) %>%
  
  mutate(label  = gsub('Value description',"",label),
         desc = gsub('Definition:',"",desc)) %>%
    fill(var) %>% 
  group_by(var) %>% fill(desc) %>%
  group_by(var) %>%
  filter(!(value == 'NA' & desc == 'NA')) %>%
  group_by(var) %>% fill(desc) %>% 
  select(var,value,label,desc) %>% 
  filter(!grepl("_FLAG",var)) %>%
  filter(!is.na(desc))   %>%
  mutate(label = ifelse(is.na(label),desc,label)) %>% 
  mutate(label = ifelse(grepl("^Don.t know$",label) | 
                          grepl("Refused",label) | grepl("^Don.t Know$", label), "Not applicable", label)) %>%
  mutate(var = trimws(var)) 
  
  
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
# Detect structural dependencies
# Which variables have missing values and how frequent are they?
na.count <- colSums(is.na(d))
na.count <- na.count[na.count > 0]
na.count  # See which variables have NA's

d <- d %>%
  mutate(
         
         F_WHOLE_CUPS = (((as.numeric(F_TOTAL) * HEI_GRAMS)/100) - (as.numeric(F_JUICE) * HEI_GRAMS)/100),   #Only whole fruit 
         F_TOTAL_CUPS = ((as.numeric(F_TOTAL) * HEI_GRAMS)/100),  #Total fruit including whole fruit and juices
         V_TOTAL_CUPS = (V_TOTAL * HEI_GRAMS)/100,  #Total vegetables 
         V_DRKGR_LEG_CUPS  =   ((V_DRKGR + V_LEGUMES) * HEI_GRAMS)/100, #Total dark greens and legumes
         G_WHOLE_OZ = (G_WHOLE * HEI_GRAMS)/100,      #Whole grains
         D_TOTAL_CUPS = (D_TOTAL * HEI_GRAMS)/100,   #Dairy
         PROTEIN_OZ = ((PROTEIN*0.035274* HEI_GRAMS)/100), #Protein
         PF_SEAFOOD_OZ = ((PF_SEAFD_HI + PF_SEAFD_LOW) * HEI_GRAMS)/100, #Seafood
         
         TOT_ENG = (ENERGY * HEI_GRAMS)/100, 

         G_REFINED_OZ = (G_REFINED* HEI_GRAMS)/100, #Refined grains
         SODIUM_G = (SODIUM * HEI_GRAMS)/(100*1000),
         SUGAR_ENG =  (ADD_SUGARS * 16 * HEI_GRAMS)/100, #https://epi.grants.cancer.gov/hei/mean-ratio-method.html
         SATFAT_ENG = (as.numeric(as.character(SATFAT))*9* HEI_GRAMS)/100) #https://epi.grants.cancer.gov/hei/mean-ratio-method.html


# Add/create variables for geographic concordance with variables in 'geo_concordance.fst'
# Assemble final output
# NOTE: var_label assignment is done AFTER any manipulation of values/classes, because labels can be lost when classes are changed
h.final <- d %>%
  mutate_if(is.factor, safeCharacters) %>%
  mutate_if(is.numeric, convertInteger) %>%
  mutate_if(is.double, cleanNumeric, tol = 0.001) %>%
  labelled::set_variable_labels(.labels = setNames(as.list(safeCharacters(codebook$desc)), codebook$var), .strict = FALSE) %>%  # Set descriptions for codebook variables
 # labelled::set_variable_labels(.labels = setNames(as.list(paste(gvars, "geographic concordance")), gvars)) %>%  # Set descriptions for geo identifiers
  rename(
    hid = HHNUM#,  # Rename ID and weight variables to standardized names
   # weight = HHWGT
  ) %>%
  rename_with(tolower) %>%  # Convert all variable names to lowercase
  select(hid,everything()) %>%   # Reorder columns with replicate weights at the end
  arrange(hid)

#----------------

# Save data to disk (.fst)
fst::write_fst(x = h.final, path = "survey-processed/FAPS/2013/FAPS_2013_FAHN_processed.fst", compress = 100)
fst::write_fst(x = codebook, path = "survey-processed/FAPS/2013/FAPS_2013_FAHN_codebook.fst", compress = 100)


##Summarize variable at the household level#######
h.sum <- d  %>%
        group_by(HHNUM) %>%
        summarise(
                    TOT_ENG = sum(as.numeric(TOT_ENG)),
          
                    F_TOTAL_CUPS = sum(as.numeric(F_TOTAL_CUPS)),
                    F_WHOLE_CUPS = sum(as.numeric(F_WHOLE_CUPS)),
          
                    V_TOTAL_CUPS  = sum(as.numeric(V_TOTAL_CUPS)),
                    V_DRKGR_LEG_CUPS  = sum(as.numeric(V_DRKGR_LEG_CUPS)),
                    G_WHOLE_OZ  = sum(as.numeric(G_WHOLE_OZ)),
                    D_TOTAL_CUPS  = sum(as.numeric(D_TOTAL_CUPS )),
                    PROTEIN_OZ  = sum(as.numeric(PROTEIN_OZ)),
                    PF_SEAFOOD_OZ = sum(as.numeric(PF_SEAFOOD_OZ)),  
                                 
                    #FAT_RATIO = (sum(as.numeric(POLYFAT)) + sum(as.numeric(MONOFAT)))/sum(as.numeric(SATFAT)),
                    
                    POLYFAT = sum(as.numeric(POLYFAT)),
                    MONOFAT = sum(as.numeric(MONOFAT)),
                    SATFAT = sum(as.numeric(SATFAT)),
          
                    G_REFINED_OZ  = sum(as.numeric(G_REFINED_OZ)),
                    SODIUM_G  = sum(as.numeric(SODIUM_G)),
                    SUGAR_ENG = sum(as.numeric(SUGAR_ENG)),
                    SATFAT_ENG = sum((SATFAT_ENG)),
                    HEI_GRAMS = sum((HEI_GRAMS))) %>% ungroup()
  

codebook_fah <- codebook %>% filter(var %in% (intersect(colnames(h.sum), codebook$var))) %>%
                filter(var != 'HHNUM') %>%
                mutate(desc = paste0(desc," at home")) %>% ungroup() %>%
  
  add_row(.,var = 'F_WHOLE_CUPS', desc = "Total only whole fruit cups at home",label= NA, value = NA) %>%
  add_row(.,var = 'F_TOTAL_CUPS', desc = "Total all fruit cups at home",label= NA, value = NA)  %>%
  add_row(.,var = 'V_TOTAL_CUPS', desc = "Total vegetable cups at home",label= NA, value = NA)  %>%
  add_row(.,var = 'V_DRKGR_LEG_CUPS', desc = "Total dark green vegetables and legumes cups at home",label= NA, value = NA)  %>%
  add_row(.,var = 'G_WHOLE_OZ', desc = "Total whole grain (oz) at home",label= NA, value = NA)  %>%
  add_row(.,var = 'D_TOTAL_CUPS', desc = "Total dairy cups at home",label= NA, value = NA)  %>%
  add_row(.,var = 'PROTEIN_OZ', desc = "Total protein (oz) at home",label= NA, value = NA)  %>%
  add_row(.,var = 'PF_SEAFOOD_OZ', desc = "Total seafood and plant protein cups at home",label= NA, value = NA)  %>%
  add_row(.,var = 'TOT_ENG', desc = "Total energy (kcal) at home ",label= NA, value = NA)  %>%
  add_row(.,var = 'G_REFINED_OZ', desc = "Total refined grains (oz) at home",label= NA, value = NA)  %>%
  add_row(.,var = 'SODIUM_G', desc = "Total sodium (g) at home ",label= NA, value = NA)  %>%
  add_row(.,var = 'SUGAR_ENG', desc = "Total added sugar energy (kcal) at home",label= NA, value = NA)  %>%
  add_row(.,var = 'SATFAT_ENG', desc = "Total saturated fat energy (kcal) at home",label= NA, value = NA)  %>%
 add_row(.,var = 'HEI_GRAMS', desc = "Total food at home",label= NA, value = NA)  %>%
  
  
 # add_row(.,var = 'FAT_RATIO', desc = "Fat ratio at home",label= NA, value = NA)  %>%
  add_row(.,var = 'G_REFINED_OZ', desc = "Refined grains at home",label= NA, value = NA)  %>%
  add_row(.,var = 'SODIUM_G', desc = "Soidum content at home",label= NA, value = NA)  %>%
  add_row(.,var = 'SATFAR_ENG', desc = "Saturated fat at home",label= NA, value = NA)  %>%
  add_row(.,var = 'HEI_INDEX', desc = "Healthy eating index at home",label= NA, value = NA)  %>%
    mutate(var = paste0(var,"_FAH")) # %>% mutate(var = tolower(var))


# NOTE: var_label assignment is done AFTER any manipulation of values/classes, because labels can be lost when classes are changed
h.sumfinal <- h.sum %>%
  mutate_if(is.factor, safeCharacters) %>%
  mutate_if(is.numeric, convertInteger) %>%
  mutate_if(is.double, cleanNumeric, tol = 0.001) %>%
  labelled::set_variable_labels(.labels = setNames(as.list(safeCharacters(codebook_fah$desc)), codebook_fah$var), .strict = FALSE) %>%  # Set descriptions for codebook variables
  rename(
    hid = HHNUM#,  # Rename ID and weight variables to standardized names
    # weight = HHWGT
  ) %>%
  rename_with(tolower) %>% 
  rename_with(~ if_else(. == "hid", ., paste0(., "_fah")), everything()) %>%

  # Convert all variable names to lowercase
  select(hid,everything()) %>%   # Reorder columns with replicate weights at the end
  arrange(hid)

#----------------
#rm(d)
# Save data to disk (.fst)
fst::write_fst(x = h.sumfinal, path = "survey-processed/FAPS/2013/FAPS_2013_sumFAHN_processed.fst", compress = 100)
fst::write_fst(x = codebook_fah, path = "survey-processed/FAPS/2013/FAPS_2013_sumFAHN_codebook.fst", compress = 100)




