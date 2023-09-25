## Program for cleaning the data dictionary provided by Pulse
## Goal: make this generalizable for any wave of pulse - currently built for w = 50
## Created: HA 8/11/23
## Updated: 

clean_HPS_dict <- function(w){
  
  dict <- readxl::read_excel(paste0('./survey-raw/HPS/HPS_Week', w, '_PUF_CSV/pulse2022_data.dictionary_CSV_', w, '.xlsx'), skip = 4) %>% as.data.table()
  
  names(dict) <- c('qID', 'variable', 'type', 'description', 'question', 'range', 'notes')
  dict[ , notes := NULL]
  
  # add variable order 
  dict[!is.na(variable), varorder := .I]
  
  # reshape the dictionary so one row per variable
  dict <- dict %>%
    fill(variable) %>% 
    group_by(variable) %>%
    fill(qID, question, range, varorder) %>%
    as.data.table() %>%
    dcast(varorder + variable + qID + question + range ~ type, value.var = 'description') %>%
    arrange(varorder) %>%
    as.data.table()
  
  ## values and labels ----
  
  # turn the variable values into lists (for numeric variables - this will be missing)
  dict[ , val := str_extract_all(Values, pattern = '-?[:digit:]+(?=\\))')]
  dict[ , lbl := str_split(Values, pattern = '\r\n')]
  
  # correction for state and met area - different format 
  dict[variable == 'EST_ST', val := str_extract_all(Values, pattern = '[:digit:][:digit:]')]
  #dict[variable == 'EST_ST', val := str_remove_all(val, pattern = '^0')]
  dict[variable == 'EST_ST', val := str_extract_all(Values, pattern = '[1-9]?(?<=[:digit:])[0-9]')]
  dict[variable == 'EST_ST', lbl := str_extract_all(Values, pattern = '(?<=\')[:alpha:]+')]
  
  dict[variable == 'EST_MSA', val := str_extract_all(Values, pattern = '[:digit:]+')]
  dict[variable == 'EST_MSA', lbl := str_extract_all(Values, pattern = '(?<== ).+')]
  
  # correction for variables with no labels
  dict[is.na(Values), val := NA]
  dict[is.na(Values), lbl := NA]
  
  # correction for non-categorical variables
  dict[str_detect(Values, '^[:digit:]+-[:digit:]+$'), val := NA]
  dict[str_detect(Values, '^[:digit:]+-[:digit:]+$'), lbl := NA]
  dict[str_detect(Values, '(whole number)'), val := NA]
  dict[str_detect(Values, '(whole number)'), lbl := NA]
  dict[variable %in% c('WEEK', 'PWEIGHT', 'SCRAM'), val := NA]
  dict[variable %in% c('WEEK', 'PWEIGHT', 'SCRAM'), lbl := NA]
  
  # for child care cost, it is numeric but has missing values -99 and -88 --> need to deal with this distinctly
  dict[variable == 'TCCARECOST', val := NA]
  dict[variable == 'TCCARECOST', lbl := NA]
  
  # for formula type, -1 shows up in the name of the formulas and is confusing the labels -> correct manually
  dict[variable == 'FRMLA_TYP4', val := c("1", "-99", "-88")]
  
  # for number of doses, there was a typo so -88 wasn't recorded as a missing value -> correct manually
  dict[variable == 'NUMDOSES', val := c("2", "1", "-99", "-88")]
  
  # for two variables (KIDBHVR3 and KIDBHVR4) the -88 value was not recorded in the dictionary -> correct manually
  for (v in c('KIDBHVR3', 'KIDBHVR4')){
    dict[variable == v, val := c("1", "-99", "-88")]
    lab <- dict[variable == v, lbl][[1]]
    dict[variable == v, lbl := c(lab, "Missing / Did not report")]
  }
  
  ## type of variable ----
  # record type of variable
  dict[ , type := ifelse(is.na(val), 'numeric', 'factor')]
  table(dict$type, useNA = 'always')
  dict[type == 'numeric', variable]
  
  ## universe ----
  # many variables are only valid for certain subsets of the population
  # translate these into code that can be easily subset on 
  
  dict[ , denom := ifelse(str_detect(tolower(Universe), 'all persons'), NA, tolower(Universe))]
  dict[ , denom := str_replace_all(denom, '=', '==')]
  dict[ , denom := str_replace_all(denom, ' or ', ' | ')]
  dict[ , denom := str_replace_all(denom, 'and', '&')]
  dict[ , denom := str_replace_all(denom, 'in', '%in%')]
  dict[ , denom := str_remove_all(denom, 'if')]
  dict[ , denom := str_replace_all(denom, '\\((?=[:digit:])', 'c\\(')]
  dict[ , denom := str_replace_all(denom, ' (?=[:digit:]:[:digit:])', ' c\\(')]
  dict[ , denom := str_replace_all(denom, '(?<=[:digit:]:[:digit:])$', ')')]
  dict[ , denom := str_trim(denom)]
  
  # there is a typo in one of the universes where 'recvdacc' should actually be 'recvdvacc'
  dict[ , denom := str_replace_all(denom, 'recvdacc', 'recvdvacc')]
  dict[ , denom := str_replace_all(denom, ' hhld_numkid', ' thhld_numkid')]
  dict[ , denom := str_replace_all(denom, '^hhld_numkid', 'thhld_numkid')]
  dict[denom == 'kidgetvac %in% c(2:5)', denom := 'kidgetvac_lt5y %in% c(2:5) | kidgetvac_5_11y %in% c(2:5) | kidgetvac_12_17y %in% c(2:5)']
  dict[ , denom := str_replace_all(denom, '(?<!t)enroll', 'tenroll')]
  
  # for symptmimpct, condition on currently having covid (symptmnow) - tihnk this is a mistake in the denom
  dict[variable == 'SYMPTMIMPCT', denom := "hadcovidrv == 1 & symptmnow == 1"]
  
  unique(dict[str_detect(denom, 'enroll'), denom])
  
  # add 'not in universe' as a level and label
  for (var in dict[!is.na(denom), variable]){
    
    vals <- dict[variable == var, val][[1]]
    lbls <- dict[variable == var, lbl][[1]]
    
    dict[variable == var, val := c(vals, -999)]
    dict[variable == var, lbl := c(lbls, "Not in universe")]
    
  }
  
  # output list of variables for selection (don't want all)
  names(dict)
  fwrite(dict[ , c('variable', 'Description', 'Universe', 'Values')], file = paste0('./survey-processed/HPS/variable_list_week', w, '.csv'))
  
  return(dict)
  
}