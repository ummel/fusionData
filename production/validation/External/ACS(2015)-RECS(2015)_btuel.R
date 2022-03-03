# script to manually fuse acs-recs fusion
# Karthik Akkiraju and Peter Berrill, Januray 2022
rm(list=ls()) # clear workspace i.e. remove saved variables
cat("\014") # clear console
setwd("/Users/user/Documents/Yale/FusionData/fusionData")
# before loading fusionData, needs to be re-built locally after adding the new AHS fst file and dictionary
# this can be done either with devtools::install() or in RStudio Build -> Install and Restart
library(shiny)
library(fst)
library(fusionData)
compileDictionary()
# open the universal survey dictionary

universe()
# see dictionaries for each survey
acs.dictionary <- readRDS("survey-processed/ACS/2015/ACS_2015_H_dictionary.rds")
recs.dictionary <- readRDS("survey-processed/RECS/2015/RECS_2015_H_dictionary.rds")


# original RECS
recs <- fst::read_fst("survey-processed/RECS/2015/RECS_2015_H_processed.fst")
# original ACS
acs <- fst::read_fst("survey-processed/ACS/2015/ACS_2015_H_processed.fst")


# Open "Survey Harmonization Tool" Shiny app
harmony()
# harmony between RECS and ACS is based on the following matching variables;
#  bedrooms__bdsp,  desktop__laptop, education__schl, elpay__elefp, employhh__wkhpand fuelheat__hflt, hhage__agep, hhsex__sex, householder_race__rac1p,internet__access,kownrent__ten,moneypy__hincp, ngpay__gasfp, nhsldmem__np,numadult__agep,numchild__agep, numfrig__refr, numtablet__handheld,occupyyrange__mv, sdescent__hisp, stoven__stov,totrooms__rmsp, typehuq__bld,yearmaderange__yb;
# The file describing how to harmonize RECS 2015 and ACS 2015 variables is RECS_2015__ACS_2015.R.
concordance <- fst::fst("geo-processed/concordance/geo_concordance.fst")

# step 1, fuse RECS to ACS #########
# Prepare RECS 2015 household microdata for fusion with ACS 2015 microdata

prep <- prepare(donor = "RECS_2015", 
                recipient = "ACS_2015", 
                respondent = "household",
                implicates = 5)

data<-assemble(prep,
               fusion.variables=c('btuel'), # Electricity variable used for fusing 
               window = 3,
               pca = c(30, 0.9))

lapply(data, dim)



library(fusionModel)

fit <- fusionModel::train(data = data$RECS_2015, 
                          y = c('btuel'),# here is the one fusion variable used 
                          ignore = "recs_2015_hid", 
                          weight = "weight",
                          cores = 1,
                          maxcats = 10,
                          lasso = 0.95)

sim <- fuse(data = data$ACS_2015,
            train.object = fit) %>%
 
  cbind(data$ACS_2015["acs_2015_hid"]) %>%
  select(acs_2015_hid, everything())

stopifnot(!anyNA(sim))

summary(data$RECS_2015$btuel)
summary(sim$btuel)

#Generating implicates
#n.imp <- 200
#sim1  <- lapply(1:n.imp, function(i) fuse(data = data$ACS_2015, train.object = fit))
#df  <- as.data.frame(do.call(cbind, sim1)) 

#colnames(df) <- paste("btuel",1:ncol(df), sep = "")
#df1 <- cbind(df, data$ACS_2015["acs_2015_hid"]) 

#nX <- ncol(df1)-1
#recs_sim_imp <- lapply(1:nX, function(x) NULL)
#for(i in 1:(ncol(df1)-1)) {
  #tempdf <- data.frame(df1$acs_2015_hid, df1[i])
 # fst::write_fst(tempdf, paste0("recs_sim_imp", i, ".fst",compress = 100))
#}

r <- ggplot () + geom_density(data=recs, aes(x = btuel), color = "#ff6961", size = 1) + geom_density(data=sim, aes(x = btuel),size = 1)
r + theme(text = element_text(size=18)) + theme(axis.line.x.bottom = element_line(color="black", size = 0.5), axis.line.x.top = element_line(color="black", size = 1.5), axis.line.y.left = element_line(color="black", size = 0.5), axis.line.y.right = element_line(color="black", size = 1.5)) + theme(axis.ticks.length = unit(-0.25, "cm")) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+theme(panel.border = element_rect( fill = NA, size = 1))

fst::write_fst(sim,"production/fusion/RECS_2015_sim_btuelx.fst", compress = 100)
