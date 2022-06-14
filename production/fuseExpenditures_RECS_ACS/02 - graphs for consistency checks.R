##########################
# Various graphs to check
# the consistency of the 
# expenditure estimates 
##########################

require(ggplot2)

# Color Blind Friendly Palette 
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# All Fuels: RECS vs fusion
plot_data<-rbind(
  data.frame(data="RECS",fuel="Electricity",value=donor$btuel,weight=donor$weight),
  data.frame(data="RECS",fuel="Natural Gas",value=donor$btung,weight=donor$weight),
  data.frame(data="RECS",fuel="LPG",value=donor$btulp,weight=donor$weight),
  data.frame(data="RECS",fuel="Fuel Oil",value=donor$btufo,weight=donor$weight),
  data.frame(data="fusion",fuel="Electricity",value=recipient$btuel,weight=recipient$weight),
  data.frame(data="fusion",fuel="Natural Gas",value=recipient$btung,weight=recipient$weight),
  data.frame(data="fusion",fuel="LPG",value=recipient$btulp,weight=recipient$weight),
  data.frame(data="fusion",fuel="Fuel Oil",value=recipient$btufo,weight=recipient$weight))

plot_data$data<-factor(plot_data$data,levels=c("RECS","fusion","ACS"))

ggplot(plot_data[plot_data$value>0,])+
  geom_density(aes(value,weight=weight,col=data),size=1)+
  labs(x="Expenditure",y="Density")+
  scale_color_manual(values=cbPalette,name="")+
  facet_wrap(facets="fuel")+
  theme_bw()

## Compare with ACS
# Electricity
acs <- fst::read_fst("survey-processed/ACS/2015/ACS_2015_H_processed.fst")

plot_data<-rbind(data.frame(label="RECS",value=donor$dollarel,weight=donor$weight,division=donor$loc..recs_division),
                 data.frame(label="fusion",value=recipient$dollarel,weight=recipient$weight,division=recipient$loc..recs_division),
                 data.frame(label="ACS",value=12*acs$elep,weight=acs$weight,division=recipient$loc..recs_division))

plot_data$label<-factor(plot_data$label,levels=c("RECS","fusion","ACS"))

ggplot(plot_data)+
  geom_density(aes(value,weight=weight,col=label),size=1)+
  labs(x="Expenditure on Electricity",y="Density")+
  scale_color_manual(values=cbPalette,name="")+
  theme_bw()

ggplot(plot_data)+
  geom_density(aes(value,weight=weight,col=label),size=1)+
  labs(x="Expenditure on Electricity",y="Density")+
  scale_color_manual(values=cbPalette,name="")+
  facet_wrap(facets="division")+
  theme_bw()

# Gas and Others
plot_data<-rbind(
  data.frame(data="RECS",fuel="Gas",value=donor$dollarng,weight=donor$weight),
  data.frame(data="RECS",fuel="Other Fuels",value=donor$dollarfo+donor$dollarlp,weight=donor$weight),
  data.frame(data="fusion",fuel="Gas",value=recipient$dollarng,weight=recipient$weight),
  data.frame(data="fusion",fuel="Other Fuels",value=recipient$dollarfo+recipient$dollarlp,weight=recipient$weight),
  data.frame(data="ACS",fuel="Gas",value=12*acs$gasp,weight=acs$weight),
  data.frame(data="ACS",fuel="Other Fuels",value=acs$fulp,weight=acs$weight))

plot_data$data<-factor(plot_data$data,levels=c("RECS","fusion","ACS"))

ggplot(plot_data[plot_data$value>0,])+
  geom_density(aes(value,weight=weight,col=data),size=1)+
  labs(x="Expenditure",y="Density")+
  scale_color_manual(values=cbPalette,name="")+
  facet_wrap(facets="fuel")+
  theme_bw()


# Consistency with real prices
# NOTE: Obtained from https://www.eia.gov/state/seds/seds-data-complete.php?sid=US
# specifically, "Full reports & data files" | "Prices" 
# https://www.eia.gov/state/seds/sep_prices/total/csv/pr_all.csv
real_mean_prices<-read.csv("production/fuseExpenditures_RECS_ACS/Additional files for testing/pr_all.csv")
real_mean_prices<-real_mean_prices[real_mean_prices$MSN=="ESRCD",]
real_mean_prices$price<-real_mean_prices$X2015/10 # *100/1000
real_mean_prices<-real_mean_prices[c("State","price")]
names(real_mean_prices)<-c("state_iso","mean_price")
real_mean_prices<-merge(real_mean_prices,read.csv("production/fuseExpenditures_RECS_ACS/Additional files for testing/state_iso.csv"),by="state_iso",all.x=T)
names(real_mean_prices)<-c("state_iso","mean_price","state_name")

geo_corr<-fst::read_fst("geo-processed/concordance/geo_concordance.fst")
geo_corr<-unique(geo_corr[c("state","state_name")])
real_mean_prices<-merge(geo_corr,real_mean_prices[c("state_name","mean_price")],by="state_name")

recipient$state<-acs$state
recipient<-merge(recipient,real_mean_prices,by="state")
recipient$priceel<-100*recipient$dollarel/recipient$btuel

ggplot(recipient)+
  geom_density(aes(priceel))+
  geom_vline(aes(xintercept=mean_price),col="red",size=1)+
  labs(x="Electricity Prices (black: distribution on fusion, red: actual average)",y="Density")+
  facet_wrap(facets="state_name")

