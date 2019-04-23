library(MASS)
library(tidyverse)
library(reshape2)
library(chron)
library(nlme)
library(sjstats)
library(lme4)
library(car)
library(glmmTMB)
library(vegan)
require(doBy)
library(RMariaDB) 
library(DHARMa)
source("user.R")
source("psw.R")

conn <- dbConnect(RMariaDB::MariaDB(), host = '160.94.186.138',  dbname='fragbees', user = user, password = psw, port=8889)
dbListTables(conn)
specimen<- dbReadTable(conn, "fragbees2017_specimen") 
species<- dbReadTable(conn, "fragbees2017_species")
frag_sites<- dbReadTable(conn, "fragbees_sites")
quadrats<- dbReadTable(conn, "fragbees2017_veg_quad")

dbDisconnect(conn)

### create gs column
species$gs <- paste(species$genus, species$species, sep="_")


## Setting Up Data frame

sp_spec<- merge(specimen, species, by="specimenID", all.y=TRUE, all.x=FALSE)


master<- merge(sp_spec, frag_sites, by="site", all.x=TRUE)

master$bee_sp<- paste(master$genus, master$species, sep="_")

master<- master[!(master$bee_sp=="_"),]
master<- master[!(master$bee_sp=="_wasp"),]

length(unique(master$bee_sp))


######################
### all bees
abun<- master %>% filter(site%in%alan_sites)%>%
  filter(round>3|round==3)%>%
  group_by(site,round,month,day) %>%
  tally()%>%group_by(site)%>%
  summarise(sum_abun=sum(n))
### alter here for grouping by round or not

mean<- master%>%filter(site%in%alan_sites)%>%
  filter(round>3|round==3)%>%
  group_by(site,round,month,day)%>%
  tally()%>%
  group_by(site)%>%
  summarise(avg_abun=mean(n))
#capitalize first letter of sites
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
sites$site<-firstup(sites$site)

abun_sites<-abun%>%left_join(sites, by="site")
mean_sites<-mean%>%left_join(sites, by="site")

abun_sites%>%ggplot(aes(prop.c,sum_abun))+geom_point(aes(color=site))+geom_smooth(method="glm")
mean_sites%>%ggplot(aes(prop.c,avg_abun))+geom_point(aes(color=site))+geom_smooth(method="lm")

####
#using an average count
log_lm<-lm(log(sum_abun)~prop.c,data=abun_sites)

summary(log_lm)
qqPlot(resid(log_lm))
plot(log_lm)





### using count data with or without random effects
## the sum of all bees caught at each site round examined
library(MASS)
abun_nb<-glm.nb(sum_abun~prop.c,data=abun_sites)
summary(abun_nb)
plot(abun_nb)
qqPlot(resid(abun_nb))

#if we use 3 rounds that overlapped
#one unit increase in ag:
exp(4.58671+0.01596*20)
#1.7 bees gained, roughly

exp(4.58671)

1.00-(98.17/135.0858)
#27% gain in bee abundance


#if we use all rounds
#20 unit increase in ag:
exp(6.010410+0.008544*20)

1.00-(407.6504/483.6154)
#15% gain in bee numbers
# that's a pretty steep drop in effect size 


###


exp(0.01596)
98.17092*(1.016088)^20
### this has a great fit...
### 


library(DHARMa)
sim_abun<-simulateResiduals(fittedModel =abun_nb, n = 250)
plot(sim_abun)
testUniformity(sim_abun)
testDispersion(sim_abun)


### This seems strange
### I'm going to add a floral frequency variable to add floral abundance to the model.
###

# Create plant frequency variable
quadrats$pgs<-paste(quadrats$plant_genus,quadrats$plant_species, sep="_")


quad_plants<-quadrats%>%
  select(pgs,site,round,transect,quadrat,flowering_units)%>%#step 1: sum by quadrat
  group_by(site,round,transect,quadrat)%>%
  summarise(flw_per_quad=sum(flowering_units))%>%
  group_by(site,round,transect)%>%
 summarise(flw_quads=sum(flw_per_quad>0),noflw_quads=sum(flw_per_quad==0))%>%
  group_by(site,round)%>%
  summarise(flw_quads_sum=sum(flw_quads), noflw_quads_sum=sum(noflw_quads),
            sum_quads=flw_quads_sum+noflw_quads_sum,flw_freq=flw_quads_sum/sum_quads)
### now try a richness measure?
quad_rich<-quadrats%>%
  select(pgs,site,round,transect,quadrat,flowering_units)%>%#step 1: sum by quadrat
  group_by(site,round)%>%summarize(richness=n_distinct(pgs))


# you can calculate a mean frequency
mean_quad_plants<-quad_plants%>%filter(site%in%alan_sites)%>%
  full_join(mean_sites, by="site")%>%
  full_join(quad_rich, by=c("site","round"))%>%
  filter(round>3|round==3)%>%
  group_by(site,prop.c,avg_abun)%>%summarise(mean_flw_freq=mean(flw_freq),
                                             mean_flw_quads=mean(flw_quads_sum))

mean_quad_plants%>%ggplot(aes(mean_flw_freq,avg_abun))+geom_point()
mean_quad_plants%>%ggplot(aes(mean_flw_quads,avg_abun))+geom_point()


mean_quad_plants$scale_avg_abun<-scale(mean_quad_plants$avg_abun)

mean_quad_plants%>%ggplot(aes(prop.c,scale_avg_abun))+geom_point()


# alternatively, add up across rounds calculate frequency from total
sum_quad_rich<-quadrats%>%
  select(pgs,site,round,transect,quadrat,flowering_units)%>%#step 1: sum by quadrat
  group_by(site)%>%summarize(richness=n_distinct(pgs))


sum_quad_plants<-quad_plants%>%filter(site%in%alan_sites)%>%
  full_join(abun_sites, by="site")%>%
  full_join(sum_quad_rich, by=c("site"))%>%
  filter(round>3|round==3)%>%
  group_by(site,prop.c,sum_abun,richness)%>%
  summarise(sum_flw_freq=(sum(flw_quads_sum)/sum(sum_quads)),
                          sum_flw_quads=sum(flw_quads_sum))

sum_quad_plants%>%ggplot(aes(sum_flw_quads,sum_abun,color=site))+geom_point()
sum_quad_plants%>%ggplot(aes(richness,sum_abun,color=site))+geom_point()


  
abun_flw_nb<-glm.nb(sum_abun~prop.c,data=sum_quad_plants)
summary(abun_flw_nb)

#richness and flw_abundance (frequency or number of quadrats with flowers) aren't changing models

#adding additional rounds seems to be the biggest driver of model change.


##################bumbles only

## Bumble Bees Only

master_bumble<-master[master$genus=="Bombus",]

master_bt<- master_bumble[master_bumble$method=="net",]
unique(master_bt$site)
alan_sites<-c("Braaten","Howe","Nelson","Rudningen","Silis","Staples","Woltjer")

masbb <- master_bumble %>% filter(site%in%alan_sites)%>%
  filter(round>3|round==3)%>%
  group_by(site,round,month,day) %>%
  tally()

masbb_net <- master_bumble %>% filter(site%in%alan_sites)%>%
  filter(round>3|round==3)%>%filter(method=="net")%>%
  group_by(site,round,month,day,method) %>%
  tally()
#####################