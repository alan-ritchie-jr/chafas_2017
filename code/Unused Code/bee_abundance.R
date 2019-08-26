#Note: this code is not used in the chafas manuscript as of July 2019

####
#this script uses the chafas seed set data and Ian's bee abundance data
#it examines how landscape, bee abundance, and seed set may relate.

library(MASS)
library(tidyverse)
library(reshape2)
library(chron)
library(nlme)
library(lme4)
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
specimen<- dbReadTable(conn, "fragbees2017_allbees") 
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

  group_by(site,round,month,day) %>%
  tally()%>%group_by(site)%>%
  summarise(sum_abun=sum(n))
### alter here for grouping by round or not

mean<- master%>%filter(site%in%alan_sites)%>%
  
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
mean_abun<-mean%>%left_join(abun_sites, by="site")

mean_abun%>%ggplot(aes(prop.c,sum_abun))+geom_point(aes(color=site))+geom_smooth(method="glm")
mean_abun%>%ggplot(aes(prop.c,avg_abun))+geom_point(aes(color=site))+geom_smooth(method="lm")
mean_abun$site<-tolower(mean_abun$site)
####
#using an average count
log_lm<-lm(log(sum_abun)~prop.c,data=mean_abun)

summary(log_lm)
qqPlot(resid(log_lm))
plot(log_lm)





### using count data with or without random effects
## the sum of all bees caught at each site round examined
abun_nb<-glm.nb(sum_abun~prop.c,data=mean_abun)
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


#### flower data
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

### check if bee abundance is predictive of the total seed set produced at a site.

### seed set data
seed_land<-read.csv("data/seed_land.csv")
seed_land <-filter(seed_land, plot=="hi")

seed_land$round<-as.factor(seed_land$round)


# summary table
plt_nr<- seed_land %>%
  group_by(site,prop.c,trmnt, ID)%>%
  summarise(frt=n(), trtflw=sum(unique(flowers)),
            polov=sum(poll_ovules),
            avg_polov=mean(poll_ovules),
            seeds=sum(total.seeds),
            avg_seed=mean(total.seeds),
            totov=sum(total.ovules),
            avg_totov=mean(total.ovules))

plt_nr%>%ggplot(aes(site,frt,fill=site))+geom_boxplot()
#use plt_nr to get a site level measure of seed set
#note model takes the form of tot_seed/totfrt, so just some those?
# first let's try with treatment
plt_tot<-plt_nr%>% group_by(site,prop.c)%>%
  summarise(frt=sum(frt),seedset=sum(seeds))
# now, join to mean_abun

plt_bee<-plt_tot%>%right_join(mean_abun,by=c("site","prop.c"))


plt_bee_id<-plt_nr%>%right_join(mean_abun,by=c("site","prop.c"))
##add bumble data%>%

## Bumble Bees Only

master_bumble<-master[master$genus=="Bombus",]

master_bt<- master_bumble[master_bumble$method=="net",]
unique(master_bt$site)
alan_sites<-c("Braaten","Howe","Nelson","Rudningen","Silis","Staples","Woltjer")

masbb <- master_bumble %>% filter(site%in%alan_sites)%>%
 filter(round==3|round>3)%>%
  group_by(site,round,month,day) %>%
  tally()%>%group_by(site)%>%
  summarise(bb_abun=sum(n))%>%mutate(site=tolower(site))
##
plt_bee_bb<-plt_bee%>%left_join(masbb, by="site")

plt_bee_bb_id<-plt_bee_id%>%left_join(masbb, by="site")
### model
#nb
seed_bb_nb<-glm.nb(seedset~bb_abun+offset(log(frt)),data=plt_bee_bb)
summary(seed_bb_nb)
## just 7 data points; does bee abundance explain the site level average of seeds per fruit


# coefficent with all rounds
exp(-0.005976)
# with only the rounds that overlapped with us
exp(-0.006532)

## intercept
exp(2.408299)
# 11.11504 seeds per fruit
## if we increase the number of bumbles by 30
# roughly the difference between our highes and lowest counts
exp(2.408299-0.006532*20)
# the mean number of seeds produced is about 2 fewer
## not a huge difference
# INterpretation:
# a one unit increase in bumblebees leads to a 0.99% reduction in seed set across our plants.

ggplot(data=plt_bee_bb_id, aes(bb_abun,seeds/frt, color=site))+geom_point()


#all plants as data points
# does bumble bee abundance explain individual level means of seeds per fruit?
#without scaling
seed_bb_id<-glm.nb(seeds~trmnt+prop.c+bb_abun+offset(log(frt)),data=plt_bee_bb_id)
summary(seed_bb_id)

#coefficients with no other paramters
exp(-0.005889)
exp(2.385424)

### if you add trmnt
#wihtout ag
exp(-0.006178)

#with ag the effect of a 20 unit increase in bb abund
exp(2.458711-0.005642*20)
#lose approximately 1 seed, holding ag constant

#20 unit increase in bumbles
exp(2.363359-0.006178*20)
#again,about a 1 seed decrease


#with scaling
seed_bb_id_sc<-glm.nb(seeds~scale(bb_abun)+trmnt+prop.c+offset(log(frt)),data=plt_bee_bb_id)
summary(seed_bb_id_sc)

exp(2.15616)
exp(-0.05471)
exp(2.15616-0.05471*20)

## thats a huge decrase! what's up? with this?
sim_nb<-simulateResiduals(fittedModel =seed_bb_id, n = 250)
plot(sim_nb)
testUniformity(sim_nb)

sim_sc<-simulateResiduals(fittedModel =seed_bb_id_sc, n = 250)
plot(sim_sc)
testUniformity(sim_sc)
testDispersion(sim_sc)
plotResiduals(sim_sc$scaledResidua)


plotResiduals(plt_nr$prop.c, sim$scaledResiduals)
plotResiduals(plt_nr$trmnt, sim$scaledResiduals)
pred()
plotResiduals(plt_bee_bb_id$bb_abun, sim_sc$scaledResiduals)

### random effect
seed_bb_id_re<-glmer.nb(seeds~scale(bb_abun)+trmnt+scale(prop.c)
                        +offset(log(frt))+(1|site),data=plt_bee_bb_id)

seed_bb_id_re2<-glmmTMB(seeds~scale(bb_abun)+trmnt+scale(prop.c)
                        +offset(log(frt))+(1|site),
                       family=nbinom2,data=plt_bee_bb_id)

summary(seed_bb_id_re)
summary(seed_bb_id_re2)### getting crazy tiny RE variances...


## parameter almost the same
exp(-0.04674)
##################bumbles only



masbb_net <- master_bumble %>% filter(site%in%alan_sites)%>%
  filter(round>3|round==3)%>%filter(method=="net")%>%
  group_by(site,round,month,day,method) %>%
  tally()%>%group_by(site)%>%
  summarise(sum_abun=sum(n))
#####################