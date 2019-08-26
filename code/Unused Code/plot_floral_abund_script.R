pollination<-read.csv("data/pollination_2017_pp_plant_trait.csv")#pollination event data
seed<-read.csv("data/pollination_2017_pp_fruit_seedset.csv")# seed count data
landscape<-read.csv("data/pollination_2017_siteinfo.csv") #landscape data
event<-read.csv("data/event.csv")# environmental data for visits
# also contains a measure of time between treatement days 


### Merging Data
#install.packages("tidyverse")
library(tidyverse)#upload tidy packages





########################################################
#Generate floral abundance and density for pollination data
########################################################

#Separate date column in pollination into month, day, and year columns

pollination$dateMDY<-pollination$date
pollination<-separate(pollination, "date", c("month", "day", "year"), sep = "/")
#edit pollination data frame

pollination<-pollination[!is.na(pollination$ID),] #drop NA rows


###Unused in July 2016 draft
#Create flower abundance per site, per plot, per round, 

#
poll_n<-pollination%>%
  group_by(site,round,plot) %>%
  mutate(.,chafas_ab=sum(flowers))%>% #generate chafas abundance for plot
  mutate(.,chafas_dens=chafas_ab/area)%>% #generate chafas flowers per m^2 
  nest() #retains all other columns!
poll_n # a nested data frame
# examine data to check


poll_m<-poll_n%>%
  unnest()