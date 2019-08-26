# add IDs and plot IDs to seed data
### the plant unique IDs were originally added to the 

library(tidyverse)#upload tidy packages

###read in data from db
library(RMariaDB) 
#get user name and password
source("user.R")
source("psw.R")


#connect to DB

conn <- dbConnect(RMariaDB::MariaDB(), host = '160.94.186.138',  dbname='fragbees', user = user, password = psw, port=8889)
dbListTables(conn)
### put dataframes here!


dbDisconnect(conn)

###read in data frames
pollination<-read.csv("data/pollination_2017_pp_plant_trait.csv")#pollination event data
seed<-read.csv("data/pollination_2017_pp_fruit_seedset.csv")# seed count data
# also contains a measure of time between treatement days 


###############################
#Merging Tables into Dataframe

#ID check
#each ID should be listed no more than 4 times!
poll_check<-pollination%>%group_by(ID)%>%summarize(n=n())
max(poll_check$n) #we're good!

#extract IDs
IDs<-poll%>%select(ID,round,orig_plant_position,plot)
####

####
#to do: 7/17: add in IDs to the seed data to make things more straightforward.

#######
###seed checks
######

#merge seeds with IDs from pollination dataframe
seed_ID<-seed%>%left_join(IDs,by=c("round","orig_plant_position"))
## check to make sure nothing got goofed up:

#this line checks for non unique orig_plant_positionxRound combinations 
#in the seed_ID dataframe
# if more than one plant_position ID (ID#) or prev plant position ID (na or ID#) 
#is represented by a orig_plantxRound combo then we messed up something

seed_check<-seed_ID%>%
  group_by(ID,orig_plant_position,round)%>%
  summarise(prev_n=n_distinct(prev_plant_position),present_n=n_distinct(plant_position))
max(seed_check$prev_n)
max(seed_check$present_n)
# looks good max is one for both columns

#make sure no ID in the seed data frame is represented more than 4 times
#this would indicate that our IDs were missassigned
seed_check2<-seed_check%>%group_by(ID)%>%summarize(n=n())
max(seed_check2$n) #we're good!


#this is an alternative means for checking the data for nonsense values or combinations
#first clean poll
poll$prev_plant_position<-as.character(poll$prev_plant_position)
#change factor level from NA to "na" for merge

#now 
poll_check<-poll%>%
  group_by(orig_plant_position,round,prev_plant_position,plant_position)%>%
  summarise()


#now compare to seed_check using anti_join
seed_poll_check<-anti_join(seed_check,poll_check)
# this should be 0 like the previous anti_join check

#now drop rows not needed for further analysis
seed_ID_drop<-
