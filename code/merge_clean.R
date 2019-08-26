### Merging and cleaning the Chamaecrista Data

###What this script does:

## 1. Imports relevant data sets
## 2. cleans and checksthe pollination treatment and seedset dataframes by:
####### dropping high density plots, 
## 3. merges them together


####
# Run this 1st; summary_results 2nd; models 3rd; plot code 4th!
###
#install.packages("tidyverse")
library(tidyverse)#upload tidy packages

###read in data from db
library(RMariaDB) 
#get user name and password
source("user.R")
source("psw.R")


#connect to DB

conn <- dbConnect(RMariaDB::MariaDB(), host = '160.94.186.138',  dbname='cham_poll', user = user, password = psw, port=8889)
dbListTables(conn)
### 5 tables--LCCMRsites is the full list of sites and landscape buffer measures from Ian
### we need cham2017_sites, cham2017_event, cham2017_fruit_seedset,cham2017_plant_treatment

pollination<- dbReadTable(conn, "cham2017_plant_treatment")
seed<- dbReadTable(conn, "cham2017_fruit_seedset")
landscape<- dbReadTable(conn, "cham2017_sites")
event<- dbReadTable(conn, "cham2017_event")


dbDisconnect(conn)

##local if db connection down
###read in data frames
#pollination<-read.csv("data/pollination_2017_pp_plant_treatment.csv")#pollination event data
#seed<-read.csv("data/pollination_2017_pp_fruit_seedset.csv")# seed count data
#landscape<-read.csv("data/pollination_2017_siteinfo.csv") #landscape data
#event<-read.csv("data/event.csv")# environmental data for visits


#drop notes column
pollination$notes<-NULL
seed$notes<-NULL
landscape$notes<-NULL
event$notes<-NULL
##


#drop the hi density plot observations that were conducted in rounds 1 & 2
poll<-pollination%>%filter(plot=="hi")

###############################
#Merging Tables into Dataframe

#ID check
#each ID should be listed no more than 4 times!
poll_check<-poll%>%group_by(ID)%>%summarize(n=n())
max(poll_check$n) #we're good!

### check for missing observations; each of the 8 sites needs 90 observations, except for henningson and nygard which will have 30
nrow(poll[which(poll$site=="braaten"),])
nrow(poll[which(poll$site=="grohn"),])
nrow(poll[which(poll$site=="howe"),])
nrow(poll[which(poll$site=="silis"),])
nrow(poll[which(poll$site=="staples"),])
nrow(poll[which(poll$site=="woltjer"),])
nrow(poll[which(poll$site=="nelson"),])
nrow(poll[which(poll$site=="rudningen"),])

#2 sites, henningson and nygard were dropped from the study, and will only have 30
nrow(poll[which(poll$site=="henningson"),])
nrow(poll[which(poll$site=="nygard"),])



#extract IDs
IDs<-poll%>%select(ID,round,orig_plant_position,plot)
####

#######
###seed checks
######

#merge seeds with IDs from pollination dataframe
seed_ID<-seed%>%left_join(IDs,by=c("round","orig_plant_position"))%>%filter(!is.na(plot))

#merge seed and pollination data by round, ID, and og.plantposition, and plot
#round and orig_plant_position are a unique identifier for the fruit count data
seed_poll<-right_join(poll,seed_ID, c("round","orig_plant_position","ID","plot"))


######################
nrow(seed_ID) ##check to make sure no seed observation rows were dropped--should be 549
nrow(seed_poll)### good


###merge landscape data by site
seed_land<-merge(seed_poll, landscape, "site")

nrow(seed_land) # check to make sure new df has the same number of observations as seed_poll
##drop NT rows from analysis, one nt plant had marked fruits

seed_land<-seed_land[!(seed_land$trmnt=="nt"),]
nrow(seed_land)
##drop NA OR 0 from seed counts; one fruit was too undeveloped to count seeds on
## if using locally stored data drop "#VALUE!" rather than 0
seed_land<-seed_land[!(seed_land$CHECK_row_sum==0),]
nrow(seed_land)

####
#Drop henningson and nygard; not enought data points
seed_land<-seed_land[!(seed_land$site=="nygard"),]
seed_land<-seed_land[!(seed_land$site=="henningson"),]
nrow(seed_land)
## drop hi density
seed_land<-seed_land%>%filter(plot=="hi")

nrow(seed_land)
#IF wanted you can drop extraneous data check columns by name


#######
nrow(seed_land)# 539


#####################################
##create pollinated_ovules  + poll_lim
###################################
#sum total seeds and aborted ovules to get total # pollinated ovules per fruit
seed_land$poll_ovules<-seed_land$total_seeds+seed_land$abrt_ovules
#subtract pollinated ovules per fruit from total ovules per fruit to get pollen limitation at fruit level


# ID should be categorical
seed_land$ID<-as.factor(seed_land$ID)
levels(seed_land$ID)
seed_land$round<-as.factor(seed_land$round)


##Write csv with merged and cleaned data , save to data folder in gitrepositry 

write.csv(seed_land, "data/seed_land.csv")
###
summary(seed_land)
