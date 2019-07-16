### Merging Chamaecrista Data

#install.packages("tidyverse")
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
landscape<-read.csv("data/pollination_2017_siteinfo.csv") #landscape data
event<-read.csv("data/event.csv")# environmental data for visits
  # also contains a measure of time between treatement days 



#Separate date column in pollination into month, day, and year columns

pollination$dateMDY<-pollination$date
poll<-separate(pollination, "date", c("month", "day", "year"), sep = "/")


###############################
#Merging Tables into Dataframe
###############################
## merge event and pollination by date and site, and remove low density plots
poll_site<-merge(poll, event, c("month", "day", "year", "site","round"))%>%filter(plot=="hi")


#ID check
#each ID should be listed no more than 4 times!
poll_check<-poll_site%>%group_by(ID)%>%summarize(n=n())
max(poll_check$n) #we're good!

### check for missing observations; each of the 8 sites needs 60 observations, except for henningson and nygard which will have 30
nrow(poll_site[which(poll_site$site=="braaten"),])
nrow(poll_site[which(poll_site$site=="grohn"),])
nrow(poll_site[which(poll_site$site=="howe"),])
nrow(poll_site[which(poll_site$site=="silis"),])
nrow(poll_site[which(poll_site$site=="staples"),])
nrow(poll_site[which(poll_site$site=="woltjer"),])
nrow(poll_site[which(poll_site$site=="nelson"),])
nrow(poll_site[which(poll_site$site=="rudningen"),])

#2 sites, henningson and nygard were dropped from the study, and will only have 15
nrow(poll_site[which(poll_site$site=="henningson"),])
nrow(poll_site[which(poll_site$site=="nygard"),])


#extract IDs
IDs<-poll_site%>%select(ID,round,orig_plant_position,plot)
####

####
#to do: 7/17: add in IDs to the seed data to make things more straightforward.

#######
###seed checks
######
# due to herbivory, plants were moved around to accomodate loss. 
# therefore, some plants were replaced with new plants
#or plants were moved from the low density plot to the high density plot between rounds 2&3
# to keep track of this, I had to assign unique IDs to all plants in the pollination data
# before this I used a plants position at a site as its ID
# if a plant was moved from its original location or replaced, it was given a second tag with this information
# thus when seed data was colelcted, the ID consisted of only the plants "current","former", and "original"position
###the metadata contain a synopsis of what these variables mean
# but for the data, it
#a plants original plant position (orig_plant_position) and the fruit treatment round (round) link it to the pollination data
# no original position and round should be repeated, while a plants current position could have  These are linked to true unique IDs.
#

seed_ID<-seed%>%left_join(IDs,by=c("round","orig_plant_position"))

#check if seed has any round/original plant position combos not included in IDs; 
seed_ID_check<-seed%>%anti_join(IDs,by=c("round","orig_plant_position"))


seed_check<-seed_ID%>%
  group_by(ID,orig_plant_position,round)%>%
  summarise(prev_n=n_distinct(prev_plant_position),present_n=n_distinct(plant_position))

#make sure no ID in the seed data frame is represented more than 4 times
#this would indicate that our IDs were missassigned
seed_check2<-seed_check%>%group_by(ID)%>%summarize(n=n())
max(seed_check2$n) #we're good!

# the st 23 og plant position (plant 203)occupied position st 23 rds 1-2 and then st 14 rds 3-4; ID=203 
# plant originally in st 14 was nt
# the st 25 og plant position (plant 205) occupied position st 25 rds 1-2 and then st 10 rds 3-4; 
# plant originally in st 10 was nt
poll_site$prev_plant_position<-as.character(poll_site$prev_plant_position)
poll_site$prev_plant_position[is.na(poll_site$prev_plant_position)] <- "na"

poll_check<-poll_site%>%
group_by(orig_plant_position,round,prev_plant_position,plant_position)%>%
  summarise()

#check and see if there are any seed data not in the plant data
seed_poll_check<-anti_join(seed_check,poll_check)


#merge seed and pollination data by round and og.plantposition 
#round and orig_plant_position are a unique identifier for the fruit count data
seed_poll<-right_join(poll_site,seed, c("round","orig_plant_position"))

#run anti_join to confirm that all round/orig_plant_position combinations
#contained in seed are contained in poll_site
seed_poll2<-anti_join(seed,poll_site,c("round","orig_plant_position"))%>%
  group_by(orig_plant_position,round,prev_plant_position,plant_position)%>%summarize()
# all observations in poll_site


nrow(seed) ##check to make sure no seed observation rows were dropped
nrow(seed_poll)### good

### do some joining to figure out if any ID stuff got miss-assigned
seed_ID<-seed_poll%>%filter(plot=="hi")%>%
  select(ID,plant_position.x,plant_position.y,orig_plant_position,prev_plant_position.x,prev_plant_position.y,round)

######
### compare newly created data frames with seed_land to make sure data wasn't lost.
############################################


###merge landscape data by site
seed_land<-merge(seed_poll, landscape, "site")


nrow(seed_land) # check to make sure new df has the same number of observations as seed_poll
##drop NT rows from analysis, one nt plant had marked fruits

seed_land<-seed_land[!(seed_land$trmnt=="nt"),]
##drop NA from seed counts; one fruit was too undeveloped to count seeds on
seed_land<-seed_land[!(seed_land$CHECK_row_sum=="#VALUE!"),]

seed_land<-transform(seed_land, round=as.factor(round))## make round a factor

####
#Drop henningson and nygard; not enought data points
seed_land<-seed_land[!(seed_land$site=="nygard"),]
seed_land<-seed_land[!(seed_land$site=="henningson"),]
seed_land<-seed_land%>%filter(plot=="hi")

#Drop extraneous data check columns by name
ncol(seed_land)# check number columns pre drop
drops <- c("eventID.x", "CHECK.total.ovules","CHECK.row.sum", "Ratio.CHECK.Total.Total","Ratio..row.sum.total")
seed_land<-seed_land[ , !(names(seed_land) %in% drops)]
ncol(seed_land) # should drop 5 columns

#######
nrow(seed_land)


#####################################
##create pollinated_ovules  + poll_lim
###################################
#sum total seeds and aborted ovules to get total # pollinated ovules per fruit
seed_land$poll_ovules<-seed_land$total.seeds+seed_land$abrt.ovules
#subtract pollinated ovules per fruit from total ovules per fruit to get pollen limitation at fruit level
seed_land$poll_lim<-seed_land$total.ovules-seed_land$poll_ovules

# mean should be the same between new column and what we entered
mean(seed_land$poll_lim)
mean(seed_land$total.ovules-seed_land$poll_ovules)

# ID should be categorical
seed_land$ID<-as.factor(seed_land$ID)
levels(seed_land$ID)
seed_land$round<-as.factor(seed_land$round)
## create visit day of year column by duplicating from date as a check

seed_land$yday= seed_land$dateMDY

library(lubridate) # Load lubridate to change date to day of year


### Test code to see how lubridate functions mdy and yday works
y = mdy('08/12/2017')
yday(y)
x = mdy('08/27/2017')
yday(x)

###run day_treated through mdy() and yday()
x=mdy(seed_land$yday)
seed_land$yday= yday(x)
seed_land$yday

# We can verify this worked by comparing the year day from day_treated to the date column
y = mdy('08/12/2017')### this should be 224 according to the dataframe
yday(y)
## yep!

### fecundity
### let's create a variable that measures reproductive investment at each visit
## wither_flow+frt_removed
seed_land$fecund<-seed_land$wither_flow+seed_land$frt_removed


##Write csv with merged and cleaned data , save to data folder in gitrepositry 

write.csv(seed_land, "data/seed_land.csv")
###

