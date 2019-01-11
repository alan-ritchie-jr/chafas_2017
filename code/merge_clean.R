
###Upload data

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


# New column for area of plot
pollination$area<-ifelse(pollination$plot== "hi", 6.93, 692.82)
pollination$area

### Create flower abundance per site, per plot, per round, 


poll_n<-pollination%>%
  group_by(site,round,plot) %>%
  mutate(.,chafas_ab=sum(flowers))%>% #generate chafas abundance for plot
  mutate(.,chafas_dens=chafas_ab/area)%>% #generate chafas flowers per m^2 
  nest() #retains all other columns!
poll_n # a nested data frame
# examine data to check

         
poll_m<-poll_n%>%
  unnest()
         
###############################
#Merging Tables into Dataframe
###############################
## merge event and pollination by date and site
poll_m<-merge(poll_m, event, c("month", "day", "year", "site","round"))

nrow(poll_m)
### check for missing observations; each site needs 90 observations, except for henningson and nygard which will have 30
nrow(poll_m[which(poll_m$site=="braaten"),])
nrow(poll_m[which(poll_m$site=="grohn"),])
nrow(poll_m[which(poll_m$site=="howe"),])
nrow(poll_m[which(poll_m$site=="silis"),])
nrow(poll_m[which(poll_m$site=="staples"),])
nrow(poll_m[which(poll_m$site=="woltjer"),])
nrow(poll_m[which(poll_m$site=="nelson"),])
nrow(poll_m[which(poll_m$site=="rudningen"),])
nrow(poll_m[which(poll_m$site=="henningson"),])
nrow(poll_m[which(poll_m$site=="nygard"),])
### check event ID
nrow(poll_m[which(poll_m$eventID.y==100001),]) ### 30 plants should have this event ID

### create new dataframe to clean and export for sql called poll_o
poll_o<-poll_m
### drop columns event environment columns
poll_o<-poll_o[,-25:-36]
### drop redundant mmddyyy column & area calc columns
poll_o<-poll_o[,-20:-23]
### drop date, site, round, which are contained in event ID
poll_o<-poll_o[,-1:-5]
poll_o$prevplantID<-NULL
poll_o$position<-NULL
### rename plantID to position; it reflects plants position at sampling event, not actual unique ID
names(poll_o)[names(poll_o) == 'eventID.y'] <- 'eventID'
names(poll_o)[names(poll_o) == 'plantID'] <- 'plantposition'
names(poll_o)[names(poll_o) == 'ID'] <- 'uniqueID'

poll_o$eventID<-as.factor(poll_o$eventID)
poll_o$uniqueID<-as.factor(poll_o$uniqueID)

### create unique ID based on eventID and uniqueID
poll_o<-poll_o%>% arrange(eventID,uniqueID)%>%
   tibble::rowid_to_column("pollID2")
poll_o$prevplantID<-NULL
### add
 poll_o$pollID2<- poll_o$pollID2+20000
 
### check that primary key has all unique values

nrow(poll_o)


### write cleaned csv to upload to sql database
write.csv(poll_o, "output/poll_db.csv")


#merge seed and pollination data by round and og.plantposition to associate fruit data with plant unique ID
seed_poll<-merge(poll_m,seed, c("round","og.plantposition"))

nrow(seed) ##check to make sure no rows were dropped
nrow(seed_poll)### good


# If nrow turns up different numbers-->
##Check for mismatched nrow by creating second data frame with all.y = TRUE in merge , and locating discrepencies
#seed_poll1[which(seed_poll1$round=="1"),]
#seed_poll[which(seed_poll$round=="1"),]
#seed_poll1[which(seed_poll1$round=="2"),]
#seed_poll[which(seed_poll$round=="2"),]
#seed_poll1[which(seed_poll1$round=="3"),]
#seed_poll[which(seed_poll$round=="3"),]##missing 1 observation
#seed_poll1[which(seed_poll1$round=="4"),]
#seed_poll[which(seed_poll$round=="4"),]##missing 2 observations

### Now let's edit seed_poll into a usable dataframe for the database
### we'll only want to retain pollID and eventID

seed_p<-seed_poll
ncol(seed_p)# check number columns pre drop
###drop all columns except fruitID, pollID, collection MDY (year.y, etc.), and the seed data
### all event and plant info is contained in the other IDs.
colnames(seed_p)
###rename the collection date columns because you want that info, yo

names(seed_p)[names(seed_p) == 'year.y'] <- 'coll.year'
names(seed_p)[names(seed_p) == 'month.y'] <- 'coll.month'
names(seed_p)[names(seed_p) == 'day.y'] <- 'coll.day'
names(seed_p)[names(seed_p)== 'observer.y']<-'seed.counter'
colnames(seed_p)
drops <- c(,"type" ,"AM.PM", "start.time", "end.time",              
           "temp.start"," temp.end","windstart.mean", "windend.mean" ,          
          "windstart.max", "windend.max", "sky", "notes.y",
  "eventID.x", "CHECK.total.ovules","CHECK.row.sum", "Ratio.CHECK.Total.Total","Ratio..row.sum.total")
seed_land<-seed_land[ , !(names(seed_land) %in% drops)]
ncol(seed_p)



######
### compare newly created data frames with seed _land to make sure data wasn't lost.
############################################


###merge landscape data by site
seed_land<-merge(seed_poll, landscape, "site")


nrow(seed_land) # check to make sure new df has the same number of observations as seed_poll
##drop NT rows from analysis, one nt plant had marked fruits

seed_land<-seed_land[!(seed_land$trmnt=="nt"),]
##drop NA from seed counts; one fruit was too undeveloped to count seeds on
seed_land<-seed_land[!(seed_land$CHECK.row.sum=="#VALUE!"),]

seed_land<-transform(seed_land, round=as.factor(round))## make round a factor
##
###drop round 4
#seed_land<-seed_land[!(seed_land$round=="4"),]


####
#Drop henningson and nygard; not enought data points
seed_land<-seed_land[!(seed_land$site=="nygard"),]
seed_land<-seed_land[!(seed_land$site=="henningson"),]


#Drop

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

