#Calculate area of counties
#LAST TRY - WASN"T WORKING BUT NOW IT IS AFTER UPDATE
# Some great mapping: http://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html

#add in minnesota relative to US map; then MN map.
#install.packages("maps")
install.packages('ggsn',dependencies=TRUE)
 #remove.packages('TMB')
library(TMB)
library(ggsn)
library(tidyverse)
library(maps)
library(ggsn)# for legend
library(ggplot2)


counties <- map_data("county")
state <- map_data("state")
usa<- map_data("usa")
sites<- read.csv("data/pollination_2017_siteinfo.csv")
site_points<-sites%>%select(site,latitude,longitude,prop.c)%>%
  rename(lat=latitude,long=longitude)%>%filter(site!="henningson"&site!="nygard")
#two mn county data are for different scope. In the second (smaller 16 county dataset for inset, should keep county with highest % of semi-natural to keep scale consistent

#Remove axes and background
ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)



###USA map
### dif colors
ggplot(data = state) + 
  geom_polygon(aes(x = long, y = lat, fill = region, group = group), color = "white") + 
  coord_fixed(1.3) +
  guides(fill=FALSE)+ ditch_the_axes 


 ggplot() + 
  geom_polygon(data = usa, aes(x=long, y = lat, group = group), fill = "gray", color = "black") + 
   borders("state", xlim = c(-91.52137, -91.521371), ylim = c( 48.8227,  48.8228),colour="black") + 
   coord_fixed(1.3)+#now add star for site ara
   geom_point(aes(x =-95.43029, y = 45.52295),inherit.aes = FALSE,shape= 18,size = 3)+
   ditch_the_axes 
 
####
map_dat <- read_csv("data/map_dat.csv")
source("code/mn_all_counties.R")
str(mn_counties)
mn_map <- ggplot(data = mn_counties, mapping = aes(x = long, y = lat, group = as.character(group))) + 
  coord_fixed(1.3) + 
  #geom_polygon(color = "black", fill = "white") + #state county lines
  theme_bw() + 
  borders("state", xlim = c(-91.52137, -91.521371), ylim = c( 48.8227,  48.8228),colour="black") + 
  ditch_the_axes + 
  #geom_point(data = site_points, aes(x = long, y = lat),inherit.aes = FALSE, size = 3)
  geom_point(aes(x =-96.0, y = 45.52295),inherit.aes = FALSE,shape= 18,size = 10)

##look at your nice map!
mn_map



###zoomed map
mn_map_zoom <- ggplot(data = mn_counties, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "white") + 
  theme_bw() + 
  borders("state", xlim = c(-91.52137, -91.52137), ylim = c( 48.8227,  48.8227)) + 
  ditch_the_axes + 
  geom_point(data = site_points, aes(x = long, y = lat),inherit.aes = FALSE, size = 3)+
  geom_text(data=site_points,aes(x=long, y=lat, label=prop.c),inherit.aes = FALSE,hjust=0.5, vjust=-0.6)+
  coord_cartesian(xlim=c(-96.8, -95), ylim=c(46, 45))


mn_map_zoom

### need scale bar?


###old dan code####
geom_point(data = map_dat, aes(x = long, y = lat, color=dan_mix,shape=ag),inherit.aes = FALSE, size =3)
 ############ 

# the borders command just limits the what states will show up. If the range is within a state, then it is displayed.
mn2<- mn_base +   + 


## below creates a plot with the counties of interest. mn_county2 is a list of the counties of interest. May need to create.
mn2 + 
  geom_polygon(data = mn_county2, color="black", fill = "grey")


#add groupings
#grouping of landcover types. Note the read_csv from tidyverse. also, mutate_each from tidyverse
groups <- read_csv("data/cdl_groups.csv")
groups <- mutate_all(groups, funs(tolower))
lcover <-read_csv("data/MNcounty_landcover.csv")
lcover <- mutate_all(lcover, funs(tolower))

total <- merge(lcover,groups,by.x="category", by.y="CDL", all.x=TRUE)

#The following two lines of code are to rename categories
mapvalues <- plyr::mapvalues # do this as there are naming conflicts with tidyverse
total$grouping <-mapvalues(total$grouping, from = "grass", to = "semi") #rename for semi-natural

#Now group and merge.
total$acreage <-as.numeric(total$acreage)
total$count <-as.numeric(total$count)

#total$grouping <-as.factor(total$grouping)
#broad category grouping
group_ac<-summarise(group_by(total, county, grouping), acreages=sum(acreage))
#narrow category grouping
group_acs<-summarise(group_by(total, county, category), acreages=sum(acreage))

#get total acres per county
total_ac<-summarise(group_by(total, county), acres=sum(acreage))
# add column for total acres to divide 
final <-merge(group_ac,total_ac,by="county", all.x=TRUE)
final$perc <- final$acreages/final$acres

#summarise(group_by(final, county), check1=sum(perc))
#head(final)
#now merge with polygon map data
mn_type <- merge(mn_county, final, by.x = "subregion", by.y ="county", all.x=TRUE)
#cate here helps with naming in the title. Good if you are making a lot of figures. 'cate' adds it to the title

cate= "ag"
#subset to make map based on percentages for a given landcover typoe
mn_cat <- subset(mn_type, grouping == cate)

mn_cat %>%
  select(subregion, perc) %>%
  group_by(subregion) %>%
  summarise(percent=max(perc)) %>%
  arrange(desc(percent))


title <- paste("Percentage of ", cate, " Per County", sep="") #sep is due to annoying space in categories
subheader <- paste("(data from USDA 2016 Cropland Data Layer)")
mn2 + 
  geom_polygon(data = mn_cat, aes(fill = perc), color = FALSE) +
  scale_fill_gradient(low = "white", high = "black") +
  ggtitle(paste0(title,"\n",subheader)) +
  theme(plot.title = element_text(hjust = 0.5)) 


############################################
############################################
##### Now do the second map ################
############################################
############################################

# Zoomed in area. Currently, need to rerun above so that it gives new data for the 16 counties (+max/min)

#First Find the Max/Min counties to keep legend the same 
min_c<-mn_cat[which(mn_cat$perc == min(mn_cat$perc)), ][1,1]
max_c<-mn_cat[which(mn_cat$perc == max(mn_cat$perc)), ][1,1]

#subset to make map based on percentages for a given landcover type
#now recreate dataframe with new counties (run mn_county2 in county_subset)
mn_type2 <- merge(mn_county2, final, by.x = "subregion", by.y ="county", all.x=TRUE)
mn_cat2 <- subset(mn_type2, grouping == cate)
#Second make the map
mn_base2 <- ggplot(data = mn_county2, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "grey")

mn1<-mn_base2 + theme_bw() + ditch_the_axes
mn1 + 
  geom_polygon(data = mn_cat2, aes(fill = perc), color = FALSE) +
  scale_fill_gradient(low = "white", high = "black") +
  ggtitle(paste0(title,"\n",subheader)) +
  theme(plot.title = element_text(hjust = 0.5)) 

#Pivot Table
unc0 <- unique(mn_county2$subregion) #this subsets the data in the mn_county. Need to also remove max county
unc1<-unc0[unc0 !=min_c]
unc<-unc1[unc1 !=max_c]


final1 <- final[final$county %in% unc,]
final1 <-merge(group_ac,total_ac,by="county", all.x=TRUE)
final1$perc_group <- final1$acreages/final1$acres
final1 <- final1[final1$county %in% unc,]
final1

final2 <-merge(group_acs,total_ac,by="county", all.x=TRUE)
final2$perc_cat <- final2$acreages/final2$acres
final2 <- final2[final2$county %in% unc,]

# can use this to check that percentages add to one
#summarise(group_by(final, county), check1=sum(perc))

final1 %>%
  group_by(grouping) %>%
  summarise(percentage = mean(perc_group)) %>%
  arrange(desc(percentage))

final2 %>%
  group_by(category) %>%
  summarise(percentage = 100*(mean(perc_cat))) %>%
  arrange(desc(percentage)) %>%
  print(n=40)

total %>%
  group_by(category, value) %>%
  summarise(percentage = 100*(mean(acreage))) %>%
  arrange(value)

