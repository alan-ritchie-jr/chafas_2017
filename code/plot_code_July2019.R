require(ggplot2)
#load tidyverse
library(tidyverse)
library(sjPlot)
install.packages("dichromat")
library(dichromat)
###figure 1: hypothesized PL- lansdcape relationship

#create desired function to plot
hand_poll<- function(x) {x+ 5}
open_poll<- function(x) {-2.5*x+5}
hand_poll(c(5,4,3,2,1))#test
#or simulate data
# A different data set
df1 <- data.frame(
  Treatment= factor(c("Supplemental Pollination","Supplemental Pollination","Supplemental Pollination","Supplemental Pollination","Supplemental Pollination", "Ambient Pollination", "Ambient Pollination","Ambient Pollination","Ambient Pollination","Ambient Pollination")),
  landscape = c("20%","30%","40%","50%","60%","20%","30%","40%","50%","60%"),
  seedset = c(5,5,5,5,5, 5,4,3,2,1))
df1

df1$linetype= ifelse(df1$Treatment=="Supplemental Pollination", "solid", "dashed")
#create legend
test.labels<-c("Supplemental Pollinated","Open Pollinated")
#create plot
#f <- ggplot(data.frame(x = c(0,0.2,0.4,0.6,0.8,1.0), y=c(0,1,2,3,4,5)), aes(x))
ggplot(data=df1, aes(landscape, seedset, group=Treatment))+
  geom_line(aes(linetype=Treatment),size=2)+
  xlab("% Agriculture Surrounding Restoration")+
  ylab("Seed Set")  + ylim(0.5,7)+ ggtitle("Fig.1: Hypothesized Relationship Between Seed Set and \n Surrounding Agricultural Land Use")+
  theme_bw() +
  scale_linetype_manual(values=c("dashed", "solid"))+
  theme(axis.text=element_text(size=34,color="black"),
        axis.line = element_line(colour = "black", size=2),
        axis.title=element_text(size=38),
        title=element_text(size=40),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),legend.position = "none")

 #### figure S1 -- gradient

#load in seed data

seed_land<-read.csv("data/seed_land.csv")
seed_land <-filter(seed_land, plot=="hi")

seed_land$round<-as.factor(seed_land$round)

###plot of ag gradient and sites
seed_land$site<-recode(seed_land$site,"braaten"="Site 1", "nelson"="Site 2", "rudningen"="Site 3","howe"="Site 4",
                "woltjer"="Site 5", "staples"="Site 6","silis"="Site 7","grohn"="Site 8")

seed_land%>%ggplot(aes(fct_reorder(site, prop.c),prop.c))+geom_point(size=8)+
  labs(x="Site" ,y="% Agriculture")+
  ggtitle("Study System Agricultural Land Use Gradient")+theme_bw()+
  theme(axis.text=element_text(size=36,color="black"),
        axis.title=element_text(size=38),title=element_text(size=38),
        axis.line = element_line(colour = "black"),panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),legend.position = "none") # change legend text size
 ####################
###figure s1-- US & site map
#load map package
library(maps)
library(ggsn)

##load in data
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
  panel.grid = element_blank(),
  axis.title = element_blank()
)
#for USA map
ditch_the_axes2 <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  panel.border = element_blank(),
  axis.ticks = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)



###USA wide map--using this one

ggplot() + 
  geom_polygon(data = usa, aes(x=long, y = lat, group = group),fill="gray", color = "black") + 
  borders("state", xlim = c(-91.52137, -91.521371), ylim = c( 48.8227,  48.8228),colour="black") + 
  coord_fixed(1.3)+#now add star for site ara
  geom_point(aes(x =-95.43029, y = 45.52295),inherit.aes = FALSE,shape= 18,size = 3)+
  theme_bw()+ditch_the_axes2

####state map, not using this one.
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
  geom_point(data = site_points, aes(x = long, y = lat),inherit.aes = FALSE, size = 5)+
  coord_cartesian(xlim=c(-96.8, -95), ylim=c(46, 45))

### zoom map
mn_map_zoom

############################
#Fig 2: LMM predicted response vs raw data

##########################

####note!!!: model object is generated in model script! Run it first!

#########################
#Plotting predicted effect and actual data
################

#Used sjPlot package and ggeffects
#use get_model_data to predict fitted values for hp and op at each level of % ag (prop.c)
#conditioned on either fixed or random effects
#fixed effects have narrower CIs
#Info on how this works:
#https://www.rdocumentation.org/packages/ggeffects/versions/0.8.0/topics/ggaverage

### get_model_data + ggplot version
theme_set(theme_bw())
theme_set(theme_classic())
#run get_model_data to extract ggplot usable output
s<-get_model_data(seed_mean_mod_TMB,ci.lvl= .95, type="pred",terms=c("prop.c","trmnt"), 
                  pred.type="re") 
s<-s%>%mutate(group = fct_recode(group, 
                                 "Supplemental Pollination" = "SP","Open Pollination"="OP"))
### make separate dataframes for CI for plot
shp<-s%>%filter(group=="Supplemental Pollination")
sop<-s%>%filter(group=="Open Pollination")

###make plot for seed model
ggplot(data=s, aes(x, predicted),linetype=group)+
  geom_line(aes(linetype=group),size=3)+
  geom_point(data=plt_nr,aes(prop.c, (seeds/frt),shape=trmnt),position="jitter", inherit.aes = FALSE,size=7)+
  geom_ribbon(data=shp,aes(x=x,ymin=conf.low, ymax=conf.high),alpha = 0.3,inherit.aes = FALSE)+
  geom_ribbon(data=sop,aes(x=x,ymin=conf.low, ymax=conf.high),alpha = 0.3,inherit.aes = FALSE)+
  labs(shape="Treatment",linetype="Predicted Response")+
  xlab("% Agriculture Surrounding Restoration")+ylab("Mean Seeds per Treated Fruit")+
  ggtitle("Fig. 2: Effect of Surrounding Agriculture on Seed Set",
          subtitle=" LMM Predicted Response vs. Data")+
  theme(axis.text=element_text(size=34,color="black"),
        axis.title=element_text(size=38),
        title=element_text(size=40),
        legend.text=element_text(size=38),
        axis.line = element_line(colour = "black",size=2),panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),legend.position = "none")


###
###Fig 3: 
#mean seeds produced per fruits collected for a plant 
seed_land%>% 
  mutate(trmnt = fct_recode(trmnt, "Supplemental Pollination" = "hp", "Open Pollination"="op"))%>%
  group_by(trmnt, ID)%>%
  summarize(mean_seed=mean(total.seeds))%>%
  ggplot(aes(trmnt ,mean_seed))+geom_boxplot(lwd=2,outlier.size=5)+ theme_bw()+
  labs(x="Treatment" ,y="Mean Seeds per Treated Fruit")+
  ggtitle("Fig. 3: Mean Seed Set per Treatment")+
  theme(axis.text=element_text(size=38,color="black"),
        axis.title=element_text(size=40),title=element_text(size=40),
        axis.line = element_line(colour = "black", size=2),panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),legend.position = "none")


