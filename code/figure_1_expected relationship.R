require(ggplot2)
#load tidyverse
library(tidyverse)

###figure 1: hypothesized PL- lansdcape relationship

#create desired function to plot
hand_poll<- function(x) {x+ 5}
open_poll<- function(x) {-2.5*x+5}
hand_poll(c(5,4,3,2,1))#test
#or simulate data
# A different data set
df1 <- data.frame(
  Treatment= factor(c("Supplemental Pollination","Supplemental Pollination","Supplemental Pollination","Supplemental Pollination","Supplemental Pollination", "Open Pollination", "Open Pollination","Open Pollination","Open Pollination","Open Pollination")),
  landscape = c("20%","30%","40%","50%","60%","20%","30%","40%","50%","60%"),
  seedset = c(5,5,5,5,5, 5,4,3,2,1))
df1


#create legend
test.labels<-c("Supplemental Pollinated","Open Pollinated")
#create plot
#f <- ggplot(data.frame(x = c(0,0.2,0.4,0.6,0.8,1.0), y=c(0,1,2,3,4,5)), aes(x))
f<- ggplot(data=df1, aes(landscape, seedset, group=Treatment, shape=Treatment))
#f + stat_function(fun = HP.density, size=2) +stat_function(fun=Open Pollination.density, size=1.5, linetype=2)+ 
f+ geom_line(aes(linetype=Treatment), size=1.5)+
  xlab("% Agriculture Surrounding Restoration")+
  ylab("Seed Set")  + ylim(0.5,7)+ ggtitle("Fig.1: Predicted Relationship Between Seed Set and \n Surrounding Agricultural Land-Use")+
  theme_bw() +
  theme(axis.text=element_text(size=14,color="black"),
        axis.title=element_text(size=16))+ # change legend text size
  theme(legend.text = element_text( size = 16,color="black"),
                                    legend.title=element_text(size=20),
        title=element_text(size=18))

#### figure S1 -- gradient

#load in seed data

seed_land<-read.csv("data/seed_land.csv")
seed_land <-filter(seed_land, plot=="hi")

seed_land$round<-as.factor(seed_land$round)

###plot of ag gradient and sites
seed_land$site<-recode(seed_land$site,"braaten"="Site 1", "nelson"="Site 2", "rudningen"="Site 3","howe"="Site 4",
                "woltjer"="Site 5", "staples"="Site 6","silis"="Site 7","grohn"="Site 8")

seed_land%>%ggplot(aes(fct_reorder(site, prop.c),prop.c))+geom_point()+
  labs(x="Site" ,y="% Agriculture")+
  ggtitle("Fig. 2: Study System Agricultural Land-use Gradient")+theme_bw()+
  theme(axis.text=element_text(size=14,color="black"),
        axis.title=element_text(size=16),title=element_text(size=18)) # change legend text size
 
###figure s1-- US & site map

 
  
  
  
