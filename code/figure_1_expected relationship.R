require(ggplot2)
install.packages("directlabels")
library(directlabels)
#create desired function to plot
hand_poll<- function(x) {x+ 5}
OPen_poll<- function(x) {-2.5*x+5}
hand_poll(c(5,4,3,2,1))#test
#or simulate data
# A different data set
df1 <- data.frame(
  treatment= factor(c("HP","HP","HP","HP","HP", "OP", "OP","OP","OP","OP")),
  landscape = c(.1,.2,.3,.4,.5,.1,.2,.3,.4,.5),
  seedset = c(5,5,5,5,5, 5,4,3,2,1))
df1


#create legend
test.labels<-c("Hand Pollinated","Open Pollinated")
#create plot
#f <- ggplot(data.frame(x = c(0,0.2,0.4,0.6,0.8,1.0), y=c(0,1,2,3,4,5)), aes(x))
f<- ggplot(data=df1, aes(landscape, seedset, group=treatment, shape=treatment))
#f + stat_function(fun = HP.density, size=2) +stat_function(fun=OP.density, size=1.5, linetype=2)+ 
f+ geom_line(aes(linetype=treatment), size=1.5)+
  xlab("% Agriculture")+
  ylab("Seed Set") + xlim(0.1,.6) + ylim(0.5,7)+
  theme_bw() + 
  #Change legend title
  scale_linetype_discrete(name  ="Treatment",
                          breaks=c("OP","HP"),
                          labels=c("OP", "HP"))+
  #set axis limits 
  
  #Change legend size
  theme(legend.title = element_text( size=20, face="bold"))+
  
  # change legend text size
  theme(legend.text = element_text( size = 18, face = "bold"))+
  #flip legend order; aesthetic varies w/ plot type (fill, linetype, color, etc)
  guides(linetype= guide_legend(reverse=TRUE))+
  #make legend guides bigger
  theme(legend.key.width = unit(2.5, "cm"))+
  #remove legend box
  theme(legend.key = element_rect(fill = NA, colour = NA, size = 0.25))+
  
  #legend.text.align
  #(legend.text.align=2)+
  #remove background and gridlines
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() )+
  #bolden axis
  theme(axis.line.x = element_line(color="black", size = 1.5),
        axis.line.y = element_line(color="black", size = 1.5))+
  #Text size
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20,face="bold"))+
  #remove tick marks
  theme( axis.ticks.y= element_blank(),axis.text.y = element_blank())+
  theme( axis.ticks.x= element_blank(),axis.text.x = element_blank())+
  #manipulate position of plot margine
  theme(plot.margin=unit(c(1.5,1.5,1.5,1.5),"cm"))+
  #move axis titles
  theme(axis.title.y=element_text(margin=margin(0,40,0,0)))+
  theme(axis.title.x=element_text(margin=margin(40,0,0,0)))

