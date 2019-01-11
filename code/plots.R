
##################
##Pivot Tables###
################

#Upload seed_land df
seed_land<-read.csv("data/seed_land.csv")
pollination<-read.csv("data/pollination_2017_pp_plant_trait.csv")



##Look at differences in proportion of ovules filled among treatments
ggplot(seed_land,aes(trmnt,((poll_ovules)/total.ovules)))+geom_jitter(aes(color=trmnt)) +
  facet_grid(round~ plot)+ ylab("Pollinated Ovules/Total Ovules")+
  ggtitle("Proportion Ovules Pollinated by Treatment") # add facets for round and plot type


##Compare # pollinated ovules between treatments across rounds
ggplot(seed_land,aes(trmnt,(poll_ovules)))+geom_jitter(aes(color=trmnt)) +
  facet_grid(round~ plot)+ ggtitle("Pollinated Ovules per Treatment")# add facets for round and plot type

##compare ratio of pollinated to total ovules by plot
ggplot(seed_land,aes(trmnt,((poll_ovules)/total.ovules)))+geom_jitter(aes(color=trmnt)) +
  facet_grid(.~ plot)+ ylab("Pollinated Ovules/Total Ovules")+ 
  ggtitle("Proportion Ovules Pollinated by Plot Type")# add facets for density treatment

### site###
###Compare # pollinated ovules between treatments by site ordered by prop.c
ggplot(seed_land,aes(trmnt,(poll_ovules)))+geom_jitter(aes(color=trmnt)) +
  facet_grid(.~ reorder(site,prop.c))+ ylab("# Pollinated Ovules")+ ## 
  ggtitle( "Pollinated Ovules per Treatment and Site")# add facets for density treatment

### Compare #pollen limitation between treatments and sites
ggplot(seed_land,aes(trmnt,poll_lim))+geom_jitter(aes(color=trmnt)) +
  facet_grid(.~ reorder(site,prop.c))+ ylab("Degree of Pollen limitation")+ ## 
  ggtitle( "Fruit level Pollen Limitation by Site")# add facets for density treatment

### prop.c and fertilization
ggplot(seed_land,aes(prop.c,(poll_lim)))+geom_jitter(aes(color=site)) +
  facet_grid(.~ trmnt) +  xlab("% Developed")+ylab("Degree of Pollen limitation")+ ## 
  ggtitle( "Fruit level Pollen Limitation by % Developed") 

ggplot(seed_land,aes(prop.c,(poll_lim)))+geom_jitter(aes(color=trmnt)) +
  facet_grid(.~ trmnt)

ggplot(seed_land,aes(prop.c,(poll_lim)))+geom_jitter(aes(color=trmnt)) +
  facet_grid(.~round)


### Density plot####
ggplot(seed_land,aes(poll_ovules, color=trmnt))+geom_density()+ xlab("Pollinated Ovules")

ggplot(seed_land,aes(poll_lim, color=trmnt))+geom_density()+ xlab("Degree of Pollen Limitation") +ylab("Frequency of Observation")
## Differences in round####

###
ggplot(seed_land,aes(poll_lim, color=trmnt))+geom_density()+
  facet_grid(.~round) + xlab("Degree of Pollen limitation") + ggtitle("Pollen limitation by % Developed and Round")


### Violin plots####


### Violin plots of fruit level pollen limitation by treatment and Round 
ggplot(seed_land,aes(trmnt,poll_lim))+geom_violin(aes(color=trmnt), draw_quantiles = .5)+ 
  facet_grid(.~round) + xlab("Round") + ylab("Degree of Pollen Limitation")+
  ggtitle("Distribution of fruit level pollen limitation by Round")

### Violin plot of fruit level pollinated ovules by treatment and round
ggplot(seed_land,aes(trmnt,poll_ovules))+geom_violin(aes(color=trmnt), draw_quantiles = .5)+ 
  facet_grid(.~round) + xlab("Round") + ylab("Degree of Pollen Limitation")+
  ggtitle("Fruit level pollen limitation by Round")

### Violin plots of fruit level pollen limitation by treatment and site
ggplot(seed_land,aes(trmnt,poll_lim))+geom_violin(aes(color=trmnt), draw_quantiles = .5)+ 
  facet_grid(.~reorder(site,prop.c)) + xlab("Site, Ordered by % Develop") + ylab("Degree of Pollen Limitation")+
  ggtitle("Fruit level pollen limitation by Site")



##################################
###Correlation between variables##
##################################
## one of the variables that should be numeric is not

is.numeric(seed_land$filled.seeds)##Checked and found that this variable isn't numeric

##Convert column to numeric
seed_land<-transform(seed_land, filled.seeds=as.numeric(filled.seeds))
seed_land$poll_ovules
seed_data<-seed_land[,24:29]### extract seed data from main dataframe

seed_cor<-cor(seed_data) ###generate correlation matrix

cor.test(seed_land$poll_ovules,seed_land$virgin.ovules) ###cor.test between poll_ovules and virgin.ovules.
### results indciate that these variables are strongly negatively correlated (pearson's corr of ~-.789
## So plants that had a high number of pollinated ovules had a low number of virgin ovules, and vice versa
## This means I may be able to use count data without having to account for a variable number of ovules per fruit
##poisson or NB distribution might be acceptable

