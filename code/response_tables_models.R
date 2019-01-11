

#########################################
#########################################


#######
### Plant Level Response Variables==> generate tables and pass these tables to models
####

#Plant responses: fruit level response averaged at plant level
#seeds--> avg Seeds per fruit
#polovs--> avg Ovules per fruit
#pr--> avg pollinated ovule ratio
# sr--> avg seed to ovule # ratio
#fecundity--> fruit removed + withered flowers
# Predictors:

# site --> site name, makes plotting nicer
# Round --> which round treatment took place
# prop.c --> % agriculture around site
# trmnt --> needed to compare hp and op
# chafas abundance --> how many chamaecrista flowers were open on the day of treatment
# ID --> only for the inital plant level predictors



#load tidyverse and lme4

library(tidyverse)
library(lme4)
library(multcomp)
library(car)
library(corrplot)


# first, drop low density plots
seed_land<-read.csv("data/seed_land.csv")
seed_land <-filter(seed_land, plot=="hi")

#########################
# Plant level averages
######################

##By round
plt_rp<- seed_land %>%
  group_by(site,prop.c,round,trmnt,ID,flowers, fecund, yday)%>%
  summarise(fruit_count=n(), flower_count=sum(unique(flowers)),
            abrt_rate=1-(fruit_count/flower_count),
            avg_pr=mean(poll_ovules/total.ovules),
            var_pr=var(poll_ovules/total.ovules),
            avg_pl=mean(poll_lim),
            avg_polov=mean(poll_ovules), var_polov=var(poll_ovules),
            total_seed=sum(total.seeds),
            avg_seed=mean(total.seeds), var_seed= var(total.seeds),avg_sr=mean(total.seeds/total.ovules),
            var_sr=var(total.seeds/total.ovules))

### by round, drop braaten
plt_rpnb<- seed_land %>%
  group_by(site,prop.c,round,trmnt,ID,flowers, fecund,yday)%>%
  summarise(fruit_count=n(), flower_count=sum(unique(flowers)),
            abrt_rate=1-(fruit_count/flower_count),
            avg_pr=mean(poll_ovules/total.ovules),
            var_pr=var(poll_ovules/total.ovules),
            avg_pl=mean(poll_lim),
            avg_polov=mean(poll_ovules), var_polov=var(poll_ovules),
            total_seed=sum(total.seeds),
            avg_seed=mean(total.seeds), var_seed= var(total.seeds),avg_sr=mean(total.seeds/total.ovules),
            var_sr=var(total.seeds/total.ovules))%>%
  filter(site!="braaten")


sum(plt_rp$fruit_count)
sum(plt_rp_d1$fruit_count)

#Rerun and drop plants with singleton fruits at a round
plt_rp_d1<- seed_land %>%
  group_by(site,prop.c, round, trmnt,ID,chafas_ab,fecund, yday) %>%
  summarise(fruit_count=n(), avg_pr=mean(poll_ovules/total.ovules),
            flower_count=sum(unique(flowers)),
            abrt_rate=1-(fruit_count/flower_count),
            var_pr=var(poll_ovules/total.ovules),
            avg_polov=mean(poll_ovules), var_polov=var(poll_ovules), avg_pl=mean(poll_lim),
            avg_seed=mean(total.seeds), var_seed= var(total.seeds),avg_sr=mean(total.seeds/total.ovules),
            var_sr=var(total.seeds/total.ovules)) %>%
  filter(fruit_count>1)

#drop single fruit rounds and braaten
plt_rpnb1<- seed_land %>%
  group_by(site,prop.c,round,trmnt,ID,flowers,fecund,yday)%>%
  summarise(fruit_count=n(), flower_count=sum(unique(flowers)),
            abrt_rate=1-(fruit_count/flower_count),
            avg_pr=mean(poll_ovules/total.ovules),
            var_pr=var(poll_ovules/total.ovules),
            avg_pl=mean(poll_lim),
            avg_polov=mean(poll_ovules), var_polov=var(poll_ovules),
            total_seed=sum(total.seeds),
            avg_seed=mean(total.seeds), var_seed= var(total.seeds),avg_sr=mean(total.seeds/total.ovules),
            var_sr=var(total.seeds/total.ovules))%>%
  filter(site!="braaten")%>%
  filter(fruit_count>1)

#average over round
plt_rp2<- seed_land %>%
  group_by(site,prop.c,trmnt,ID,yday) %>%
  summarise(fruit_count=n(),avg_pr=mean(poll_ovules/total.ovules),
            var_pr=var(poll_ovules/total.ovules),avg_pl=mean(poll_lim),
            avg_polov=mean(poll_ovules), var_polov=var(poll_ovules), 
            avg_seed=mean(total.seeds), var_seed= var(total.seeds),avg_sr=mean(total.seeds/total.ovules),
            var_sr=var(total.seeds/total.ovules))

#average over round, drop braaten
plt_rp2nb<- seed_land %>%
  group_by(site,prop.c,trmnt,ID,yday) %>%
  summarise(fruit_count=n(),avg_pr=mean(poll_ovules/total.ovules),
            var_pr=var(poll_ovules/total.ovules),avg_pl=mean(poll_lim),
            avg_polov=mean(poll_ovules), var_polov=var(poll_ovules), 
            avg_seed=mean(total.seeds), var_seed= var(total.seeds),avg_sr=mean(total.seeds/total.ovules),
            var_sr=var(total.seeds/total.ovules))%>%
  filter(site!="braaten")

#averaged over round, drop single fruits
plt_rp_2d1<- seed_land %>%
  group_by(site,prop.c, trmnt,ID,yday) %>%
  summarise(fruit_count=n(), avg_pr=mean(poll_ovules/total.ovules),
            var_pr=var(poll_ovules/total.ovules),avg_pl=mean(poll_lim),
            avg_polov=mean(poll_ovules), var_polov=var(poll_ovules), 
            avg_seed=mean(total.seeds), var_seed= var(total.seeds),avg_sr=mean(total.seeds/total.ovules),
            var_sr=var(total.seeds/total.ovules)) %>%
  filter(fruit_count>1)

# averaged over round, drop single fruits + braaten
plt_rp2nb1<- seed_land %>%
  group_by(site,prop.c,trmnt,ID,yday) %>%
  summarise(fruit_count=n(),avg_pr=mean(poll_ovules/total.ovules),
            var_pr=var(poll_ovules/total.ovules),avg_pl=mean(poll_lim),
            avg_polov=mean(poll_ovules), var_polov=var(poll_ovules), 
            avg_seed=mean(total.seeds), var_seed= var(total.seeds),avg_sr=mean(total.seeds/total.ovules),
            var_sr=var(total.seeds/total.ovules))%>%
  filter(site!="braaten")%>%
  filter(fruit_count>1)



#########################################
### Site level averaged
###############################


## Distinct rounds
st_rp<-plt_rp %>%
  group_by(prop.c,site,round,trmnt,yday)%>% 
  summarise(avg_seed=mean(avg_seed),avg_pl=mean(avg_pl), avg_abrt=mean(abrt_rate),
            avg_polov=mean(avg_polov),
            avg_sr=mean(avg_sr), avg_pr=mean(avg_pr), avg_fecund=mean(fecund))
#distinc rounds + drop braaten
st_rpnb<-plt_rp %>%
  group_by(prop.c,site,round,trmnt,yday)%>% 
  summarise(avgavg_seed=mean(avg_seed),avg_pl=mean(avg_pl), avg_abrt=mean(abrt_rate),
            avg_polov=mean(avg_polov),
            avg_sr=mean(avg_sr), avg_pr=mean(avg_pr))%>%
  filter(site!="braaten")

###drop singletons
st_rpd1<-plt_rp_d1 %>%
  group_by(prop.c,site,round,trmnt,yday)%>% 
  summarise(fruit_count=sum(fruit_count),avg_abrt=mean(abrt_rate),avg_seed=mean(avg_seed),avg_pl=mean(avg_pl),
            avg_polov=mean(avg_polov),
            avg_sr=mean(avg_sr), avg_pr=mean(avg_pr),
            avg_ab=mean(chafas_ab))

#Averaged rounds
st_rp2<-plt_rp %>%
  group_by(prop.c,site,trmnt)%>% 
  summarise(obs= n(),fruit_count=sum(fruit_count), avg_abrt=mean(abrt_rate),avg_pl=mean(avg_pl), avg_seed=mean(avg_seed),
            avg_polov=mean(avg_polov),avg_sr=mean(avg_sr), avg_pr=mean(avg_pr))

#averaged rounds no braaten
st_rp2nb<-plt_rp %>%
  group_by(prop.c,site,trmnt)%>% 
  summarise(obs= n(),fruit_count=sum(fruit_count), avg_abrt=mean(abrt_rate),avg_pl=mean(avg_pl), avg_seed=mean(avg_seed),
            avg_polov=mean(avg_polov),avg_sr=mean(avg_sr), avg_pr=mean(avg_pr))%>%
  filter(site!="braaten")

#averaged rounds + drop singletons
st_rp2d1<-plt_rp_d1 %>%
  group_by(prop.c,site,trmnt,yday)%>% 
  summarise(obs= n(),fruit_count=sum(fruit_count),avg_pl=mean(avg_pl), avg_seed=mean(avg_seed),
            avg_polov=mean(avg_polov),avg_sr=mean(avg_sr), avg_pr=mean(avg_pr))

###############
#HPOP responses
##########

### rounds distinct
HPOP_site<-st_rp %>%
  group_by(prop.c,site,round,yday)%>%
  filter(n()==2) %>%# drop braaten, howe, and rudningen rounds with only one of the treatments represented
  summarise(HPOP_seed=(avg_seed[trmnt=="hp"])-(avg_seed[trmnt=="op"]),
            HPOP_pl=(avg_pl[trmnt=="hp"])-(avg_pl[trmnt=="op"]),
            HPOP_ov=(avg_polov[trmnt=="hp"])-(avg_polov[trmnt=="op"]),
            HPOP_pr=(avg_pr[trmnt=="hp"])-(avg_pr[trmnt=="op"]),
            HPOP_sr=(avg_sr[trmnt=="hp"])-(avg_sr[trmnt=="op"]))
### averaged over round
HPOP_site2<-st_rp2 %>%
  group_by(prop.c,site,yday)%>%
  filter(n()==2) %>%# drop braaten, howe, and rudningen rounds with only one of the treatments represented
  summarise(HPOP_seed=(avg_seed[trmnt=="hp"])-(avg_seed[trmnt=="op"]),
            HPOP_pl=(avg_pl[trmnt=="hp"])-(avg_pl[trmnt=="op"]),
            HPOP_ov=(avg_polov[trmnt=="hp"])-(avg_polov[trmnt=="op"]),
            HPOP_pr=(avg_pr[trmnt=="hp"])-(avg_pr[trmnt=="op"]),
            HPOP_sr=(avg_sr[trmnt=="hp"])-(avg_sr[trmnt=="op"]))

#drop braaten and averaged over round
HPOP_site2nb<-st_rp2 %>%
  group_by(prop.c,site,yday)%>%
  filter(n()==2) %>%# drop braaten, howe, and rudningen rounds with only one of the treatments represented
  summarise(HPOP_seed=(avg_seed[trmnt=="hp"])-(avg_seed[trmnt=="op"]),
            HPOP_pl=(avg_pl[trmnt=="hp"])-(avg_pl[trmnt=="op"]),
            HPOP_ov=(avg_polov[trmnt=="hp"])-(avg_polov[trmnt=="op"]),
            HPOP_pr=(avg_pr[trmnt=="hp"])-(avg_pr[trmnt=="op"]),
            HPOP_sr=(avg_sr[trmnt=="hp"])-(avg_sr[trmnt=="op"]))%>%
  filter(site != "braaten")

HPOP_site<-st_rp %>%
  group_by(prop.c,site,round,avg_ab,yday)%>%
  filter(n()==2) %>%# drop braaten, howe, and rudningen rounds with only one of the treatments represented
  summarise(HPOP_seed=(avg_seed[trmnt=="hp"])-(avg_seed[trmnt=="op"]),
            HPOP_pl=(avg_pl[trmnt=="hp"])-(avg_pl[trmnt=="op"]),
            HPOP_ov=(avg_polov[trmnt=="hp"])-(avg_polov[trmnt=="op"]),
            HPOP_pr=(avg_pr[trmnt=="hp"])-(avg_pr[trmnt=="op"]),
            HPOP_sr=(avg_sr[trmnt=="hp"])-(avg_sr[trmnt=="op"]))

HPOP_sitenb<-st_rp %>%
  group_by(prop.c,site,round,yday)%>%
  filter(n()==2) %>%# drop braaten, howe, and rudningen rounds with only one of the treatments represented
  summarise(HPOP_seed=(avg_seed[trmnt=="hp"])-(avg_seed[trmnt=="op"]),
            HPOP_pl=(avg_pl[trmnt=="hp"])-(avg_pl[trmnt=="op"]),
            HPOP_ov=(avg_polov[trmnt=="hp"])-(avg_polov[trmnt=="op"]),
            HPOP_pr=(avg_pr[trmnt=="hp"])-(avg_pr[trmnt=="op"]),
            HPOP_sr=(avg_sr[trmnt=="hp"])-(avg_sr[trmnt=="op"]))%>%
  filter(site != "braaten")


### let's compare this to a new df that averages HPOP_site  
HPOP_d1<-st_rpd1 %>%
  group_by(prop.c,site,round,avg_ab,yday)%>%
  filter(n()==2) %>%# drop braaten, howe, and rudningen rounds with only one of the treatments represented
  summarise(HPOP_seed=(avg_seed[trmnt=="hp"])-(avg_seed[trmnt=="op"]),
            HPOP_ov=(avg_polov[trmnt=="hp"])-(avg_polov[trmnt=="op"]),
            HPOP_pr=(avg_pr[trmnt=="hp"])-(avg_pr[trmnt=="op"]),
            HPOP_sr=(avg_sr[trmnt=="hp"])-(avg_sr[trmnt=="op"]))

##################
# Plots, correlation, etc.
###############

cor.test(plt_rp$fecund,plt_rp$avg_sr)



#### Seed_land plots
ggplot(seed_land, aes(yday,reorder(site,prop.c)))+geom_point()

ggplot(seed_land, aes(reorder(site,prop.c), fecund, color=trmnt))+
  geom_boxplot()+facet_grid(.~trmnt)+
  labs(x="site" ,y="Fecundity")+
  ggtitle( "Diff in Plant reproductive effort per site")

ggplot(seed_land, aes(yday, fecund, color=trmnt))+
  geom_jitter()+facet_grid(.~reorder(site,prop.c))+geom_smooth(method=lm,se=FALSE)+
  labs(x="yday" ,y="Fecundity")+

  ggtitle( "Diff in Plant reproductive effort over time")


ggplot(seed_land, aes(yday,total.ovules, color=trmnt))+
  geom_jitter()+
  labs(x="Day of Year" ,y="Total Ovules per fruit")+
    geom_smooth(method=lm,se=FALSE)+
  ggtitle( "Diff in ovules per fruit over time")

ggplot(frt_rp, aes(yday,abrt_rate)) +
  geom_jitter(aes(color=round))+
  labs(x="Treatment" ,y="# flowers")+
  ggtitle( "Diff in fruit loss over time")

ggplot(seed_land, aes(yday,flowers, color=trmnt)) +
  geom_jitter()+
  labs(x="Treatment" ,y="# flowers")+
  ggtitle( "Diff in # flowers per treatment, per round")

ggplot(seed_land, aes(trmnt,flowers)) +
  geom_violin()+facet_grid(.~round)+
  labs(x="Treatment" ,y="# flowers")+
  ggtitle( "Diff in # flowers per treatment, per round")


### let's look at some relationships without braaten, which had an obscene amount of herbivory

ggplot(seed_land, aes(yday,total.ovules, color=trmnt))+
  geom_jitter()+
  labs(x="Day of Year" ,y="Total Ovules per fruit")+
  geom_smooth(method=lm,se=FALSE)+
  ggtitle( "Diff in ovules per fruit over time")

ggplot(seed_nb, aes(prop.c,total.seeds, color=trmnt))+
  geom_jitter()+facet_grid(.~round)+
  labs(x="%ag" ,y="Total Seeds per fruit")+
  geom_smooth(method=lm,se=FALSE)+
  ggtitle( "Diff in Seeds per fruit with % Ag")

ggplot(seed_land, aes(prop.c,total.seeds, color=trmnt))+
  geom_jitter()+facet_grid(.~round)+
  labs(x="%ag" ,y="Total Seeds per fruit")+
  geom_smooth(method=lm,se=FALSE)+
  ggtitle( "Diff in Seeds per fruit with % Ag")

################################
###plt_rp plots
####################
ggplot(plt_rp, aes(trmnt,flower_count)) +
  geom_boxplot(aes(color=trmnt))+facet_grid(.~round)+
  labs(x="Treatment" ,y="# flowers")+
  ggtitle( "Diff in # flowers per treatment, per round")

ggplot(plt_rp, aes(trmnt,abrt_rate)) +
  geom_boxplot(aes(color=trmnt))+facet_grid(.~round)+
  labs(x="Treatment" ,y="Fruit Loss")+
  ggtitle( "Diff in Fruit Loss per treatment, per round")

ggplot(plt_rp, aes(flower_count,abrt_rate)) +
  geom_jitter(aes(color=trmnt))+facet_grid(.~round)+
  labs(x="# flowers" ,y="Fruit loss")+
  ggtitle( "Change in fruit loss with # flowers, per round")

ggplot(plt_rp, aes(flower_count,avg_pr, color=trmnt)) +
  geom_point()+facet_grid(reorder(site,prop.c)~round)+
  labs(x="# flowers" ,y="Poll. Ov/ Total Ov")+
  ggtitle( "Change in PR with # flowers, per round")+
  geom_smooth(method=lm,se=FALSE)

ggplot(plt_rp, aes(flower_count,avg_seed, color=trmnt)) +
  geom_point()+facet_grid(reorder(site,prop.c)~round)+
  labs(x="# flowers" ,y="# seeds per fruit")+
  ggtitle( "Change in Avg Seeds per fruit with # flowers, per round")+
  geom_smooth(method=lm,se=FALSE)


## From this we can run initial models, but will have to account for plant as an ID
## Let's look at plots


### Violins

ggplot(plt_rp, aes(trmnt,avg_pr, color=trmnt)) +geom_violin()+ 
  labs(x="Treatment" ,y="Avg. Poll. Ovules/Total Ovules per plant, per round")+
  ggtitle("Avg Pollinated/Total Ovules per plant, per round") 

ggplot(plt_rp_d1, aes(trmnt,avg_pr, color=trmnt)) +geom_violin()+ 
  labs(x="Treatment" ,y="Average Poll. Ovules/Total Ovules per Site")+
  ggtitle("Avg Pollinated/Total Ovules per Plant", subtitle="Distinct rounds, Drop single fruit plants") 

ggplot(plt_rp2, aes(trmnt,avg_pr, color=trmnt)) +geom_violin()+ 
  labs(x="Treatment" ,y="Avg. Poll. Ovules/Total Ovules per plant")+
  ggtitle("Avg Pollinated/Total Ovules per plant", subtitle="Averaged across rounds") 

### Fruit Loss
ggplot(plt_rp, aes(trmnt, abrt_rate, color=trmnt)) +geom_boxplot()+ 
  labs(x="Treatment" ,y="Fruit Loss")+
  ggtitle("Fruit Loss by treatment", subtitle= "Distinct rounds") 

ggplot(plt_rp_d1, aes(trmnt, abrt_rate, color=trmnt)) +geom_boxplot()+ 
  labs(x="Treatment" ,y="Fruit Loss")+
  ggtitle("Fruit Loss by treatment", subtitle= "Distinct rounds, drop 1") 

ggplot(plt_rp, aes(prop.c,abrt_rate, color=trmnt)) +geom_jitter()+ 
  labs(x="% Agriculture" ,y="Fruit Loss per plant per round")+
  ggtitle("Fruit Loss by % Development") +
  geom_smooth(method=lm,se=FALSE)

ggplot(plt_rp_d1, aes(prop.c,abrt_rate, color=trmnt)) +geom_jitter()+ 
  labs(x="% Agriculture" ,y="Abortion rate per plant per round")+
  ggtitle("Abortion rate by % Development", subtitle="drop singletons") +
  geom_smooth(method=lm,se=FALSE)

### Avg pr

ggplot(plt_rp, aes(prop.c,avg_pr, color=trmnt)) +geom_point()+ 
  labs(x="% Agriculture" ,y="Poll. Ovules/Total Ovules per Fruit")+
  ggtitle("Pollinated Ovules/Total Ovules by % Development", subtitle="Averaged per individual, per round") +
  geom_smooth(method=lm,se=FALSE)

ggplot(plt_rpnb, aes(prop.c,avg_pr, color=trmnt)) +geom_point()+ 
  labs(x="% Agriculture" ,y="Poll. Ovules/Total Ovules per Fruit")+
  ggtitle("Pollinated Ovules/Total Ovules by % Development", subtitle="Dropped braaten, avg per individual, per round") +
  geom_smooth(method=lm,se=FALSE)

ggplot(plt_rp2nb, aes(prop.c,avg_pr, color=trmnt)) +geom_point()+ 
  labs(x="% Agriculture" ,y="Poll. Ovules/Total Ovules per Fruit")
  ggtitle("Pollinated Ovules/Total Ovules by % Development", subtitle="Dropped braaten, avg per individual, over all rounds") +
  geom_smooth(method=lm,se=FALSE)

ggplot(plt_rpnb1, aes(prop.c,avg_pr, color=trmnt)) +geom_point()+ 
  labs(x="% Agriculture" ,y="Poll. Ovules/Total Ovules per Fruit")+
  ggtitle("Pollinated Ovules/Total Ovules by % Development", subtitle="Dropped braaten & Singletons, avg per individual, per round") +
  geom_smooth(method=lm,se=FALSE)

ggplot(plt_rp2nb1, aes(prop.c,avg_pr, color=trmnt)) +geom_point()+ 
  labs(x="% Agriculture" ,y="Poll. Ovules/Total Ovules per Fruit")+
  ggtitle("Pollinated Ovules/Total Ovules by % Development", subtitle="Dropped braaten & Singletons, avg per individual over  all rounds") +
  geom_smooth(method=lm,se=FALSE)

ggplot(plt_rp2, aes(prop.c,avg_pr, color=trmnt)) +geom_point()+ 
  labs(x="% Agriculture" ,y="Poll. Ovules/Total Ovules per Fruit")+
  ggtitle("Avg Pollinated/Total Ovules by % Development", subtitle="Averaged per individual") +
  geom_smooth(method=lm,se=FALSE)

ggplot(plt_rp_2d1, aes(prop.c,avg_pr, color=trmnt)) +geom_point()+ 
  labs(x="% Agriculture" ,y="Poll. Ovules/Total Ovules per Fruit")+
  ggtitle("Avg Pollinated/Total Ovules by % Development (Drop Singletons)") +
geom_smooth(method=lm,se=FALSE)

###avg sr
ggplot(plt_rp2, aes(prop.c,avg_sr, color=trmnt)) +geom_point()+
  labs(x="% Agriculture" ,y="Seeds/Total Ovules per Fruit")+
  ggtitle("Total Seeds/Total Ovules by % Development ", subtitle="Averaged per individual, per round") +
  geom_smooth(method=lm,se=FALSE)

ggplot(plt_rp2nb1, aes(prop.c,avg_sr, color=trmnt)) +geom_point()+
  labs(x="% Agriculture" ,y="Seeds/Total Ovules per Fruit")+
  ggtitle("Avg Seed/Total Ovules by % Development", subtitle="avg per individual over all rounds, dropped braaten and singletons") +
geom_smooth(method=lm,se=FALSE)

ggplot(plt_rp2nb, aes(prop.c,avg_sr, color=trmnt)) +geom_point()+
  labs(x="% Agriculture" ,y="Seeds/Total Ovules per Fruit")+
  ggtitle("Avg Seed/Total Ovules by % Development", subtitle="avg per individual over all rounds, dropped braaten") +
  geom_smooth(method=lm,se=FALSE)

ggplot(plt_rpnb, aes(prop.c,avg_sr, color=trmnt)) +geom_point()+
  labs(x="% Agriculture" ,y="Seeds/Total Ovules per Fruit")+
  ggtitle("Avg Seed/Total Ovules by % Development", subtitle="avg per individual over all rounds, dropped braaten") +
  geom_smooth(method=lm,se=FALSE)

ggplot(plt_rpnb, aes(yday,avg_sr, color=trmnt)) +geom_point()+
  labs(x="Day of Year" ,y="Seeds/Total Ovules per Fruit")+
  ggtitle("Avg Seed/Total Ovules by % Development", subtitle="avg per individual, per round, dropped braaten") +
  geom_smooth(method=lm,se=FALSE)

### Avg polovs
ggplot(plt_rp, aes(prop.c,avg_polov, color=trmnt)) +geom_point()+
  labs(x="% Agriculture" ,y="Pollinated Ovules per Fruit")+
  ggtitle("Pollinated Ovules by % Development", subtitle="Averaged per individual, per round") +
  geom_smooth(method=lm,se=FALSE)
ggplot(plt_rpnb, aes(prop.c,avg_polov, color=trmnt)) +geom_point()+
  labs(x="% Agriculture" ,y="Pollinated Ovules per Fruit")+
  ggtitle("Pollinated Ovules by % Development", subtitle="Drop braaten, Averaged per individual, per round") +
  geom_smooth(method=lm,se=FALSE)
ggplot(plt_rp2, aes(prop.c,avg_polov, color=trmnt)) +geom_point()+
  labs(x="% Agriculture" ,y="Pollinated Ovules per Fruit")+
  ggtitle("Pollinated Ovules by % Development", subtitle="Averaged per individual, across round") +
  geom_smooth(method=lm,se=FALSE)

ggplot(plt_rp_d1, aes(prop.c,avg_polov, color=trmnt)) +geom_point()+
  labs(x="% Agriculture" ,y="Pollinated Ovules per Fruit")+
  ggtitle("Avg Pollinated Ovules per by % Development (Drop Singletons)") +
geom_smooth(method=lm,se=FALSE)+
  ggsave("plantlvl_polovd1.pdf")

ggplot(plt_rp2nb, aes(prop.c,avg_polov, color=trmnt)) +geom_point()+
  labs(x="% Agriculture" ,y="Pollinated Ovules per Fruit")+
  ggtitle("Pollinated Ovules by % Development", subtitle="Dropped Braaten, Averaged per individual, across round") +
  geom_smooth(method=lm,se=FALSE)
ggplot(plt_rp2nb1, aes(prop.c,avg_polov, color=trmnt)) +geom_point()+
  labs(x="% Agriculture" ,y="Pollinated Ovules per Fruit")+
  ggtitle("Pollinated Ovules by % Development", subtitle="Dropped Braaten & singletons, Averaged per individual, across round") +
  geom_smooth(method=lm,se=FALSE)
### Avg Seeds
ggplot(plt_rp, aes(prop.c,avg_seed, color=trmnt)) +geom_point()+
  labs(x="% Agriculture" ,y="Seeds per Fruit ")+
  ggtitle("Seeds by % Development", subtitle="Averaged per individual, per round")+
geom_smooth(method=lm,se=FALSE)


ggplot(plt_rp_d1, aes(prop.c,avg_seed, color=trmnt)) +geom_point()+
  labs(x="% Agriculture" ,y="Seeds per Fruit ")+ facet_grid(.~round)+
  ggtitle("Avg # Seeds by % Development(drop)") +geom_smooth(method=lm,se=FALSE)

ggplot(plt_rp2nb, aes(prop.c,avg_seed, color=trmnt)) +geom_point()+
  labs(x="% Agriculture" ,y="Seeds/Total Ovules per Fruit")+
  ggtitle("Avg Seed by % Development", subtitle="avg per individual over all rounds, dropped braaten") +
  geom_smooth(method=lm,se=FALSE)


ggplot(plt_rp2nb1, aes(prop.c,avg_seed, color=trmnt)) +geom_point()+
  labs(x="% Agriculture" ,y="Seeds/Total Ovules per Fruit")+
  ggtitle("Avg Seed by % Development", subtitle="avg per individual over all rounds, dropped braaten and singletons") +
  geom_smooth(method=lm,se=FALSE)
ggplot(plt_rpnb, aes(prop.c,avg_seed, color=trmnt)) +geom_point()+
  labs(x="% Agriculture" ,y="Seeds per Fruit")+facet_grid(.~round)+
  ggtitle("Avg Seed/Total Ovules by % Development", subtitle="avg per individual, per round, dropped braaten") +
  geom_smooth(method=lm,se=FALSE)

### Violin plots of avg_pr 
ggplot(plt_rp2, aes(trmnt,avg_pr, color=trmnt)) +geom_violin()+ 
  labs(x="Treatment" ,y="Average Poll. Ovules/Total Ovules per Site")+
  ggtitle("Avg Pollinated/Total Ovules per Plant", subtitle="Averaged across round") 
ggplot(plt_rp_2d1, aes(trmnt,avg_pr, color=trmnt)) +geom_violin()+ 
  labs(x="Treatment" ,y="Average Poll. Ovules/Total Ovules per Site")+
  ggtitle("Avg Pollinated/Total Ovules per Plant", subtitle="Averaged across round, Drop single fruit plants") 

## Avg pr

ggplot(st_rp, aes(prop.c,avg_pr, color=trmnt)) +geom_point()+ 
  labs(x="% Agriculture" ,y="Poll. Ovules/Total Ovules per Fruit")+
  ggtitle("Pollinated Ovules/Total Ovules by % Development",
          subtitle="Averaged per site, per round") +
  geom_smooth(method=lm,se=FALSE)

ggplot(st_rp2, aes(prop.c,avg_pr, color=trmnt)) +geom_point()+ 
  labs(x="% Agriculture" ,y="  Poll. Ovules/Total Ovules per Fruit")+
  ggtitle(" Pollinated/Total Ovules by % Development",
          subtitle="Averaged per site") +
  geom_smooth(method=lm,se=FALSE)

ggplot(plt_rp_2d1, aes(prop.c,var_pr, color=trmnt)) +geom_point()+ 
  labs(x="% Agriculture" ,y="Poll. Ovules/Total Ovules per Fruit")+
  ggtitle("Avg Pollinated/Total Ovules by % Development (Drop Singletons)") +
  geom_smooth(method=lm,se=FALSE)

###avg sr
ggplot(st_rp, aes(prop.c,avg_sr, color=trmnt)) +geom_point()+
  labs(x="% Agriculture" ,y="Seeds/Total Ovules per Fruit")+
  ggtitle("Seed/Total Ovules by % Development",
          subtitle="Averaged per site, per round")  +
  geom_smooth(method=lm,se=FALSE)

ggplot(st_rp2nb, aes(prop.c,avg_sr, color=trmnt)) +geom_point()+
  labs(x="% Agriculture" ,y="Seeds/Total Ovules per Fruit")+
  ggtitle("Seed/Total Ovules by % Development",
          subtitle="Averaged per site")  +
  geom_smooth(method=lm,se=FALSE)

ggplot(st_rp, aes(yday,avg_fecund, color=trmnt)) +geom_point()+
  labs(x="% Agriculture" ,y="Seeds per Fruit")+
  ggtitle("Fecundity by % Development",
          subtitle="Averaged per site, per round")  +
  geom_smooth(method=lm,se=FALSE)

ggplot(st_rp2, aes(prop.c,avg_seed, color=trmnt)) +geom_point()+
  labs(x="% Agriculture" ,y="Seeds per Fruit")+
  ggtitle("Seed per fruit by % Development",
          subtitle="Averaged per site")  +
  geom_smooth(method=lm,se=FALSE)



ggplot(plt_rp2nb1, aes(prop.c,avg_sr, color=trmnt)) +geom_point()+
  labs(x="% Agriculture" ,y="Avg Seeds/Total Ovules per Fruit")+
  ggtitle("Avg Seed/Total Ovules by % Development") +
  geom_smooth(method=lm,se=FALSE)

ggplot(plt_rp_2d1, aes(prop.c,avg_sr, color=trmnt)) +geom_point()+
  labs(x="% Agriculture" ,y="Seeds/Total Ovules per Fruit")+
  ggtitle("Avg Seed/Total Ovules by % Development (Drop Singletons)") +
  geom_smooth(method=lm,se=FALSE)+
  ggsave("plant_nord_srd1.pdf")

### Avg polovs
ggplot(plt_rp2, aes(prop.c,avg_polov, color=trmnt)) +geom_point()+
  labs(x="% Agriculture" ,y="Pollinated Ovules per Fruit")+
  ggtitle("Avg Pollinated Ovules per by % Development") +
  geom_smooth(method=lm,se=FALSE)+
  ggsave("plant_nord_polov.pdf")



ggplot(plt_rp_2d1, aes(prop.c,avg_polov, color=trmnt)) +geom_point()+
  labs(x="% Agriculture" ,y="Pollinated Ovules per Fruit")+
  ggtitle("Avg Pollinated Ovules per by % Development (Drop Singletons)") +
  geom_smooth(method=lm,se=FALSE)+
  ggsave("plant_nord_polovd1.pdf")

### Avg Seeds
ggplot(plt_rp2, aes(prop.c,avg_seed, color=trmnt)) +geom_point()+
  labs(x="% Agriculture" ,y="Seeds per Fruit ")+
  ggtitle("Avg # Seeds by % Development") +geom_smooth(method=lm,se=FALSE)+
  ggsave("plant_nord_seed.pdf")

ggplot(plt_rp2, aes(prop.c,var_seed, color=trmnt)) +geom_point()+
  labs(x="% Agriculture" ,y=" var Seeds per Fruit ")+
  ggtitle("Avg # Seeds by % Development") +geom_smooth(method=lm,se=FALSE)+
  ggsave("plant_nord_varseed.pdf")

ggplot(plt_rp_2d1, aes(prop.c,avg_seed, color=trmnt)) +geom_point()+
  labs(x="% Agriculture" ,y="Seeds per Fruit ")+
  ggtitle("Avg # Seeds by % Development(drop)") +geom_smooth(method=lm,se=FALSE)+
  ggsave("plant_nord_seedd1.pdf")

ggplot(plt_rp_2d1, aes(prop.c,var_seed, color=trmnt)) +geom_point()+
  labs(x="% Agriculture" ,y="Var Seeds per Fruit ")+
  ggtitle("Avg # Seeds by % Development(drop)") +geom_smooth(method=lm,se=FALSE)+
  ggsave("plant_nord_varseedd1.pdf")


### General take away: Dropping singletons doesn't change relationship drastically
### averaging over rounds at the plant level refines relationship between % ag and responses



ggplot(st_rp, aes(trmnt, avg_pr, color=trmnt)) +geom_boxplot()+ 
  labs(x="Treatment" ,y="Abortion Rate")+
  ggtitle("Pollinated", subtitle= "Average per site") 

ggplot(st_rp2, aes(trmnt, avg_abrt, color=trmnt)) +geom_point()+ 
  labs(x="Treatment" ,y="Abortion Rate")+
  ggtitle("Abortion Rate by treatment", subtitle= "Averaged rounds") 

ggplot(st_rp2, aes(trmnt, avg_abrt, color=trmnt)) +geom_boxplot()+ 
  labs(x="Treatment" ,y="Abortion Rate")+
  ggtitle("Abortion Rate by treatment", subtitle= "Averaged rounds") 

ggplot(st_rp2, aes(prop.c,avg_abrt, color=trmnt)) +geom_point()+ 
  labs(x="% Agriculture" ,y="Avg abortion rate")+
  ggtitle("Avg abortion rate by % Development") +
  geom_smooth(method=lm,se=FALSE)

## abortion rate confounded by braaten, which had high herbivory relative to other sites

### So will need to drop those rounds with only one observation
### Need to generate a variable that  counts the # of rows per site per round

### then need to create summarise that for each site and round combination generates HPOP values
### could use neste dif else, but would be sloppy
### 



####
ggplot(HPOP_site, aes(prop.c,HPOP_pr)) +geom_point()+
  labs(x="% Agriculture" ,y="HP-OP Poll ovs/Total Ovs")+
  ggtitle("HP-OP Poll Ovs/Total Ovs by % Ag", subtitle="Diff between HP and OP treatements by round and site") +
geom_smooth(method=lm,se=FALSE)

ggplot(HPOP_d1, aes(prop.c,HPOP_pr)) +geom_point()+
  labs(x="% Agriculture" ,y="Avg HP-OP for Pr per round, per site")+
  ggtitle("HP-OP Pr by % Ag", subtitle="Pollinated Ovules/Total Ovules") +
  geom_smooth(method=lm,se=FALSE)

ggplot(HPOP_site2, aes(prop.c,HPOP_pr)) +geom_point()+
  labs(x="% Agriculture" ,y="HP-OP Poll ovs/Total Ovs")+
  ggtitle("HP-OP Poll Ovs/Total Ovs by % Ag", 
          subtitle="Diff between HP and OP treatements by site") +
  geom_smooth(method=lm,se=FALSE)

ggplot(HPOP_site2nb, aes(prop.c,HPOP_pr)) +geom_point()+
  labs(x="% Agriculture" ,y="HP-OP Poll ovs/Total Ovs")+
  ggtitle("HP-OP Poll Ovs/Total Ovs by % Ag", 
          subtitle="Diff between HP and OP treatements by site, drop braaten") +
  geom_smooth(method=lm,se=FALSE)
##Seed ratio 
ggplot(HPOP_site2nb, aes(prop.c,HPOP_sr)) +geom_point()+
  labs(x="% Agriculture" ,y="Avg HP-OP for Sr per round, per site")+
  ggtitle("HP-OP Sr by % Ag",subtitle="Seeds/Total Ovules") +
  geom_smooth(method=lm,se=FALSE)

### Seed ratio drop singletons fruits
ggplot(HPOP_d1, aes(prop.c,HPOP_sr)) +geom_point()+
  labs(x="% Agriculture" ,y="Avg HP-OP for Pl per round, per site")+
  ggtitle("HP-OP Pr by % Ag", subtitle="Total Ovules- Pollinated Ovules") +
  geom_smooth(method=lm,se=FALSE)

ggplot(HPOP_site2, aes(prop.c,HPOP_seed)) +geom_point()+
  labs(x="% Agriculture" ,y="Avg HP-OP for Seeds per round, per site")+
  ggtitle("Avg HP-OP Seeds by % Ag") +
  geom_smooth(method=lm,se=FALSE)

ggplot(HPOP_d1, aes(prop.c,HPOP_seed)) +geom_point()+
  labs(x="% Agriculture" ,y="Avg HP-OP for Seeds per round, per site")+
  ggtitle("Avg HP-OP Seeds by % Ag", subtitle="Drop single fruit plants") +
  geom_smooth(method=lm,se=FALSE)

ggplot(HPOP_d1, aes(prop.c,HPOP_seed)) +geom_point()+
  labs(x="% Agriculture" ,y=" HP-OP for variance in Seeds per round, per site")+
  ggtitle("VarHP-OP Seeds by % Ag", subtitle="Drop single fruit plants") +
  geom_smooth(method=lm,se=FALSE)
              
ggplot(HPOP_site, aes(prop.c,HPOP_ov)) +geom_point()+
  labs(x="% Agriculture" ,y="Avg HP-OP for Poll Ovules per round, per site")+
  ggtitle("HP-OP Poll Ov by % Ag") +
  geom_smooth(method=lm,se=FALSE)


### with abundance

ggplot(HPOP_site, aes(avg_ab,HPOP_seed)) +geom_point()+
  labs(x="Chamaecrista bloom abundance" ,y="Avg HP-OP for Seeds per round, per site")+
  ggtitle("Avg HP-OP Seeds by bloom abundance") +
  geom_smooth(method=lm,se=FALSE)

ggplot(HPOP_d1, aes(avg_ab,HPOP_seed)) +geom_point()+
  labs(x="Chamaecrista bloom abundance" ,y="Avg HP-OP for Seeds per round, per site")+
  ggtitle("Avg HP-OP Seeds by bloom abundance", subtitle="Drop single fruit plants") +
  geom_smooth(method=lm,se=FALSE)

ggplot(HPOP_site, aes(avg_ab,HPOP_ov)) +geom_point()+
  labs(x="Chamaecrista bloom abundance" ,y="Avg HP-OP for Poll Ovs per round, per site")+
  ggtitle("Avg HP-OP poll ovs by bloom abundance") +
  geom_smooth(method=lm,se=FALSE)

ggplot(HPOP_d1, aes(avg_ab,HPOP_ov)) +geom_point()+
  labs(x="Chamaecrista bloom abundance" ,y="Avg HP-OP for Poll ovs per round, per site")+
  ggtitle("Avg HP-OP Seeds by bloom abundance", subtitle="Drop single fruit plants") +
  geom_smooth(method=lm,se=FALSE)


ggplot(HPOP_site, aes(avg_ab,HPOP_pr)) +geom_point()+
  labs(x="Chamaecrista bloom abundance" ,y="Avg HP-OP for pr per round, per site")+
  ggtitle("Avg HP-OP pr by bloom abundance") +
  geom_smooth(method=lm,se=FALSE)

ggplot(HPOP_d1, aes(avg_ab,HPOP_pr)) +geom_point()+
  labs(x="Chamaecrista bloom abundance" ,y="Avg HP-OP for pr per round, per site")+
  ggtitle("Avg HP-OP pr by bloom abundance", subtitle="Drop single fruit plants") +
  geom_smooth(method=lm,se=FALSE)

ggplot(HPOP_site, aes(avg_ab,HPOP_sr)) +geom_point()+
  labs(x="Chamaecrista bloom abundance" ,y="Avg HP-OP for sr per round, per site")+
  ggtitle("Avg HP-OP sr by bloom abundance") +
  geom_smooth(method=lm,se=FALSE)

ggplot(HPOP_d1, aes(avg_ab,HPOP_sr)) +geom_point()+
  labs(x="Chamaecrista bloom abundance" ,y="Avg HP-OP for sr per round, per site")+
  ggtitle("Avg HP-OP sr by bloom abundance", subtitle="Drop single fruit plants") +
  geom_smooth(method=lm,se=FALSE)

#### violin plots of site data


ggplot(st_rp, aes(trmnt,avg_pr, color=trmnt)) +geom_violin()+ 
  labs(x="Treatment" ,y="Average Poll. Ovules/Total Ovules per Site")+
  ggtitle("Avg Pollinated/Total Ovules per Site", subtitle="Distinct Rounds") 

ggplot(st_rpd1, aes(trmnt,avg_pr, color=trmnt)) +geom_violin()+ 
  labs(x="Treatment" ,y="Average Poll. Ovules/Total Ovules per Site")+
  ggtitle("Avg Pollinated/Total Ovules per Site",subtitle="Distinct rounds, Drop Single fruit plants") 

ggplot(site_lvl, aes(trmnt,avg_pr, color=trmnt)) +geom_violin()+ 
  labs(x="Treatment" ,y="Average Poll. Ovules/Total Ovules per Site")+
  ggtitle("Avg Pollinated/Total Ovules per Site",subtitle="Averaged across round") 

###########
## Collinearity of predictors
##########

## use corvif function in Zuur

#Select covariates
Z<-seed_land[,c("prop.c", "flowers",
                "yday", "fecund")]
corvif(Z)      #Part of Table 1

#collinearity low, doesn't seem to be an issue



#Let's start with the broadest, most simple models:
#HPOP_site and _site2 contain the difference in treatments for our response variables
#HPOP_site averaged for each round at each site
#while HPOP_site 2 averages across rounds within sites
# Response variables:
# HPOP_sr== Diff in avg total seeds/total ovules per fruit per plant, between treatments 
# HPOP_pr== Diff in avg pollinated ovules/total ovules per fruit per plant, between treatments 
# HPOP_seed== Diff in avg total seedsper fruit per plant, between treatments 
# HPOP_ov== Diff in pollinated ovules per fruit per plant, between treatments 
# HPOP_ pl== Diff in Total ovules - Pollinated ovules per fruit per plant, between treatments


###negative binomial or poisson model, maybe ZIF? 


lmer(plt_rp (1|site/ID))
## plot.lmer function??



#seed/total ovules
sr_model<-lmer(HPOP_sr~prop.c+round+avg_ab+(1|site), data=HPOP_site)
summary(sr_model)
drop1(sr_model,test="Chisq")

#seed/total ovules dropped single fruit plants
sr_mod1<-lmer(HPOP_sr~prop.c+round+avg_ab+(1|site), data=HPOP_d1)
summary(sr_mod1)
drop1(sr_mod1,test="Chisq")


op <- par(mfrow = c(2, 2), mar = c(5, 4, 1, 2))
plot(sr_mod1, add.smooth = FALSE, which = 1)
residuals<- resid(sr_mod1)
plot(residuals)
plot(HPOP_d1$avg_ab, residuals, xlab = "avg_ab",
     ylab = "Residuals")
plot(HPOP_d1$prop.c, residuals, xlab = "% Ag",
     ylab = "Residuals")
plot(HPOP_d1$round, residuals, xlab = "round",
     ylab = "Residuals")
par(op)

### Residuals for round not even


# pollinated ovule/total ovules
pr_model<-lmer(HPOP_pr~prop.c+round+avg_ab+(1|site), data=HPOP_site)
summary(pr_model)
drop1(pr_model,test="Chisq")

op <- par(mfrow = c(2, 2), mar = c(5, 4, 1, 2))
plot(pr_model, add.smooth = FALSE, which = 1)
E <- resid(pr_model)
plot(E)
plot(HPOP_site$avg_ab, E, xlab = "avg_ab",
     ylab = "Residuals")
plot(HPOP_site$prop.c, E, xlab = "% Ag",
     ylab = "Residuals")
plot(HPOP_site$round, E, xlab = "round",
     ylab = "Residuals")
par(op)

# pollinated ovule/total ovules dropped single fruit plants
pr_mod1<-lmer(HPOP_pr~prop.c+round+avg_ab+(1|site), data=HPOP_d1)
summary(pr_mod1)
drop1(pr_mod1,test="Chisq")

op <- par(mfrow = c(2, 2), mar = c(5, 4, 1, 2))
plot(pr_mod1, add.smooth = FALSE, which = 1)
residuals <- resid(pr_mod1)
plot(residuals)
plot(HPOP_d1$avg_ab, residuals, xlab = "avg_ab",
     ylab = "Residuals")
plot(HPOP_d1$prop.c, residuals, xlab = "% Ag",
     ylab = "Residuals")
plot(HPOP_d1$round, residuals, xlab = "round",
     ylab = "Residuals")
par(op)


# Seed model
seed_model<-lmer(HPOP_seed~prop.c+round+avg_ab+(1|site), data=HPOP_site)
summary(seed_model)
drop1(seed_model,test="Chisq")

### seed model diagnostics
op <- par(mfrow = c(2, 2), mar = c(5, 4, 1, 2))
plot(seed_model, add.smooth = FALSE, which = 1)
residuals <- resid(seed_model)
plot(residuals)
plot(HPOP_site$avg_ab, residuals, xlab = "chamaecrista flower #",
     ylab = "Residuals")
plot(HPOP_site$prop.c, residuals, xlab = "% Ag",
       ylab = "Residuals")
plot(HPOP_site$round, residuals, xlab = "round",
       ylab = "Residuals")
par(op)
 

### Residual spread different at different rounds


### seed drop single fruit plants
seed_mod1<-lmer(HPOP_seed~prop.c+round+avg_ab+(1|site), data=HPOP_d1)
summary(seed_mod1)
drop1(seed_mod1,test="Chisq")

op <- par(mfrow = c(2, 2), mar = c(5, 4, 1, 2))
plot(seed_mod1, add.smooth = FALSE, which = 1)
residuals <- resid(seed_mod1)
plot(residuals)
plot(HPOP_d1$avg_ab, residuals, xlab = "chamaecrista flower #",
     ylab = "Residuals")
plot(HPOP_d1$prop.c, residuals, xlab = "% Ag",
     ylab = "Residuals")
plot(HPOP_d1$round, residuals, xlab = "round",
     ylab = "Residuals")
par(op)

## pollinated ovule model
ov_model<-lmer(HPOP_ov~prop.c+round+(1|site), data=HPOP_site)
summary(ov_model)
drop1(ov_model,test="Chisq")


### poll ov model drop single fruit plants
ov_mod1<-lmer(HPOP_ov~prop.c+round+(1|site), data=HPOP_d1)
summary(ov_mod1)
drop1(ov_mod1,test="Chisq")
### Ovule model diagnostics


### Dropping single fruit plants drastically affects the residuals for round



#############
# Treatment models
######
sm<-lmer(total.seeds~trmnt+prop.c+yday+fecund+(1|site), data=seed_land)
summary(sm)
drop1(sm,test="Chisq")

op <- par(mfrow = c(2, 2), mar = c(5, 4, 1, 2))
plot(sm, add.smooth = FALSE)
residuals <- resid(sm)
plot(residuals)
plot(seed_land$yday, residuals, xlab = "round",
     ylab = "Residuals")
plot(seed_land$prop.c, residuals, xlab = "% Ag",
     ylab = "Residuals")
plot(seed_land$trmnt, residuals, xlab = "trmnt",
     ylab= "Residuals")
par(mfrow=c(1,1))

qqnorm(resid(sm))
qqline(resid(sm))

qqPlot(resid(sm))
### Plants have been averaged into treatments to account for variable # of fruit per plant
# st_rp contains all fruit
# st_rpd1 dropped single fruit plants
# st_rp2 has averaged over round
## First, let's look at a pairplot of responses and explanatory variables



pr_s<-lmer(avg_pr~trmnt+prop.c+avg_ab+(1|site), data=st_rp2)
summary(pr_s)
drop1(pr_s,test="Chisq")

## correlation
pairs(avg_pr~trmnt+prop.c+round+yday, data=st_rp)

##histogram of residuals
hist(residuals, xlab = "Residuals", main = "")


op <- par(mfrow = c(2, 2), mar = c(5, 4, 1, 2))
plot(pr_s, add.smooth = FALSE)
residuals <- resid(pr_s)
plot(residuals)
plot(st_rp$round, residuals, xlab = "round",
     ylab = "Residuals")
plot(st_rp$prop.c, residuals, xlab = "% Ag",
     ylab = "Residuals")
plot(st_rp$trmnt, residuals, xlab = "trmnt",
     ylab= "Residuals")
par(mfrow=c(1,1))

qqnorm(resid(pr_s))
qqline(resid(pr_s))


qqPlot(resid(pr_s))


### Two outliers. 
### Variance not equal across treatments
### Maybe look at added variable plots to see where 


### alternative using package multcomp()
tmp <- as.data.frame(confint(glht(pr_s))$confint)
tmp$Comparison <- rownames(tmp)
ggplot(tmp, aes(x = Comparison, y = Estimate, ymin = lwr, ymax = upr)) +
  geom_errorbar() + geom_point()
## trmnt CIs overlap, need to check means parameterization code for round

#avg rounds together
pr_s2<-lmer(avg_sr~trmnt+prop.c+(1|site),data=st_rp2)
summary(pr_s2)
drop1(pr_s2,test="Chisq")

## correlation
pairs(avg_pr~trmnt+prop.c, data=st_rp2)

### residuals
op <- par(mfrow = c(2, 2), mar = c(5, 4, 1, 2))
plot(pr_s2, add.smooth = FALSE)
residuals <- resid(pr_s2)
plot(residuals)
hist(residuals, xlab = "Residuals", main = "")
plot(st_rp2$prop.c, residuals, xlab = "% Ag",
     ylab = "Residuals")
plot(st_rp2$trmnt, residuals, xlab = "trmnt",
     ylab= "Residuals")
par(mfrow=c(1,1))
### histogram doesn't look like it fully follows a normal distribution
### Other variables don't seem to exhibit any major issues

qqnorm(resid(pr_s2))
qqline(resid(pr_s2))

qqPlot(resid(pr_s2))

###residuals look okay here. 


### check CIs for trmnt
tmp <- as.data.frame(confint(glht(pr_s2))$confint)
tmp$Comparison <- rownames(tmp)
ggplot(tmp, aes(x = Comparison, y = Estimate, ymin = lwr, ymax = upr)) +
  geom_errorbar() + geom_point()
### Trmnt CIs overlap.




### Seed ratio
sr_s<-lmer(avg_sr~trmnt+prop.c+yday+(1|site), data=st_rp)
summary(sr_s)
drop1(sr_s,test="Chisq")


op <- par(mfrow = c(2, 2), mar = c(5, 4, 1, 2))
plot(sr_s, add.smooth = FALSE)
residuals <- resid(sr_s)
plot(residuals)
plot(st_rp$round, residuals, xlab = "round",
     ylab = "Residuals")
plot(st_rp$prop.c, residuals, xlab = "% Ag",
     ylab = "Residuals")
plot(st_rp$trmnt, residuals, xlab = "trmnt",
     ylab= "Residuals")
par(mfrow=c(1,1))
### variance not equal over rounds
## check histogram
hist(residuals, xlab = "Residuals", main = "")
## histogram of residuals looks pretty good
qqnorm(resid(sr_s))
qqline(resid(sr_s))


qqPlot(resid(sr_s))

### still two outliers.

#### Seed ratio averaging over round

sr_s2<-lmer(avg_sr~trmnt+prop.c+(1|site), data=st_rp2)
summary(sr_s2)
drop1(sr_s2,test="Chisq")

op <- par(mfrow = c(2, 2), mar = c(5, 4, 1, 2))
plot(sr_s2, add.smooth = FALSE)
residuals <- resid(sr_s2)
plot(residuals)
hist(residuals, xlab = "Residuals", main = "")
plot(st_rp2$prop.c, residuals, xlab = "% Ag",
     ylab = "Residuals")
plot(st_rp2$trmnt, residuals, xlab = "trmnt",
     ylab= "Residuals")
par(mfrow=c(1,1))

## Residuals don't look normally distributed based off histogram but everything else looks okay

qqnorm(resid(sr_s2))
qqline(resid(sr_s2))


qqPlot(resid(sr_s2))

### residuals again follow normal distribution, fall within CI region generated by car() 


### Total Ovules- Pollinated ovules
pl_s<-lmer(avg_seed~trmnt+prop.c+round+(1|site), data=st_rp)
summary(pl_s)
drop1(pl_s,test="Chisq")


### Total Ovules- Pollinated ovules averaged over round
pl_s2<-lmer(avg_polov~trmnt+prop.c+(1|site), data=st_rp2)
summary(pl_s2)
drop1(pl_s2,test="Chisq")



op <- par(mfrow = c(2, 2), mar = c(5, 4, 1, 2))
plot(pl_s2, add.smooth = FALSE)
residuals <- resid(pl_s2)
plot(residuals)
hist(residuals, xlab = "Residuals", main = "")
plot(st_rp2$prop.c, residuals, xlab = "% Ag",
     ylab = "Residuals")
plot(st_rp2$trmnt, residuals, xlab = "trmnt",
     ylab= "Residuals")
par(mfrow=c(1,1))

### heteroscedasticity in residuals at higher level of ag, trmnts look similar

qqnorm(resid(pl_s2))
qqline(resid(pl_s2))


qqPlot(resid(pl_s2))

###### Take away Higher levels of prop.c tended to exhibit heteroscedasticity--
###Talk with Dan, and think about relaxing assumptions 



