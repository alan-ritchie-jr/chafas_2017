### tidyverse practice

library(tidyverse)
pollination<-read.csv("data/pollination_2017_pp_plant_trait.csv")
#upload seed data merged with the environmental data
seed_land<-read.csv("data/seed_land.csv")
seed_land<-seed_land%>%filter(plot=="hi")

######
# # of distinct plants used
pollination<-pollination%>%filter(plot=="hi")
n_distinct(pollination$ID) # of distinct plants

pollination%>% group_by(ID)%>% summarize(n_visits=n())# # of times treated
# # of distinct plants collected from
n_distinct(seed_land$ID)

# of plants with data
# table of treatment results
seed_tbl<-seed_land%>%
  filter(plot=="hi")%>% # select only the hi density plots
  group_by(trmnt)%>%
  summarise(n_plants=n_distinct(ID),n_fruit=n(),avg_seed=mean(total.seeds),median=median(total.seeds), 
                    sd=sd(total.seeds), var=var(total.seeds))
seed_tbl
mean(seed_tbl$n_fruit)
nrow(seed_land)


tot_seed_tbl<-seed_land%>%
  filter(plot=="hi")%>%
  group_by(trmnt,ID)%>%
  summarise(sum_seeds=sum(total.seeds), n_fruit=n())%>%
  group_by(trmnt)%>%
  summarise(n_plants=n(),total_fruit=sum(n_fruit),
            avg_n_fruit=mean(n_fruit), total_seeds=sum(sum_seeds),avg_sum_seeds=mean(sum_seeds), 
            seeds_fruit=total_seeds/total_fruit,
            avg_seeds_fruit=avg_sum_seeds/avg_n_fruit)


# table of flowers treated, fruit recovered, and %fruit loss
flower_tbl<-seed_land%>%
  filter(plot=="hi")%>%
  group_by(trmnt,ID)%>%
  summarize(n_fruit=n(), n_flowers=sum(flowers))%>%
  group_by(trmnt)%>%
    summarise(n_fruit=sum(n_fruit),n_flowers=sum(n_flowers),
              frt_loss=1-(n_fruit/n_flowers))
ggplot(flower_tbl, aes(trmnt ,frt_loss, color=trmnt)) +geom_jitter()+ 
  labs(x="Treatment" ,y="Fruit Loss")+
  ggtitle("Average Fruit Loss by Treatment")

mean(flower_tbl$n_flowers)
median(flower_tbl$n_fruit)

median(flower_tbl$frt_loss)

1-(mean(flower_tbl$n_fruit/mean(flower_tbl$n_flowers)))


  ### Pollinated Ovules /  Fruit

trmnt_tbl<-seed_land%>%
  group_by(round, ID,trmnt)%>%
  summarise(n_fruit=n(),n_flowers=sum(unique(flowers)),
            frt_loss=(1-(n_fruit/n_flowers)))%>% 
  group_by(ID,trmnt)%>%
  summarise(n_fruit=sum(n_fruit),n_flowers=sum(n_flowers),
            frt_loss=1-(n_fruit/n_flowers))=
  group_by(trmnt)%>%
  summarise(n_fruit=sum(n_fruit),n_flowers=sum(n_flowers),
            frt_loss=(1-(n_fruit/n_flowers))) 
  
ggplot(trmnt_tbl, aes(trmnt ,frt_loss, color=trmnt)) +geom_jitter()+ 
  labs(x="Treatment" ,y="Fruit Loss")+
  ggtitle("Average Fruit Loss by Treatment")


# table of mean, median, variance in poll_ovules

### These numbers seem to match Lee & Bazzaz 1982:
### Control plants in their study (no fruit removal) matured fruit w/ avg 11.3 ov and 11.1 devo seeds
### Plants with highest level of fruit removal (80% fruit removed) matured fruit w/ avg 9.9 ov and 8.2 seeds

### Other nuggets of info:
### We collected more HP fruit than OP fruit
### OP fruit had, on average, .5 more ovules than HP fruit.


# Pollinated Ovules/Total Ovules / Fruit

pollratio_tbl<-summarise(seed_land,fruit_count=n(), avg_pol=mean(poll_ovules/total.ovules), med_pol=median(poll_ovules/total.ovules),
                         var_pol=var(poll_ovules/total.ovules), sd_pol=sd(poll_ovules/total.ovules), 
                         coef_var_pol=(sd_pol/avg_pol))

pollratio_tbl



# Pollen Limitation (Total Ovules - Pollinated Ovules) / Fruit


pl_tbl<-summarise(seed_land,n_fruit=n(), avg=mean(poll_lim),median=median(poll_lim), 
                  sd=sd(poll_lim), var=var(poll_lim),coef_var=sd/avg)
pl_tbl




############
##### Site Response Variable Tables


#Total Seeds / Site

#Seeds collected per site

seed_site_total<- seed_land %>%
  group_by(site) %>%
  summarise(fruit_count = n(), seeds=sum(total.seeds),avg_seeds=mean(total.seeds), med_seeds=median(total.seeds),
            sd_seeds=sd(total.seeds), var_seeds=var(total.seeds))
seed_site_total

### seeds collected per site, round, and treatment

seed_site<- seed_land %>%
  group_by(site,prop.c,round,trmnt) %>%
  summarise(fruit_count = n(), seeds=sum(total.seeds),avg_seeds=mean(total.seeds), med_seeds=median(total.seeds),
            sd_seeds=sd(total.seeds), var_seeds=var(total.seeds))

seed_site



#Pollen Limitation / Site Table + Scatter plots

#Degree of pollen limitation (Poll ovules - total ovules) per fruit at the site level

### Pollen Limitation by site and round
pl_site<- seed_land %>%
  group_by(site,prop.c,trmnt) %>%
  summarise(fruit_count = n(), avg_pl=mean(poll_lim), med_pl=median(poll_lim),
            var_pl=var(poll_lim), sd_pl=sd(poll_lim), coef_var_pl=(sd_pl/avg_pl))


#ggplot(pl/site, aes(reorder(site,prop.c), avg_pl)) + geom_violin(draw_quantiles=.5) +ggtitle("avgPL by Site")
ggplot(pl_site, aes(prop.c,avg_pl,color=trmnt)) +geom_jitter() +
  ggtitle("Avg PL by % Development")+geom_smooth(method=lm,se=FALSE)

ggplot(pl_site, aes(prop.c,med_pl, color=trmnt)) +geom_point() +
  ggtitle("Med PL by % Development") +geom_smooth(method=lm,se=FALSE)


ggplot(pl_site, aes(prop.c,var_pl, color=trmnt)) +geom_point()+
  geom_smooth(method=lm, se=FALSE) +ggtitle("Variance in PL by % Development")


ggplot(pl_site, aes(prop.c,coef_var_pl)) +geom_point(aes(color=site)) +ggtitle("CV of PL by % Development")



####
# Pollinated Ovules / Site Table

#Number Ovules pollinated in a fruit at the site level

####
pol_ov_site<- seed_land %>%
  group_by(site,prop.c) %>%
  summarise(fruit_count = n(), avg_pol=mean(poll_ovules), med_pol=median(poll_ovules),
            var_pol=var(poll_ovules), sd_pol=sd(poll_ovules), coef_var_pol=(sd_pol/avg_pol))


ggplot(pol_ov_site, aes(prop.c,avg_pol)) +geom_point(aes(color=site)) +ggtitle("Avg Pol Ov by % Development")

ggplot(pol_ov_site, aes(prop.c,med_pol)) +geom_point(aes(color=site)) +ggtitle("Med Poll Ov by % Development")

ggplot(pol_ov_site, aes(prop.c,var_pol)) +geom_point(aes(color=site)) +ggtitle("Variance in Pol Ov by % Development")

ggplot(pol_ov_site, aes(prop.c,coef_var_pol)) +geom_point(aes(color=site)) +ggtitle("CV of Pol Ov by % Development")


# Pollinated Ovule/Virgin Ovules / Site & % Developed

#The proportion of available ovules pollinated in a fruit at the site level


polr_Site<- seed_land %>%
  group_by(site,prop.c,trmnt) %>%
  summarise(fruit_count=n(), avg_pol=mean(poll_ovules/total.ovules), med_pol=median(poll_ovules/total.ovules),
            var_pol=var(poll_ovules/total.ovules), sd_pol=sd(poll_ovules/total.ovules), 
            coef_var_pol=(sd_pol/avg_pol))



ggplot(pol_ratio_site , aes(prop.c,avg_pol)) +geom_point(aes(color=site))  

+ggtitle("Avg Pol Ratio by % Development")

ggplot(pol_ratio_site, aes(prop.c,med_pol)) +geom_point(aes(color=site)) +ggtitle("Med Poll Ov Ratio by % Development")

ggplot(pol_ratio_site, aes(prop.c,var_pol)) +geom_point(aes(color=site)) +ggtitle("Variance in Pol Ov Ratio by % Development")

ggplot(pol_ratio_site, aes(prop.c,coef_var_pol)) +geom_point(aes(color=site)) +ggtitle("CV of Pol Ov Ratio by % Development")           

######################################
# Big ole tables of response variables-->
#  Treatment/Site & % Developed
 # & Treatment/Site x Round & % Devo
####################################

### The ratio of pollinated ovules/total ovules and total - pollinated responses are decreasing 
### with increasing agricultural development around site.
### let's look at how treatments are influencing these results

### First let's build a big table including all response variables

#Running avg, med, var for each response variable:
# Pollinated Ovules/TOtal Ovules (pr), TotOv-PolOv (Topo), pollinated ovules (polov), and seed (seed)
### Maybe add total.seeds/total.ovules (so)??


trmnt_sitexx<- seed_land %>%# initially group by ID
  group_by(site,prop.c,plot,trmnt,ID) %>%
  summarise(fruit_count=n(), avg_pr=mean(poll_ovules/total.ovules), med_pr=median(poll_ovules/total.ovules),
            var_pr=var(poll_ovules/total.ovules), avg_topo=mean(total.ovules-poll_ovules),
           var_topo=var(total.ovules-poll_ovules),
            avg_polov=mean(poll_ovules), var_polov=var(poll_ovules), 
            avg_seed=mean(total.seeds), var_seed= var(total.seeds),avg_so=mean(total.seeds/total.ovules),
           var_so=var(total.seeds/total.ovules))%>%
#Now filter out plant with 1 observation, as these observations may skew the data
filter(fruit_count>1) %>%
  filter(plot=="hi")%>%
  group_by(site, prop.c,plot,trmnt)%>%
  summarise(fruit_count=sum(fruit_count), avg_pr=mean(avg_pr), med_pr=median(med_pr), 
            var_pr=mean(var_pr), avg_topo=mean(avg_topo), var_topo=mean(var_topo), 
            avg_polov=mean(avg_polov), var_polov=mean(var_polov), 
            avg_seed=mean(avg_seed), var_seed= mean(var_seed), avg_so=mean(avg_so), var_so=mean(var_so))


### Same table, but keep plants with 1 fruit
trmnt_site_1<- seed_land %>%# initially group by ID
  group_by(site,prop.c,plot,trmnt) %>%
  summarise(fruit_count=n(), avg_pr=mean(poll_ovules/total.ovules), med_pr=median(poll_ovules/total.ovules),
            var_pr=var(poll_ovules/total.ovules), avg_topo=mean(total.ovules-poll_ovules),
            var_topo=var(total.ovules-poll_ovules),
            avg_polov=mean(poll_ovules), var_polov=var(poll_ovules), 
            avg_seed=mean(total.seeds), var_seed= var(total.seeds), avg_so=mean(total.seeds/total.ovules),
            var_so=var(total.seeds/total.ovules))

#### Same tables, but with round also included

trmnt_site_rd<- seed_land %>%# initially group by ID
  group_by(site,round, prop.c,trmnt,ID) %>%
  summarise(fruit_count=n(), avg_pr=mean(poll_ovules/total.ovules), med_pr=median(poll_ovules/total.ovules),
            var_pr=var(poll_ovules/total.ovules), avg_topo=mean(total.ovules-poll_ovules),
            var_topo=var(total.ovules-poll_ovules),
            avg_polov=mean(poll_ovules), var_polov=var(poll_ovules), 
            avg_seed=mean(total.seeds), var_seed= var(total.seeds), avg_so=mean(total.seeds/total.ovules),
            var_so=var(total.seeds/total.ovules))%>%
  #Now filter out plant with 1 observation, as these observations may skew the data
  filter(fruit_count>1) %>%
  filter(plot=="hi")%>%
  group_by(site,round, prop.c,trmnt)%>%
  summarise(fruit_count=sum(fruit_count), avg_pr=mean(avg_pr), med_pr=median(med_pr), 
            var_pr=mean(var_pr), avg_topo=mean(avg_topo), var_topo=mean(var_topo), 
            avg_polov=mean(avg_polov), var_polov=mean(var_polov), 
            avg_seed=mean(avg_seed), var_seed= mean(var_seed),avg_so=mean(avg_so), var_so=mean(var_so))


### Same table, but keep plants with 1 fruit
trmnt_site_rd_1<- seed_land %>%# initially group by ID
  group_by(site,round,prop.c,trmnt) %>%
  summarise(fruit_count=n(), avg_pr=mean(poll_ovules/total.ovules), med_pr=median(poll_ovules/total.ovules),
            var_pr=var(poll_ovules/total.ovules), avg_topo=mean(total.ovules-poll_ovules),
            var_topo=var(total.ovules-poll_ovules),
            avg_polov=mean(poll_ovules), var_polov=var(poll_ovules), 
            avg_seed=mean(total.seeds), var_seed= var(total.seeds),avg_so=mean(total.seeds/total.ovules),
            var_so=var(total.seeds/total.ovules))

####################
#### Plots ####
########################
######

### Site only plots#####

#########
### Polr

### Plot of avg polr
ggplot(trmnt_sitexx, aes(prop.c,avg_pr, color=trmnt)) +geom_point()+
  ggtitle("Avg Pol Ratio by % Development") +geom_smooth(method=lm,se=FALSE)
### Intercept HP ~ .85, Intercept OP ~.9 
### Slope is slightly lower in op
### As prop.c increases avg polr (pollinated ovules/total Ovules) decreases
### this decrease is more intense in op but never crosses hp line
### Adding CIs shows significant overlap
### PR starts at difference of ~.05 between hp and op
### Or ~.55 fewer ovules for hp fruit
### This reduces down to a ~.01-.02 difference in hp and op PR at high develpment
### Or ~.11-.22 fewer ovules for hp fruit.


### Now compare if we drop single fruits
ggplot(trmnt_site, aes(prop.c,avg_pr, color=trmnt)) +geom_point()+
  ggtitle("Avg Pol Ratio by % Development") +geom_smooth(method=lm,se=FALSE)
### Now slopes are effectively equal. OP intercept is a little lower as well.
### Interpretation: pr in hp is ~ .03-.05 less than op. 
### There were ~11 total ovules per fruit, so this works out to ~.33-.55 fewer pollinated ovules in HP fruit
###
### Plot of var polr without dropping single fruit plants  
ggplot(trmnt_site_1, aes(prop.c,var_pr, color=trmnt)) +geom_point() +
  ggtitle("Variance in Pol Ov Ratio by % Development") +geom_smooth(method=lm, se=FALSE)
### Intercept higher for hp, indicating treating flowers added a small amount of variability
### but slope higher in op, leading to higher variability at higher prop.c
### So, prop.c had a marginally stronger effect on op plant PR. 
### had there been pollen limitation we would notice either a sharper slope for OP or a higher intercept
### in general variability seems low.
##


ggplot(trmnt_site, aes(prop.c,var_pr, color=trmnt)) +geom_point() +
  ggtitle("Variance in Pol Ov Ratio by % Development") +geom_smooth(method=lm, se=FALSE)

##Slope in HP plants almost 0. 
#Indicates there was just some variability inherent for treating across all propc
## Slope in op higher. Indicates variability grows with increasing prop.c, approaching variability in treating
# Intercepts ~ .043 HP and ~.0225 OP.
## Biggest change was to Staples, which seems to have had many single fruit plants 
# Takeaway: The amount of variability added by treatment exceeds that created by the gradient
# Prop. c didn't influence HP plants, indicating HP treatments were likely consistently effective
# BUT they seem to have been marginally detrimental to pollination of ovules.

###############
### Seed Set


ggplot(trmnt_site_1, aes(prop.c,avg_seed, color=trmnt)) +geom_point()+
  ggtitle("Avg Seed per fruit by % Development") +geom_smooth(method=lm,se=FALSE)
### INTERP:


### Now compare if we drop single fruits
ggplot(trmnt_sitexx, aes(prop.c,avg_seed, color=trmnt)) +geom_point()+
  ggtitle("Avg Seed per fruit by % Development") +geom_smooth(method=lm,se=FALSE)
### INTERP: ~.5 fewer seeds in hp at lowest pop.c, to 0 difference at highest prop.c
### HP treatment consistent, but either depressed sed set slightly 
### OR plant level effects and low sample size are skewing prop.c effect (probs a little of both)

ggplot(trmnt_site_1, aes(prop.c,var_seed, color=trmnt)) +geom_point() +
  ggtitle("Variance in seed per fruitRatio by % Development") +geom_smooth(method=lm, se=FALSE)
### INTERP: var same for op & hp at intercept but slope higher for op.
### hp treatment doesn't mute the effect of prop.c, small sample size may be skewing +plant level effects


ggplot(trmnt_site, aes(prop.c,var_seed, color=trmnt)) +geom_point() +
  ggtitle("Variance in seed/fruit % Development") +geom_smooth(method=lm, se=FALSE)
### Interp:
### Doesn't seem to be a relationship. lots of variability in one site.
### 


#########
##### topo 


ggplot(trmnt_site_1, aes(prop.c,avg_topo, color=trmnt)) +geom_point()+
  ggtitle("Avg total - pollinated ovules per fruit by % Development") +geom_smooth(method=lm,se=FALSE)
### INTERP: hp treatment fruit ~.4-.5 fewer ovules at low prop.c to .1 or less at high.


### Now compare if we drop single fruits
ggplot(trmnt_sitexx, aes(prop.c,avg_topo, color=trmnt)) +geom_point()+
  ggtitle("Avg total - pollinated ovules per fruit by % Development") +geom_smooth(method=lm,se=FALSE)
### INTERP: slopes decreased after dropping single fruits
### hp treatment fruit cosistently .3-.4 ovules fewer than op fruit

ggplot(trmnt_site_1, aes(prop.c,var_topo, color=trmnt)) +geom_point() +
  ggtitle("Variance in total- pollinated ovules per fruit by % Development") +geom_smooth(method=lm, se=FALSE)
### INTERP: Var increases for both treatments, slightly quicker for op.

ggplot(trmnt_site, aes(prop.c,var_topo, color=trmnt)) +geom_point() +
  ggtitle("Variance in Total -pollinated ovules per fruit by % Development") +geom_smooth(method=lm, se=FALSE)

### Interp: var higher in hp by a large amount once you get rid of singletons...
### topo may follow negative binomial; plot looks similar to averages
### so higher levels of topo have a higher level of variability **in general**
### We'll see if this holds


#####
## Polov


ggplot(trmnt_site_1, aes(prop.c,avg_polov, color=trmnt)) +geom_point()+
  ggtitle("Avg pollinated ovules per fruit by % Development") +geom_smooth(method=lm,se=FALSE)
### INTERP: 9 pollinated ovules per fruit in hp plants versus 10 at sites with low development
## much sharper slope in op plants


### Now compare if we drop single fruits
ggplot(trmnt_sitexx, aes(prop.c,avg_polov, color=trmnt)) +geom_point()+
  ggtitle("Avg pollinated ovules by % Development") +geom_smooth(method=lm,se=FALSE)
### INTERP: slopes decreased after dropping single fruits
### almost complete flat slope in hp fruits; about 8.7 ovules per fruit at all levels of prop.c
### compared to op frits that have about 9.6 ovules per fruit down to 8.7 at high prop.c 
### treatments were consistent, but slightly depressed seed set 
### OR site level effects are skewing that level of development
### OR plant level effects and who the fruit are coming form is drivng data
###

ggplot(trmnt_site_1, aes(prop.c,var_polov, color=trmnt)) +geom_point() +
  ggtitle("Variance in pollinated ovules by % Development") +geom_smooth(method=lm, se=FALSE)
### INTERP: var same for op & hp at intercept but slope higher for op.
### hp treatment doesn't mute the effect of prop.c, small sample size may be skewing +plant level effects


ggplot(trmnt_site, aes(prop.c,var_polov, color=trmnt)) +geom_point() +
  ggtitle("Variance in pollinated ovules by % Development") +geom_smooth(method=lm, se=FALSE)
### Interp: variance starts out low in op plants but shoots up over that of hp plants






###Site X Round plots

### topo

ggplot(trmnt_site_rd_1, aes(prop.c,avg_topo, color=trmnt)) +geom_point()+facet_grid(.~round)+
  ggtitle("Avg total - pollinated ovules per fruit by % Development") +geom_smooth(method=lm,se=FALSE)
### INTERP: All over the damn place


### Now compare if we drop single fruits
ggplot(trmnt_site_rd, aes(prop.c,avg_topo, color=trmnt)) +geom_point()+facet_grid(.~round)+
  ggtitle("Avg total - pollinated ovules per fruit by % Development") +geom_smooth(method=lm,se=FALSE)
### INTERP: 

ggplot(trmnt_site_rd_1, aes(prop.c,var_topo, color=trmnt)) +geom_point() +facet_grid(.~round)+
  ggtitle("Variance in total- pollinated ovules per fruit by % Development") +geom_smooth(method=lm, se=FALSE)
### INTERP: Variance in rd one hp fruit declines at higher values of prop.

ggplot(trmnt_site_rd, aes(prop.c,var_topo, color=trmnt)) +geom_point() +facet_grid(.~round)+
  ggtitle("Variance in Total -pollinated ovules per fruit by % Development") +geom_smooth(method=lm, se=FALSE)

########
### Polr

### Plot of avg polr


ggplot(trmnt_site_rd_1, aes(prop.c,avg_pr, color=trmnt)) +geom_point()+ facet_grid(.~round)+
  ggtitle("Avg Pol Ratio by % Development") +geom_smooth(method=lm,se=FALSE)

### 

### Now compare if we drop single fruits
ggplot(trmnt_site_rd, aes(prop.c,avg_pr, color=trmnt)) +geom_point()+ facet_grid(.~round)+
  ggtitle("Avg Pol Ratio by % Development (Drop Single Fruits)") +geom_smooth(method=lm,se=FALSE)



### Plot of var polr without dropping single fruit plants  
ggplot(trmnt_site_rd_1, aes(prop.c,var_pr, color=trmnt)) +geom_point() +facet_grid(.~round)+
  ggtitle("Variance in Pol Ov Ratio by % Development") +geom_smooth(method=lm, se=FALSE)



ggplot(trmnt_site_rd, aes(prop.c,var_pr, color=trmnt)) +geom_point() +facet_grid(.~round)+
  ggtitle("Variance in Pol Ov Ratio by % Development (Drop Single Fruits)") +geom_smooth(method=lm, se=FALSE)

# Variance very high in round 4, particularly in op fruit
# Variance in general low
#
ggplot(trmnt_site_rd_1, aes(prop.c,avg_seed, color=trmnt)) +geom_point()+facet_grid(.~round)+
  ggtitle("Avg Seed per fruit by % Development") +geom_smooth(method=lm,se=FALSE)
### INTERP:



### Now compare if we drop single fruits
ggplot(trmnt_site_rd, aes(prop.c,avg_seed, color=trmnt)) +geom_point()+facet_grid(.~round)+
  ggtitle("Avg Seed per fruit by % Development") +geom_smooth(method=lm,se=FALSE)


ggplot(trmnt_site_rd_1, aes(prop.c,var_seed, color=trmnt)) +geom_point() +facet_grid(.~round)+
  ggtitle("Variance in seed per fruitRatio by % Development") +geom_smooth(method=lm, se=FALSE)


ggplot(trmnt_site_rd, aes(prop.c,var_seed, color=trmnt)) +geom_point() +
  ggtitle("Variance in seed/fruit % Development") +geom_smooth(method=lm, se=FALSE)





#########################################
#########################################


#######
### Plant Level Response Variables==> generate tables and pass models
####

### Diff in HP-OP####

##First we need to generate averages and totals for each plot for each response variable we're interested in
HPOP_plant<-seed_land %>%
  group_by(prop.c,site,plot, trmnt)%>% 
  summarise(fruit_count=n(),seed_total=sum(total.seeds), avg_seeds=mean(total.seeds),
            total_pol=sum(poll_ovules),avg_pol=mean(poll_ovules))%>%

  filter(plot=="hi")

HPOP_plot<-HPOP_plant %>%
summarise(seed_diff= (seed_total[trmnt=="hp"])-(seed_total[trmnt=="op"]), 
          avg_seed_diff=(avg_seeds[trmnt=="hp"])-(avg_seeds[trmnt=="op"]),
          pollov_diff=(total_pol[trmnt=="hp"])-(total_pol[trmnt=="op"]),
          avg_pollov_diff=(avg_pol[trmnt=="hp"])-(avg_pol[trmnt=="op"] ))
   
### DROP LO DENSITY FRUIT        
##First we need to generate averages and totals for each plot for each response variable we're interested in
HPOP_droplo<-seed_land %>%
  group_by(prop.c,site,plot,trmnt,ID)%>% 
  summarise(fruit_count=n(),seed_total=sum(total.seeds), avg_seeds=mean(total.seeds),
            total_pol=sum(poll_ovules),avg_pol=mean(poll_ovules)) %>%
  #Now we need to pass this table to a new argument that will subtract the OP row from the HP row 
  #(or vice versa)
  # We should drop the low density treatment fruit from this to make the  observations more consistent
  filter(plot=="hi")%>%
  summarise(seed_diff= (seed_total[trmnt=="hp"])-(seed_total[trmnt=="op"]), 
            avg_seed_diff=(avg_seeds[trmnt=="hp"])-(avg_seeds[trmnt=="op"]),
            pollov_diff=(total_pol[trmnt=="hp"])-(total_pol[trmnt=="op"]),
            avg_pollov_diff=(avg_pol[trmnt=="hp"])-(avg_pol[trmnt=="op"] ))




### intact data set
ggplot(HPOP_plot, aes(prop.c,avg_seed_diff)) +geom_point()+
  ggtitle("Avg diff in seed between trmnts by % Development") +geom_smooth(method=lm,se=FALSE)

### droplow density
ggplot(HPOP_droplo, aes(prop.c,avg_seed_diff)) +geom_point()+
  ggtitle("Avg diff in seed between trmnts by % Development (Drop low)") +geom_smooth(method=lm,se=FALSE)

### intact data set
ggplot(HPOP_plot, aes(prop.c,avg_pollov_diff)) +geom_point()+
  ggtitle("Avg diff in pollovs between trmnts by % Development") +geom_smooth(method=lm,se=FALSE)

### droplow density
ggplot(HPOP_droplo, aes(prop.c,avg_pollov_diff)) +geom_point()+
  ggtitle("Avg diff in pollovs between trmnts by % Development (Drop low)") +geom_smooth(method=lm,se=FALSE)







 #seeds_op=sum(total.seeds [which(seed_land$trmnt=="op"),]), HPOP=seeds_hp-seeds_op)

ggplot(seed_plant,aes(trmnt,avg_seeds))+geom_violin(draw_quantiles=.5)
ggplot(seed_plant,aes(trmnt,med_seeds))+geom_violin(draw_quantiles=.5)

ggplot(seed_plant,aes(prop.c,med_seeds))+geom_jitter(aes(color=site))
ggplot(seed_plant,aes(prop.c,avg_seeds))+geom_jitter(aes(color=site))
ggplot(seed_plant,aes(prop.c,var_seeds))+geom_jitter(aes(color=site))
ggplot(seed_plant,aes(prop.c,CV_seeds))+geom_jitter(aes(color=site))


#Total seeds collected / plant


seed_plant<- seed_land %>%
  group_by(prop.c,site,ID, trmnt)%>%
  summarise(fruit_count=n(),seed_total=sum(total.seeds), avg_seeds=mean(total.seeds), med_seeds=median(total.seeds), 
            var_seeds=var(total.seeds), sd_seeds=sd(total.seeds), CV_seeds=sd_seeds/avg_seeds)

ggplot(seed_plant,aes(trmnt,avg_seeds))+geom_violin(draw_quantiles=.5)
ggplot(seed_plant,aes(trmnt,med_seeds))+geom_violin(draw_quantiles=.5)

ggplot(seed_plant,aes(prop.c,med_seeds))+geom_jitter(aes(color=site))
ggplot(seed_plant,aes(prop.c,avg_seeds))+geom_jitter(aes(color=site))
ggplot(seed_plant,aes(prop.c,var_seeds))+geom_jitter(aes(color=site))
ggplot(seed_plant,aes(prop.c,CV_seeds))+geom_jitter(aes(color=site))



#Pollinated Ovules / Plant table


### Pollinated ovules x plant table
pollov_plant<-seed_land %>%
  group_by(site,round, prop.c,ID,trmnt)%>%
  summarise(fruit_count=n(), avg_pol=mean(poll_ovules), med_pol=median(poll_ovules),
            var_pol=var(poll_ovules), sd_pol=sd(poll_ovules), cv_pol=(sd_pol/avg_pol))

ggplot(pollov_plant, aes(trmnt,med_pol)) +geom_violin(draw_quantiles=.5) +ggtitle("Plant lvl Avg of Pol Ovs by treatment")

ggplot(pollov_plant, aes(trmnt,med_pol)) +geom_violin(draw_quantiles=.5) +ggtitle("Plant lvl median of Pol Ovs by treatment")

ggplot(pollov_plant, aes(trmnt,var_pol)) +geom_violin(draw_quantiles=.5) +ggtitle("Var Estimate of Pol Ovs by treatment")
ggplot(pollov_plant, aes(trmnt,cv_pol)) +geom_violin(draw_quantiles=.5) +ggtitle("Cv of Pol Ovs by treatment")

ggplot(pollov_plant, aes(prop.c,avg_pol)) +geom_jitter(aes(color=site)) 
ggplot(pollov_plant, aes(prop.c,med_pol)) +geom_jitter(aes(color=site)) 

ggplot(pollov_plant, aes(prop.c,var_pol)) +geom_jitter(aes(color=site)) 

ggplot(pollov_plant, aes(prop.c,cv_pol)) +geom_jitter(aes(color=site))


#Pollinated Ovule Ratio / Plant table

pol_ratio_plant<-seed_land %>%
  group_by(site,round, prop.c, ID,trmnt)%>% 
  summarise(fruit_count=n(), avg_pol=mean(poll_ovules/total.ovules), med_pol=median(poll_ovules/total.ovules),
            var_pol=var(poll_ovules/total.ovules), sd_pol=sd(poll_ovules/total.ovules), 
            coef_var_pol=sd_pol/avg_pol)

ggplot(pol_ratio_plant, aes(trmnt,avg_pol)) +geom_violin(draw_quantiles=.5)+
  ggtitle(" Mean Ratio of Pollinated Ovules per Treatment")

ggplot(pol_ratio_plant, aes(trmnt,avg_pol)) +geom_violin(draw_quantiles=.5)+
  ggtitle(" Median Ratio of Pollinated Ovules per Treatment")

ggplot(pol_ratio_plant, aes(trmnt,avg_pol)) +geom_violin(draw_quantiles=.5)+
  ggtitle(" Var of Ratio of Pollinated Ovules per Treatment")

ggplot(pol_ratio_plant, aes(trmnt,avg_pol)) +geom_violin(draw_quantiles=.5)+
  ggtitle(" CV of Ratio of Pollinated Ovules per Treatment")

ggplot(pol_ratio_plant, aes(prop.c,avg_pol)) +geom_jitter(aes(color=site))+
  ggtitle("Change in Mean Ratio of Pollinated Ovule with % Developed")
ggplot(pol_ratio_plant, aes(prop.c,med_pol)) +geom_jitter(aes(color=site))+
  ggtitle("Change in Median Ratio of Pollinated Ovule with % Developed")
ggplot(pol_ratio_plant, aes(prop.c,var_pol)) +geom_jitter(aes(color=site))+
  ggtitle("Change in Ratio of Pollinated Ovule Variance with % Developed")
ggplot(pol_ratio_plant, aes(prop.c,coef_var_pol)) +geom_jitter(aes(color=site)) +
  ggtitle("Change in Ratio of Pollinated Ovule CV with % Developed")



#Pollen Limitation / Plant table


pl_plant<-seed_land%>%
  group_by(site,round, prop.c,ID,trmnt)%>%
  summarise(frt_coll=n(),trtd_flwrs=sum(unique(flowers)),frt_flw_ratio= frt_coll/trtd_flwrs,
            avg_pl=mean(poll_lim), med_pl=median(poll_lim),
            sd_pl=sd(poll_lim), var_pl=var(poll_lim), cv_pl=(sd_pl/avg_pl))

pl_plant%>%
  ggplot( aes(trmnt,avg_pl)) +geom_violin(draw_quantiles=.5)+
  ggtitle(" Mean Pollen Limitation per Treatment")

pl_plant%>%
  ggplot(aes(trmnt,med_pl)) +geom_violin(draw_quantiles=.5)+
  ggtitle(" Median Pollen Limitation per Treatment")

pl_plant%>%
  ggplot( aes(trmnt,var_pl)) +geom_violin(draw_quantiles=.5)+
  ggtitle(" Var of Pollen Limitation per Treatment")

pl_plant%>%
  ggplot( aes(trmnt,cv_pl)) +geom_violin(draw_quantiles=.5)+
  ggtitle(" CV of Pollen Limitation per Treatment")
pl_plant%>%
  ggplot( aes(prop.c,avg_pl)) +geom_jitter(aes(color=site))+
  ggtitle("Change in Mean PL with % Developed")

pl_plant%>%
  ggplot( aes(prop.c,med_pl)) +geom_jitter(aes(color=site))+
  ggtitle("Change in Median PL with % Developed")

pl_plant%>%
  ggplot( aes(prop.c,var_pl)) +geom_jitter(aes(color=site))+
  ggtitle("Change in PL Variance with % Developed")

pl_plant%>%
  ggplot(aes(prop.c,cv_pl)) +geom_jitter(aes(color=site)) +
  ggtitle("Change in PL CV with % Developed")


### HP- OP 



#purrr
##### Not sure if this is actually useful without a bit more work on manipulating the table
# let's group fruits by plant

n_distinct(seed_land$ID) # # of unique plants we collected seeds from
n_seeds <- seed_land %>% # pipeline passes outpute from this line of code to next line, simplifying code
  group_by(ID) %>% #I've passed the seed_land df to this line, and would like to group it by the unique ID of the plant
  mutate(.,coll_fruits=n())%>% # number of fruit collected per plant
  nest() 
nrow(n_seeds) # should match n_distinct from seed_land df


#use unnest to pull the nested dataframe back into shape.


##m_seeds is a nested data frame containing all of the fruit seed count data for a given plant'


### # of fruit collected at each site, the average number of fruit per plant, median, etc.

###plot fruit count data for each treatment and round!


ggplot(m_seeds,aes(trmnt,(coll_fruits)))+geom_violin(aes(color=trmnt)) 



#################
# We can aslo pass n_seeds through a function using mutate and map to count fruit per plant
count_f <- function(df)
  {count=nrow(df)} ## function that counts number of fruit for a given plant

m_seeds <- n_seeds%>%
  mutate(coll_fruit = map(data, count_f))%>%
  nest()# use mutate and map to apply function to n_seeds
length(m_seeds$coll_fruit) #length should match number of rows from n_seeds

### However, this creates a separate column not included within the original nested dataframe.





