### This script provides summary information for various parts of the ms
install.packages("geosphere")
library(geosphere)
library(lme4)
library(nlme)
library(glmmTMB)
library(MASS)
library(car)
library(tidyverse)

#upload plant data(flowers treated, fruits removed, etc.)
pollination<-read.csv("data/pollination_2017_pp_plant_trait.csv")

#upload seed data merged with the environmental data
seed_land<-read.csv("data/seed_land.csv")
seed_land<-seed_land%>%filter(plot=="hi")

###mean, max, min distances between sites using geosphere
distm(c(sites$latitude,sites$longitude),fun=distHaversine)

site_coord=sites%>%select(longitude,latitude)

site_dist<-distm(site_coord,fun=distHaversine)
###
##data presented in ms under study design section
mean(site_dist)
max(site_dist)
min(site_dist)
max(sites$prop.c)
min(sites$prop.c)
mean(sites$prop.c)
sd(sites$prop.c)


#General summary information for the results section

# # of distinct plants we collected seed from
n_distinct(seed_land$ID)


# seed and fruit data summaryS


# Table of total fruit and seeds produced per plant 
# (same framework as plt_nr table in models code)
tot_seed_tbl<-seed_land%>%
  filter(plot=="hi")%>%
  group_by(trmnt,ID)%>%
  summarise(sum_seeds=sum(total_seeds), n_fruit=n(), seeds_fruit=sum_seeds/n_fruit)%>%
  group_by(trmnt)%>%
  summarise(n_plants=n(),total_fruit=sum(n_fruit),sd_fruit=sd(n_fruit),mean_seeds=mean(seeds_fruit),
            mean_fruit=mean(n_fruit), sum_seeds=sum(sum_seeds),sd_mean_seeds=sd(seeds_fruit),
            avg_sum_seeds_per_plant=mean(sum_seeds), med_sum_seeds_per_plant=median(sum_seeds))
# mean_seeds, n_plants, total_fruit, and mean_fruit 
# are reported in first paragraph of results

## 95CIs for mean # fruit per treatment; presented in results section of MS
### These are reported  in first paragraph of results
# sp 
u_sp <- 5.17
 sd_s <- 3.67
 n_s<- 62
 error_s <- qnorm(0.975)*sd_s/sqrt(n_s)
 left <- u_sp-error_s
 right <- u_sp+error_s
 left
error_s
 right
#OP
 u_op <- 4.25
 sd <- 2.81
 n <- 51
 error <- qnorm(0.975)*sd/sqrt(n)
 left <- u_op-error
 right <- u_op+error
 left
 error
 right
 
################################################################ 
#95Confidence interval for mean seeds/fruit 
 
 
 u_sp <- 8.68
 sd_s <- 2.17
 n_s<- 62
 error_s <- qnorm(0.975)*sd_s/sqrt(n_s)
 left <- u_sp-error_s
 right <- u_sp+error_s
 left
 error_s
 right
 #OP
 u_op <- 8.78
 sd <- 2.65
 n <- 51
 error <- qnorm(0.975)*sd/sqrt(n)
 left <- u_op-error
 right <- u_op+error
 left
 error
 right 
 
