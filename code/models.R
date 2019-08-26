###################
###Seed models####
#################
#install.packages("glmmTMB")
#install.packages("ggeffects")
#install.packages("sjPlot")
#install.packages("car")
#install.packages("lme4")
#install.packages("Matrix")
#install.packages("snakecase")
#install.packages("DHARMa")
#install.packages("optimx")
#install.packages("simr")
####



library(car)
library(MASS)
library(glmmTMB)
library(lme4)
library(tidyverse)
library(sjPlot)
library(ggeffects)
library(sjlabelled)
library(snakecase)
library(DHARMa)

### run merge_clean first

#seed_land can be pulled from the data subfolder or generated as a df in merge_clean
seed_land<-read.csv("data/seed_land.csv")


# summary table of plant
plt_seed<- seed_land %>%mutate( trmnt = fct_recode(trmnt, "Supplemental Pollination" = "hp", "Open Pollination"="op"), 
                              seed_ov= total_seeds/total_ovules)%>%
  group_by(site,prop.c,trmnt,ID,round)%>%#calculate fruit removed
  mutate(frt_removed_rd=sum(frt_removed)/n(), trt_flw_rd=sum(flowers)/n())%>%
  ungroup()%>%
  group_by(site,prop.c,trmnt,ID)%>%
  summarise(frt=n(),trt_flw=sum(trt_flw_rd),tot_frt_removed=sum(frt_removed_rd),
            ab_ov=sum(abrt_ovules),
            nopol_ov=sum(nofert_ovules)+sum(abrt_ovules),
            seeds=sum(total_seeds),
            mean_seed=mean(total_seeds),
            mean_seed_ov=mean(seed_ov),
            totov=sum(total_ovules),
            mean_totov=mean(total_ovules),
            seeds_frt=seeds/frt,mean_sf=mean(seeds/frt))


#Post-hoc t.test for difference in mean untreated frt remvoal between treatment

t.test(tot_frt_removed~trmnt,data=plt_seed)
t.test(trt_flw~trmnt,data=plt_seed)
#
summary(lm(tot_frt_removed~prop.c,data=plt_seed))
         
############
#   models #
###########

## which distributions are appropriate for the data?
qqp(plt_seed$mean_seed, "norm")# avg seed per fruit looks pretty good

nbinom <- fitdistr(plt_seed$seeds, "Negative Binomial")
qqp(plt_seed$seeds, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])
#count data also look good for nb distribution

####
###Models 1&2:
#mean seed per fruit response

#model 1:
#using glmmTMB
seed_mean_mod_TMB<-glmmTMB(mean_seed~trmnt*prop.c +(1|site), data=plt_seed) 
summary(seed_mean_mod_TMB)#reported in table 1

#using lme4
seed_mean_mod<-lmer(mean_seed~trmnt*prop.c +(1|site), data=plt_seed)

summary(seed_mean_mod)


###model 2:
#no interaction term
seed_mean_mod2_TMB<-glmmTMB(mean_seed~trmnt+prop.c +(1|site), data=plt_seed) 
summary(seed_mean_mod2_TMB)#reported in table 2

seed_mean_mod2<-lmer(mean_seed~trmnt+prop.c +(1|site), data=plt_seed) 
summary(seed_mean_mod2)



### Confidence intervals


seed_mod_CI_TMB_wald<- confint(seed_mean_mod_TMB)
seed_mod_CI_TMB_wald # used in table 1

seed_mod_CI2<-confint(seed_mean_mod2_TMB)
seed_mod_CI2# used in table 2


# Model diagnostics using DHARMa

# Diagnostiscts on model 1: 
sim_mod1<-simulateResiduals(fittedModel = seed_mean_mod_TMB, n = 250)# warning message because glmmTMB was used
plot(sim_mod1) # no strange patterns in predicted vs residuals
testDispersion(sim_mod1)
testUniformity(sim_mod1)


#compare variation among trmnts and sites
#control for re
new_plt<-plt_seed
# set ID and site to NA in new DF
new_plt$ID=NA
new_plt$site=NA

#calculate new predicted values that average over the REs  
pred = predict(seed_mean_mod_TMB, newdata = new_plt)
#
plotResiduals(pred, sim_mod1$scaledResiduals) #doesn't differ from direct approach
#plot against other predictors
par(mfrow=c(1,2))
plotResiduals(plt_seed$prop.c, sim_mod1$scaledResiduals)
plotResiduals(plt_seed$trmnt, sim_mod1$scaledResiduals)
par(mfrow=c(1,1))
## looks good!

# Diagnostiscts on model 2: 
sim_mod2<-simulateResiduals(fittedModel = seed_mean_mod2_TMB, n = 250)# warning message because glmmTMB was used
plot(sim_mod2) # no strange patterns in predicted vs residuals
testDispersion(sim_mod2)
testUniformity(sim_mod2)


#compare variation among trmnts and sites
#control for re
new_plt<-plt_seed
# set ID and site to NA in new DF
new_plt$ID=NA
new_plt$site=NA

#calculate new predicted values that average over the REs  
pred = predict(seed_mean_mod2_TMB, newdata = new_plt)
#
plotResiduals(pred, sim_mod2$scaledResiduals) #doesn't differ from direct approach
#plot against other predictors
par(mfrow=c(1,2))
plotResiduals(plt_seed$prop.c, sim_mod2$scaledResiduals)
plotResiduals(plt_seed$trmnt, sim_mod2$scaledResiduals)
par(mfrow=c(1,1))
## looks good!


### 
#Supplemental models

#model s1a
#Frt matured as a response
frt_mod<-glmmTMB(frt~trmnt*prop.c+(1|site), data=plt_seed,family=nbinom2)
#output used in supplemental table 1a
summary(frt_mod)

frt_mod_CI<- confint(frt_mod)
frt_mod_CI

#model s1b
#no interaction frt model
frt_mod2<-glmmTMB(frt~trmnt+prop.c+(1|site), data=plt_seed,family=nbinom2)
#output used in supplemental table 1b
summary(frt_mod2)
#frt unimpacted by prop.c or trmtn

frt_mod2_CI<- confint(frt_mod2)
frt_mod2_CI

# Diagnostiscts on model s1a: 
sim_frt<-simulateResiduals(fittedModel = frt_mod, n = 250)# warning message because glmmTMB was used
plot(sim_frt) # no strange patterns in predicted vs residuals
testDispersion(sim_frt)
testUniformity(sim_frt)


#compare variation among trmnts and sites
#control for re
new_plt<-plt_seed
# set ID and site to NA in new DF
new_plt$ID=NA
new_plt$site=NA

#calculate new predicted values that average over the REs  
pred = predict(frt_mod, newdata = new_plt)
#
plotResiduals(pred, sim_frt$scaledResiduals) #doesn't differ from direct approach
#plot against other predictors
par(mfrow=c(1,2))
plotResiduals(plt_seed$prop.c, sim_frt$scaledResiduals)
plotResiduals(plt_seed$trmnt, sim_frt$scaledResiduals)
par(mfrow=c(1,1))
## frt mod looks okay! Modify the above code for the second model for diagnostics


##models s2a&b

#Seed mod, no offset
seed_mod<-glmmTMB(seeds~trmnt*prop.c+(1|site), data=plt_seed,family=nbinom2)
summary(seed_mod)#data for table s2a


seed_mod_CI<- confint(seed_mod)
seed_mod_CI

seed_mod2<-glmmTMB(seeds~trmnt+prop.c+(1|site), data=plt_seed,family=nbinom2)
summary(seed_mod2)#data for table s1a


seed_mod2_CI<- confint(seed_mod2)
seed_mod2_CI

####diagnostics on models s2a&b
sim_seed2<-simulateResiduals(fittedModel = seed_mod2, n = 250)# warning message because glmmTMB was used
plot(sim_seed2) # no strange patterns in predicted vs residuals
testDispersion(sim_seed2)
testUniformity(sim_seed2)

#now for no interaction model
sim_seed<-simulateResiduals(fittedModel = seed_mod, n = 250)
plot(sim_seed)
testDispersion(sim_seed)#both this and interaction model have similar results, 
#Few other tests: uniformity 
testUniformity(sim_seed)





#########################
#Plotting predicted effect and actual data
################

##Note that this is also in the plot_code script

#Used sjPlot package and ggeffects
#use get_model_data to predict fitted values for hp and op at each level of % ag (prop.c)
#conditioned on either fixed or random effects
#fixed effects have narrower CIs
#Info on how this works:
#https://www.rdocumentation.org/packages/ggeffects/versions/0.8.0/topics/ggaverage

### get_model_data + ggplot version
theme_set(theme_bw())
#run get_model_data to extract ggplot usable output
s<-get_model_data(seed_mean_mod_TMB,ci.lvl= .95, type="pred",terms=c("prop.c","trmnt"), 
                  pred.type="re", colors= "bw") 
s<-s%>%mutate(group = fct_recode(group, 
                                 "Supplemental Pollination" = "SP","Open Pollination"="OP"))
### make separate dataframes for CI for plot
shp<-s%>%filter(group=="Supplemental Pollination")
sop<-s%>%filter(group=="Open Pollination")

###make plot for seed model
ggplot(data=s, aes(x, predicted),linetype=group)+
  geom_line(aes(linetype=group),size=1)+
  geom_point(data=plt_nr,aes(prop.c, (seeds/frt),shape=trmnt),position="jitter",
             inherit.aes = FALSE,size=3)+
  geom_ribbon(data=shp,aes(x=x,ymin=conf.low, ymax=conf.high),alpha = 0.3,inherit.aes = FALSE)+
  geom_ribbon(data=sop,aes(x=x,ymin=conf.low, ymax=conf.high),alpha = 0.3,inherit.aes = FALSE)+
  labs(shape="Treatment",linetype="Predicted Response")+
  xlab("% Agriculture")+ylab("Mean Seeds/Treated Fruit per Plant ")+
  ggtitle("Fig. 3: Effect of % Agriculture on Seed Set",
          subtitle="Predicted Response vs. Data")+
  theme(axis.text=element_text(size=14,color="black"),
        axis.title=element_text(size=16),
        title=element_text(size=18),
        legend.text=element_text(size=14),
        axis.line = element_line(colour = "black"),panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),legend.position = "none")

#Note: I haven't figured out how to facet both the data and the predicted values by treatment

