#### old seed model code snippets


###################
###Seed models####
#################
#install.packages("glmmTMB")
#install.packages("ggeffects")
#install.packages("sjPlot")
#install.packages("blmeco")
#install.packages("aods3")
#install.packages("car")
#install.packages("lme4")
#install.packages("Matrix")
#install.packages("snakecase")
#install.packages("DHARMa")
#install.packages("optimx")
#install.packages("simr")
####


# These are all needed for s
library(TMB)
library(optimx)
library(aods3)
library(blmeco)
library(car)
library(glmmTMB)
library(lme4)
library(tidyverse)
library(sjPlot)
library(ggeffects)
library(sjlabelled)
library(snakecase)
library(DHARMa)
library(simr)

seed_land<-read.csv("data/seed_land.csv")
seed_land <-filter(seed_land, plot=="hi")

seed_land$round<-as.factor(seed_land$round)
# add seedOv


# summary table of plant
plt_nr<- seed_land %>%mutate( trmnt = fct_recode(trmnt, "Supplemental Pollination" = "hp", "Open Pollination"="op"), 
                              seed_ov= total.seeds/total.ovules)%>%
  group_by(site,prop.c,trmnt,ID,round)%>%#calculate fruit removed
  mutate(frt_removed_rd=sum(frt_removed)/n(), trt_flw_rd=sum(flowers)/n())%>%
  ungroup()%>%
  group_by(site,prop.c,trmnt,ID)%>%
  summarise(frt=n(),trt_flw=sum(trt_flw_rd),tot_frt_removed=sum(frt_removed_rd),
            ab_ov=sum(abrt.ovules),
            nopol_ov=sum(virgin.ovules)+sum(abrt.ovules),
            seeds=sum(total.seeds),
            mean_seed=mean(total.seeds),
            mean_seed_ov=mean(seed_ov),
            totov=sum(total.ovules),
            mean_totov=mean(total.ovules),
            seeds_frt=seeds/frt,mean_sf=mean(seeds/frt))
### site level and treatment level means for pollen limitation metric (seed stab or effect size)
#site level means
plt_site<-plt_nr%>%group_by(site,prop.c,trmnt)%>%
  summarise(sum_frt=sum(frt),n_ID=n_distinct(ID),sum_totov=sum(totov),sum_seed=sum(seeds),
            mean_frt=mean(frt),mean_totov=mean(mean_totov),mean_seed=mean(mean_seed),
            mean_seed_ov_site=mean(mean_seed_ov),
            sd_seed_ov=sd(mean_seed_ov),
            seed_frt=sum_seed/sum_frt,
            mean_totov_site=mean(mean_totov),
            mean_polov_site=mean(mean_poll_ov),
            seed_stab=mean_seed_ov_site/(sd_seed_ov*100))

trmnt_mean<-plt_nr%>%group_by(trmnt)%>%
  summarise(sum_frt=sum(frt),n_ID=n_distinct(ID),sum_totov=sum(totov),sum_seed=sum(seeds),
            mean_frt=mean(frt),mean_totov=mean(mean_totov),mean_seed=mean(mean_seed),
            mean_seed_ov_site=mean(mean_seed_ov),
            sd_seed_ov=sd(mean_seed_ov),
            seed_frt=sum_seed/sum_frt,
            mean_totov_site=mean(mean_totov),
            seed_stab=mean_seed_ov_site/(sd_seed_ov*100))

#plot seed stab and other metrics against prop.c
plt_site%>%ggplot(aes(prop.c,seed_stab, color=trmnt))+geom_point()+
  geom_smooth(method=lm)

plt_site%>%ggplot(aes(prop.c,mean_seed_ov_site, color=trmnt))+geom_point()+
  geom_smooth(method=lm)

plt_site%>%ggplot(aes(prop.c,mean_polov_site, color=trmnt))+geom_point()+
  geom_smooth(method=lm)

plt_site%>%ggplot(aes(prop.c,n_ID, color=trmnt))+geom_point()+
  geom_smooth(method=lm)

##pollen limitation metric
plt_site_pl<-plt_site%>%
  group_by(site,prop.c)%>%
  summarise(pl=log(mean_seed_ov_site[trmnt=="SP"]/mean_seed_ov_site[trmnt=="OP"]),
            pl_seed=log(mean_seed[trmnt=="SP"]/mean_seed[trmnt=="OP"]),
            pl_polov=log(mean_polov_site[trmnt=="SP"]/mean_polov_site[trmnt=="OP"]))

##
trmnt_means<-plt_nr%>%group_by(trmnt)%>%
  summarise(sum_frt=sum(frt),sum_totov=sum(totov),sum_seed=sum(seeds),
            mean_frt=mean(frt),mean_totov=mean(totov),mean_seed=mean(mean_seed),
            mean_seed_ov=mean(mean_seed_ov),
            seed_frt=sum_seed/sum_frt,
            totov_frt=sum_totov/sum_frt, seed_totov=sum_seed/sum_totov)


####Linear models for site level variables


#mean seeds/ovule per site
site_seed_ov_mod<-lm(mean_seed_ov_site~trmnt*prop.c,data=plt_site)

summary(site_seed_ov_mod)
qqPlot(residuals(site_seed_ov_mod4))
plot(site_seed_ov_mod4)

#mean poll ovules/ovule per site
site_pol_ov_mod<-lm(mean_polov_site~trmnt+prop.c,data=plt_site)
summary(site_pol_ov_mod)
#same issue w/ no interaction model

#mean seeds per site per trmnt lm
site_mean_seed_mod<-lm(mean_seed~trmnt*prop.c,data=plt_site)
summary(site_mean_seed_mod)
qqPlot(residuals(site_mean_seed_mod))

site_mean_seed_mod2<-lm(mean_seed~trmnt,data=plt_site)
summary(site_mean_seed_mod2)

###linear model for pollen limitation effect size measure from ekroos 2015 & Knight 2005

#diagnostic plot of linear relationship between ag and pl (using mean seed per ovule)
plt_site_pl%>%ggplot(aes(prop.c,pl))+geom_point()+geom_smooth(method="lm")
plt_site_pl%>%ggplot(aes(prop.c,pl_polov))+geom_point()+geom_smooth(method="lm")
#pl mod
site_pl_mod<-lm(pl~prop.c,data=plt_site_pl)

summary(site_pl_mod)
AIC(site_pl_mod)
plot(site_pl_mod)
ggplot(site_pl_mod)

####plot of model
pl.predict<-cbind(as.data.frame(plt_site_pl), predict(site_pl_mod, interval = 'confidence'))

# plot the points (actual observations), regression line, and confidence interval
p <- ggplot(pl.predict, aes(prop.c,pl))
p <- p + geom_point()
p <- p + geom_line(aes(prop.c, fit))
p <- p + geom_ribbon(aes(ymin=lwr,ymax=upr), alpha=0.3)
p

#t.test for difference in mean untreated frt remvoal between treatment
t.test(tot_frt_removed~trmnt,data=plt_nr)
t.test(trt_flw~trmnt,data=plt_nr)
#
summary(lm(tot_frt_removed~prop.c,data=plt_nr))


plt_nr%>%ggplot(aes(as.factor(prop.c),mean_seed_ov, color=trmnt))+geom_boxplot()
plt_nr%>%ggplot(aes(as.factor(prop.c),mean_seed, color=trmnt))+geom_boxplot()


plt_nr%>%ggplot(aes(prop.c,totov, color=trmnt))+geom_point()+
  geom_smooth(method=lm)

plt_nr%>%ggplot(aes(prop.c,frt, color=trmnt))+geom_jitter()+
  geom_smooth(method=lm)

plt_nr%>%ggplot(aes(prop.c,seeds, color=trmnt))+geom_jitter()+
  geom_smooth(method=lm)

plt_nr%>%ggplot(aes(prop.c,mean_seed, color=trmnt))+geom_jitter()+
  geom_smooth(method=lm)

plt_nr%>%ggplot(aes(prop.c,mean_seed_ov, color=trmnt))+geom_jitter()+
  geom_smooth(method=lm)

plt_nr%>%ggplot(prop.c)
site_var<-lm(mean_seed~site,plt_nr)

trt_frt_var<-lm(log(frt/trt_flw)~trmnt,plt_nr)#trt_flw

dat <- plt_nr

trt_frt_var<-lm(seeds/totov~prop.c*trmnt,plt_nr)
summary(trt_frt_var)

summary(trt_frt_var)
qqPlot(residuals(trt_frt_var))


sim2<-simulateResiduals(fittedModel = seed_ov_var, n = 250)# warning message because glmmTMB was used
plot(sim2) # no strange patterns in predicted vs residuals
testDispersion(sim2)
testUniformity(sim2)
testZeroInflation(sim2)

site_anova<-aov(mean_seed~site,plt_nr)
summary(site_anova)
qqPlot(residuals((site_var)))##fits
qqPlot(residuals(site_anova))

seed_land%>%ggplot(aes(as.factor(prop.c),total.ovules, color=trmnt))+geom_boxplot()
plt_site%>%ggplot(aes(as.factor(prop.c),mean_seed_ov, color=trmnt))+geom_point()
plt_nr%>%ggplot(aes(as.factor(prop.c),frt, color=trmnt))+geom_boxplot()

qplot(mean_seed,var_s,data=plt_nr)+
  ## linear (quasi-Poisson/NB1) fit
  geom_smooth(method="lm",formula=y~x-1)+
  ## smooth (loess)
  geom_smooth(colour="red")+
  ## semi-quadratic (NB2/LNP)
  geom_smooth(method="lm",formula=y~I(x^2)+offset(x)-1,colour="purple")+
  ## Poisson (v=m)
  geom_abline(a=0,b=1,lty=2)


###plot of ag gradient
plt_nr%>%ggplot(aes(fct_reorder(site, prop.c),prop.c))+geom_point()+
  labs(x="Site" ,y="% Agriculture")+
  ggtitle("Fig. 2: Study System Agricultural Land-use Gradient")+theme_bw()

############
#   models #
###########
## which distributions are appropriate for the data?
qqp(plt_nr$mean_seed, "norm")# avg seed per fruit looks pretty good

nbinom <- fitdistr(plt_nr$seeds, "Negative Binomial")
qqp(plt_nr$seeds, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])
#count data also look good for nb distribution


########################
###NEgative binomial models and interpretation of fixed effects####
##########
seed_ov_lm<-lm(mean_seed_ov~trmnt, data=plt_site)
summary(seed_ov_lm)
###
seed_ov_mod<-glmmTMB(mean_seed_ov~trmnt +(1|site), data=plt_nr)

summary(seed_ov_mod)
qqPlot(residuals(seed_ov_mod))

###########################
#### average seed models; same response as nb models, just normall distributed######

#mean seed

seed_mean_mod_TMB<-glmmTMB(mean_seed~trmnt*prop.c +(1|site), data=plt_nr) 
summary(seed_mean_mod_TMB)

seed_mean_mod<-lmer(mean_seed~trmnt*prop.c +(1|site), data=plt_nr)

summary(seed_mean_mod)



seed_mean_mod2_TMB<-glmmTMB(mean_seed~trmnt+prop.c +(1|site), data=plt_nr) 
summary(seed_mean_mod2_TMB)

seed_mean_mod2<-lmer(mean_seed~trmnt+prop.c +(1|site), data=plt_nr) 
summary(seed_mean_mod2)



### Confidence intervals

seed_mod_CI_TMB_uni<- confint(seed_mean_mod_TMB,method="uniroot")
seed_mod_CI_TMB_uni

seed_mod_CI_TMB_wald<- confint(seed_mean_mod_TMB)
seed_mod_CI_TMB_wald

seed_mod_CI2<-confint(seed_mean_mod2_TMB)
seed_mod_CI2



#Frt mod; was there a difference in frt matured between  
frt_mod<-glmmTMB(frt~trmnt*prop.c+(1|site), data=plt_nr,family=nbinom2)
summary(frt_mod)

frt_mod2<-glmmTMB(frt~trmnt+prop.c+(1|site), data=plt_nr,family=nbinom2)
summary(frt_mod2)


# frt unimpacted by prop.c or trmtn
sim2<-simulateResiduals(fittedModel = frt_mod, n = 250)# warning message because glmmTMB was used
plot(sim2) # no strange patterns in predicted vs residuals
testDispersion(sim2)
testUniformity(sim2)
testZeroInflation(sim2)
#control for re
new_plt<-plt_nr
# set ID and site to NA in new DF
new_plt$ID=NA
new_plt$site=NA
#calculate new predicted values that average over the REs  
pred = predict(frt_mod, newdata = new_plt)
#
plotResiduals(pred, sim2$scaledResiduals) #doesn't differ from direct approach
#plot against other predictors
par(mfrow=c(1,2))
plotResiduals(plt_nr$prop.c, sim2$scaledResiduals)
plotResiduals(plt_nr$trmnt, sim2$scaledResiduals)
## frt mod looks okay!


#Seed mod, no offset
seed_mod_no<-glmmTMB(seeds~trmnt*prop.c+(1|site), data=plt_nr,family=nbinom2)
summary(seed_mod_no)

seed_mod_no2<-glmmTMB(seeds~trmnt+prop.c+(1|site), data=plt_nr,family=nbinom2)
summary(seed_mod_no2)


####
sim2<-simulateResiduals(fittedModel = seed_mod2, n = 250)# warning message because glmmTMB was used
plot(sim2) # no strange patterns in predicted vs residuals
testDispersion(sim2)
testUniformity(sim2)
testZeroInflation(sim2)
#now for no interaction model
sim<-simulateResiduals(fittedModel = seed_mod, n = 250)
plot(sim)
testDispersion(sim)#both this and interaction model have similar results, 
#including the ID RE seems to effect dispersion test
testUniformity(sim)
testZeroInflation(sim)




###Use DHARMa to check residuals and assumptions
### compare nbinom1 vs nbinom2 parameterizations. 
#if huge differences between residual plots further explore

sim<-simulateResiduals(fittedModel = seed_mod_glmer, n = 250)# warning message because glmmTMB was used
plot(sim) # no strange patterns in predicted vs residuals
testDispersion(sim)
testUniformity(sim)
#now for no interaction model
sim2<-simulateResiduals(fittedModel = seed_mod2, n = 250)
plot(sim2)
testDispersion(sim2)#both this and interaction model have similar results, 
#including the ID RE seems to effect dispersion test
testUniformity(sim2)

#As a check because glmmTMB can't separate out random effect from predicted effects
# recalculate predictions by hand - see help ?predict.glmmTMB
# create new data from plt_nr
new_plt<-plt_nr
# set ID and site to NA in new DF
new_plt$ID=NA
new_plt$site=NA
#calculate new predicted values that average over the REs  
pred = predict(seed_mod, newdata = new_plt)
#
plotResiduals(pred, sim$scaledResiduals) #doesn't differ from direct approach
#plot against other predictors
par(mfrow=c(1,2))
plotResiduals(plt_nr$prop.c, sim$scaledResiduals)
plotResiduals(plt_nr$trmnt, sim$scaledResiduals)
#heteroscedascticity doesn't look excessive




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
  geom_line(aes(linetype=group))+
  geom_point(data=plt_nr,aes(prop.c, (seeds/frt),shape=trmnt),position="jitter",
             inherit.aes = FALSE)+
  geom_ribbon(data=shp,aes(x=x,ymin=conf.low, ymax=conf.high),alpha = 0.3,inherit.aes = FALSE)+
  geom_ribbon(data=sop,aes(x=x,ymin=conf.low, ymax=conf.high),alpha = 0.3,inherit.aes = FALSE)+
  labs(shape="Treatment",linetype="Predicted Response")+
  xlab("% Agriculture")+ylab("Mean Seeds/Treated Fruit per Plant ")+
  ggtitle("Fig. 3: Effect of % Agriculture on Seed Set",
          subtitle="Predicted Response vs. Data")

#Note: I haven't figured out how to facet both the data and the predicted values by treatment


# other model types available through this package
plot_model(seed_mod,type="est") #forest plot of parameter estimates



#now for polov model

p<-get_model_data(polov_mod,type="pred",terms=c("prop.c","trmnt"), 
                  pred.type="re", colors= "bw",ci.lvl= .95) 

#re=random effect conditioned 


### make separate dataframes for CI for plot
php<-p%>%filter(group=="hp")
pop<-p%>%filter(group=="op")

###make plot for polov model
ggplot(data=plt_nr,aes(prop.c, polov))+
  geom_point(aes(shape=trmnt),position="jitter")+
  geom_line(data=p, aes(x, predicted,linetype=group))+
  geom_ribbon(data=php,aes(x=x,ymin=conf.low, ymax=conf.high),alpha = 0.3,inherit.aes = FALSE)+
  geom_ribbon(data=pop,aes(x=x,ymin=conf.low, ymax=conf.high),alpha = 0.3,inherit.aes = FALSE)+
  labs(shape="Treatment",linetype="Predicted Response")+
  xlab("% Agriculture")+ylab("Total Pollinated Ovules")+
  ggtitle("Effect of % Agriculture on Pollination",
          subtitle="Predicted Response vs. Data")



s<-get_model_data(seed_mod,type="pred",terms=c("prop.c","trmnt"), 
                  pred.type="re", colors= "bw",ci.lvl= .95) 

#re=random effect conditioned 


### make separate dataframes for CI for plot
shp<-s%>%filter(group=="hp")
sop<-s%>%filter(group=="op")

###make plot for seed model
ggplot(data=plt_nr,aes(prop.c, (seeds/frt)))+
  geom_point(aes(shape=trmnt),position="jitter")+
  geom_line(data=s, aes(x, predicted,linetype=group))+
  geom_ribbon(data=shp,aes(x=x,ymin=conf.low, ymax=conf.high),alpha = 0.3,inherit.aes = FALSE)+
  geom_ribbon(data=sop,aes(x=x,ymin=conf.low, ymax=conf.high),alpha = 0.3,inherit.aes = FALSE)+
  labs(shape="Treatment",linetype="Predicted Response")+
  xlab("% Agriculture")+ylab("Seeds per Fruit")+
  ggtitle("Effect of % Agriculture on Seed Production",
          subtitle="Predicted Response vs. Data")

