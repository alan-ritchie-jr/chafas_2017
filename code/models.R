###################
###Seed models####
#################
install.packages("glmmTMB")
install.packages("ggeffects")
install.packages("sjPlot")
install.packages("blmeco")
install.packages("aods3")
install.packages("car")
install.packages("lme4")
install.packages("Matrix")
install.packages("snakecase")
install.packages("DHARMa")

####

#Dan start here; run lines 20-59

# These are all needed for s
library(TMB)
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

seed_land<-read.csv("data/seed_land.csv")
seed_land <-filter(seed_land, plot=="hi")

seed_land$round<-as.factor(seed_land$round)


# summary table
plt_nr<- seed_land %>%
  group_by(site,prop.c,trmnt, ID)%>%
  summarise(frt=n(), trtflw=sum(unique(flowers)),
            polov=sum(poll_ovules),
            avg_polov=mean(poll_ovules),
            seeds=sum(total.seeds),
            avg_seed=mean(total.seeds),
            totov=sum(total.ovules),
            avg_totov=mean(total.ovules))





############
#   models #
###########
## which distributions are appropriate for the data?
qqp(plt_nr$avg_seed, "norm")# avg seed per fruit looks pretty good

nbinom <- fitdistr(plt_nr$seeds, "Negative Binomial")
qqp(plt_nr$seeds, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])
#count data also look good for nb distribution

# Dan run 69-96


### with DHARMa 

###

##nb models
# total seed produced by treated fruit; number of fruit as offset
seed_mod<-glmmTMB(seeds~trmnt*scale(prop.c) +
                  offset(log(frt)) +
                    (1|site),family=nbinom1, data=plt_nr) 

summary(seed_mod)

seed_mod2<-glmmTMB(seeds~trmnt*scale(prop.c) +
                     offset(log(frt)) +
                     (1|site),family=nbinom2, data=plt_nr) 

summary(seed_mod2)



###normal models
seed_mod3<-lmer(avg_seed~trmnt+scale(prop.c)+(1|site),data=plt_nr)
summary(seed_mod3)
plot(seed_mod3)

qqnorm(resid(seed_mod3))
qqline(resid(seed_mod3))

seed_mod3p<-lme(avg_seed~trmnt*scale(prop.c),data=plt_nr,random=~1|site)
summary(seed_mod3p)

#if you drop scaling you get very high negative correlation between intercept and prop.c

seed_mod3TMB<-glmmTMB(avg_seed~trmnt*prop.c+(1|site),data=plt_nr)
summary(seed_mod3TMB)

  
  
seed_mod4<-lmer(avg_seed~trmnt+prop.c+(1|site),data=plt_nr)          
summary(seed_mod4)
qqnorm(resid(seed_mod4))


###using glmer.nb

####glmer.nb
seed_mod_glmer<-glmer.nb(seeds~trmnt*scale(prop.c) +
                           offset(log(frt)) +
                           (1|site), data=plt_nr)

summary(seed_mod_glmer)
isSingular(seed_mod_glmer)

# these give identical results


seed_mod_glmer2<-glmer.nb(seeds~trmnt+scale(prop.c) +
                            offset(log(frt)) +
                            (1|site), data=plt_nr)

summary(seed_mod_glmer2)
isSingular(seed_mod_glmer2)#nto coming up similar, parameters are similar



#CIs
seed_mod_CI<- confint(seed_mod_glmer,method="profile")
seed_mod_CI_Wald <- confint(seed_mod,method="wald")
seed_mod_CI_root<- confint(seed_mod2,method="uniroot")
seed_mod_CI_root
seed_mod_CI_Wald




####
#the model specification is getting confusing.
####



sim<-simulateResiduals(fittedModel = seed_mod3, n = 250)# warning message because glmmTMB was used
plot(sim) # no strange patterns in predicted vs residuals
testDispersion(sim)
testUniformity(sim)
#now for no interaction model
sim2<-simulateResiduals(fittedModel = seed_mod4, n = 250)
plot(sim2)
testDispersion(sim2)#both this and interaction model have similar results, 
#including the ID RE seems to effect dispersion test
testUniformity(sim2)


#gaussian mixed model
sim_lmm<-simulateResiduals(fittedModel = seed_lmm, n = 250)
plot(sim_lmm)
testUniformity(sim_lmm)
testDispersion(sim_lmm)
#lm with averaged seeds per plant
sim_lm<-simulateResiduals(fittedModel = seed_lm, n = 250)# warning message because glmmTMB was used
plot(sim_lm)
testDispersion(sim_lm) 
testUniformity(sim_lm)



####glmer.nb
seed_mod_glmer<-glmer.nb(seeds~trmnt*scale(prop.c) +
                           offset(log(frt)) +
                           (1|site), data=plt_nr)

summary(seed_mod_glmer)
isSingular(seed_mod_glmer)

# these give identical results

seed_mod_glmer_uns<-glmer.nb(seeds~trmnt*prop.c +
                           offset(log(frt)) +
                           (1|site), data=plt_nr)


seed_mod_glmer2<-glmer.nb(seeds~trmnt+scale(prop.c) +
                           offset(log(frt)) +
                           (1|site), data=plt_nr)

summary(seed_mod_glmer2)
isSingular(seed_mod_glmer2)

#######################
#Code 12Feb 2019###
####
# No interation; drop to look at fixed effects in isolation

#gaussian mixed model, only site as RE
seed_lmm<-lmer(avg_seed~trmnt*scale(prop.c)+
                    (1|site), data=plt_nr) 
seed_lmm<-lme(avg_seed~trmnt*prop.c,data=plt_nr,random=~1|site)
summary(seed_lmm)

#confidence intervals

seed_mod_CI<- confint(seed_mod,method="profile")
seed_mod_CI_Wald <- confint(seed_mod2,method="wald")
seed_mod_CI_root<- confint(seed_mod2,method="uniroot")
seed_mod_CI_root
seed_mod_CI_Wald

seed_mod2_CI<- confint(seed_mod_glmer,method="profile")
seed_mod2_CI
seed_mod2_CI_Wald <- confint(seed_mod_glmer,method="Wald")
seed_mod2_CI_Wald
seed_mod2_CI_boot<- confint(seed_mod2,method="boot")
#
seed_mod_CI<- confint(seed_mod_glmer,method="profile")
seed_mod_CI_Wald <- confint(seed_mod_glmer,method="wald")
seed_mod_CI_boot<- confint(seed_mod_glmer,method="boot")
seed_mod_CI_Wald
seed_mod_CI_boot




###Use DHARMa to check residuals

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
#gaussian mixed model
sim_lmm<-simulateResiduals(fittedModel = seed_lmm, n = 250)
plot(sim_lmm)
testUniformity(sim_lmm)
testDispersion(sim_lmm)
#lm with averaged seeds per plant
sim_lm<-simulateResiduals(fittedModel = seed_lm, n = 250)# warning message because glmmTMB was used
plot(sim_lm)
testDispersion(sim_lm) 
testUniformity(sim_lm)
#pollinated ovules model

sim_pol<-simulateResiduals(fittedModel = polov_mod, n = 250)
plot(sim_pol)

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

#for no interaction model
pred2 = predict(seed_mod2, newdata = new_plt)
plotResiduals(pred2, sim$scaledResiduals) 
plotResiduals(plt_nr$prop.c, sim2$scaledResiduals)
plotResiduals(plt_nr$trmnt, sim2$scaledResiduals)
#same here

#for lmm
par(mfrow=c(1,2))
plotResiduals(plt_nr$prop.c, sim_lmm$scaledResiduals)
plotResiduals(plt_nr$trmnt, sim_lmm$scaledResiduals)
testDispersion(sim_lmm) 
par(mfrow=c(1,1))
### some other plots

par(mfrow=c(1,2))
plotResiduals(plt_nr$prop.c, sim_lm$scaledResiduals)
plotResiduals(plt_nr$trmnt, sim_lm$scaledResiduals)

testDispersion(sim) #test for overdispersion; our model accounts for this somewhat
#its worth nothing that while we get a significant value, the parameter is very small


#histogram to check for zero values in response
ggplot(plt_nr, aes(x=seeds)) + 
  geom_histogram(fill="grey", color="black")+ylab("Count")+xlab("Seeds Produced by Treated Fruit")
+facet_grid(.~trmnt)
#for pollinated ovules
ggplot(plt_nr, aes(x=polov)) + 
  geom_histogram(fill="grey", color="black")+
  ylab("Count")+xlab("Pollinated Ovules Produced by Treated Fruit")+facet_grid(.~trmnt)

ggplot(plt_nr, aes(x=avg_seed)) + 
  geom_histogram(fill="grey", color="black")+ylab("Count")+xlab("Avg Seed per Treated Fruit")+
  facet_grid(.~trmnt)

### not many zeroes!



###################################

#total pollinated ovules produced by treated fruit; number of fruit as offset
polov_mod<-glmmTMB(polov~trmnt*scale(prop.c) +
                     offset(log(frt)) +
                     (1|site/ID),family=nbinom1, data=plt_nr) 

summary(polov_mod)

#CIs 
polov_mod_CI_prof <- confint(polov_mod)
polov_mod_CI_quad <- confint(polov_mod,method="wald")
polov_mod_CI_prof
polov_mod_CI_quad



#Plotting predicted effect and actual data


#Used sjPlot package and ggeffects
#use get_model_data to predict fitted values for hp and op at each level of % ag (prop.c)
#conditioned on either fixed or random effects
#fixed effects have narrower CIs
#Info on how this works:
#https://www.rdocumentation.org/packages/ggeffects/versions/0.8.0/topics/ggaverage

### get_model_data + ggplot version
theme_set(theme_bw())
#run get_model_data to extract ggplot usable output
s<-get_model_data(seed_mod,ci.lvl= .95, type="pred",terms=c("prop.c","trmnt"), 
                  pred.type="re", colors= "bw") 

scaled_values<-scale(plt_nr$prop.c)
attributes(scaled_values)
### make separate dataframes for CI for plot
shp<-s%>%filter(group=="hp")
sop<-s%>%filter(group=="op")
s2hp<-s2%>%filter(group=="hp")
s2op<-s2%>%filter(group=="op")
s$predicted

effect_plot

###make plot for seed model
ggplot(data=s, aes(x, predicted),linetype=group)+
  geom_line(aes(linetype=group))+
  geom_point(data=plt_nr,aes(prop.c, (seeds/frt),shape=trmnt),position="jitter",
             inherit.aes = FALSE)+
  geom_ribbon(data=shp,aes(x=x,ymin=conf.low, ymax=conf.high),alpha = 0.3,inherit.aes = FALSE)+
  geom_ribbon(data=sop,aes(x=x,ymin=conf.low, ymax=conf.high),alpha = 0.3,inherit.aes = FALSE)+
  labs(shape="Treatment",linetype="Predicted Response")+
  xlab("% Agriculture")+ylab("Seeds per Fruit")+
  ggtitle("Effect of % Agriculture on Seed Production",
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



#Dan after this point its mdoel assessment code 




# Test of significance of fixed effects using 
#not sure whether type II or III anova is appropriate to test fixed effects
# type III is the p value of the coefficient conditional on the other coefficients and the interaction!

Anova(seed_mod, type= "III")

drop1(seed_mod,test="Chisq")

#residuals
ggplot(data = NULL, aes(y = resid(seed_mod, type = "pearson"),
                        x = fitted(seed_mod))) + geom_point()
qq.line = function(x) {
  # following four lines from base R's qqline()
  y <- quantile(x[!is.na(x)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  return(c(int = int, slope = slope))
}
QQline = qq.line(resid(seed_mod, type = "pearson"))
ggplot(data = NULL, aes(sample = resid(seed_mod, type = "pearson"))) +
  stat_qq() + geom_abline(intercept = QQline[1], slope = QQline[2])

###r2 approximation from https://stats.stackexchange.com/questions/95054/how-to-get-an-overall-p-value-and-effect-size-for-a-categorical-factor-in-a-mi
r2.corr.mer <- function(m) {
  lmfit <-  lm(model.response(model.frame(m)) ~ fitted(m))
  summary(lmfit)$r.squared
}

r2.corr.mer(seed_mod)

r2.corr.mer(polov_mod)

r2.corr.mer(prop_ov_mod)
#alternative measure from same source as r2.corr.mer and in the following paper: https://onlinelibrary.wiley.com/doi/abs/10.1002/sim.1572
1-var(residuals(seed_mod))/(var(model.response(model.frame(seed_mod))))
####

### plot fitted values for response
#conditioned on random effects
#### Try ggeffects and plot function??

mydf<-ggpredict(seed_mod, terms = c("prop.c","trmnt"),type="fe")
ggplot(mydf, aes(x, predicted,shape=group)) + geom_line()+facet_wrap(~group)




##########
### alternative parameterization (see bolker glmm FAQ)
stmb_plt2<-glmmTMB(seeds~trmnt*scale(prop.c)+
                   scale(temp.start)+ offset(log(fruit_count))+
                    (1|site/ID),family=nbinom2, data=plt) 

summary(stmb_plt2)
### add type 3
Anova(stmb_plt2)


ggplot(data = NULL, aes(y = resid(stmb_plt2, type = "pearson"),
                        x = fitted(stmb_plt2))) + geom_point()
qq.line = function(x) {
  # following four lines from base R's qqline()
  y <- quantile(x[!is.na(x)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  return(c(int = int, slope = slope))
}
QQline = qq.line(resid(stmb_plt2, type = "pearson"))
ggplot(data = NULL, aes(sample = resid(stmb_plt2, type = "pearson"))) +
  stat_qq() + geom_abline(intercept = QQline[1], slope = QQline[2])

#confidence intervals
stmb_plt2_CI_prof <- confint(stmb_plt2)
stmb_plt2_CI_quad <- confint(stmb_plt2,method="Wald")
stmb_plt2_CI_prof
stmb_plt2_CI_quad


r2.corr.mer(stmb_plt2)
#alternative measure from same source as r2.corr.mer and in the following paper: https://onlinelibrary.wiley.com/doi/abs/10.1002/sim.1572
1-var(residuals(stmb_plt2))/(var(model.response(model.frame(stmb_plt2))))
####

## starting temp is most significant predictor
# significantly colder days in rd 4 (and some in rd 3) impacted seed set

##total polov glmmTMB
potmb_plt<-glmmTMB(polov~trmnt*scale(prop.c) +scale(temp.start)+ 
                     offset(log(fruit_count))+
                     (1|site/ID),family=nbinom1, data=plt) 
summary(potmb_plt)
# same results with polov


### Plotting output





