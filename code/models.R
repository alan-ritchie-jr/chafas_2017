###################
###Seed models####
#################
#install.packages("glmmTMB")
#install.packages("ggeffects")
#install.packages("TMB")
#install.packages("sjPlot")
#install.packages("blmeco")
#install.packages("aods3")
#install.packages("car")
#install.packages("MCMCglmm")
#### 
library(aods3)
library(blmeco)
library(car)
library(glmmTMB)
library(ggeffects)
library(lme4)
library(tidyverse)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(multcomp)
library(effects)
library(MCMCglmm)
seed_land<-read.csv("data/seed_land.csv")
seed_land <-filter(seed_land, plot=="hi")

seed_land$round<-as.factor(seed_land$round)
# Create table containing:
# PR
# SR
# Seed
# Poll Ovs
# Fecund
# # flowers
# prop.c
# yday?

##Let's make sure our continuous responses aren't collinear; 
#code for corvif is located in zuur correlation script file
# from Zuur 2010, "A protocol to avoid common statistical problems"

#Z<-seed_land[,c("prop.c", "flowers", "yday", "fecund")]
#corvif(Z)    


### First create tabl:
# for each plant at each round has response variables averaged
### the proportional responses can't be modeled directly
### nor will using non-integers (averaged values)
### instead we'll try using totals of the response variables to make the proportional responses
plt<- seed_land %>%
  group_by(site,prop.c,trmnt, round, ID,flowers, fecund,yday,temp.start)%>%
  summarise(fruit_count=n(), trtflw=sum(unique(flowers)),
            polov=sum(poll_ovules),
            avg_polov=mean(poll_ovules),
            seeds=sum(total.seeds),
            avg_seed=mean(total.seeds),
            totov=sum(total.ovules),
            avg_totov=mean(total.ovules))
plt$temp.start[plt$temp.start=="na"] <- 22.3 # some na temp values; subbed for nearby site
plt$temp.start<-as.numeric(plt$temp.start)# make numeric

plt_n1<- seed_land %>%
  group_by(site,prop.c,trmnt, round, ID,flowers, fecund,yday)%>%
  summarise(fruit_count=n(), trtflw=sum(unique(flowers)),
            polov=sum(poll_ovules),
            avg_polov=mean(poll_ovules),
            seeds=sum(total.seeds),
            avg_seed=mean(total.seeds),
            totov=sum(total.ovules),
            avg_totov=mean(total.ovules))%>%
  filter(fruit_count>1)
  #now let's take these values and create a table where they're averaged and totaled over rounds

plt_nr<-plt %>%
  group_by(prop.c,site,trmnt,ID)%>% 
  summarise(frt=sum(fruit_count),avg_frt=mean(fruit_count),
            fecund=sum(fecund),avg_fecund=mean(fecund),
            trtflw=sum(trtflw),
            avg_trtflw=mean(trtflw),
            seeds=sum(seeds),
            avg_seed=mean(avg_seed), 
            polov=sum(polov),
            avg_polov=mean(avg_polov),
            totov=sum(totov),
           avg_totov=mean(avg_totov))  
######################################
#### check differences at population level
plt_nr_plot<-plt %>%
  group_by(prop.c,site,trmnt)%>% 
  summarise(frt=sum(fruit_count),plants=n_distinct(ID),avg_frt=mean(fruit_count),
            fecund=sum(fecund),avg_fecund=mean(fecund),
            trtflw=sum(trtflw),
            avg_trtflw=mean(trtflw),
            seeds=sum(seeds),
            avg_seed=mean(avg_seed), 
            seeds_plt=seeds/plants,
            polov=sum(polov),
            avg_polov=mean(avg_polov),
            totov=sum(totov),
            avg_totov=mean(avg_totov))  


#### These are important figures!!!
### Seeds 
ggplot(data=plt_nr, aes(prop.c,seeds, color=trmnt))+geom_point()+
geom_smooth(method=lm)
### poll ovules
ggplot(data=plt_nr, aes(prop.c,polov, color=trmnt))+geom_point()+
  geom_smooth(method=lm)
### seed  over time
ggplot(data=plt,aes(prop.c,seeds, color=trmnt))+geom_point()+
  geom_smooth(method=lm)+ facet_grid(.~round)

ggplot(data=plt,aes(trmnt,seeds, color=trmnt))+geom_boxplot()+
 facet_grid(.~round)

#Show that if we ignore round/time effects (which show up as more significant in most models)
# Prop.c doesn't really have an effect on seed or poll ovules, hp typically, but not always
# had more


###The problem with total ovules ####
### is that chamaecrista will mature HP fruit with fewer viable ovules in general
### (Fenster or bazazz)
ggplot(data=plt_nr, aes(prop.c,(seeds/totov), color=trmnt))+geom_point()+
  geom_smooth(method=lm)
ggplot(data=plt_nr, aes(prop.c,(polov/totov), color=trmnt))+geom_point()+
  geom_smooth(method=lm)
####
####effects of starting temperature on treatments
ggplot(data=plt, aes(temp.start,seeds, color=trmnt))+geom_point()+
  geom_smooth(method=lm)

### temperature effects hp slightly more than op, but unsure of significance
### this would make sense, because pollination would proceed as normal otherwise.
 ############
#   models #
###########
# using plt data frame
# let's model total seeds and poll_ovules first
### check random effects
# total seed, number of fruit as offset
smod_plt<-glmer.nb(seeds~trmnt*scale(prop.c)+scale(temp.start)+offset(log(fruit_count))+ (1|ID) , data=plt)

summary(smod_plt)


#total seed model using glmmTMB
stmb_plt<-glmmTMB(seeds~trmnt*scale(prop.c) +scale(temp.start)+offset(log(fruit_count)) +(1|site/ID),family=nbinom1, data=plt) 

summary(stmb_plt)

### you can remove scaling with round
stmb_plt<-glmmTMB(seeds~trmnt*prop.c + round+ scale(temp.start)+ offset(log(fruit_count)) +(1|site/ID),family=nbinom1, data=plt) 

summary(stmb_plt)
## both round or yday work, although yday  needs to be scaled. 
## effect of temp is very significant!


##total polov glmmTMB
potmb_plt<-glmmTMB(polov~trmnt*prop.c + round+ offset(log(fruit_count))+(1|site/ID),family=nbinom1, data=plt) 
summary(potmb_plt)
# same results with polov


###now for pr and sr


#first try on raw data, seed_land
prmod<-glmer(poll_ovules/total.ovules~ trmnt*prop.c+fecund+scale(yday)+(1|site/ID), 
                 family=binomial, weights=total.ovules,data=seed_land)
summary(prmod)

#Takeaway:
#as number of flowers increase there's a decrease in the log odds of an ovule being pollinated
plot(allEffects(prmod))
plot(ggeffect(prmod,c("prop.c","trmnt")))
plot(ggeffect(prmod,c("flowers","trmnt")))
sjp.lmer(prmod,  type='re.qq')
## drop interaction term, because the parameter is not significant for model fit

# total seeds/ total ovules per plant, per round; incorporating julian day
srmod<-glmer(total.seeds/total.ovules~ trmnt*I(prop.c/100)+I(flowers/100)*round+(1|site/ID), 
              family=binomial, weights=total.ovules,data=seed_land)
summary(srmod)


### same model, but let's test if there's an interaction between # of flowers and yday and use scale
### mean centering + scaling by standard deviation

srmod_sc<-glmer(total.seeds/total.ovules~ trmnt*scale(prop.c)+flowers+scale(yday)+(1|site/ID), 
             family=binomial, weights=total.ovules,data=seed_land)
summary(srmod_sc)
### no interaction between yday and flowers, so drop.

srmodf_sc<-glmer(total.seeds/total.ovules~ trmnt*scale(prop.c)+fecund+scale(yday)+(1|site/ID), 
                family=binomial, weights=total.ovules,data=seed_land)
summary(srmodf_sc)
overdisp_fun(srmodf_sc)
## no interaction between yday and fecundity; also slightly better fit than flowers


### scale has same results, but parameters may be a little easier to interpret
plot(allEffects(srmod_sc))
plot(ggeffect(srmod,c("prop.c","trmnt")))
plot(ggeffect(srmod_sc,c("flowers","yday")))
sjp.lmer(srmod,type='re.qq')
## althought interaction is dropped, I think this is worth thinking about
## more or less, later in the season 

## so using seeds is 
### check for homogeneity, etc.
op <- par(mfrow = c(2, 2), mar = c(5, 4, 1, 2))
plot(prmod, add.smooth = FALSE, which = 1)
par(mfrow=c(1,1))
residuals <- resid(prmod)
plot(residuals)
plot(seed_land$flowers, residuals, xlab = "trtflw",
     ylab = "Residuals")
plot(seed_land$prop.c, residuals, xlab = "% Ag",
     ylab = "Residuals")
plot(seed_land$trmnt, residuals, xlab = "trmnt",
     ylab = "Residuals")
plot(seed_land$round, residuals, xlab = "round",
     ylab = "Residuals")

sjp.lmer(prmod,  type='re.qq')
sjp.lmer(srmod,  type='re.qq')



#Now try using plt dataframe
### lowest AIC model includes julian day, which I left in the plt dataframe
prmod_plt<-glmer(polov/totov~ trmnt*scale(prop.c)+scale(yday)+scale(fecund)+(ID|site), 
                 family=binomial, weights=totov,data=plt)
summary(prmod_plt)
#Incorporating random slope shows that yday is the only significant parameter in the model

### check for overdispersion
overdisp_fun <- function(model) {
  ## number of variance parameters in 
  ##   an n-by-n variance-covariance matrix
  vpars <- function(m) {
    nrow(m)*(nrow(m)+1)/2
  }
  model.df <- sum(sapply(VarCorr(model),vpars))+length(fixef(model))
  rdf <- nrow(model.frame(model))-model.df
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}
overdisp_fun(prmod_plt)
### waaaaaaaaaaaaaaaay overdispersed

## some plots that show possible overdispersion
sjp.glmer(prmod_plt,  type='re.qq')
plot(allEffects(prmod_plt))## huge confidence regions 
sjp.glmer(prmod_plt, facet.grid=FALSE, sort="sort.all")
sjp.glmer(prmod_plt, type = "fe", facet.grid=FALSE)## what does huge ci mean?

sjp.glmer(srmod_plt,  type='re.qq')

### Sr plots

##  
srmodf_plt<-glmer(seeds/totov~ trmnt*scale(prop.c)+scale(fecund)+scale(yday)+(1|site/ID), 
                  family=binomial, weights=totov, data=plt)
summary(srmodf_plt)
## test for overdispersion
overdisp_fun(srmodf_plt)
### more overdispersion

### some plots
plot(allEffects(srmodf_plt))
  
sjp.glmer(srmodf_plt,  type='re.qq')

# These are more or less the same results as the PR model


### Now let's sum a plants seeds, polovs, and totovs over all rounds.
### So instead of 4 or less observations per plant, it's 1 observation per plant.

seed_nr<-glmer.nb(seeds~trmnt*scale(prop.c)+offset(log(trtflw))+(1|site/ID), data=plt_nr)
summary(seed_nr) #failure to converge


#Nbinom glmm using glmmTMB
seedtmb_nr<-glmmTMB(seeds~trmnt*prop.c+scale(fecund)+ offset(log(trtflw))+(1|site/ID),family=nbinom1, data=plt_nr)
summary(seedtmb_nr)#this model comes out the same using a gaussian distribution
## only trtlowers significant predictor for total seeds

plot(ggpredict(seedtmb_nr, c("trtflw","trmnt")))
plot(ggpredict(seedtmb_nr, c("prop.c","trmnt")))

#this model comes out the same using a gaussian distribution

### trtflw significant, but this is using total seeds collected from the plant
### so more trtflw=more seeds versus ag actually having an effect

#avg seed doesn't converge
potmb_nr<-glmmTMB(polov~trmnt*prop.c+trtflw+(1|site/ID),family=nbinom1,data=plt_nr)
summary(potmb_nr)
plot(ggpredict(potmb_nr, c("trtflw","trmnt")))
plot(ggpredict(potmb_nr, c("prop.c","trmnt")))
## doesn't converge using fruit removed, probably because there are some really high numbers


##################
### total polov/totov
######################
prmod_nr<-glmer(seeds~ trmnt*scale(prop.c)+offset(log(frt))+(1|site), 
                 family=poisson, data=plt_nr)
summary(prmod_nr)# no convergence without scaling, but
#scaling makes prop.c only marginally significant with interaction term included
#trtflw significant for # of successes
overdisp_fun(prmod_nr) #  a little overdispersed

plot(allEffects(prmod_nr))
plot(ggeffect(prmod_nr,c("prop.c", "trmnt"))) ### about the same.
plot(ggeffect(srmod_nr,c("prop.c", "trmnt")))
plot(ggeffect(srmod_nr,c("trtflw", "trmnt")))

######################
### total seed/totov
######################


### scale function instead of manually scaling
srmod_nb<-glmmTMB(seeds~ trmnt*scale(prop.c)+fecund+offset(log(totov))+(1|site),
                  family=nbinom1, data=plt_nr)
### drop ID level random effect since there's only one observation per plant
### I think this may
summary(srmod_nb)### overdispersed


#compare overdispersion measures
overdisp_fun(srmod_nb)### model looks good
gof(srmod_nrsc)
sum(residuals(srmod_nrsc,"pearson")^2)# 

ggeffect(srmod_nb, c("trmnt","prop.c"))

plot(srmod_nb, add.smooth = FALSE, which = 1)
par(mfrow=c(1,1))
residuals <- resid(srmod_nb)
plot(residuals)

plot(plt_nr$prop.c, residuals, xlab = "% Ag",
     ylab = "Residuals")
plot(plt_nr$trmnt, residuals, xlab = "trmnt",
     ylab = "Residuals")
plot(plt_nr$fecund, residuals, xlab = "fecund",
     ylab = "Residuals")
plot(plt_nr$trtflw, residuals, xlab = "trtflw",
     ylab = "Residuals")
qqnorm(residuals)
qqline(residuals)


### effect plots
plot(srmod_nb)




### other gosh dern plotting methods

lattice::xyplot(seeds~trmnt| site, groups=site, data=plt, type=c('p','r'), auto.key=F)

lattice::xyplot(seeds~trmnt| site, groups=site, data=plt_nr, type=c('p','r'), auto.key=F)

lattice::xyplot((seeds/totov)~trmnt| site, groups=site, data=plt, type=c('p','r'), auto.key=F)

lattice::xyplot((seeds/totov)~trmnt| site, groups=site, data=plt_nr, type=c('p','r'), auto.key=F)
``



