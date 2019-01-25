###################
###Seed models####
#################
#install.packages("glmmTMB")
#install.packages("ggeffects")
#nstall.packages("TMB")
#install.packages("sjPlot")
#install.packages("blmeco")
#install.packages("aods3")
#install.packages("car")
#install.packages("MCMCglmm")
#install.packages("lme4")
#install.packages("pbkrtest")
#install.packages("effects")
#install.packages("merTools")

#### 
library(TMB)
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
library(merTools)
seed_land<-read.csv("data/seed_land.csv")
seed_land <-filter(seed_land, plot=="hi")

seed_land$round<-as.factor(seed_land$round)
#seed_land$temp.start[seed_land$temp.start=="na"] <- 22.3 
#seed_land$temp.start<-as.numeric(levels(seed_land$temp.start))[seed_land$temp.start]

# Create table containing:
# PR
# SR
# Seed
# Poll Ovs
# Fecund
# # flowers
# prop.c
# yday?
# start temperature


##Let's make sure our continuous responses aren't collinear; 
#code for corvif is located in zuur_correlation script file
# from Zuur 2010, "A protocol to avoid common statistical problems"


Z<-plt[,c("yday","temp.start")]
corvif(Z)    


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
plt$temp.start<-as.numeric(levels(plt$temp.start))[plt$temp.start]# make numeric

#responses summed ignoring round
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

#### These are important figures!!!
### Seeds 
ggplot(data=plt, aes(prop.c,seeds, color=trmnt))+geom_point()+
geom_smooth(method=lm)
### poll ovules
ggplot(data=plt_nr, aes(prop.c,polov, color=trmnt))+geom_point()+
  geom_smooth(method=lm)
### seed  over time
ggplot(data=plt,aes(temp.start,seeds, color=trmnt))+geom_point()+
  geom_smooth(method=lm)

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
ggplot(data=plt, aes(round,temp.start, group=round))+geom_boxplot()

### temperature effects hp slightly more than op, but unsure of significance
### this would make sense, because pollination would proceed as normal otherwise.
############################### 

############
#   models #
###########

# total seed or pollinated ovules, number of fruit as offset
#total seed model using glmmTMB (see Bolker GLMM FAQ for notes)


#using glmmTMB
stmb_plt1<-glmmTMB(seeds~(trmnt)*scale(prop.c)+
                           offset(log(fruit_count)) +
                           (1|site/ID),family=nbinom1, data=plt) 
summary(stmb_plt1)
ranef(stmb_plt1)

### plotting predicted values
ggplot(plt, aes(x = prop.c, y = seeds, colour = trmnt)) +
  geom_point() +


#not sure whether type II or III anova is appropriate to test fixed effects
# type III is the p value of the coefficient conditional on the other coefficients and the interaction!

Anova(stmb_plt1, type= "III")

drop1(stmb_plt1,test="Chisq")

#residuals-- not sure whether this is necessary or interpretable
ggplot(data = NULL, aes(y = resid(stmb_plt1, type = "pearson"),
                        x = fitted(stmb_plt1))) + geom_point()
qq.line = function(x) {
  # following four lines from base R's qqline()
  y <- quantile(x[!is.na(x)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  return(c(int = int, slope = slope))
}
QQline = qq.line(resid(stmb_plt1, type = "pearson"))
ggplot(data = NULL, aes(sample = resid(stmb_plt1, type = "pearson"))) +
  stat_qq() + geom_abline(intercept = QQline[1], slope = QQline[2])


#confidence intervals
stmb_plt1_CI_prof <- confint(stmb_plt1)
stmb_plt1_CI_quad <- confint(stmb_plt1,method="Wald")
stmb_plt1_CI_prof
stmb_plt1_CI_quad


###r2 approximation from https://stats.stackexchange.com/questions/95054/how-to-get-an-overall-p-value-and-effect-size-for-a-categorical-factor-in-a-mi
r2.corr.mer <- function(m) {
  lmfit <-  lm(model.response(model.frame(m)) ~ fitted(m))
  summary(lmfit)$r.squared
}

r2.corr.mer(stmb_plt1)

#alternative measure from same source as r2.corr.mer and in the following paper: https://onlinelibrary.wiley.com/doi/abs/10.1002/sim.1572
1-var(residuals(stmb_plt1))/(var(model.response(model.frame(stmb_plt1))))
####

### plot fitted values for response
#conditioned on random effects
theme_set(theme_bw())
plot_model(stmb_plt1,type="pred",terms=c("prop.c","trmnt"), 
           pred.type="re", show.data = TRUE,colors="bw")

#conditioned on fixed effects 
theme_set(theme_bw())
plot_model(stmb_plt1,type="pred",terms=c("prop.c","trmnt"),
           pred.type="fe", show.data=TRUE, colors="bw")

#conditioning on fixed effects narrows CIs
#Info on how "pred" works:
#https://www.rdocumentation.org/packages/ggeffects/versions/0.8.0/topics/ggaverage

# other model types
plot_model(stmb_plt1, type="re",show.values=TRUE)+ylim(-.5,.5)
plot_model(stmb_plt1,type="est")
p[[2]]+ylim(-.5,.5)

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




