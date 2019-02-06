###################
###Seed models####
#################
#remove.packages(c("TMB","glmmTMB","lme4"))
#install.packages("TMB", type='source')
#install.packages("glmmTMB")
#install.packages("ggeffects")
#install.packages("sjPlot")
#install.packages("blmeco")
#install.packages("aods3")
#install.packages("car")
#install.packages("MCMCglmm")
#install.packages("lme4")
#install.packages("pbkrtest")\
#install.packages("Matrix")
####

#Dan start here; run lines 20-67

library(TMB)
library(car)
library(glmmTMB)
library(lme4)
library(ggeffects)
library(tidyverse)

seed_land<-read.csv("data/seed_land.csv")
seed_land <-filter(seed_land, plot=="hi")

seed_land$round<-as.factor(seed_land$round)
#seed_land$temp.start[seed_land$temp.start=="na"] <- 22.3 
#seed_land$temp.start<-as.numeric(levels(seed_land$temp.start))[seed_land$temp.start]



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

############
#   models #
###########


# Dan run 75-85

# total seed or pollinated ovules, number of fruit as offset
stmb_plt1<-glmmTMB(seeds~trmnt*prop.c +
                  offset(log(fruit_count)) +
                    (1|site/ID),family=nbinom1, data=plt) 

summary(stmb_plt1)

#confidence intervals
stmb_plt1_CI_prof <- confint(stmb_plt1)
stmb_plt1_CI_quad <- confint(stmb_plt1,method="wald")
stmb_plt1_CI_prof
stmb_plt1_CI_quad

#Dan after this point its mdoel assessment code


# Test of significance of fixed effects using 
#not sure whether type II or III anova is appropriate to test fixed effects
# type III is the p value of the coefficient conditional on the other coefficients and the interaction!

Anova(stmb_plt1, type= "III")

drop1(stmb_plt1,test="Chisq")

#residuals
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

###r2 approximation from https://stats.stackexchange.com/questions/95054/how-to-get-an-overall-p-value-and-effect-size-for-a-categorical-factor-in-a-mi
r2.corr.mer <- function(m) {
  lmfit <-  lm(model.response(model.frame(m)) ~ fitted(m))
  summary(lmfit)$r.squared
}

r2.corr.mer(stmb_plt1)

#alternative measure from same source as r2.corr.mer and in the following paper: https://onlinelibrary.wiley.com/doi/abs/10.1002/sim.1572
1-var(residuals(stmb_plt1))/(var(model.response(model.frame(stmb_plt1))))
####
###########


### alternative parameterization (see bolker glmm FAQ)
stmb_plt2<-glmmTMB(seeds~trmnt*prop.c+ offset(log(fruit_count))+
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
#


