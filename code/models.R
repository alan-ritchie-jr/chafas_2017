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
install.packages("pbkrtest")
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
library(pbkrtest)
library(MCMCglmm)
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
stmb_plt1<-glmmTMB(seeds~trmnt*scale(prop.c)+scale(temp.start) +
                  offset(log(fruit_count)) +
                    (1|site/ID),family=nbinom1, data=plt) 

summary(stmb_plt1)

#not sure whether type II or III anova is appropriate to test fixed effects
Anova(stmb_plt1, type= "III")

# type III is the p value of the coefficient conditional on the other coefficients and the interaction!

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

#confidence intervals
stmb_plt1_CI_prof <- confint(stmb_plt1)
stmb_plt1_CI_quad <- confint(stmb_plt1,method="Wald")
stmb_plt1_CI_prof
stmb_plt1_CI_quad

### parametric bootstrap CI from BBolker
PBsimfun <- function(m0,m1,x=NULL) {
  if (is.null(x)) x <- simulate(m0)
  m0r <- try(refit(m0,x[[1]]),silent=TRUE)
  if (inherits(m0r,"try-error")) return(NA)
  m1r <- try(refit(m1,x[[1]]),silent=TRUE)
  if (inherits(m1r,"try-error")) return(NA)
  c(-2*(logLik(m0r)-logLik(m1r)))
}

#reduced model to test against
stmb_plt0<-glmmTMB(seeds~trmnt*scale(prop.c)+
                     offset(log(fruit_count)) +
                     (1|site/ID),family=nbinom1, data=plt) 
set.seed(101)
PBrefdist <- replicate(400,PBsimfun(stmb_plt0,stmb_plt1))

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


###### Old plotting code; may not work with glmTMB
### check for homogeneity, etc.
op <- par(mfrow = c(2, 2), mar = c(5, 4, 1, 2))
plot(stmb_plt1, add.smooth = FALSE, which = 1)
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



### other gosh dern plotting methods

lattice::xyplot(seeds~trmnt| site, groups=site, data=plt, type=c('p','r'), auto.key=F)

lattice::xyplot(seeds~trmnt| site, groups=site, data=plt_nr, type=c('p','r'), auto.key=F)

lattice::xyplot((seeds/totov)~trmnt| site, groups=site, data=plt, type=c('p','r'), auto.key=F)

lattice::xyplot((seeds/totov)~trmnt| site, groups=site, data=plt_nr, type=c('p','r'), auto.key=F)
``



