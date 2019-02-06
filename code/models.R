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
####

#Dan start here; run lines 20-45

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

seed_land<-read.csv("data/seed_land.csv")
seed_land <-filter(seed_land, plot=="hi")

seed_land$round<-as.factor(seed_land$round)


# summary table
plt<- seed_land %>%
  group_by(site,prop.c,trmnt, round, ID,flowers, fecund,yday,temp.start)%>%
  summarise(fruit_count=n(), trtflw=sum(unique(flowers)),
            polov=sum(poll_ovules),
            avg_polov=mean(poll_ovules),
            seeds=sum(total.seeds),
            avg_seed=mean(total.seeds),
            totov=sum(total.ovules),
            avg_totov=mean(total.ovules))


############
#   models #
###########


# Dan run 57-105

# total seed or pollinated ovules, number of fruit as offset
stmb_plt1<-glmmTMB(seeds~trmnt*scale(prop.c) +
                  offset(log(fruit_count)) +
                    (1|site/ID),family=nbinom1, data=plt) 

summary(stmb_plt1)

#confidence intervals
stmb_plt1_CI_prof <- confint(stmb_plt1)
stmb_plt1_CI_quad <- confint(stmb_plt1,method="wald")
stmb_plt1_CI_prof
stmb_plt1_CI_quad



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
p<-get_model_data(stmb_plt1,type="pred",terms=c("prop.c","trmnt"), 
                  pred.type="re", colors= "bw",ci.lvl= .95) 

#re=random effect conditioned 


### make separate dataframes for CI for plot
php<-p%>%filter(group=="hp")
pop<-p%>%filter(group=="op")

###make plot
ggplot(data=plt,aes(prop.c, seeds))+
  geom_point(aes(shape=trmnt),position="jitter")+
  geom_line(data=p, aes(x, predicted,linetype=group))+
  geom_ribbon(data=php,aes(x=x,ymin=conf.low, ymax=conf.high),alpha = 0.3,inherit.aes = FALSE)+
  geom_ribbon(data=pop,aes(x=x,ymin=conf.low, ymax=conf.high),alpha = 0.3,inherit.aes = FALSE)+
  labs(shape="Treatment",linetype="Predicted Response")+
  xlab("% Agriculture")+ylab("Seeds Produced")+
  ggtitle("Effect of % Agriculture on Seed Production",
          subtitle="Predicted Response vs. Data")

#Note: I haven't figured out how to facet both the data and the predicted values by treatment


# other model types available through this package
plot_model(stmb_plt1,type="est") #forest plot of parameter estimates



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

### plot fitted values for response
#conditioned on random effects
#### Try ggeffects and plot function??

mydf<-ggpredict(stmb_plt1, terms = c("prop.c","trmnt"),type="fe")
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
<<<<<<< HEAD
#
=======
## starting temp is most significant predictor
# significantly colder days in rd 4 (and some in rd 3) impacted seed set

##total polov glmmTMB
potmb_plt<-glmmTMB(polov~trmnt*scale(prop.c) +scale(temp.start)+ 
                     offset(log(fruit_count))+
                     (1|site/ID),family=nbinom1, data=plt) 
summary(potmb_plt)
# same results with polov


### Plotting output


>>>>>>> b6e08b2e1c79a8c2b236d88c2e302f104386cbf5


