library(lme4)
library(ggplot2)
library(geepack)
library(splines)
library(R2jags)
library(R2WinBUGS)
library(mcmcplots)

plt_nr<-read.csv("data/plt_nr.csv")


glmer.nhoot<-glmer(FemaleVisit.5m~ DofY + Place + PrePost + (1|MaleID), data=nat.hoots, family=binomial())
summary(glmer.nhoot)

glmmfit<-function(){
  
  # Priors: intercepts
  for(i in 1:nmales){
    alpha[i]~dnorm(mu.int, tau.int)
  }
  mu.int~dnorm(0, 0.001) # mean intercept
  sig.int~dunif(0,50) # variance of the intercepts
  tau.int<-1/(sig.int*sig.int)
  
  ###############################
  # Priors from kery 
  for (i in 1:n.groups){
    alpha[i] ~ dnorm(mu.int, tau.int) # Intercepts
    beta[i] ~ dnorm(mu.beta, tau.beta) # Slopes
  }
  mu.int ~ dnorm(0, 0.001) # Hyperparameter for random intercepts
  tau.int <– 1 / (sigma.int * sigma.int)
  sigma.int ~ dunif(0, 10)
  
  mu.beta ~ dnorm(0, 0.001 # Hyperparameter for random slopes
                  tau.beta <– 1 / (sigma.beta * sigma.beta)
                  sigma.beta ~ dunif(0, 10)
  
  ###########################
  
  
  # Priors for regression parameters
  beta.DofY~dnorm(0,0.001)
  beta.PlaceTZ~dnorm(0,0.001)
  beta.PlaceWZ~dnorm(0,0.001)
  beta.Post ~ dnorm(0,0.001)
#####################################################
##modeling proportional binomial in JAGS
##From: https://stackoverflow.com/questions/29977578/logistic-regression-when-response-is-a-proportion-using-jags
  
  
  jagsdata <- data.frame(y=rbinom(10, 500, 0.2),
                         n=sample(500:600, 10),
                         x=sample(0:100, 10))
  model <- function() {
    ## Specify likelihood
    for(i in 1:10){
      y[i] ~ dbin(p[i], n[i])
      logit(p[i]) <- b0 + b1*x[i]
    }
    ## Specify priors
    b0 ~ dnorm(0, 0.0001)
    b1 ~ dnorm(0, 0.0001)
  }
######################################
  # Likelihood
  
  for(i in 1:n){
    # Likelihood for each individual
    logitp[i]<-alpha[male[i]]+beta.DofY*DofY[i]+beta.PlaceTZ*I.TZ[i]+
      beta.PlaceWZ*I.WZ[i]+beta.Post*I.Post[i]
    logitseed_ratio
    p[i]<-exp(logitp[i])/(1+exp(logitp[i])) 
    
    I.female.visit[i]~dbern(p[i])
    
    seed_ratio[i]~dbin(p[i],n[i])
                        
                        )
    glmer.nhoot<-glmer(FemaleVisit.5m~ DofY + Place + PrePost + (1|MaleID) 
    # Generate "new" data
    I.new.visit[i]~dbin(p[i],N[i])
    
    for (i in 1:n) {
      C[i] ~ dbin(p[i], N[i])
      logit(p[i]) <– alpha[pop[i]] + beta[pop[i]]* prec[i]
    }
  }
    
    # Pearson residuals
    presi[i]<-(I.female.visit[i]-p[i])/sqrt(p[i]*(1-p[i])) # Pearson Resid
    presi.new[i]<-(I.new.visit[i]-p[i])/sqrt(p[i]*(1-p[i])) # Pearson Resid new dataq
    
    # Discrepancy measures
    D[i]<-pow(presi[i], 2)
    D.new[i]<-pow(presi.new[i],2)
  }
  fit<-sum(D[])
  fit.new<-sum(D.new[])
} 

###form of model###
FemaleVisit.5m~ DofY + Place + PrePost + (1|MaleID), data=nat.hoots, family=binomial())

Seeds/totov~ trmnt*scale(prop.c)+trtflw+(1|site/ID),data=plt_nr, weights=totov, family=binomial
                           
seeds~trmnt*prop.c+trtflw
# Bundle data
nmales<-length(unique(nat.hoots$MaleID))  # number of males-- this is used for hyperparameters
nplants<-length(unique(plt_nr$ID))  # number of plants-- this should be used along with 

n<-nrow(nat.hoots)
n<-nrow(plt_nr)

male<-as.numeric(as.factor(nat.hoots$MaleID))
plant<-as.numeric(as.factor(plt_nr$ID))

jags.data <- list(n=n, nmales=nmales, male=male, DofY= as.numeric(nat.hoots$DofY), 
                  I.TZ=1*I(nat.hoots$Place=="TZ"), I.WZ=1*I(nat.hoots$Place=="WZ"),
                  I.Post=1*I(nat.hoots$PrePost=="Post"),  I.female.visit=nat.hoots$FemaleVisit.5m)

jags

# Parameters to estimate
params <- c("beta.DofY", "beta.PlaceTZ", "beta.PlaceWZ", "beta.Post", "p", "presi", "presi.new", "fit", "fit.new", "mu.int","sig.int", "I.new.visit")

# MCMC settings
nc <- 3 
ni <- 80000
nb <- 10000
nt <- 3

# Start sampler 
out.glmm <- jags(data=jags.data, parameters.to.save=params, 
                 model.file=glmmfit, n.thin=nt, n.chains=nc, n.burnin=nb, 
                 n.iter=ni, progress.bar="gui")