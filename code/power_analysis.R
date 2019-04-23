library(simr)
library(lme4)
library(binom)
#make a power analysis model to tinker with
seed_pow<-seed_mod_glmer

#look at fixed effects 
fixef(seed_pow)

#fixed effect for trmntop:scale(prop.c)

fixef(seed_pow)["trmntop:scale(prop.c)"]

#specify a power to detect "biological relevance"
fixef(seed_pow)["trmntop"]<--0.05

powerSim(seed_pow)
