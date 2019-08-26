### Not used in manuscript as of July 2019

# post-hoc tests between treatments

### April 2019: check out what fruit level responses and ovules

######
#post- hoc test
#did the number of fruit differ between treatments?
##glm anova with nb error distribution of n_fruit~trmnt (per plant basis)

library(lme4)
library(nlme)
library(glmmTMB)
library(MASS)
library(car)
library(tidyverse)

pollination<-read.csv("data/pollination_2017_pp_plant_trait.csv")

#upload seed data merged with the environmental data
seed_land<-read.csv("data/seed_land.csv")
seed_land<-seed_land%>%filter(plot=="hi")

trmnt_mod<-glm.nb(n_fruit~trmnt,data=indi_seed_tbl)
summary(trmnt_mod)
qqnorm(residuals(trmnt_mod))
plot(trmnt_mod)

#likelihood ratio test 
trmnt_mod_drop<-glm.nb(n_fruit~1,data=indi_seed_tbl)

anova(trmnt_mod,trmnt_mod_drop)
anova(trmnt_mod_drop,trmnt_mod)

#tukey
library(multcomp)
summary(glht(trmnt_mod, linfct = mcp(trmnt = "Tukey")), test = adjusted("holm"))
#
qqnorm(residuals(trmnt_mod))
plot(trmnt_mod)


###Post-hoc 2
# did the number of seeds/ fruit differ between treatments?
## seeds/fruit

##lm
trmnt_mod2_lm<-lm(seeds_fruit~trmnt,data=indi_seed_tbl)
summary(trmnt_mod2_lm)

#some diagnostic plots
qqnorm(residuals(trmnt_mod2_lm))
plot(trmnt_mod2_lm)
qqPlot(trmnt_mod2_lm$residuals)
qqPlot(residuals(trmnt_mod2_lm))
### fits assumptions
trmnt_mod2_drop<-lm(seeds_fruit~1,data=indi_seed_tbl)
#post-hoc anova on model
anova(trmnt_mod2_lm,trmnt_mod2_drop)
# but seeds/fruit were the same.

#tukey
summary(glht(trmnt_mod2_lm, linfct = mcp(trmnt = "Tukey")), test = adjusted("holm"))

#these agree

### posthoc 3

### did the number of seeds/ovules look the same between treatments?


s_o_trmnt_mod<-glm(sum_seeds/sum_ovules~trmnt,family=binomial,weights=sum_ovules,
                   data=indi_seed_tbl)

summary(s_o_trmnt_mod)

s_o_trmnt_q<-glm(sum_seeds/sum_ovules~trmnt,family=quasibinomial,weights=sum_ovules,
                 data=indi_seed_tbl)

summary(s_o_trmnt_q)

# quasi and normal binomial give same result.

#test glmm
s_o_trmnt_glmm<-glmer(sum_seeds/sum_ovules~trmnt+(1|ID),family=binomial,weights=sum_ovules,
                      data=indi_seed_tbl)
summary(s_o_trmnt_glmm)

s_o_sim<-simulateResiduals(fittedModel = s_o_trmnt_mod, n = 250)
plot(s_o_sim) # no strange patterns in predicted vs residuals
testDispersion(s_o_sim)
testUniformity(s_o_sim)
testZeroInflation(s_o_sim)

s_o_simq<-simulateResiduals(fittedModel = s_o_trmnt_q, n = 250)
plot(s_o_sim) # no strange patterns in predicted vs residuals
testDispersion(s_o_simq)
testUniformity(s_o_simq)
testZeroInflation(s_o_simq)

s_o_glmm_sim<-simulateResiduals(fittedModel = s_o_trmnt_glmm, n = 250)# warning message because glmmTMB was used
plot(s_o_glmm_sim) # no strange patterns in predicted vs residuals
testDispersion(s_o_glmm_sim)
testUniformity(s_o_glmm_sim)
testZeroInflation(s_o_glmm_sim)

####
# # of ovules per fruit
#####
ov_frt_mod<-glm.nb(sum_ovules~trmnt+offset(log(n_fruit)),data=indi_seed_tbl)

summary(ov_frt_mod)

ov_frt_mod_drop<-glm.nb(sum_ovules~1+offset(log(n_fruit)),data=indi_seed_tbl)

anova(ov_frt_mod,ov_frt_mod_drop)

ov_frt_lm<-lm(ovules_fruit~trmnt,data=indi_seed_tbl)
summary(ov_frt_lm)

