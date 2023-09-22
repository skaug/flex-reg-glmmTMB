library(glmmTMB)
library(bbmle)
library(plyr)
library(DHARMa)
library(ggplot2); theme_set(theme_bw())

download.file("https://raw.githubusercontent.com/embruna/Brooks_etal_2019_HeliconiaData/master/hdat.RData", dest = "tmp.rda")
load("tmp.rda")


m1=glmmTMB(infl ~ lsize * habitat +(1|plot)+ (1|year)+ (1|ID), 
					 hdat, family="poisson")

VarCorr(m1)

summary(m1)

#to fit and AR(1) term you need a factor
hdat$times = factor(hdat$year)

m2=glmmTMB(infl ~ lsize * habitat +(1|plot)+ (1|year)+ ar1(times+0|ID), hdat, family="poisson")
VarCorr(m2)

summary(m2)

AICtab(m1, m2)


