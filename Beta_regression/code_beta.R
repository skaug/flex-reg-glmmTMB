library(betareg)
library(glmmTMB)
library(bbmle)
library(ggplot2); theme_set(theme_bw())

data("GasolineYield")

ggplot(GasolineYield, aes(temp, yield))+geom_point(aes(colour=batch))

#gy2 = betareg(yield ~ batch + temp | temp, data = GasolineYield)

gy3 = glmmTMB(yield ~ batch + temp, 
              disp=~ temp, 
              data = GasolineYield, 
              family=beta_family())

#summary(gy2)
summary(gy3)

?sigma.glmmTMB
fixef(gy3)$disp

gy4 = glmmTMB(yield ~ batch + temp, 
              data = GasolineYield, 
              family=beta_family())

AICtab(gy3, gy4)
