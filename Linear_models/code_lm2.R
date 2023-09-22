library(glmmTMB)
library(car) # for Anova
library(bbmle) # for AICctab
library(ggplot2); theme_set(theme_bw()) 
library(ecostats) # for data
data("globalPlants")
?globalPlants

head(globalPlants)

m1 = glmmTMB(height~lat, globalPlants)
summary(m1) # z-statistics

m2 = glmmTMB(height~ lat+rain+temp, globalPlants)

anova(m1, m2) # nested models

Anova(m2) # ANOVA table

AICctab(m1, m2) # works with non-nested models

#profile method
?profile.glmmTMB
prof= profile(m2)
str(prof)

ggplot(prof,aes(.focal,sqrt(value))) +
	geom_point() + geom_line()+
	facet_wrap(~.par,scale="free_x")+
	geom_hline(yintercept=1.96,linetype=2)

#confint method
?confint.glmmTMB
confint(m2, method="profile")
