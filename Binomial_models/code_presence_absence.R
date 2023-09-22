library(glmmTMB)
library(bbmle)
library(ggplot2); theme_set(theme_bw()) 
library(plyr)
library(sjPlot)

data("Salamanders")
?Salamanders
summary(Salamanders)

Salamanders = transform(Salamanders, 
	present = Salamanders$count>0,
	mined=factor(mined, levels=c("no", "yes")))

ddply(Salamanders, ~spp+mined, summarize,
			present = mean(present))

m1 = glmmTMB(present~0+spp+ spp:mined,
									family=binomial("cloglog"), 
									data=Salamanders)
summary(m1)

