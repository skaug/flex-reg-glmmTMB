library(glmmTMB)
library(bbmle)
library(plyr)
library(DHARMa)
library(ggplot2); theme_set(theme_bw())

data("Salamanders")
Salamanders = transform(Salamanders, 
	mined=factor(mined, levels=c("no", "yes")))

ggplot(Salamanders, aes(x=count, fill=mined))+
	geom_histogram(position="identity" , alpha=.7, binwidth = 1)+
	facet_wrap(~spp, scale="free")+
	ylab(NULL)+xlab(NULL)

zinbm = glmmTMB(count~0+spp + spp:mined+
                (1|site),
								zi=~ mined, 
								Salamanders, family=nbinom2)
summary(zinbm)

nbm = glmmTMB(count~0+spp + spp:mined+
                  (1|site),
                Salamanders, family=nbinom2)

AICctab(zinbm, nbm)
