library(glmmTMB)
library(bbmle)
library(plyr)
library(DHARMa)
library(ggplot2); theme_set(theme_bw())


grass = read.csv("https://datadryad.org/stash/downloads/file_stream/1619635")

grass = subset(grass, !is.na(prior_yr_mean_daily_mean_temp)) 

grass=transform(grass, 
								prop=percent_cover/100,
								temp = prior_yr_mean_daily_mean_temp,
								quadrat = paste(location, quadrat, sep="_")
)

ggplot(grass, aes(temp, prop))+geom_point()

# very simple model with lemon squeezer---- 
n = nrow(grass)
grass= transform(grass, newprop = (prop*(n-1)+1/2)/n)

m0 = glmmTMB(newprop ~temp + (1|location) + (1|year) + (1|quadrat), 						 
						 family="beta_family", 
						 data = grass
)
r0 = simulateResiduals(m0, n=1000)
testResiduals(r0)


# try zero-inflation with the original proportion data----

grass[which(grass$prop>.99),]$prop = .99 #truncate

mz0 = glmmTMB(prop ~temp + (1|location) + (1|year) + (1|quadrat), 	
							zi = ~temp,
						 family="beta_family", 
						 data = grass
)
rz0 = simulateResiduals(mz0, n=1000)
testResiduals(rz0)
summary(mz0)


# try adding a spline for more realistic shape----
library(splines)
mz1= glmmTMB(prop ~bs(temp, df=3) + (1|location) + (1|year) + (1|quadrat), 	
						 zi = ~bs(temp, df=3),
						family="beta_family", 
						data = grass
)
rz1 = simulateResiduals(mz1, n=1000)
testResiduals(rz1)

AICctab(mz0, mz1)

# plot model predictions----

grass$zprob = predict(mz1, type="zprob", re.form=NA)

sgrass = ddply(grass, ~year+location, summarize,
								temp=temp[1],
								prop_with_grass = mean(prop>0),
								zprob=mean(zprob)
)

ggplot(sgrass, aes(temp, prop_with_grass))+
	geom_point()+geom_line(aes(y=1-zprob))+
	xlab("Temperature (degrees C)")+
	ylab("Proportion of quadrats \n with eelgrass")

grass$pred = predict(mz1, type="conditional", re.form=NA)

sgrass2 = ddply(subset(grass, prop>0), ~year+location, summarize,
								 temp=temp[1],
								 prop = mean(prop),
								 pred=mean(pred))

ggplot(sgrass2, aes(temp, prop))+
	geom_point()+geom_line(aes(y=pred))+
	xlab("Temperature (degrees C)")+
	ylab("Proportion cover \n in quadrats with eelgrass")
