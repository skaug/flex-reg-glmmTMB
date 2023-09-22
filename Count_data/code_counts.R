library(glmmTMB)
library(DHARMa)
library(ggplot2); theme_set(theme_bw())

download.file("https://raw.githubusercontent.com/embruna/Brooks_etal_2019_HeliconiaData/master/hdat.RData", dest = "tmp.rda")
load("tmp.rda")
head(hdat)
#For now, we're ignoring pseudoreplication (Hurlbert 1984)

ggplot(hdat, aes(x=size, colour=habitat))+
	facet_grid(.~habitat)+
	ylab("inflorescences")+
	stat_sum(aes(y=infl, size=..n..), alpha=0.5)

m_pois = glmmTMB(infl ~ lsize * habitat, hdat, family=poisson)

r_pois = simulateResiduals(m_pois)
testDispersion(r_pois)

m_nb = glmmTMB(infl ~ lsize * habitat, hdat, family=nbinom2)

#not much need to worry about overdispersion with nbinom2, but just to see
r_nb = simulateResiduals(m_nb)
testDispersion(r_nb)
testZeroInflation(r_nb)

# Make predictions from the model

hdat$ pred = predict(m_nb, type="response")

# plot the predictions with the data
ggplot(hdat, aes(x=lsize, colour=habitat))+
	geom_line(aes(y=pred))+
	facet_grid(.~habitat)+
	ylab("inflorescences")+
	stat_sum(aes(y=infl, size=..n..), alpha=0.5)


# Data from Brooks, M. E., K. Kristensen, M. R. Darrigo, P. Rubim, 
# M. Uriarte, E. Bruna, and B. M. Bolker. 2019. 
# Statistical modeling of patterns in annual reproductive rates. 
# Ecology 00(00):e02706. 10.1002/ecy.2706
