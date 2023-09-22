library(glmmTMB)
library(ggplot2); theme_set(theme_bw())

#simulate data
sim1 = function(nID=1000, nt=10, IDsd=0.5, tsd=0.25, mu=0, residsd=1)
{
	dat = expand.grid(ID=1:nID, t=1:nt)
	n = nrow(dat)
	dat$REID = rnorm(nID, sd=IDsd)[dat$ID]
	dat$REt = rnorm(nt, sd=tsd)[dat$t]
	dat$y = rnorm(n, mean=mu, sd=residsd) + dat$REID + dat$REt
	dat
}
set.seed(101) 
d1 = sim1(mu=100, residsd=10)
set.seed(101) # to get same ID and t random effects
d2 = sim1(mu=200, residsd=5)
d1$treatment = "ten"
d2$treatment = "five"
dat = rbind(d1, d2)

# plot the data
ggplot(dat)+geom_histogram(aes(y))+ylab(NULL)
ggplot(dat)+geom_histogram(aes(y, fill=treatment))+ylab(NULL)

# model
m0 = glmmTMB(y ~ treatment + (1|t)+(1|ID), 
						 dispformula=~treatment, data=dat)

# extract results
fixef(m0)$disp
c(log(5^2), log(10^2)-log(5^2)) # expected dispersion model coefficients

fixef(m0)
