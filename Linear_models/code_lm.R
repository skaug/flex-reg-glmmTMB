library(glmmTMB)

## Annette Dobson (1990) "An Introduction to Generalized Linear Models" pg 9.
ctl = c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt = c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group = gl(2, 10, 20, labels = c("Ctl","Trt"))
weight = c(ctl, trt)
dat = data.frame(weight, group)
head(dat)

head(model.matrix(weight~group, dat))

tail(model.matrix(weight~group, dat))

(lm.D9 <- glmmTMB(weight ~ group, dat))

# for demonstration purposes, we can change the contrast
dat2 = transform(dat, group=factor(group, levels=c("Trt", "Ctl")))
(lm.D92 <- glmmTMB(weight ~ group, dat2))

# normal homoscedastic residuals
res = residuals(lm.D9)
plot(res)
