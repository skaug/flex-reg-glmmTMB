library(glmmTMB)
library(GLMsData)
data(motorins1)
?motorins1
motorins1$Kilometres = as.factor(motorins1$Kilometres)
motorins1$Bonus = as.factor(motorins1$Bonus)
motorins1$Make = as.factor(motorins1$Make)

hist(motorins1$Claims,xlab="Num Claims (N)",
     main="Aggregated insurance data \n (20 exact 0's)")

hist(motorins1$Payment,xlab="Total payment for company (y) in Swedish kroner",
     main="Aggregated payments for insurance data \n (20 exact 0's)", nclass=30)

plot(motorins1$Insured,motorins1$Payment,xlab="Num Cars Insured (policy years)",
     ylab="Total payment (y)",main="Motivaton for using \"offset\"")

t1 = glmmTMB(Payment ~ offset(log(Insured)) + Kilometres + Bonus + Make, 
             family="tweedie", motorins1)
summary(t1)
family_params(t1)
