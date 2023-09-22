#Density-dependent larvae survival From Reilly and Hajek (2008), via Bolker

dd = read.table(header=TRUE,
								 text="
density larvae surviving
1   90  60
5   90  60
10  89  56
15  87  41
20  93  31
")

dd

mquad = glmmTMB(cbind(surviving, larvae-surviving) ~ 
									density + I(density^2), 
								family="binomial", dd)

dd$pred_pquad = predict(mquad, type="response")
ggplot(dd, aes(x=density))+ 
	geom_point(aes(y=surviving/larvae))+
	geom_line(aes(y=pred_pquad))
