### Phylogenetic Specialization ~ Quantitative Specialization ####
m1=glm(DSI~H2, family="quasibinomial", data=com_data)
plot(com_data$DSI~com_data$H2, pch=16, ylab="Phylogenetic Specialization (DSI)", xlab="Quantitative Specialization (H2')")
curve((exp(m1$coefficients[1]+m1$coefficients[2]*x))/(1+(exp(m1$coefficients[1]+m1$coefficients[2]*x))), add=T, lwd=2, lty=2)
### Quantitative Specialization ~ Parasite Richness and Latitude ####
m2=glm(H2~spar+Latitude, family="quasibinomial", data=com_data)
#
plot(com_data$H2~com_data$spar, pch=16, ylab="Quantitative Specialization (H2')", xlab="Parasite Richness")
curve((exp(m2$coefficients[1]+m2$coefficients[2]*x+m2$coefficients[3]*0))/(1+(exp(m2$coefficients[1]+m2$coefficients[2]*x+m2$coefficients[3]*0))), add=T, lwd=2, lty=2, col=2)
curve((exp(m2$coefficients[1]+m2$coefficients[2]*x+m2$coefficients[3]*20))/(1+(exp(m2$coefficients[1]+m2$coefficients[2]*x+m2$coefficients[3]*20))), add=T, lwd=2, lty=2, col=3)
curve((exp(m2$coefficients[1]+m2$coefficients[2]*x+m2$coefficients[3]*40))/(1+(exp(m2$coefficients[1]+m2$coefficients[2]*x+m2$coefficients[3]*40))), add=T, lwd=2, lty=2, col=4)
legend(x=60,y=1,legend = c("Latitude: 0","Latitude: 20","Latitude: 40"), lty=1,lwd=2, col=c(2,3,4),bty = "n",y.intersp = 1.5)
#
plot(com_data$H2~com_data$Latitude, pch=16, ylab="Quantitative Specialization (H2')", xlab="Latitude")
curve((exp(m2$coefficients[1]+m2$coefficients[2]*10+m2$coefficients[3]*x))/(1+(exp(m2$coefficients[1]+m2$coefficients[2]*10+m2$coefficients[3]*x))), add=T, lwd=2, lty=2, col=2)
curve((exp(m2$coefficients[1]+m2$coefficients[2]*25+m2$coefficients[3]*x))/(1+(exp(m2$coefficients[1]+m2$coefficients[2]*25+m2$coefficients[3]*x))), add=T, lwd=2, lty=2, col=3)
curve((exp(m2$coefficients[1]+m2$coefficients[2]*40+m2$coefficients[3]*x))/(1+(exp(m2$coefficients[1]+m2$coefficients[2]*40+m2$coefficients[3]*x))), add=T, lwd=2, lty=2, col=4)
legend(x=30,y=.3,legend = c("Par richness: 10","Par richness: 25","Par richness: 40"), col=c(2,3,4),bty = "n",y.intersp = 1.5,lty=1, lwd=2)
### Binary Specialization ~ Parasite Richness, Host Richness and Latitude ####
m3=glm(invconn~spar+shost+Latitude,family="quasibinomial", data=com_data)
#
plot(com_data$invconn~com_data$spar, pch=16, ylab="Binary Specialization", xlab="Parasite Richness")
#
plot(com_data$invconn~com_data$shost, pch=16, ylab="Binary Specialization", xlab="Host Richness")
#
plot(com_data$invconn~com_data$Latitude, pch=16, ylab="Binary Specialization", xlab="Latitude")
curve((exp(m3$coefficients[1]+m3$coefficients[2]*15+m3$coefficients[3]*30+m3$coefficients[4]*x))/(1+(exp(m3$coefficients[1]+m3$coefficients[2]*15+m3$coefficients[3]*30+m3$coefficients[4]*x))), add=T, lwd=2, lty=2, col=2)
curve((exp(m3$coefficients[1]+m3$coefficients[2]*30+m3$coefficients[3]*30+m3$coefficients[4]*x))/(1+(exp(m3$coefficients[1]+m3$coefficients[2]*30+m3$coefficients[3]*30+m3$coefficients[4]*x))), add=T, lwd=2, lty=2, col=3)
curve((exp(m3$coefficients[1]+m3$coefficients[2]*15+m3$coefficients[3]*60+m3$coefficients[4]*x))/(1+(exp(m3$coefficients[1]+m3$coefficients[2]*15+m3$coefficients[3]*60+m3$coefficients[4]*x))), add=T, lwd=2, lty=2, col=4)
curve((exp(m3$coefficients[1]+m3$coefficients[2]*30+m3$coefficients[3]*60+m3$coefficients[4]*x))/(1+(exp(m3$coefficients[1]+m3$coefficients[2]*30+m3$coefficients[3]*60+m3$coefficients[4]*x))), add=T, lwd=2, lty=2, col=5)
legend(x=30,y=.75,legend = c("Richness","Par: 15  Host: 30","Par: 15  Host: 60","Par: 30  Host: 30","Par: 30  Host: 60"), col=c(NA,2,4,3,5),bty = "n",y.intersp = 1.5,lty=1, lwd=2)
### Parasite Richness ~ Host Phylogenetic Diversity, Host Richness and Sampling ####
m4=glm(spar~phydiversity+shost+nhost, family="poisson", data=com_data)
#
plot(com_data$spar~com_data$phydiversity, pch=16, ylab="Parasite Richness", xlab="Host Phylogenetic Diversity")
#
plot(com_data$spar~com_data$shost, pch=16, ylab="Parasite Richness", xlab="Host Richness")
curve((exp(m4$coefficients[1]+m4$coefficients[2]*mean(com_data$phydiversity)+m4$coefficients[3]*x+m4$coefficients[4]*100)), add=T, lwd=2, lty=2, col=2)
curve((exp(m4$coefficients[1]+m4$coefficients[2]*mean(com_data$phydiversity)+m4$coefficients[3]*x+m4$coefficients[4]*200)), add=T, lwd=2, lty=2, col=3)
curve((exp(m4$coefficients[1]+m4$coefficients[2]*mean(com_data$phydiversity)+m4$coefficients[3]*x+m4$coefficients[4]*400)), add=T, lwd=2, lty=2, col=4)
legend(x=10,y=75,legend = c("Sampling: 100","Sampling: 200","Sampling: 400"), lty=1,lwd=2, col=c(2,3,4),bty = "n",y.intersp = 1.5)
#
plot(com_data$spar~com_data$nhost, pch=16, ylab="Parasite Richness", xlab="Sampling")
curve((exp(m4$coefficients[1]+m4$coefficients[2]*mean(com_data$phydiversity)+m4$coefficients[3]*30+m4$coefficients[4]*x)), add=T, lwd=2, lty=2, col=2)
curve((exp(m4$coefficients[1]+m4$coefficients[2]*mean(com_data$phydiversity)+m4$coefficients[3]*60+m4$coefficients[4]*x)), add=T, lwd=2, lty=2, col=3)
legend(x=10,y=75,legend = c("Host Richness: 30","Host Richness: 60"), lty=1,lwd=2, col=c(2,3,4),bty = "n",y.intersp = 1.5)
### Host Phylogenetic Diversity ~ Host Richness ####
m5=glm(phydiversity~shost,data=com_data)
plot(com_data$phydiversity~com_data$shost, pch=16, ylab="Host Phylogenetic Diversity", xlab="Host Richness")
abline(m5, lwd=2, lty=2)
### Host Richness ~ Sampling and Latitude ####
m6=glm(shost~nhost+Latitude, family="poisson", data=com_data)
plot(com_data$shost~com_data$nhost, pch=16, ylab="Host Richness", xlab="Sampling")
#
plot(com_data$shost~com_data$Latitude, pch=16, ylab="Host Richness", xlab="Latitude")
curve(exp(m6$coefficients[1]+m6$coefficients[2]*100+m6$coefficients[3]*x), add=T, lwd=2, lty=2, col=2)
curve(exp(m6$coefficients[1]+m6$coefficients[2]*200+m6$coefficients[3]*x), add=T, lwd=2, lty=2, col=3)
curve(exp(m6$coefficients[1]+m6$coefficients[2]*400+m6$coefficients[3]*x), add=T, lwd=2, lty=2, col=4)

legend(x=30,y=140,legend = c("Sampling: 100","Sampling: 200","Sampling: 400"), lty=1,lwd=2, col=c(2,3,4),bty = "n",y.intersp = 1.5)