# Script3. Correlations
load("com_data.RData")
# Removing communities without H2 and DSI
com_data=com_data[!is.na(com_data$H2)&!is.na(com_data$DSI),]
com_data$Latitude=abs(com_data$Latitude)
com_data$invconn=1-com_data$connectance
save(com_data,file="com_data.RData")
# Correlations between specialization and latitude ####
library(plotrix)
BIO=as.numeric(factor(com_data$Biome))
COLORS=c("#91a800ff","#f8756bff","#d19100ff","#00b835ff","#00bf9eff","#619cffff","#d970faff","#ff61c2ff")
#Partner specialization
plot(com_data$Latitude,com_data$invconn, pch=16,col=COLORS[BIO])
abline(lm(com_data$invconn~com_data$Latitude))
for (i in c(1:2,4:8)){
  ablineclip(lm(com_data$invconn[BIO==i]~com_data$Latitude[BIO==i]), col=COLORS[i],x1=min(com_data$Latitude[BIO==i]),x2=max(com_data$Latitude[BIO==i]))
}
cor.test(com_data$invconn,com_data$Latitude)
#Partner specialization (residuals)
m1=glm(invconn~spar*shost, family="quasibinomial", data=com_data)
cor.test(residuals(m1),com_data$Latitude)
#Frequency specialization
plot(com_data$Latitude,com_data$H2, pch=16,col=COLORS[BIO])
abline(lm(com_data$H2~com_data$Latitude))
for (i in c(1:2,4:8)){
  ablineclip(lm(com_data$H2[BIO==i]~com_data$Latitude[BIO==i]), col=COLORS[i],x1=min(com_data$Latitude[BIO==i]),x2=max(com_data$Latitude[BIO==i]))
}
cor.test(com_data$H2,com_data$Latitude)
#Frequency specialization (residuals)
m1=glm(H2~spar*shost, family="quasibinomial", data=com_data)
cor.test(residuals(m1),com_data$Latitude)
#Phylogenetic specialization
plot(com_data$Latitude,com_data$DSI, pch=16,col=COLORS[BIO])
abline(lm(com_data$DSI~com_data$Latitude))
for (i in c(1:2,4:8)){
  ablineclip(lm(com_data$DSI[BIO==i]~com_data$Latitude[BIO==i]), col=COLORS[i],x1=min(com_data$Latitude[BIO==i]),x2=max(com_data$Latitude[BIO==i]))
}
cor.test(com_data$DSI,com_data$Latitude)
#Phylogenetic specialization(residuals)
m1=glm(DSI~spar*shost, family="quasibinomial", data=com_data)
cor.test(residuals(m1),com_data$Latitude)
rm(BIO,COLORS,i,m1)