#script4. Structural Equation Model
library (piecewiseSEM)
load("com_data.RData")
# Piecewise SEM
# Initial model
Model1=psem(
  glm(DSI~H2+invconn+spar+phydiversity+shost+nhost+Latitude, family="quasibinomial", data=com_data),
  glm(H2~invconn+spar+phydiversity+shost+nhost+Latitude, family="quasibinomial", data=com_data),
  glm(invconn~spar+phydiversity+shost+nhost+Latitude,family="quasibinomial", data=com_data),
  glm(spar~phydiversity+shost+nhost+Latitude, family="poisson", data=com_data),
  glm(phydiversity~shost+nhost+Latitude,data=com_data),
  glm(shost~nhost+Latitude, family="poisson", data=com_data),
  glm(nhost~Latitude,family="poisson", data=com_data)
)
Sum1=summary(Model1, conserve=T)
Sum1$IC
Sum1$Cstat
# Stepwise reduction : Nested Models
# Model 2 (removed: invconn ~ nhost)
Model2=update(Model1,invconn~spar+phydiversity+shost+Latitude)
Sum2=summary(Model2, conserve=T)
Sum2$IC
Sum2$Cstat
# Model 3 (removed: H2 ~ invconn)
Model3=update(Model2,H2~spar+phydiversity+shost+nhost+Latitude)
Sum3=summary(Model3, conserve=T)
Sum3$IC
Sum3$Cstat
# Model 4 (removed: H2 ~ phydiversity)
Model4=update(Model3,H2~spar+shost+nhost+Latitude)
Sum4=summary(Model4, conserve=T)
Sum4$IC
Sum4$Cstat
# Model 5 (removed: DSI ~ shost)
Model5=update(Model4,DSI~H2+invconn+spar+phydiversity+nhost+Latitude)
Sum5=summary(Model5, conserve=T)
Sum5$IC
Sum5$Cstat
# Model 6 (removed: DSI ~ spar)
Model6=update(Model5,DSI~H2+invconn+phydiversity+nhost+Latitude)
Sum6=summary(Model6, conserve=T)
Sum6$IC
Sum6$Cstat
# Model 7 (removed: H2 ~ nhost)
Model7=update(Model6,H2~spar+shost+Latitude)
Sum7=summary(Model7, conserve=T)
Sum7$IC
Sum7$Cstat
# Model 8 (removed: H2 ~ shost)
Model8=update(Model7,H2~spar+Latitude)
Sum8=summary(Model8, conserve=T)
Sum8$IC
Sum8$Cstat
# Model 9 (removed: invconn ~ phydiversity)
Model9=update(Model8,invconn~spar+shost+Latitude)
Sum9=summary(Model9, conserve=T)
Sum9$IC
Sum9$Cstat
# Model 10 (removed: DSI ~ Latitude)
Model10=update(Model9,DSI~H2+invconn+phydiversity+nhost)
Sum10=summary(Model10, conserve=T)
Sum10$IC
Sum10$Cstat
# Model 11 (removed: DSI ~ invconn)
Model11=update(Model10,DSI~H2+phydiversity+nhost)
Sum11=summary(Model11, conserve=T)
Sum11$IC
Sum11$Cstat
# Model 12 (removed: DSI ~ nhost)
Model12=update(Model11,DSI~H2+phydiversity)
Sum12=summary(Model12, conserve=T)
Sum12$IC
Sum12$Cstat
# Model 13 (removed: spar ~ latitude)
Model13=update(Model12,spar~phydiversity+shost+nhost)
Sum13=summary(Model13, conserve=T)
Sum13$IC
Sum13$Cstat
# Model 14 (removed: DSI ~ phydiversity)
Model14=update(Model13,DSI~H2)
Sum14=summary(Model14, conserve=T)
Sum14$IC
Sum14$Cstat
# Model 15 (removed: nhost ~ latitude)
Model15= psem(
  glm(DSI~H2, family="quasibinomial", data=com_data),
  glm(H2~spar + Latitude, family="quasibinomial", data=com_data),
  glm(invconn~spar + shost + Latitude,family="quasibinomial", data=com_data),
  glm(spar~phydiversity + shost + nhost, family="poisson", data=com_data),
  glm(phydiversity~shost + nhost + Latitude,data=com_data),
  glm(shost~nhost+Latitude, family="poisson", data=com_data)
)
Sum15=summary(Model15, conserve=T)
Sum15$IC
Sum15$Cstat
# Model 16 (removed: phydiversity ~ latitude)
Model16=update(Model15,phydiversity~shost + nhost)
Sum16=summary(Model16, conserve=T)
Sum16$IC
Sum16$Cstat
# Model 17 (removed: phydiversity ~ nhost)
Model17=update(Model16,phydiversity~shost)
Sum17=summary(Model17, conserve=T)
Sum17$IC
Sum17$Cstat
# Model 18 (removed: invconn ~ latitude)
Model18=update(Model17,invconn~spar + shost)
Sum18=summary(Model18, conserve=T)
Sum18$IC
Sum18$Cstat
# Model 19 (removed: H2 ~ spar ; but from model 17)
Model19=update(Model17,H2~Latitude)
Sum19=summary(Model19, conserve=T)
Sum19$IC
Sum19$Cstat
# Model 20 (removed: spar ~ phydiversity ; but from model 17)
Model20=update(Model17,spar~shost + nhost)
Sum20=summary(Model20, conserve=T)
Sum20$IC
Sum20$Cstat

## Model 17 is the final
rm(Model1,Model2,Model3,Model4,Model5,Model6,Model7,Model8,Model9,Model10,Model11,Model12,Model13,Model14,Model15,Model16,Model18,Model19, Sum1,Sum2,Sum3,Sum4,Sum5,Sum6,Sum7,Sum8,Sum9,Sum10,Sum11,Sum12,Sum13,Sum14,Sum15,Sum16,Sum18,Sum19)
# Explained Deviance (McFadden's pseudo-R²)
PRED=c("H2","Parasite richness","Latitude","Parasite richness","Host richness","Latitude","Host phylogenetic diversity","Host richness","Sampling","Host richness","Sampling","Latitude")
RESP=c("DSI","H2","H2","invconn","invconn","invconn","Parasite richness","Parasite richness","Parasite richness","Host phylogenetic diversity","Host richness","Host richness")
EST=rep(NA,12)
DEV=rep(NA,12)
m1=glm(DSI~H2, family="quasibinomial", data=com_data)
EST[1]=m1$coefficients[2]
DEV[1]=anova(m1)$Deviance[2]/anova(m1)$`Resid. Dev`[1]
m2=glm(H2~spar+Latitude, family="quasibinomial", data=com_data)
EST[2]=m2$coefficients[2]
EST[3]=m2$coefficients[3]
DEV[2]=anova(m2)$Deviance[2]/anova(m2)$`Resid. Dev`[1]
DEV[3]=anova(m2)$Deviance[3]/anova(m2)$`Resid. Dev`[1]
m3=glm(invconn~spar+shost+Latitude,family="quasibinomial", data=com_data)
EST[4]=m3$coefficients[2]
EST[5]=m3$coefficients[3]
EST[6]=m3$coefficients[4]
DEV[4]=anova(m3)$Deviance[2]/anova(m3)$`Resid. Dev`[1]
DEV[5]=anova(m3)$Deviance[3]/anova(m3)$`Resid. Dev`[1]
DEV[6]=anova(m3)$Deviance[4]/anova(m3)$`Resid. Dev`[1]
m4=glm(spar~phydiversity+shost+nhost, family="poisson", data=com_data)
EST[7]=m4$coefficients[2]
EST[8]=m4$coefficients[3]
EST[9]=m4$coefficients[4]
DEV[7]=anova(m4)$Deviance[2]/anova(m4)$`Resid. Dev`[1]
DEV[8]=anova(m4)$Deviance[3]/anova(m4)$`Resid. Dev`[1]
DEV[9]=anova(m4)$Deviance[4]/anova(m4)$`Resid. Dev`[1]
m5=glm(phydiversity~shost,data=com_data)
EST[10]=m5$coefficients[2]
DEV[10]=anova(m5)$Deviance[2]/anova(m5)$`Resid. Dev`[1]
m6=glm(shost~nhost+Latitude, family="poisson", data=com_data)
EST[11]=m6$coefficients[2]
EST[12]=m6$coefficients[3]
DEV[11]=anova(m6)$Deviance[2]/anova(m6)$`Resid. Dev`[1]
DEV[12]=anova(m6)$Deviance[3]/anova(m6)$`Resid. Dev`[1]
# EFFECTS: table with the explained deviance of each predictor in each response
EFFECTS=data.frame(RESP=RESP,PRED=PRED,EST=round(EST,digits = 3),DEV=round(DEV,digits = 3))
EFFECTS
save(EFFECTS,file = "sem_effects.RData")