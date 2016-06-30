

mtin<-read.csv("C:\\satish\\IIMA\\Acads\\Term4\\AMDA\\End term\\Q3\\ETP3.csv")
attach(mtin)
names(mtin)

freq.lm <- lm(Claimfreq~ km1+km2+km3+km4+z1+z2+z3+z4+z5+z6+m1+m2+m3+m4+m5+m6+m7+m8+Bonus+Insured+Claims+Payment)
summary(freq.lm)

freq.lm <- lm(Claimfreq~ km1+km2+km3+z1+z2+z3+z4+z5+z6+m4+m5+m6+m7+Bonus)
summary(freq.lm)

par(mfrow=c(2,2))
plot(freq.lm,residuals=F,pch=16)

inflm.freq <- influence.measures(freq.lm)
summary(inflm.freq)




### gam model 
freq.lm <- gam(Claimfreq ~ s(km1,bs="tp")+s(km2,bs="tp")+s(km3,bs="tp")+s(z1,bs="tp")+s(z2,bs="tp")+s(z3,bs="tp")+s(z4,bs="tp")+s(z5,bs="tp")+s(z6,bs="tp")+s(m4,bs="tp")+s(m5,bs="tp")+s(m6,bs="tp")+s(m7,bs="tp")+s(Bonus))
summary(freq.lm)

freq.gam <- gam(Claimfreq ~ km1+km2+km3+z1+z2+z3+z4+z5+z6+m1+m2+m4+m5+m6+m8+Bonus)
summary(freq.gam)

## load mgcv package
km.1 <- as.numeric(Kilometres==1)
km.2 <- as.numeric(Kilometres==2)
km.3 <- as.numeric(Kilometres==3)
km.4 <- as.numeric(Kilometres==4)
z.1 <- as.numeric(Zone==1)
z.2 <- as.numeric(Zone==2)
z.3 <- as.numeric(Zone==3)
z.4 <- as.numeric(Zone==4)
z.5 <- as.numeric(Zone==5)
z.6 <- as.numeric(Zone==6)
m.1 <- as.numeric(Make==1)
m.2 <- as.numeric(Make==2)
m.3 <- as.numeric(Make==3)
m.4 <- as.numeric(Make==4)
m.5 <- as.numeric(Make==5)
m.6 <- as.numeric(Make==6)
m.7 <- as.numeric(Make==7)
m.8 <- as.numeric(Make==8)

freq.lm <- gam(Claimfreq~ Kilometres+s(Kilometres,by=km.1)+s(Kilometres,by=km.2)+s(Kilometres,by=km.3)+s(Kilometres,by=km.4)+Zone+s(Zone,by=z.1)+s(Zone,by=z.2)+s(Zone,by=z.3)+s(Zone,by=z.4)+s(Zone,by=z.5)+s(Zone,by=z.6)+Make+s(Make,by=m.1)+s(Make,by=m.2)+s(Make,by=m.3)+s(Make,by=m.4)+s(Make,by=m.5)+s(Make,by=m.6)+s(Make,by=m.7)+s(Make,by=m.8)+s(Bonus)+s(Insured)+s(Claims)+s(Payment))	



#freq.lm <- gam(Claimfreq~ Kilometres+s(x,by=km.1)+s(x,by=km.2)+s(x,by=km.3)+s(x,by=km.4)+s(z1)+s(z2)+s(z3)+s(z4)+s(z5)+s(z6)+s(m1)+s(m2)+s(m3)+s(m4)+s(m5)+s(m6)+s(m7)+s(m8)+s(Bonus)+s(Insured)+s(Claims)+s(Payment))	

summary(freq.lm)


# part d ######

pay.lm <- lm(Payment~ km1+km2+km3+km4+z1+z2+z3+z4+z5+z6+m1+m2+m3+m4+m5+m6+m7+m8+Bonus+Insured+Claims+Claims)
summary(pay.lm)

pay.lm <- lm(Payment~ km1+km2+km3+km4+z2+z4+m6+Bonus+Insured+Claims+Claims)
summary(pay.lm)
