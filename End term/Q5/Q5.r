## Q5

mtin<-read.csv("C:\\satish\\IIMA\\Acads\\Term4\\AMDA\\End term\\Q5\\premium.csv")
attach(mtin)
names(mtin)
Age <- sqrt(Age)

freq.lm <- lm(Rate ~ Age+sex+Smoke)
summary(freq.lm)

par(mfrow=c(2,2))
plot(freq.lm,residuals=F,pch=16)

#####  part b ######
## know how to give kernels for local linear smoothing


### part c ########
mtin<-read.csv("C:\\satish\\IIMA\\Acads\\Term4\\AMDA\\End term\\Q5\\premium.csv")
attach(mtin)
names(mtin)

freq.gam <- gam(Rate ~ s(Age)+sex+Smoke)
summary(freq.gam)

par(mfrow=c(2,2))
plot(freq.gam,residuals=F,pch=16)

