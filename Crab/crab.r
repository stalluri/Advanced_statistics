crab<-read.csv("C:\\satish\\IIMA\\Acads\\Term4\\AMDA\\Crab\\crab.csv")
attach(crab)
names(crab)
crab.lm <-lm(presz~postsz)
#crab.lm <-lm(presz~postsz+y1+y2+y3+lf)
summary(crab.lm)
par(mfrow=c(2,2))
plot(crab.lm,residuals=F,pch=16)
#plot(presz,fitted(crab.lm))
#plot(fitted(crab.lm),residuals(crab.lm))
#plot(postsz,residuals(crab.lm))

vif(crab.lm)

crab.gam <- gam(presz~postsz+y1+y2+y3+lf)
summary(crab.gam)



mpgal.lm<-lm(MILPGAL~CYLINDER+DISPLACE+HORSPWR+ACCEL+YEAR+WEIGHT+COUNTRY+PRICE+COMPANY) 
summary(mpgal.lm)
vif(mpgal.lm)
mpgal2.lm<-lm(MILPGAL~CYLINDER+HORSPWR+ACCEL+YEAR+WEIGHT+COUNTRY+PRICE+COMPANY) 
summary(mpgal2.lm)
vif(mpgal2.lm)
mpgal3.lm<-lm(MILPGAL~CYLINDER+HORSPWR+ACCEL+YEAR+COUNTRY+PRICE+COMPANY) 
summary(mpgal3.lm)
vif(mpgal3.lm)
inflm.SR <- influence.measures(mpgal3.lm)
summary(inflm.SR)
rs <- rstandard(mpgal3.lm)
for (i in 1:length(rs)){
    if (abs(rs[[i]])>2){
       print(rs[i])
       }
} 
plot.lm(mpgal3.lm)
