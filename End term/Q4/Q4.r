# Q4

# part a

infl<-read.csv("C:\\satish\\IIMA\\Acads\\Term4\\AMDA\\End term\\Q4\\ETP4.csv")
attach(infl)
names(infl)

infl.lm <- lm(INF ~ QUES+LEGAL+DEV)
summary(infl.lm)

infl.lm <- lm(INF ~ QUES+DEV)
summary(infl.lm)

par(mfrow=c(2,2))
plot(infl.lm,residuals=F,pch=16)

infl.msrs <- influence.measures(infl.lm)
summary(infl.msrs)

para_resid <- resid(infl.lm)


# part b

infl1<-read.csv("C:\\satish\\IIMA\\Acads\\Term4\\AMDA\\End term\\Q4\\ETP41.csv")
attach(infl1)
names(infl1)

infl1.lm <- lm(INF ~ QUES+LEGAL)
summary(infl1.lm)

par(mfrow=c(2,2))
plot(infl1.lm,residuals=F,pch=16)

infl2<-read.csv("C:\\satish\\IIMA\\Acads\\Term4\\AMDA\\End term\\Q4\\ETP42.csv")
attach(infl2)
names(infl2)

infl2.lm <- lm(INF ~ QUES+LEGAL)
summary(infl2.lm)

par(mfrow=c(2,2))
plot(infl2.lm,residuals=F,pch=16)


## part c ###

infl<-read.csv("C:\\satish\\IIMA\\Acads\\Term4\\AMDA\\End term\\Q4\\ETP4_org.csv")
attach(infl)
names(infl)

infl.lm <- gam (INF ~ s(QUES)+s(LEGAL)+DEV)
summary(infl.lm)

par(mfrow=c(2,2))
plot(infl.lm,residuals=F,pch=16)

infl.msrs <- influence.measures(infl.lm)
summary(infl.msrs)

# fitting para_resid with other explanotaries.

infl<-read.csv("C:\\satish\\IIMA\\Acads\\Term4\\AMDA\\End term\\Q4\\ETP4.csv")
attach(infl)
names(infl)

res.lm <- gam(resd ~ s(QUES)+s(LEGAL)+DEV)
summary(res.lm)

# part e ###

cbc.glm <- glm(DEV ~ QUES+LEGAL+INF,family=binomial)
summary(cbc.glm)

# determining the threshold
Florence <- DEV

predval<-predict(cbc.glm,type="response")

precision <- 0.005
thres <- seq(0,1,by=precision)

len <- length(thres)
error <- rep(0, len)
sensitivity <- rep(0,len)
nspecificity <- rep(0,len)
bad_good <-  rep(0,len)
good_good <-  rep(0,len)
good_bad <-  rep(0,len)
bad_bad <-  rep(0,len)

len2 <- length(predval)

Florence_good <- sum(Florence)
Florence_bad <- length(Florence) -Florence_good



for (i in 1:len ) {

digpred <- rep(0,len2)


for (j in 1:len2 ) {

if (predval[j] > thres[i]) {
digpred[j] <- 1
if (Florence[j] != 1) {
error[i] <- error[i] +1
bad_good[i] <- bad_good[i] +1
}
else { 
good_good[i] <- good_good[i] +1 }

}
else {
digpred[j] <- 0
if (Florence[j] != 0) {
error[i] <- error[i] +1
good_bad[i] <- good_bad[i] +1
}
else { 
bad_bad[i] <- bad_bad[i] +1 }

}

}

#cat (good_good, good_bad, bad_good, bad_bad,"\n")

sensitivity[i] <- good_good[i] /(good_good[i] + good_bad[i])
nspecificity[i] <- bad_good[i] / (bad_good[i] + bad_bad[i])


}


plot(thres,error)
plot (nspecificity, sensitivity)


# ROC curve area using trapezoidal rule
times <- length(nspecificity) -1
roc_area <- 0
for ( k in 1 : times ) {

roc_area <- roc_area + ( 0.5 * ( sensitivity[k] + sensitivity [k+1] ) * ( nspecificity [k] - nspecificity[k+1] ))

}

roc_area




########## part c  #### determining the threshold
c01 <- 1
c10 <- 1
misclassification_cost <- (c01 * bad_good)+(c10 * good_bad )
plot (thres, misclassification_cost )
thres[which.min(misclassification_cost)]


