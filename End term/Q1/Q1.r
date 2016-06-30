# Charles Club

# part (a)


CBC<-read.csv("C:\\satish\\IIMA\\Acads\\Term4\\AMDA\\End term\\Q1\\CBCTrain.csv")
attach(CBC)
names(CBC)

cbc.glm <- glm(Florence~Seq+ID+Gender+Monetary+Recency+Frq+FirstPurch+ChildBks+YouthBks+CookBks+DoItYBks+RefBks+ArtBks+GeogBks+ItalCook+ItalAtlas+ItalArt+RelatedPurchase,family=binomial)

summary (cbc.glm)

cbc.glm <- glm(formula = Florence ~ Gender + Frq + YouthBks + CookBks +  DoItYBks + ArtBks + GeogBks, family = binomial) 
summary (cbc.glm)


# part (b)
# predicting probability values to calculate ROC

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
c10 <- 40
misclassification_cost <- (c01 * bad_good)+(c10 * good_bad )
plot (thres, misclassification_cost )
thres[which.min(misclassification_cost)]


# predicting the value  on validation set
pred_value <-predict.glm(cbc.glm,data.frame(temp=40),type="response")


CBC<-read.csv("C:\\satish\\IIMA\\Acads\\Term4\\AMDA\\End term\\Q1\\CBCValid.csv")
attach(CBC)
names(CBC)


predval2<-predict(cbc.glm,data.frame(Gender=Gender, Frq=Frq , YouthBks=YouthBks, CookBks=CookBks,DoItYBks=DoItYBks, ArtBks=ArtBks,GeogBks=GeogBks), family = binomial, type="response")


###### part d ######

CBC<-read.csv("C:\\satish\\IIMA\\Acads\\Term4\\AMDA\\End term\\Q1\\CBCTrain1.csv")
attach(CBC)
names(CBC)

cbc.glm1 <- glm(formula = Florence ~ Gender + Frq + YouthBks + CookBks + ArtBks + GeogBks, family = binomial) 

predval1<-predict(cbc.glm1,data.frame(Gender=Gender, Frq=Frq , YouthBks=YouthBks, CookBks=CookBks,DoItYBks=DoItYBks, ArtBks=ArtBks,GeogBks=GeogBks), family = binomial, type="response")

predval <- predval1

CBC<-read.csv("C:\\satish\\IIMA\\Acads\\Term4\\AMDA\\End term\\Q1\\CBCTrain2.csv")
attach(CBC)
names(CBC)


predval2<-predict(cbc.glm,data.frame(Gender=Gender, Frq=Frq , YouthBks=YouthBks, CookBks=CookBks,DoItYBks=DoItYBks, ArtBks=ArtBks,GeogBks=GeogBks), family = binomial, type="response")


predval <- predval2

CBC<-read.csv("C:\\satish\\IIMA\\Acads\\Term4\\AMDA\\End term\\Q1\\CBCTrain3.csv")
attach(CBC)
names(CBC)


predval3<-predict(cbc.glm,data.frame(Gender=Gender, Frq=Frq , YouthBks=YouthBks, CookBks=CookBks,DoItYBks=DoItYBks, ArtBks=ArtBks,GeogBks=GeogBks), family = binomial, type="response")
predval <- predval3


CBC<-read.csv("C:\\satish\\IIMA\\Acads\\Term4\\AMDA\\End term\\Q1\\CBCTrain4.csv")
attach(CBC)
names(CBC)


predval4<-predict(cbc.glm,data.frame(Gender=Gender, Frq=Frq , YouthBks=YouthBks, CookBks=CookBks,DoItYBks=DoItYBks, ArtBks=ArtBks,GeogBks=GeogBks), family = binomial, type="response")
predval <- predval4

CBC<-read.csv("C:\\satish\\IIMA\\Acads\\Term4\\AMDA\\End term\\Q1\\CBCTrain5.csv")
attach(CBC)
names(CBC)


predval5<-predict(cbc.glm,data.frame(Gender=Gender, Frq=Frq , YouthBks=YouthBks, CookBks=CookBks,DoItYBks=DoItYBks, ArtBks=ArtBks,GeogBks=GeogBks), family = binomial, type="response")
predval <- predval5

