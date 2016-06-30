#Assignment 4 , Q3

appl<-read.csv("C:\\satish\\IIMA\\Acads\\Term4\\AMDA\\A4\\temp.csv")
attach(appl)
names(appl)

rkt.glm <- glm (result~temp,family=binomial)
summary(rkt.glm)

# predicting the value 
pred_value <-predict.glm(rkt.glm,data.frame(temp=31),type="response")


