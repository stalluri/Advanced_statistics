model<-read.csv("C:\\Documents and Settings\\Tejaswi\\Desktop\\collated data.csv")
attach(model)
names(model)

insider.lm<-lm(Insider~Time+exp(schange)+Vchange+News)
vif(insider.lm)
summary(insider.lm)
infl.SR<-influence.measures(insider.lm)
summary(infl.SR)
probrs <- rstandard(insider.lm)
for (i in 1:length(probrs)){
    if (abs(probrs[[i]])>2){
       print(probrs[i])
       }
}
plot.lm(insider.lm)

glm.outp<-glm(Insider~Time+schange+Vchange+News,family=binomial)
summary(glm.outp) 
predval<-predict(glm.outp,type="response")
n<-length (predval)
predprob<-seq(1:n)
sum<-0
for (i in 1:n)
{
predprob[i]<-predval[[i]]
sum<-sum+abs(Insider[i]-predprob[i])
}
plot(Insider,predprob)
predictmiss<-sum/n
predictmiss
