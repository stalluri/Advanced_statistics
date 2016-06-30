# Q3 
x<-c(2.8,3.2,3.2,3.4,4.3,4.9,5.1,5.2,5.9,5.9,6.6,8.3,12.6,15.1,25.1,33.1,75.9,75.9)
y<-c(6.2,9,7.1,6.8,10.2,7.8,9.3,5.9,8.9,5.5,7.1,9.1,5.1,4.7,4.7,3.1,3.2,3.1)
plot(x,y)

plot(log(x),y)
plot(x,log(y))
plot(log(x),log(y))


# simple linear regression, using the code given by prof.Laha directly.
lmy<-lm(y~x)
summary(lm(y~x))
f<-fitted(lmy)
r<-resid(lmy)
r1<-r/sd(r)
hist(r1,"FD")
qqnorm(r)
plot(f,r)
cor(x,y)
abline(lm(x~y))
pred.frame<-data.frame(x=0:12)
pc<-predict(lmy,int="c",newdata=pred.frame)
pp<-predict(lmy,int="p",newdata=pred.frame)
plot(x,y,ylim=range(x,pp,na.rm=T))
pred.x<-pred.frame$x
matlines(pred.x,pc,lty=c(1,2,2),col="black")
matlines(pred.x,pp,lty=c(1,3,3),col="black")

# Theil-sens estimator
len <- length(x)
slope <- rep(0, (len)*((len-1)/2) )
count <- 0
for (i in 1 : len) { 
for (j in 1 : len)  {
if (j>i) {
count <- count +1

slope[count] <- (y[j]-y[i])/(x[j]-x[i])}} }
hist(slope,"FD")
cat ("Theil-Sen estimator::", median(slope),"\n")

