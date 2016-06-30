#Assignment 4 , Q1

appl<-read.csv("C:\\satish\\IIMA\\Acads\\Term4\\AMDA\\A4\\conc.csv")
attach(appl)
names(appl)
plot(conc)
d1<-arima(DISH,order=c(0,2,2))
d1
tsdiag(d1)
predict(d1,n.ahead=4)
msv<-seq(1:108)
for (d in 0:2){
    for (p in 0:5){
        for (q in 0:5){
            d1<-arima(DISH,order=c(p,d,q))
            cnt<-36*d+6*p+q+1
            msv[cnt]<-d1$aic
            }
    }
}
which.min(msv)


