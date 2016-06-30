s<-seq(1:50)
	a<-0
	b<-0
	c<-0
	d<-0
	g1<-0
	g2<-0
Y<-(1:1000)

n<-50
for (i in 1:1000)
{
s<-sign(runif(50) - 0.5) * rexp(50)
u<-mean(s)
	for(j in 1:50)
	{
		a<-a+(s[j]-u)^3
 		b<-b+(s[j]-u)^2
 		c<-c+(s[j]-u)^4
 		d<-d+(s[j]-u)^2
 	}
	g1<-(a/n)/sqrt((b/n)^3)
	g2<-((c/n)/(d/n)^2) - 3
 
	Test<-(50/6)*((g1)^2+(((g2)^2)/4))
	cat(Test,"\n")
	if (Test>4.605) Y[i]=1 else Y[i]=0
}
Power<-sum(Y)/1000
Power

