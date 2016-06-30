
# for DE(0,1) random variate generate is as follows
mn <- 0
stdev <- sqrt(2)
# for Ln(0,1) , random variate generation is ::
mn <- exp(0.5)
stdev <- sqrt(exp(1)(exp(1)-1))

# 1000 sample generation

# initial sample size 
n <- 0
# initial converence gap = 100
convergence_gap <- 100
while ( convergence_gap > .01) {
n <- n+10
s <- seq (1:n)
z <- seq (1:1000)

for (i in 1:1000) {
# for DE(0,1) random variate generate is ::
suniform <- runif(n, min=-0.5, max=0.5)
s<- mn-((abs(suniform)/suniform)*(log(1-2*(abs(suniform)))))

# for Ln(0,1) , random variate generation is ::
s <- rlnorm (n,meanlog=mn,sdlog=stdev)
# mean u = 0 , sd  = sqrt(2)
xn<- mean(s)
z[i] <- ((xn-mn)*sqrt(n))/(stdev)
}
# decile calculation
decs <- seq (.1,.9,by=.1)
#for (i in decs) {

norm_values <- qnorm(decs,mean=mn, sd=stdev,lower.tail=TRUE, log.p = FALSE)
quant_values <- quantile(z, decs)

convergence_gap <- sum( abs( norm_values -quant_values))
}

print(n)
print (convergence_gap)

















