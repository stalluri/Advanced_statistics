# Assignment 2 , Q1

nsim <- 100000

# Generating Uniform random variate to generate the random variate with given F(x)

sunif <- runif(nsim,0,1)
rand_var <- rep(0,nsim)
for (i in 1:nsim) {

if (sunif[i] >=0.5 ) {
	if (sunif[i] == 0.5) {
	rand_var[i] <- 0}
	else { rand_var[i] <- (log(1/(1-sunif[i])) -log(2)) }
}
}


hist(sunif)
hist(rand_var)

cat ("Mean", mean(rand_var))
cat ("Standard Dev", sd(rand_var))

# RESULT : Mean : 0.4976 , Standard Deviation : 0.8604


