# Assignment 2 Q4

nsim <- 10000

# sample size
n<-15

test_statistic <- rep(0,nsim)

for (i in 1 : nsim) {

# random variates
s <- rcauchy(n,0,1)

# neyman-pearson test_statistic

l <- ((1+s^2)/(1+((s-1)^2)))

test_statistic[i] <- prod(l)
}

hist(test_statistic,breaks=200)

cat ("cutoff is :",quantile(test_statistic,.95),"\n")

# RESULT : cutoff is around 2.3


############
# part b
############

count <- 0

for (i in 1 : nsim) {

# random variates with mu =1
s <- rcauchy(n,1,1)

# neyman-pearson test_statistic

l <- ((1+s^2)/(1+((s-1)^2)))

tstat <- prod(l)

if (tstat > 2.3) {
count <- count +1
}

}

power <- (count/nsim)*100

cat ("power of test is:",power,"\n")

#RESULT : power of the test is 83.6 % ; Since neyman-pearson's is the most powerful test, since the test statistic and threshold for rejection is specifically designed for cauchy (1,1) vs cauchy(0,1).


############
# part c
############
nsim <- 10000

# sample size
n<-15

test_statistic <- rep(0,nsim)

for (i in 1 : nsim) {

# random variates
s <- rnorm(n,0,1)

# neyman-pearson test_statistic

l <- ((1+s^2)/(1+((s-1)^2)))

test_statistic[i] <- prod(l)
}

hist(test_statistic,breaks=200)

cat ("cutoff is :",quantile(test_statistic,.95),"\n")

# RESULT : cutoff is around 0.76


############
# part b
############

count <- 0

for (i in 1 : nsim) {

# random variates with mu =1
s <- rcauchy(n,1,1)

# neyman-pearson test_statistic

l <- ((1+s^2)/(1+((s-1)^2)))

tstat <- prod(l)

if (tstat > 0.76) {
count <- count +1
}

}

power <- (count/nsim)*100

cat ("power of test is:",power,"\n")

#RESULT : power of the test is 92.04 % ; Since neyman-pearson's is the most powerful test and since cauchy distribution is quite close to normal distribution,  with central tendency the test is still more powerful.
