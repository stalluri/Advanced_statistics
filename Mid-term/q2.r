# midterm 2008, Q2
############################
# part a 
############################

# From analytical derivation 2*(2n<x> - n(ln(2*e*<x>)) is chi-square(1)

# simulation 
nsim <- 1000

# sample size
n<-40

# test_statistic initialization
test_statistic <- seq(1:nsim)

for (i in 1:nsim) {

# generating exp variate with lamba=2
s<- rexp(n,2)
s_mean <- mean(s)

test_statistic[i] <- 2*(2*n*s_mean - n*(log(2*exp(1)*s_mean)))

}

hist(test_statistic)

# getting the 95% quantile as the cutoff
cutoff <- quantile(test_statistic,0.95)

cutoff

#############################
# part b
#############################
# simulation 
nsim <- 1000

# sample size
n<-40

# count initialization
count <- 0

# test_statistic initialization
test_statistic <- seq(1:nsim)

for (i in 1:nsim) {

# generating exp variate with lamba=2
s<- rexp(n,1)
s_mean <- mean(s)

test_statistic <- 2*(2*n*s_mean - n*(log(2*exp(1)*s_mean)))

# using cutoff of "3.86" obtained from part(a) 
if (test_statistic > 3.86) { count <- count+1 }

}

power_of_test <- (count/nsim)*100

power_of_test


##########################
# part c
##########################

# sample size
n<- 40
nsim<-1000

# sample of exp(2)
s<-rexp(n,2)

s_mean <- mean(s)

# parametric bootstrap
s_bt_lambda <- seq(1:nsim)
for (i in 1:nsim) {
# lamba = 1/s_mean

s_bootstrap <- rexp(n,(1/s_mean))

s_bt_lambda[i] <- 1/mean(s_bootstrap)

}

hist(s_bt_lambda)

se_lamba_lower_bound <- quantile(s_bt_lambda,0.025)
se_lamba_upper_bound <- quantile(s_bt_lambda,0.975)

#stdev_s_bt_lamba <- sd(s_bt_lamba)

#se_lamba_lower_bound <- (1/s_mean)-1.96*stdev_s_bt_lamba
#se_lamba_upper_bound <- (1/s_mean)+1.96*stdev_s_bt_lamba


cat("paramteric bootstrap : lower bound:",se_lamba_lower_bound,"upper bound:",se_lamba_upper_bound," \n")


# Non- parametric bootstrap
s_bt_lambda<- rep(0,nsim)

for (i in 1:nsim) {
# lamba = 1/s_mean

s_bootstrap <- sample(s,n, replace=TRUE)
s_bt_lambda[i] <- 1/mean(s_bootstrap)

}

hist(s_bt_lambda)
se_lamba_lower_bound <- quantile(s_bt_lambda,0.025)
se_lamba_upper_bound <- quantile(s_bt_lambda,0.975)

cat("paramteric bootstrap : lower bound:",se_lamba_lower_bound,"upper bound:",se_lamba_upper_bound," \n")



###########################
#part e(i)
###########################

# Data actually comes from gamma(2,4)

# simulation 
nsim <- 1000

# sample size
n<-40

# count initialization
count <- 0

# test_statistic initialization
test_statistic <- seq(1:nsim)

for (i in 1:nsim) {

# generating gamma variate with shape 2 and rate =2
s<- rgamma(n,2,4)
s_mean <- mean(s)

test_statistic <- 2*(2*n*s_mean - n*(log(2*exp(1)*s_mean)))

# using cutoff of "3.86" obtained from part(a) 
if (test_statistic > 3.86) { count <- count+1 }

}

power_of_test <- (count/nsim)*100

power_of_test

# Data actually comes from gamma(2,2)

# simulation 
nsim <- 1000

# sample size
n<-40

# count initialization
count <- 0

# test_statistic initialization
test_statistic <- seq(1:nsim)

for (i in 1:nsim) {

# generating gamma variate with shape 2 and rate =2
s<- rgamma(n,2,2)
s_mean <- mean(s)

test_statistic <- 2*(2*n*s_mean - n*(log(2*exp(1)*s_mean)))

# using cutoff of "3.86" obtained from part(a) 
if (test_statistic > 3.86) { count <- count+1 }

}

power_of_test <- (count/nsim)*100

power_of_test










