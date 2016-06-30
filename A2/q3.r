# Assignment 2 , Q3

# no of simulations
nsim <- 10000

s<- c(4.8,4.2,5.4,4.1,4,4,4.8,4.4,4.7,4.3,4.4,4.6,4.4,4.4,6.1,4.3,6,4.5,4.4,4.4,4.5,4.2,4.4,4.7,5.4,4,4.6,5.2,4.5,4.4)

s_mean <- mean(s)

# bootstrapping
s_bt_mean <- rep(0,nsim)

for (i in 1:nsim) {

s_bt <- sample(s,replace=TRUE)

s_bt_mean[i] <- mean (s_bt)

}

hist(s_bt_mean)

cat ("Standard Error of mean:",s_mean, "is:", sd(s_bt_mean),"\n")

#RESULT : Standard Error of mean: 4.603333 is: 0.0951238 


####################
# part b
####################


cat ("95% confidence interval is :",quantile(s_bt_mean,.025),"::",quantile(s_bt_mean,.975),"\n")

# RESULT : 95% confidence interval is : 4.43 :: 4.803333 

####################
# part c
####################

s_prop <- 0

for (j in 1 : length(s)){
if ( s[j] > 5.5) {
s_prop <- s_prop +1
}
}

s_prop <- s_prop/length(s)

# bootstrapping
s_bt_prop <- rep(0,nsim)

for (i in 1:nsim) {

s_bt_p <- 0

s_bt <- sample(s,replace=TRUE)

for (j in 1 : length(s_bt)){
if ( s_bt[j] > 5.5) {
s_bt_p <- s_bt_p +1
}
}

s_bt_prop[i] <- s_bt_p/length(s)

}

hist(s_bt_prop)

cat ("Standard Error of mean:",s_prop, "is:", sd(s_bt_prop),"\n")


# RESULT  : Standard Error of mean: 0.06666667 is: 0.04561 


