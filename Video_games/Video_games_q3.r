# Video Games , Q3

nsim <-1000

wtime <- c(2,0,0,0.5,0,0,0,0,2,0,0,0,0,3,1,0,0,0,2,0,2,0,2,0,0,0,0,0,0,0,0,1,0,0,0,0.1,0.5,1,0,0,0,2,2,0.5,0,2,0,0,0,2,0,0,0.5,3,0,0,0,4,30,14,0,0,0,0.5,14,1,0,0,1.5,0,0,2,0,0,0,0,0,0,0,2,1,0,0,2,0,2,2,5,0,3,0)

hist(wtime,breaks=200,freq=TRUE)

# sample mean

s_mean <- mean(wtime)

# bootstrapping
s_bt_mean <- rep(0,nsim)

for (j in 1: nsim) {

s_bt <- sample(wtime,replace=TRUE)

s_bt_mean[j] <- mean(s_bt)

}

hist(s_bt_mean,breaks=200,freq=TRUE)

cat("confidence interval for mean:",s_mean,"is:",quantile(s_bt_mean,.025),"::",quantile(s_bt_mean,.975),"\n")



