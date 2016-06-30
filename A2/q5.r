# assignment 2 , q5

nsim <- 1000

# MLE of lamba is 1/x_bar

wtime <- c(79,54,74,62,85,55,88,85,51,85,54,84,78,47,83,52,62,84,52,79,51,47,78,69,74,83,55,76,78,79)
n<- length(wtime)
s_lambda <- 1/(mean(wtime))

# standard error se = sqrt(s_lamdba^2/sample_size)
# 95% confidence interval is s_lamba+/- 1.96se

se <- sqrt(s_lambda^2/n)

cat("95% confidence interval is:", s_lambda-se, "::",s_lambda+se,"\n")
cat("95% confidence interval is:", 1/(s_lambda-se), "::",1/(s_lambda+se),"\n")

# bootstrap
s_bt_mean <- rep(0,nsim)

for (j in 1 : nsim) {

s_bt <- sample(wtime,replace=TRUE)
s_bt_mean[j] <- mean(s_bt)

}
hist(s_bt_mean,breaks=200)

cat ("95% confidence interval is:",quantile(s_bt_mean,.025),"::",quantile(s_bt_mean,.975),"\n")

#RESULT IS 95% confidence interval is: 64.4 :: 74.26667 

#part d


# COMMENT : previous interval 84.94145 :: 58.71372 , and bootstrap interval is 64.3 :: 74.535 
# Bootstrapped interval looks tight, but since its non-parametric bootstrap, the actual 95% confidence interval might not be as tight as indicated by this bootstrap, and MLE based range arrived at using lambda, is much reliable.




 




