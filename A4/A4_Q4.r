# Q4 A4

# sample 
wt <- c(79,54,74,62,85,55,88,85,51,85,54,84,78,47,83,52,62,84,52,79,51,47,78,69,74,83,55,76,78,79)
wt_size <- length(wt)

#for (i in 1 : 21) {

#wt_10 <- sum(wt[i:(i+9)]
#}


# sample size 
nsim <- 1000

# lambada = 60
lamdbad <-60

ll <- rep(0,nsim)

theta_s <- rexp(nsim,lambda)

for (i in 1: nsim) {
ll[i] <- ( (theta_s[i])^(wt_size))* (exp(-theta_s[i] * (sum(wt))))
}

for (i in 1: nsim ) {








