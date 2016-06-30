# midterm 2008, Q5
############################
# part a 
############################
nsim <- 1000

ca<-c(0.3,0.3,0.32,0.43,0.44,0.47,0.52,0.59,0.7,0.77,0.79,0.81,0.95,1.33,1.43,1.54)
cb<-c(0.04,0.14,0.19,0.21,0.25,0.26,0.32,0.35,0.4,0.4,0.42,0.53,0.53,0.56,0.57,0.68)

cab <- c(ca,cb)

# sample absoulute value of mean
s_mean <- abs( mean(ca) -mean(cb))

# permuting

# initializing count 
count <-0

#perm_mean <- rep(0,nsim)

for (i in 1:nsim) {

perm <- sample(cab,replace=FALSE)

perm_mean <- abs(mean(perm[1:length(ca)])-mean(perm[(length(ca)+1):length(perm)]))

if ( perm_mean > s_mean) { count <- count +1 }

}

p_value <- count/nsim

cat ("Difference in mean for given samples:",s_mean, "p_value:", p_value,"\n")

# sor for p_value of 0.003, we can reject Ho and so the holding capabilities of both the cannulae are different.


############################
# part b
############################
nsim <- 1000

s_quantile_diff <- abs(quantile(ca,0.25) -quantile(cb,0.25))


# permuting

# initializing count 
count <-0

perm_quant_diff <- rep(0,nsim)

for (i in 1:nsim) {

perm <- sample(cab,replace=FALSE)

perm_quant_diff[i] <- abs(quantile(perm[1:length(ca)],0.25)-quantile(perm[(length(ca)+1):length(perm)],0.25))

if ( perm_quant_diff[i] > s_quantile_diff ) { count <- count +1 }

}

p_value <- count/nsim

cat ("Difference in quantile diff for given samples:",s_quantile_diff, "p_value:", p_value,"\n")

# extracting confidence interval from the histogram of permuted quantile diff values.

q_diff_lower_bound <- quantile (perm_quant_diff,.025)
q_diff_upper_bound <- quantile (perm_quant_diff,.975)

cat ("Lower Bound:",q_diff_lower_bound,"Upper Bound:",q_diff_upper_bound,"\n")

############################
# part b
############################
nsim <- 1000

# mad ratio for the sample

mad_ratio <- mad(ca)/mad(cb)

perm_mad_ratio <- rep(0, nsim)
# bootstrapping 

for (i in 1:nsim) {

perm <- sample(cab,replace=TRUE)

perm_mad_ratio [i] <- mad(perm[1:length(ca)])/mad(perm[(length(ca)+1):length(perm)])

}

# extracting confidence interval from the histogram of permuted quantile diff values.

q_diff_lower_bound <- quantile (perm_mad_ratio,.025)
q_diff_upper_bound <- quantile (perm_mad_ratio,.975)

cat ("Lower Bound:",q_diff_lower_bound,"Upper Bound:",q_diff_upper_bound,"\n")


  
# bounds are : Lower Bound :0.3912778 Upper Bound: 2.625164











