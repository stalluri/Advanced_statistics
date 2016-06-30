# A3, Q1
 

unaff <- c (1.94,1.44,1.56,1.58,2.06,1.66,1.75,1.77,1.78,1.92,1.25,1.93,2.04,1.62,2.08)
aff <-c (1.27,1.63,1.47,1.39,1.93,1.26,1.71,1.67,1.28,1.85,1.02,1.34,2.02,1.59,1.97)

# part a
# difference will be normal.

diff_estimate <- mean(unaff)-mean(aff)
diff_se <- sqrt ((((sd(unaff))^2) + ((sd(aff))^2))/length(unaff))

# 95% confid interval is 

# since we are looking at magnitude, interval will be estimated mean and 97.5% estimate only.

cat ("95% confid interval is :",diff_estimate," ::", diff_estimate+(1.96* diff_se), "\n")

# graphically analyzing the normality of differences in volumes.
hist(unaff-aff,"FD")

# bootstrapping
diff_estimate_bt <- rep(0,1000)
order <- seq (1: length(unaff))

for (i in 1 : 1000) {
order_bt <- sample (order, replace=TRUE)

for (j in 1 : length(unaff)) {
unaff_bt <- unaff[order_bt[j]]
aff_bt <- aff[order_bt[j]]
}

diff_estimate_bt[i] <- mean(unaff_bt)-mean(aff_bt)
#diff_se_bt <- sqrt ((((sd(unaff_bt))^2) + ((sd(aff_bt))^2))/length(unaff))
}


cat ("95% confid interval is :",quantile(diff_estimate_bt,.5) ," ::",quantile(diff_estimate_bt,.975), "\n")


#Assuming magnitude of differences will be onside normal. (ie, twice the probability on one side ie +ve side).

t.test(unaff, aff, paired=T)


