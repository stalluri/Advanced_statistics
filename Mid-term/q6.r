# midterm 2008, Q6
############################
# part a 
############################

nsim <- 1000

rf<-c(146.8,383,90.9,178.1,267.5,95.5,156.5,180,90.9,139.7,200.2,171.7,187.2,184.9,70.1,58,84.1,55.6,133.1,271.8,135.9,71.9,99.4,110.6,47.5,97.8,122.7,58.4,154.4,173.7,118.8,88,84.6,171.5,254.3,185.9,137.2,138.9,96.2,85,45.2,74.7,264.9,113.8,133.4,68.1,156.4)

# calculating hubers estimator by taking K=1.28

# cutoff to determine outlier |x-median|> ((mad/.6745)*1.28)

cutoff <- ((mad(rf)/.6745)*1.28)

# counts to determine if outliers are less than or greater than median
l<-0

u<-0

rf_median <- median(rf)

sample_sum <- 0

for (i in 1: length(rf)) {

if (abs(rf[i] - rf_median) > cutoff) {

if (rf[i]> rf_median) { u <- u+1 } else { l <- l+1}

}else {

sample_sum <- sample_sum + rf[i] 
}

}

rf_huber <- ((cutoff* (u-l))+sample_sum)/(length(rf)-l-u)


# Alternatively load MASS package and use huber function

library(MASS)

huber(rf, k=1.28)

# bootstrap based estimate 
bt_huber <- rep(0,nsim)

for ( i in 1: nsim) {

s_bt <- sample (rf, replace=TRUE)

cutoff <- ((mad(s_bt)/.6745)*1.28)

# counts to determine if outliers are less than or greater than median
l<-0

u<-0

rf_median <- median(s_bt)

sample_sum <- 0

for (i in 1: length(s_bt)) {

if (abs(s_bt[i] - rf_median) > cutoff) {

if (s_bt[i]> rf_median) { u <- u+1 } else { l <- l+1}

}else {

sample_sum <- sample_sum + s_bt[i] 
}

}

bt_huber[i] <- ((cutoff* (u-l))+sample_sum)/(length(s_bt)-l-u)


}

huber_lb <- quantile(bt_huber,0.025)
huber_ub <- quantile(bt_huber,0.975)

cat ("huber est LowerB:",huber_lb,"huber est UpperB:",huber_ub,"\n")


############################
# part c
############################

# prob of rainfall < 50mm  p^

count <- 0

for (i in 1 : length(rf)) {

if (rf[i] <50) {

count <- count +1  }
 
}


prob50 <- count/length(rf)

prob50

bt_prob <- rep (0, nsim)

for ( i in 1 : nsim) {

s_bt <- sample ( rf, replace=TRUE) 

count <- 0

for (i in 1 : length(s_bt)) {

if (s_bt[i] <50) {

count <- count +1  }
 
}


bt_prob[i] <- count/length(s_bt)

}


cat ("prob50 lower bound",quantile(bt_prob50,.025),"prob 50 upper bound", quantile(bt_prob50,.975),"\n")




############################
# part d
############################

# prob of rainfall < 50mm  p^

count <- 0

for (i in 1 : length(rf)) {

if (rf[i] >250) {

count <- count +1  }
 
}


prob250 <- count/length(rf)

prob250

bt_prob250 <- rep (0, nsim)

for ( i in 1 : nsim) {

s_bt <- sample ( rf, replace=TRUE) 

count <- 0

for (i in 1 : length(s_bt)) {

if (s_bt[i] >250) {

count <- count +1  }
 
}


bt_prob250[i] <- count/length(s_bt)

}

cat ("prob250 lower bound",quantile(bt_prob250,.025),"prob 250 upper bound", quantile(bt_prob250,.975),"\n)






