# midterm 2008, Q2
############################
# part a 
############################

# using Jarque-bera test , JB which follows chi-square with 2 deg of freedom, so 1, part (a) cutoof of 4.605 is valid

sec_x<- c(75,94,50,68,75,72,59,84,84,78,57,71,80,54,59,73,71,54,64,69,76,49,76,75,98,74,47,85,53,87,72,82,67,58,94,68,83,48,63,51,55,87,54,95,47,85,83,66,91,81,73,93,91,84,66,62,60,58,45,76,86,72)

sec_y<- c(49,76,99,62,87,54,78,69,52,65,66,60,59,57,75,75,42,61,93,77,77,74,76,96,86,71,68,71,81,69,42,74,71,82,74,73,66,84,100,86,56,89,68,73,67,69,81,52,83,80,84,45,42,68,85,92,56,50,90,57,88)

# testing normality using shapiro.test
shapiro.test(sec_x)
shapiro.test(sec_y)

# Since both p_values are greater than .01, We can say that both the data are normal.

#########################
# part b
#########################

# H0: ux-uy =0  vs H1 : ux-uy =/ 0

# test_statistic : Xbar - Ybar

# se = sqrt( sx^2/nx + sy^2/ny)

# applying wald test

ux<- mean(sec_x)
uy<- mean(sec_y)

sx2<- var(sec_x)
sy2<- var(sec_y)

se<- sqrt((sx2^2/length(sec_x))+(sy2^2/length(sec_y)))


# walds test

p_value <- 2* pnorm(-abs(ux-uy)/se)

p_value


# since p_value is 0.99, we cant reject H0, and say that means of two sections are close

#########################
# part c
#########################

# For Inter quartile range we need phi(0.75)-phi(0.25) = u+0.675s-(u-0.675s) = 1.35s, since s is MLE for sigma.

iqr_x <- 1.35* sqrt(var(sec_x))
iqr_x
iqr_y <- 1.35* sqrt(var(sec_y))
iqr_y


# directly based on given samples

iqr2_x <- quantile(sec_x,0.75) - quantile(sec_x,0.25)
iqr2_x
iqr2_y <- quantile(sec_y,0.75) - quantile(sec_y,0.25)
iqr2_x

#########################
# part d
#########################
# no of simulations
nsim <-1000
# let d^ = iqrx^ -iqry^ 

# Now Ho: d^= 0 vs Ha : d^ =/0 

# se^ will be obtained through bootstrap.

# apply wald test statistic d^/se^ to find p_value and decide based on given LOS =5%

iqr_x <- 1.35* sqrt(var(sec_x))
iqr_x
iqr_y <- 1.35* sqrt(var(sec_y))
iqr_y

# to calculate se^ using bootstrap

btstrp_iqr_d <- seq(1:nsim)

for (i in 1: nsim) {

btstrp_sec_x <- sample(sec_x,length(sec_x), replace=TRUE)
btstrp_iqr_x <- 1.35* sqrt(var(btstrp_sec_x))
btstrp_sec_y <- sample(sec_y,length(sec_y), replace=TRUE)
btstrp_iqr_y <- 1.35* sqrt(var(btstrp_sec_y))

btstrp_iqr_d[i] <- btstrp_iqr_x - btstrp_iqr_y	

}

# finding stdev of bootstrapped differences in iqr
se_d <- sd (btstrp_iqr_d)


# applying wald test
 
p_value <- 2* pnorm(-abs(iqr2_x-iqr2_y)/se_d)

p_value

# since p_value is ~ 0.13, we dont reject Ho and so Inter quartile difference between the two section is not significant
# In above case I am testing actual sample value against MLE based se^


#########################
# part e
#########################
# no of simulations
nsim <-1000
# let d^ = px^ -py^  (px, py indicate proporation of students scoring more than 80)

# d^ = phi((80-uy)/sigma_y)- phi((80-ux)/sigma_x)

# Now Ho: d^= 0 vs Ha : d^ =/0 

# se^ will be obtained through bootstrap.

# apply wald test statistic d^/se^ to find p_value and decide based on given LOS =5%


# calculating sample proporations > 80

count <-0
for (i in 1 : length(sec_x)) {
if ( sec_x[i] > 80 ) { count <- count+1 }
}
sec_x_p <- count/length(sec_x)

count <-0
for (i in 1 : length(sec_y)) {
if ( sec_y[i] > 80 ) { count <- count+1 }
}
sec_y_p <- count/length(sec_y)

#  For given section samples, difference in proportion is 
s_p <-  sec_y_p - sec_x_p

# to calculate se^ using bootstrap
btstrp_p_d <- seq(1:nsim)

for (i in 1: nsim) {

btstrp_sec_x <- sample(sec_x,length(sec_x), replace=TRUE)
btstrp_p_x <- pnorm((80-mean(btstrp_sec_x))/sd(btstrp_sec_x))
btstrp_sec_y <- sample(sec_y,length(sec_y), replace=TRUE)
btstrp_p_y <- pnorm((80-mean(btstrp_sec_y))/sd(btstrp_sec_y))

btstrp_p_d[i] <- btstrp_p_y - btstrp_iqr_x	

}

# finding stdev of bootstrapped differences in iqr
se_d <- sd (btstrp_p_d)


# applying wald test
 
p_value <- 2* pnorm(-abs(s_p)/se_d)

p_value

# since p_value is ~ 0.79 we dont reject Ho and so Inter quartile difference between the two section is not significant
# In above case I am testing actual sample value against MLE based se^, we can also use actual proportions of samples in bootstrap to caluclate se^ and use that std error for wald's test.


#########################
# part f
#########################
# no of simulations

# Ho1 : p1x-p1y =0 vs Ha1 : p1x-p1y /= 0 and so on.

# px-py is estimated using px^-py^ and it has se^ of sqrt( px^(1-px^)/nx + py^(1-py^)/ny)

# we will use borrenheins multi testing. 

# so alpha ^ = 0.05/3 = 0.0167


px<- rep(0,3)

for (j in 1 : length(sec_x)) {
if (sec_x[j] < 50 ) {
px[1] <- px[1]+1
} else if (sec_x[j] <= 80 ) {
px[2] <-px[2]+1}
else  { px[3] <- px[3] +1 }
}

px[1] <- px[1]/length(sec_x)
px[2] <- px[2]/length(sec_x)
px[3] <- px[3]/length(sec_x)

px

py<-rep(0,3)

for (j in 1 : length(sec_x)) {
if (sec_x[j] < 50 ) {
py[1] <- py[1]+1
} else if (sec_x[j] <= 80 ) {
py[2] <-py[2]+1}
else  { py[3] <- py[3] +1 }
}

py[1] <- py[1]/length(sec_y)
py[2] <- py[2]/length(sec_y)
py[3] <- py[3]/length(sec_y)

py

for (i in 1:3) {

p_value = 2* pnorm ( -abs(px[i]-py[i])/(sqrt(((px[i]*(1-px[i]))/length(sec_x))+((py[i]*(1-py[i]))/length(sec_y)))))

if (p_value < (0.05/3)) { cat ( p_value,i," the H0 rejected\n") }
}



# since p_values are 0.9 , none of the individual hypothesis is rejection. Since borrenhoni passed  B-H which is a less restrictive test, will also pass.











