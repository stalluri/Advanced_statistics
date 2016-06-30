# midterm 2008, Q1

# part b (i)
# no. of simulations
nsim <-1000
# sample size given
n<-50

#double exponential random variate generation

#mean =0 , and stdev <- sqrt(2)
mn <- 0
stdev <- sqrt(2)

# Initializing count variable
count<- 0
# Running the simulation 

for (j in 1:nsim) {

# generating double exponential variate through prob integral transformation.

#suniform <- runif(n, min=-0.5, max=0.5)
#s<- mn-((abs(suniform)/suniform)*(log(1-2*(abs(suniform)))))

s <- rexp(n,1)-rexp(n,1)

# hist(s)

s_mn <- mean(s)
s_err <- (s-s_mn)
s_err2 <- (sum(s_err^2))/n
s_err3 <- (sum(s_err^3))/n
s_err4 <- (sum(s_err^4))/n


s_g1 <- s_err3/(s_err2^1.5)
s_g2 <- (s_err4/(s_err2^2))-3

test_statistic <- (n/6)*((s_g1^2) + ((s_g2^2)/4))

cat (test_statistic,"\n")

if (test_statistic > 4.605) { count <- count+1 }

}

power_of_test <- (count/nsim)*100
power_of_test

###########################################
# part b (ii)

# no. of simulations
nsim <-1000
# sample size given
n<-50


# Initializing count variable
count<- 0
# Running the simulation 

for (j in 1:nsim) {

# generating t-variate with 30 degrees of freedom

s<- rt(n,30)


# hist(s)

s_mn <- mean(s)
s_err <- (s-s_mn)
s_err2 <- (sum(s_err^2))/n
s_err3 <- (sum(s_err^3))/n
s_err4 <- (sum(s_err^4))/n


s_g1 <- s_err3/(s_err2^1.5)
s_g2 <- (s_err4/(s_err2^2))-3

test_statistic <- (n/6)*((s_g1^2) + ((s_g2^2)/4))

if (test_statistic > 4.605) { count <- count+1 }

}

power_of_test <- (count/nsim)*100

power_of_test


###########################################

# part c

# no. of simulations
nsim <-1000
# sample size given
n<-100

# for plotting power of test again p values.

p<- seq(0,0.5,by=.05)
power_of_test <- seq(1:length(p))

for (i in length(p)){
# Initializing count variable
count<- 0
# Running the simulation 

for (j in 1:nsim) {

# generating p*N(0,1)+(1-p)*N(2,3)

s<- p[i]*rnorm(n,mean=0, sd=1) + (1-p[i])*rnorm(n,mean=2, sd=sqrt(3))

# hist(s)

s_mn <- mean(s)
s_err <- (s-s_mn)
s_err2 <- (sum(s_err^2))/n
s_err3 <- (sum(s_err^3))/n
s_err4 <- (sum(s_err^4))/n


s_g1 <- s_err3/(s_err2^1.5)
s_g2 <- (s_err4/(s_err2^2))-3

test_statistic <- (n/6)*((s_g1^2) + ((s_g2^2)/4))

if (test_statistic > 4.605) { count <- count+1 }

}

power_of_test[i] <- (count/nsim)*100

}

plot(p,power_of_test)

###########################################

# part d

# no. of simulations
nsim <-1000

# for plotting power of test again n values.
n<-c(200,300,400,500,750,1000)

power_of_test <- seq(1:length(n))

for (i in length(n)){
# Initializing count variable
count<- 0
# Running the simulation 

for (j in 1:nsim) {


# generating p*N(0,1)+(1-p)*N(2,3) ; p is fixed at 0.01
p<-0.01
s<- p*rnorm(n[i],mean=0, sd=1) + (1-p)*rnorm(n[i],mean=2, sd=sqrt(3))

# hist(s)

s_mn <- mean(s)
s_err <- (s-s_mn)
s_err2 <- (sum(s_err^2))/n[i]
s_err3 <- (sum(s_err^3))/n[i]
s_err4 <- (sum(s_err^4))/n[i]


s_g1 <- s_err3/(s_err2^1.5)
s_g2 <- (s_err4/(s_err2^2))-3

test_statistic <- (n[i]/6)*((s_g1^2) + ((s_g2^2)/4))

if (test_statistic > 4.605) { count <- count+1 }

}

power_of_test[i] <- (count/nsim)*100

}

plot(n,power_of_test)
