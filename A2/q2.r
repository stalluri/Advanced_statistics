# Assignment 2 , Q2

# assigning p to 0.04
p<-0.4
nsim <-10	00
alpha <-0.05

n<- c(10,30,50,100,250,500,1000)
n_count <- rep(0,length(n))


for (i in 1:length(n)) {

# count reset to zero for each sample size to determine p^

count <- 0

# Level of significance alpha =.05
epsilon <- sqrt((1/(2*n[i]))*(log(2/alpha)))

for (j in 1:nsim) {

# resetting sample to n value
s<- rep(0,n[i])

# generating the bernouli variate
s<- rbinom(n[i],1,p)

s_mean <- mean(s)

if ( (s_mean <= (p + epsilon)) & (s_mean >= (p - epsilon)) ) {
count <- count +1
}
}

n_count[i] <- (count/nsim)
}

plot(n,n_count)













