
N <- seq (1:10000)


for ( i in 1:10000) {
x <- rgamma(1000,2,3)
# sample size of 1000 taken.

s<- 0
n<- 1
while (s <1) {
s <- s + x[n]
n<- n +1

}

N[i] <- n-1
}

hist (N)



