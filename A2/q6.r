# assignment 2, q6

# using u^-u0/se is N(0,1) to test ho : u=1 vs ha: u>1 ;

# in case of poisson ; lambda^ = x_bar ; se^ = x_bar/sqrt(n)

nsim <- 10000

for (j in 1: nsim) {

s <- rpois(
