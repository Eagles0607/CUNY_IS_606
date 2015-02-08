### Alice.R
###
### Random sums of random variables
### Assume customer's spending amounts is normal
###  with mean 14 and standard deviation 2

n = 150000
simlist <- rep(0,n)
for (i in 1:n) {
N <- rpois(1,100) # Number of customers
cust <- rnorm(N,14,2)
total <-sum(cust)
simlist[i] <- total
}
mean(simlist)
sd(simlist)

###### A one-liner
simlist <- replicate(50000, sum(rnorm(rpois(1,100),14,2)))
mean(simlist)
sd(simlist)