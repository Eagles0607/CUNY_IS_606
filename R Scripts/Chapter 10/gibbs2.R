# Gibbs.R
# Simulating a bivariate standard normal with correlation rho
# Function gibbsnormal(n, rho) generates (Xn, Yn) from the Gibbs sampler

gibbsnormal <- function(n, rho) {
	x <- 0
	y <- 0
	sd <- sqrt(1-rho^2)
	for (i in 1:n) {
		x <- rnorm(1,rho*y,sd)
		y <- rnorm(1,rho*x,sd) }
	return(c(x,y))
}


### Trivariate example

gibbsthree <- function(trials) {
	x <- 1
	p <- 1/2
	n <- 2
for (i in 1:trials) {
	x <- rbinom(1,n,p)
	p <- rbeta(1,x+1,n-x+1)
	n <- x + rpois(1,4*(1-p)) }
return(c(x,p,n))  }
simmat <- replicate(10000,gibbsthree(500))
marginal <- simmat[1,]
mean(marginal)
var(marginal)
hist(marginal)
