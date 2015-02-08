### LLN.R
###
### Weak Law of Large Numbers
###
####  X <- Bernoulli(p)
p <- 0.5
eps <- 0.1

wlln <- function(n) {
	pbinom( (n*(p + eps), n, p)- pbinom(n*(p - eps), n, p)
}

plot(wlln(1:500),type="l")

p <- 0.2
eps <- 0.01
plot(wlln(1:15000),type="l")

##### X <- Normal(mu, sigma)

mu <- 4
sig <- 10000

wlln <- function(n) {
	pnorm(n*(mu + eps) , n*mu, sig*sqrt(n))- pnorm(n*(mu - eps), n*mu, sig*sqrt(n))
}

wlln(10^(1:20))

###### X <- Poisson(lambda)


lambda <- 10
wlln <- function(n) {
	ppois(n*(lambda + eps) , n*lambda)- ppois(n*(lambda - eps), n*lambda)
}
wlln(10^(1:20))

##################################
####
### Strong Law of Large Numbers
###

### Bernoulli trials

par(mfrow=c(3,3),oma=c(0,0,5,0))
layout(matrix(1:9,nrow=3))
sllnBern <- function(n,p) {
for (i in 1:9) {
	seq <- rbinom(n,1,p)
	avgs <- cumsum(seq)/(1:n)
    plot(avgs,type="l",xlab="n",ylab="Average")
    abline(h = p)
    }
 mtext(paste("Strong Law of Large Numbers ( n = ",n,")"),side=3, font=2,outer=T)
 mtext(paste("Bernoulli trials ( p = ",p,")"),side=3, outer=T, font=2, line =-2)
 }
sllnBern(10,.5)
sllnBern(100,.5)
sllnBern(1000,.5)
    
############
#### Normal 

par(mfrow=c(3,3),oma=c(0,0,5,0))
layout(matrix(1:9,nrow=3))
sllnNorm <- function(n,mu,sigma) {
for (i in 1:9) {
	seq <- rnorm(n,mu,sigma)
	avgs <- cumsum(seq)/(1:n)
    plot(avgs,type="l",xlab="n",ylab="Average")
    abline(h = mu)
    }
 mtext(paste("Strong Law of Large Numbers ( n = ",n,")"),side=3,font=2,outer=T)
 mtext(paste("Normal Random Variables ( mu = ",mu," sigma = ",sigma,")"),side=3, outer=T, line =-2,font=2)
 }
sllnNorm(10,4,2)
sllnNorm(100,4,2)
sllnNorm(1000,4,2)
    

    
    