### SLLN.R
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


    ### Cauchy
    
par(mfrow=c(3,3),oma=c(0,0,5,0))
layout(matrix(1:9,nrow=3))
sllnCauchy <- function(n) {
for (i in 1:9) {
	seq <- rcauchy(n)
	avgs <- cumsum(seq)/(1:n)
    plot(avgs,type="l",xlab="n",ylab="Average")
    abline(h = 0)
    }
 mtext(paste("Behavior of Sequence of Averages ( n = ",n,")"),side=3, font=2,outer=T)
 mtext(paste("Cauchy distribution"),side=3, outer=T, font=2, line =-2)
 }
sllnCauchy(10)
sllnCauchy(100)
sllnCauchy(50000)
    