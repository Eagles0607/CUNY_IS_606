### AcceptReject.R
###

x <- seq(0,2,.01)
y <- (3/4)*(2*x - x^2)
plot(x,y,type="l",xlim=c(0,2),ylim=c(0,0.8),xlab="x",ylab="f(x)",main="Acceptance-Rejection Method for Simulating from Density Function")
abline(v=1.25)

