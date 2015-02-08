### Binom.R
### Visualizing the Binomial Distribution


par(mfrow=c(2,2))
n = 4; p = .40
barplot(dbinom(0:n,n,p), names.arg=0:n, main=paste("Binomial Distribution\n n = ",n, ",  p = ",p))
p = .85
barplot(dbinom(0:n,n,p), names.arg=0:n, main=paste("Binomial Distribution\n n = ",n, ",  p = ",p))
n = 8; p = 0.50
barplot(dbinom(0:n,n,p), names.arg=0:n, main=paste("Binomial Distribution\n n = ",n, ",  p = ",p))
n = 8; p = 0.15
barplot(dbinom(0:n,n,p), names.arg=0:n, main=paste("Binomial Distribution\n n = ",n, ",  p = ",p))

