# ScriptSample.R
# Area of circle
radius <- 1:20
area <- pi*radius^2
plot(radius,area, main="Area as function of radius")

#################################
# Coin flips
n <- 1000 # Number of coin flips
coinflips <- sample(0:1,n,replace=TRUE)
coinflips
heads <- cumsum(coinflips)
avg <- heads/(1:n)
plot(1:n,avg,type="l",xlab="Number of coins",ylab="Running average",
  main="Proportion of heads in 1000 coin flips")
abline(h=0.5)