### Balls.R
### Data here is from Feller's London bombing raids example
n <- 537   # balls
u <- 576   # bowls
lambda = n/u
bowls <- rep(0,u)  # Initialize the bowls to 0

for (i in 1:n)
{
	i <- sample(1:u,1)  # Pick a bowl at random
	bowls[i] <- bowls[i] + 1   # The number of balls in that bowl increases by 1
}

bowls   ### Look at all the bowls
table(bowls)   ### Frequency table
#### Compare to expected counts based on Poisson model
round(dpois(0:max(bowls),lambda)*u,2)