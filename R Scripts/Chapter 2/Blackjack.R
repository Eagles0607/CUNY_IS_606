### Blackjack.R
###
### What's the probability of Blackjack?
###

n <- 50000   ### Number of iterations
simlist <- replicate(n, 0) ## Initialize list with 0's

for (i in 1:n)
{
	trial <- sample(1:52, 2, replace=FALSE)
	## Let Ace <--> 1, 2, 3, 4
	## Let Ten card <--> 37, 38, ... , 51, 52
	success <- if (trial[1] <= 4 && trial[2] >= 37 || 
		trial[1] >= 37 && trial[2] <= 4) 1 else 0
	simlist[i] <- success
}

### Simulated result
mean(simlist)