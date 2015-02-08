### Coin flip 1.R
###
### What's the probability of getting heads in 3 coin flips?
###
### Random experiment: Flip 3 coins
### Event: All heads

#####################################
### The trial
#####################################
trial <- sample(0:1, 3, replace=TRUE)

######################################
### Success?
######################################
if (sum(trial)==3) 1 else 0

######################################
### Repeat
######################################
n <- 10000   ### Number of iterations
simlist <- replicate(n, 0) ## Initialize list with 0's
for (i in 1:n)
{
	trial <- sample(0:1, 3, replace=TRUE)
	success <- if (sum(trial)==3) 1 else 0
	simlist[i] <- success
}

### Simulated result
mean(simlist)