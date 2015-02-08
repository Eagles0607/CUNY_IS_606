### ConditionalDice.R
###
### Roll 2 dice.
### P(First die 2 | Sum is 7)

n <- 60000 
ctr <- 0
simlist <- replicate(n, 0) ## Initialize list with 0's
while (ctr < n)
{
	trial <- sample(1:6, 2, replace=TRUE) ## Roll 2 dice
	if (sum(trial) == 7) ### Check if sum is 7
	                     ### If not, skip through and roll again
	                     ### If 7, check if first die is a 2 
	{
	success <- if (trial[1] == 2) 1 else 0
	ctr <- ctr + 1
	simlist[ctr] <- success
	### simlist records successes and failures only for
	###   dice rolls that sum to 7
	}
}

### Simulated result
mean(simlist)