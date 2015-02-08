### Divisible356.R
###
### What's the probability that a random integer between 1 and 1,000
###   is divisible by 3, 5, or 6?


##########################################################################
### The function simdivis() simulates one trial, returning a 1 if the event occurs, and 0 otherwise

simdivis <- function() 
{
num <- sample(1:1000,1)   ### Pick a random integer from 1 to 1,000
if (num %% 3==0 || num %% 5 == 0 || num%%6==0) 1 else 0  
	### Return 1 if the num is divisible by 3, 5, or 6; 0, otherwise
}
###########################################################################

### Now repeat many times (say, 1000) and take the proportion of 1's as the simulated probability

simlist <- replicate(10000, simdivis() ) 
mean(simlist)