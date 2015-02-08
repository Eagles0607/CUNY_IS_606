### Bayes.R
###
### A coin is picked at random. It is either fair, 2-headed, or 2-tailed
### The coin comes up heads. 
### What's the probability the coin is 2-headed?
###

n <- 50000 
ctr <- 0
data <- c(0,0,0)  # Stores number of times coin is fair, 2-h, 2-t

while (ctr < n)
{
	coin <- sample(c(1, 2, 3), 1) ### Pick a coin at random
	p <- c(.5, 1, 0)[coin]   ### p = Prob(Heads) for the coin picked
	cointoss <- sample(0:1, size=1, prob=c(1-p,p)) 
			## Flip coin with 1-p - P(Tails), p = P		(Heads`	                			## cointoss = 1 for heads, 0 for tails
	if (cointoss == 1) ### Check if heads 
	                     ### If not, skip through and flip again
	                     ### If yes, keep track of which coin was tossed 
	{
	data[coin] <- data[coin]+1
	ctr <- ctr + 1
	}
}

### Simulated result
Coin <- c("Fair", "2-H", "2-T")
data.frame(Coin, data/n)