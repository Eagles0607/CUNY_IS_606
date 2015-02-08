# CouponCollect.R

## Function simcollect(n) simulates the
##   coupon collector's problem.
## How many draws are required to get a full set of coupons?

simcollect <-function(n) {
coupons <- 1:n # set of coupons
collect <- numeric(n)
nums <-0
while (sum(collect)<n)
{
	i <- sample(coupons,1)
	collect[i] <- 1
	nums <- nums + 1
}
nums
}

## Simulate the mean and variance 
trials <-10000
simlist <- replicate(trials,simcollect(n))
mean(simlist)
var(simlist)