### Points.R
###
### Problem of points 

### Player A needs a points
### Player B needs b points
### p = Prob(A wins)

########## SIMULATION ####################

### simpoint() returns 1 if A eventually wins
###                    0 if B eventually wins

a <- 3
b <- 5
p <- .5

simpoint <- function() {
acount <- 0
for (i in 1:(a+b-1)) acount <- acount + rbinom(1,1,p)
if (acount >= a) 1 else 0
}

simlist <- replicate(10000, simpoint())
mean(simlist)

#### EXACT SOLUTION ##########################

pofp <- function(a, b, p)
{ pnbinom(b-1,a,p)
	}

pofp(a,b,p)
