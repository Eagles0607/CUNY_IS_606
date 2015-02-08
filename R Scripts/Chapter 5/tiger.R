### tiger.R

## Define the expectaion function
## Hard code values for n and d (n = 100 and d=50)
func <- function(n)
{
	n*(1-(1-1/n)^100) - 50
}

## Uniroot finds a root numerically, looking inside the interval (50, 200)
uniroot(func,c(50,200))$root

