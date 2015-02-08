
### bus.R
### Waiting time paradox
### Assume buses come every 20 minutes (lambda =1/20)

mytime <- 200
lambda <- 1/20
simlist <- vector(length=1000)
for (i in 1:1000) {
	arrivals <- cumsum(rexp(30, lambda))
	wait <- arrivals[arrivals > mytime][1] - mytime
	simlist[i] <- wait
	}
mean(simlist)

mytime <- 200
## Length of the interval around mytime
for (i in 1:1000) {
	arrivals <- cumsum(rexp(30, lambda))
	indx <- which(arrivals>mytime)[1]
	lengthintrvl <- arrivals[indx] - arrivals[indx-1]
	simlist[i] <- lengthintrvl
	}
	

x <- rnorm(10000)
y <- x
for (i in 1:10000)
y[i] <- rnorm(1,x[i]*.5,sqrt(1-.5^2))