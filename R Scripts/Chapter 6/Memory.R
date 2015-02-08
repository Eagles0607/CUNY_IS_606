##### Memory.R

### The time until the bus arrives is Exp(1/30)
#### Amy arrives at time 0
#### Zach arrives at time 10

##### Simulating Amy's wait (10,000 times) 

Amy <- rexp(10000,1/30)

#### Simulating Zach's wait

Zach <- c()
for (i in 1:10000)
{ bus <- rexp(1,1/30)
  if (bus > 10) Zach <- c(Zach, (bus-10))
  }
	mean(Amy)    # Amy's average wait
	mean(Zach)   # Zasch's average wait
	
par(mfrow=c(1,2))
hist(Amy, main=expression(paste("Amy's Waiting Times (",mu,"=30)")), prob=T)
hist(Zach, main=expression(paste("Zach's Waiting Times (",mu,"=30)")), prob=T)