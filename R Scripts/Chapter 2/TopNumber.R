## TopNumber.R
## 
## Numbers are 1, ... , n
n <- 100  # Too number
r <- round(n/exp(1)) # r = n/e = 37
simlist <- numeric(10000)
for (j in 1:10000)
{
envlist <- sample(1:n, n)
best <- which(envlist==n) # position of largest number
prob <- 0
firstmax <- max(envlist[1:r])  # maximum of the first r  numbers
for (i in (r+1):n)  # start looking after the r-th
{  
 if (envlist[i] > firstmax) 
{   if (envlist[i] == n) prob <- 1
  	break}
  	else {prob<-0}
 }
 simlist[j] <- prob
 }
 mean(simlist)  	