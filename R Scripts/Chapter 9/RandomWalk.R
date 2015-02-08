### RandomWalk.R
### S = X1 + ... + Xn

rw <- sample(c(-1,1), 10000, replace=T, prob=c(1/2,1/2))
plot(cumsum(rw),type="l",xlab="Steps",ylab="Position",ylim=c(-200,200))


#### Final position S of the random walk
trials <- 5000
simlist <- numeric(trials)
for (i in 1:trials) {
rw <- sample(c(-1,1),10000, replace=T,prob=c(1/2,1/2))
simlist[i] <- tail(cumsum(rw),1)  # Final position of random walk
}

hist(simlist)
mean(simlist)
sd(simlist)