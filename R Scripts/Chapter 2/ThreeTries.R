## ThreeTries.R

n <- 100000
success <- 0
for (i in 1:n) {
	try1 <- sample(0:1,1,prob=c(.7,.3))
	if (try1==1) success <- success + 1	else
	{ try2 <- sample(0:1,1,prob=c(.5,.5))
      if (try2==1) success <- success + 1 else
      	{ try3 <- sample(0:1,1,prob=c(.35,.65))
          if (try3==1) success <- success + 1 }}}
success/n