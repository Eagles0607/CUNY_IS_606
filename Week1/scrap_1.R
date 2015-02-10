envelopes <- c("A", "B", "C", "D")
xdata=c()

for(i in 1:1000){
  prize <- sample(envelopes)[1]
  pick <- sample(envelopes)[1]
  open <- sample(envelopes[which(envelopes != pick & envelopes !=prize)])[1]
  switchyes <- envelopes[which(envelopes != pick & envelopes != open)]
  if(pick==prize){xdata=c(xdata,"noswitchwin")}
  if(switchyes==prize){xdata=c(xdata,"switchwin")}
}

length(which(xdata == "switchwin"))
length(which(xdata == "noswitchwin"))