

### Chap 7 Normals
setEPS()
postscript(file="../Graphics/c7normals.eps", width=3.5,height=3)
par(mar = c(2.5, 2.5, 2,2) , mex = .8, cex = .8)
curve(dnorm(x,0,1),-6,12,main="",xlab="",ylab="",lty=1)
curve(dnorm(x,2,2),-6,12,add=T,lty=2)
curve(dnorm(x,4,3),-6,12,add=T,lty=6,font=2)
leg.txt <- c("Norm(0,1)","Norm(2,4)","Norm(4,9)")
legend(x = 5,y=.35,legend=leg.txt,lty=c(1,2,6), cex=.8)
dev.off()


#### Chap binomapprox
setEPS()
postscript(file="../Graphics/c7binomapprox.eps", width=4,height=3.5)
tr <- 50000
par(mfrow=c(2,3), mar = c(4,2,1,2), oma = c(0,0,0,0),cex=0.5)
x <- rbinom(tr,4,.1)
hist(x,prob=T,xlab="",ylab="",main="Binom(4, 0.1)")
curve(dnorm(x,4*.1,sqrt(4*.1*.9)),0,4,add=T)
# title("Binom(4, 0.1)",outer=T)

x <- rbinom(tr,10,.1)
hist(x,prob=T,xlab="",ylab="",main="Binom(10, 0.1)")
curve(dnorm(x,10*.1,sqrt(10*.1*.9)),0,6,add=T)
x <- rbinom(tr,25,.1)
hist(x,prob=T,xlab="",ylab="",main="Binom(25, 0.1)")
curve(dnorm(x,25*.1,sqrt(25*.1*.9)),0,10,add=T)
x <- rbinom(tr,50,.1)
hist(x,prob=T,xlab="",ylab="",main="Binom(50, 0.1)")
curve(dnorm(x,50*.1,sqrt(50*.1*.9)),0,13,add=T)
x <- rbinom(tr,100,.1)
hist(x,prob=T,xlab="",ylab="",main="Binom(100, 0.1)")
curve(dnorm(x,100*.1,sqrt(100*.1*.9)),0,24,add=T)
x <- rbinom(tr,500,.1)
hist(x,prob=T,xlab="",ylab="",main="Binom(500, 0.1)")
curve(dnorm(x,500*.1,sqrt(500*.1*.9)),26,80,add=T)
dev.off()



### Chap 7 Gamma
setEPS()
postscript(file="../Graphics/c7gamma.eps", width=3.5,height=3)
par(mar = c(2, 3, 2,2) , mex = .8, cex = .8)
curve(dgamma(x,1,.25),0,23,main="",xlab="",ylab="",lty=1)
curve(dgamma(x,2,.33),0, 23,add=T,lty=2)
curve(dgamma(x,7,1),0,23,add=T,lty=6,font=2)
curve(dgamma(x,30,2),0,23, add=T, lty=3,font=2)
leg.txt <- c("Gamma(1,0.25)", "Gamma(1,0.333)","Gamma(7,1)", "Gamma(30,2)") 
legend(x = 12,y=.23,legend=leg.txt,lty=c(1,2,6,3),cex=.7)
dev.off()


### Chap 7 Gamma Sim
setEPS()
postscript(file="../Graphics/c7gammasim.eps", width=3.25,height=3)
par(mar = c(2.5, 3, 2,2) , mex = .8, cex = .8)
simlist <- replicate(100000, sum(rexp(7,2)))
hist(simlist,prob=T,main="",xlab="",ylab="")
curve(dgamma(x,7,2),0,12,add=T)
dev.off()

### JACK AND JILL
setEPS()
postscript(file="../Graphics/c7jackjill.eps", width=4,height=1.75)
z <- c(0.2,rexp(50,3))
par(mar = c(2, 3, 2,2) , mex = .8, cex = .8)
pp <- cumsum(z)
pp <- pp[pp< 5.5]
plot(pp,rep(0,length(pp)),pch=16,xaxt="n",ylab="",xlab="Time",yaxt="n" ,ylim=c(-.2,.6))
abline(h=0)
axis(1,at=0:5,labels=c("8","9","10","11","12", "1"))
# abline(v=2)
dat <-seq(2,5.6,.01)
lines(dat,rep(0.3,length(dat)) )
pt <- pp[pp>2]
points(pt,rep(0.3, length(pt)),pch=16)
text(-0.25,.3,"Jack",xpd=NA)
text(-.25,0,"Jill",xpd=NA)
lines(c(2,2),c(-.1,.4))
dev.off()

# Chap 7 Betaa
setEPS()
postscript(file="../Graphics/c7beta.eps", width=3.5,height=3)
par(mar = c(2, 3, 2,2) , mex = .8, cex = .8)
x <- seq(0,1,.01)

 curve(dbeta(x,1,1),0,1,xlab="x",ylab="",ylim=c(0,2))
text(0.34,.9,"a=1, b=1")

curve(dbeta(x,.5,.5),0,1,add=T)
text(.5,.55, "a=0.5, b=0.5")
curve(dbeta(x,2,2),0, 1, add=T)
text(.65,1.5, "a=2, b=2")

curve(dbeta(x,2,3.5),0,1,add=T)
text(.3,1.75,"a=2, b=3.5")
dev.off()

# Chap 7 Beta simulation
setEPS()
postscript(file="../Graphics/c7betasim.eps", width=3.5,height=3)
par(mar = c(3, 3, 2,3) , mex = .8, cex = .8)
m <- 2
n <- 8
simlist <- replicate(50000,sort(runif(n+m-1))[n])
hist(simlist,prob=T,xlab="",main="",ylab="")
curve(dbeta(x,n,m),0,1,add=T)
dev.off()


### Chap 7 Pareto
setEPS()
postscript(file="../Graphics/c7pareto.eps", width=3.5,height=3)
par(mfrow=c(2,2), mar = c(5,5,2,2), cex=0.4)
a = log(5)/log(4)
 curve(a/x^(a+1),1,5,ylab="",xlab="1<x<5")
 curve(a/x^(a+1),3,15,ylab="",xlab="3<x<15")
 curve(a/x^(a+1),9,45,ylab="",xlab="9<x<45")
 curve(a/x^(a+1),27,135,ylab="",xlab="27<x<135")
dev.off()

## C7 continuity correction
setEPS()
postscript(file="../Graphics/c7continuity.eps", width=4.5,height=2.35)
par(mfrow=c(1,2),mar=c(2,1.6,2,1.5))

left <- 11
right <- 13
n <- 20
p <-.5
 testdata <- dbinom((left-1):(right+1),n,p)
 x <- (left-1):(right+1)
 # setup plot ranges noting max of normal density is at mean
 xrange <- range(x) + c(-0.5,+0.5)
 yrange <- range(c(testdata, dnorm(n*p, n*p, sqrt(n*p*(1-p))), 0))
 plot(xrange, yrange, type = "n", xlab = "", ylab = "", xaxt = "n",cex.axis=0.6)
 axis(1, x,cex.axis=0.6)
 rect(x - 0.5, 0, x + 0.5, testdata)

 rect(left:right-.5,c(0,0,0),left:right+.5,dbinom(left:right,n,p),col="gray90")
xc <- c(seq(left,right,.1),seq(right,left,-.1))
yc <-c(rep(0,21), dnorm(seq(right,left,-.1),n*p, sqrt(n*p*(1-p))))
polygon(x=xc,y=yc ,col="gray15",density=70 )
curve(dnorm(x, n*p,sqrt(n*p*(1-p))), min(xrange), max(xrange), add = TRUE)
#text(6,.25,"Exact: P(5 <= X <= 7) = 0.359")
#text(6,.22,"Approximate: P(5 < X < 7) = 0.223")
title("Without continuity correction",cex.main=0.6)

# testdata <- dbinom(3:7,8,.5)
 #x <- 3:7
 # setup plot ranges noting max of normal density is at mean
 #xrange <- range(x) + c(-0.5,+0.5)
 #yrange <- range(c(testdata, dnorm(2.84, 2.84, 1.57), 0))
 plot(xrange, yrange, type = "n", xlab = "", ylab = "", cex.axis=0.6,xaxt = "n")
 axis(1, x,cex.axis=0.6)
 rect(x - 0.5, 0, x + 0.5, testdata)

 rect(left:right-.5,c(0,0,0),left:right+.5,dbinom(left:right,n,p),col="gray90")
xc <- c(seq(left-.5,right+.5,.1),seq(right+.5,left-.5,-.1))
yc <-c(rep(0,31), dnorm(seq(right+.5,left-.5,-.1),n*p, sqrt(n*p*(1-p))))
polygon(x=xc,y=yc ,col="gray15",density=70 )
curve(dnorm(x, n*p,sqrt(n*p*(1-p))), min(xrange), max(xrange), add = TRUE)
title("With continuity correction",cex.main=0.6)
dev.off()

### pois proc diagram?
setEPS()
postscript(file="../Graphics/c7poisproc.eps", width=4,height=1.75)
z <- c(0.1,rexp(50,3))
par(mar = c(2, 3, 2,2) , mex = .8, cex = .8)
pp <- cumsum(z)
pp <- pp[pp< 3]
plot(pp,rep(0,length(pp)),pch=16,xaxt="n",yaxt="n" ,ylim=c(-.2,.6))
abline(h=0)
axis(1,at=0:4,labels=0:4)
abline(h=0.2)
lines(c(pp[1],pp[1]),c(.18,.22))
lines(c(pp[2],pp[2]),c(.18,.22))
lines(c(pp[3],pp[3]),c(.18,.22))
lines(c(pp[4],pp[4]),c(.18,.22))

text(-0.25,.3,"Jack",xpd=NA)
text(-.25,0,"Jill",xpd=NA)
lines(c(0.9,0.9),c(-.1,.4))
lines(c(2.4,2.4),c(-.1,.4))
text(0.8,-.1, labels=paste(expression(N[0.9]),"=3"))
dev.off()

### c7 binormal
setEPS()
postscript(file="../Graphics/c7binormal.eps", width=3.5,height=3)#
par(mar = c(0, 3, 2,2) , mex = .8, cex = .8)
mu1<-0 # setting the expected value of x1
mu2<-0 # setting the expected value of x2
s11<-1 # setting the variance of x1
#s12<-1 # setting the covariance between x1 and x2
s22<-1 # setting the variance of x2
rho<- .5 # setting the correlation coefficient between x1 and x2
s12 <- rho *sqrt(s11)*sqrt(s22)
x<-seq(-3,3,length=41) # generating the vector series x1
y<-x # copying x1 to x2
#
f<-function(x,y)
{
term1<-1/(2*pi*sqrt(s11*s22*(1-rho^2)))
term2<--1/(2*(1-rho^2))
term3<-(x-mu1)^2/s11
term4<-(y-mu2)^2/s22
term5<--2*rho*((x-mu1)*(y-mu2))/(sqrt(s11)*sqrt(s22))

term1*exp(term2*(term3+term4+term5))
} # setting up the function of the multivariate normal density
#
z<-outer(x,y,f) # calculating the density values
#
persp(x, y, z,
main="",
theta=-19, phi=16,
r=30,
d=0.1,
expand=0.45,
ltheta=90, lphi=180,
shade=0.5,
ticktype="detailed",
nticks=5,cex.lab=0.8,cex.axis=0.8) # produces the 3-D plot
#
#mtext(expression(list(rho==0.8)), side=3) # adding a text line to the graph
dev.off()

install.packages("UsingR")
library(UsingR)
### c7 galton
setEPS()
postscript(file="../Graphics/c7galton.eps", width=3.5,height=3)#
par(mar = c(2, 3, 2,2) , mex = .8, cex = .8)

plot(father.son,xlab="Father's Height (inches)", ylab="Son's Height (inches)", main="",pch=20)
#abline(v=67)
#text(67.6,58.5,"    F = 67")
dev.off()


