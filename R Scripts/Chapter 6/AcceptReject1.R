## Accept Reject1.R

par(mfrow=c(2,2),mar = c(2, 2,1,1) , mex = .8, cex = .8)
n <- 3*pi/2
x <-seq(-n,n,.01)

f1 <- function(x) cos(x) + cos(2*x) + 2
f2 <- function(x) -20*x^2/(9*pi^2) + 6

plot(x, f1(x), type="l",ylim=c(0,6),xlab="",xaxt="n",yaxt="n",ylab="",xlim=c(-5,5))
par(new=TRUE)
plot(x, f2(x), ylim=c(0,6),xlim=c(-5,5),xlab="",ylab="",type="l",xaxt="n",yaxt="n")
axis(1,at=c(seq(-5,5,2)),cex.axis=.5,tck=-.03,mgp=c(3,0,0))
axis(2,at = 0:6,cex.axis=.5,tck=-.03,mgp=c(3,.2,0))

plot(x, f1(x), type="l",lwd=2,ylim=c(0,6),xlab="",xaxt="n",yaxt="n",ylab="",xlim=c(-5,5))
par(new=TRUE)
plot(x, f2(x), ylim=c(0,6),lwd=2,xlim=c(-5,5),xlab="",ylab="",type="l",xaxt="n",yaxt="n")
par(new=TRUE)
x <- runif(400,-5,5)
y <- runif(400,0,6)
plot(x,y,ylim=c(0,6),xlim=c(-5,5),pch=20,cex=.4,xaxt="n",yaxt="n",xlab="",ylab="")

xnums <- c()
ynums <- c()
for (i in 1:400) {
x1  <- x[i]
y1 <- y[i]
if (y1 < f2(x1) & y1 > f1(x1) ) {
	xnums <- c(xnums,x1)
	ynums <- c(ynums,y1)
} }
plot(xnums,ynums,xlim=c(-5,5),ylim=c(0,6),pch=20,cex=.4,xaxt="n",yaxt="n",xlab="",ylab="")
xnums <- c()
ynums <- c()
for (i in 1:5000) {
x <- runif(1,-5,5)
y <- runif(1,0,6)
if (y < f2(x) & y > f1(x)   {
	xnums <- c(xnums,x)
	ynums <- c(ynums,y)
}  }
plot(xnums,ynums,xlim=c(-5,5),ylim=c(0,6),pch=20,cex=.4,xaxt="n",yaxt="n",xlab="",ylab="")
