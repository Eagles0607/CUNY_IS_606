## Triangle.R
## Simulating uniform points on the triangle with vertices (0,0), (1,0), (1,1)
## Use accept-reject method

xsim <- c()
ysim <- c()
for (i in 1:10000) {
  x <- runif(1)
  y <- runif(1)
  if (y < x) {
     xsim <- c(xsim, x)
     ysim <- c(ysim,y)
     }
     }
     
     plot(xsim,ysim,pch=20,cex=.5,main="Uniform model on triangle with accept-reject")
     
     cov(xsim,ysim)
     cor(xsim,ysim)