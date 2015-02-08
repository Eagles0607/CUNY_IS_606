
   ----------------
   
   ### bivariate normal
   
par(mfrow=c(2,2),mar=c(1,1,5,0))
x <- seq(-3.5,3.5,.2)
y <- x
rho <- 0 
expa <- 0.65 # expand parameter
thet <- 60
xl = c(-3.5,3.5)
yl = c(-3.5,3.5)
zl = c(0,.2)

f <- function(x, y) {
	(1/(2*pi*sqrt(1-rho^2)))*exp(-(x^2 -2*rho*x*y+ y^2)/(2*(1-rho^2)))
}
dat <- outer(x,y,f)
persp(x,y,dat, theta=thet, phi=20, d = 1.25, xlim=xl,ylim=yl, zlim=zl, ticktype="detailed",  expand=expa, cex = .5,
   cex.axis=.5, zlab="",main= "")
   title(main=list(expression(paste("Bivariate normal ",rho," = 0")), cex=0.85))
   
 x <- seq(-.02,.02,.01)
y <- seq(-3,3,.1)
dat <- outer(x,y,f)
persp(x,y,dat, theta=thet, phi=20, d = 1.25, xlim=xl,ylim=yl, zlim=zl, ticktype="detailed",expand=expa,
   cex.axis=.5, zlab="")   title(main=list(expression(paste("Conditional density of Y given X = 0, 1, 2")), cex=0.85))
  
par(new=TRUE)
x <- seq(.98,1.02,.01)
y <- seq(-3,3,.1)
dat <- outer(x,y,f)
persp(x,y,dat, theta=thet, phi=20, d = 1.25, xlim=xl,ylim=yl, zlim=zl, ticktype="detailed",expand=expa,
   cex.axis=.5, zlab="")
par(new=TRUE)
x <- seq(1.98,2.02,.01)
y <- seq(-3,3,.1)
dat <- outer(x,y,f)
persp(x,y,dat, theta=thet, phi=20, d = 1.25, xlim=xl,ylim=yl, zlim=zl, ticktype="detailed",expand=expa,
   cex.axis=.5, zlab="")
   
  ### 2nd row
   
x <- seq(-3.5,3.5,.2)
y <- x
rho <- 0.8 
expa <- 0.65 # expand parameter
xl = c(-3.5,3.5)
yl = c(-3.5,3.5)
zl = c(0,.3)

f <- function(x, y) {
	(1/(2*pi*sqrt(1-rho^2)))*exp(-(x^2 -2*rho*x*y+ y^2)/(2*(1-rho^2)))
}
dat <- outer(x,y,f)

persp(x,y,dat, theta=thet, phi=20, d = 1.25, xlim=xl,ylim=yl, zlim=zl, ticktype="detailed",  expand=expa,
   cex.axis=.5, zlab="",main="")
      
   title(main=list(expression(paste("Bivariate normal ",rho," = 0.8")), cex=0.85))
   
   
 
x <- seq(-.02,.02,.01)
y <- seq(-3,3,.1)
dat <- outer(x,y,f)
persp(x,y,dat, theta=thet, phi=20, d = 1.25, xlim=xl,ylim=yl, zlim=zl, ticktype="detailed",expand=expa,
   cex.axis=.5, zlab="")
   

par(new=TRUE)
x <- seq(.98,1.02,.01)
y <- seq(-3,3,.1)
dat <- outer(x,y,f)
persp(x,y,dat, theta=thet, phi=20, d = 1.25, xlim=xl,ylim=yl, zlim=zl, ticktype="detailed",expand=expa,
   cex.axis=.5, zlab="")
par(new=TRUE)
x <- seq(1.98,2.02,.01)
y <- seq(-3,3,.1)
dat <- outer(x,y,f)
persp(x,y,dat, theta=thet, phi=20, d = 1.25, xlim=xl,ylim=yl, zlim=zl, ticktype="detailed",  expand=expa, cex = .5,
   cex.axis=.5, zlab="",main= "")

   
   
   


   
   
   


   
   


   
   
   

