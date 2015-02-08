# 3-D plots
#
#
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
main="Bivariate Standard Normal Distribution",
theta=-19, phi=16,
r=30,
d=0.1,
expand=0.5,
ltheta=90, lphi=180,
shade=0.5,
ticktype="detailed",
nticks=5) # produces the 3-D plot
#
mtext(expression(list(rho==0.8)), side=3) # adding a text line to the graph
