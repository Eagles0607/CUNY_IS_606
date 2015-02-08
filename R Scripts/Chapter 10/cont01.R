r1 <- rep(1,17)
r2 <- c(rep(1,11),0,0,1,0,1,1)
r3 <- c(rep(1,13),0,1,0,0)
r4 <- c(rep(1,13),0,0,0,0)
r5 <- c(1,0, rep(1,11),0,0,0,0)
r6 <- c(rep(1,11),0,0,0,0,0,0)
r7 <- c(rep(1,5),0,1,1,0,0,0,0,0,1,0,1,1)
r8 <- c(rep(1,6), 0,1,1,1,1,0,0,0,0,0,0)
r9 <- c(rep(1,10), 0,0,0,0,0,0,0)
r10 <- c(1,1,1,1,0,1,1,0,0,0,0,0,0,0,0,0,0)
r11<-c(rep(0,13),1,1,0,0)
r12<- c(1,1,rep(0,15))
r13 <- c(0,0,0,0,1,rep(0,12))
mat <- matrix(c(r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13),nrow=13,byrow=T)
 


#mat <- matrix(rbinom(100,1,1/2), nrow=10)
iter <- 10
simlist<-numeric(iter)
for (z in 1:iter)  {

r <- dim(mat)[1]
c <- dim(mat)[2]
trials <- 1500
for (k in 1:trials) {
ct <- 0
cd <- c()
for (i in 1:(r-1))   {
	for (j in 1:(c-1))   {
	if ( (mat[i,j] == 1 & mat[i,j+1]==0 & mat[i+1,j ]==0 & mat[i+1,j+1]==1) | (mat[i,j]==0 & mat[i,j+1]==1 & mat[i+1,j]==1 & mat[i+1,j+1]==0)) {ct  <- ct +1
	cd <- c(cd, (i-1)*r + j)
	}}}
b <- cd[sample(1:length(cd),1)]
uni <- ceiling(b/r)
unj <-	b-(uni-1)*r
 
	mat[uni,unj]<- 1-mat[uni,unj]
	mat[uni,unj+1] <- 1-mat[uni,unj+1]
	mat[uni+1,unj] <- 1-mat[uni+1,unj]
	mat[uni+1,unj+1] <- 1-mat[uni+1,unj+1]
	}
	simlist[z] <- length(cd)
	}