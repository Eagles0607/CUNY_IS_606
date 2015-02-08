# contingency.R

rawdat <-c( 0, 0,  1,  1,  1,  1,  1,  1,  1,  1,  0,  1, 1, 1, 1,1,  1, 1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  0,  1,  0,  1,  1,  0,   0,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  0,1,  1,  0,  0,  0,  0,  1,  1,  1,  0,  0,  1,  0,  1,  0,  1,  1,  0,  1,  1,  1,   1,1,   1, 0,  1,  1,  1,  1,  1,  1,  0, 1 ,0,1,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  0,  1,  0,  0,  0,  0,  0,  0,  1,  1,  1,  1,  1,  1,  1,  0,  0,  1,  0,  1, 1, 0,  0,0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 0 ,1, 0,  0,  0,  0,  0,  0,  0,  1,  1,  1,  1,  1,  1, 1,  1,  0,  1,  0,  0,  1,  0,  0, 0,  0,  1,  1,  1,  1,  1,  1,  1,  1,  0,  1,  0,  1,  1,  0,  0,  0,  0,  1,  1,  1,  0,  1,  1,  0,  1,  0,  0,  0,  0,  0,  0,  0, 0,  0,  1,  1,  0,  0,  0, 0,0,  0,  0,  0,  0,  0,  0,  0,  0, 1,  1,  1,  1,  1,  1,  1,  1,  1,  1, 1,  1,  1,  1,  1,  1,1)

occmat <- matrix(rawdat,nrow=13,byrow=T)
r <- dim(occmat)[1]
c <- dim(occmat)[2]

idx1 <- 1:(r-1)
idx2 <- 1:(c-1)
submat <- function(mat) {
id <- c()
for (i in idx1)
for (j in idx2)
if (     (mat[i,j] + mat[i,j+1])==1 & (mat[i+1,j] + mat[i+1,j+1] ==1) & (mat[i,j] + mat[i+1,j]==1)) { id <- c(id,j + 16*(i-1))}
id }

mat <- matrix(rpois(16,20),nrow=4)
rs <- rowSums(mat)
cs <- colSums(mat)
rr <- length(rs)
cc <- length(cs)
trials <- 100000
simlist<-numeric(trials)
for (i in 1:trials) {
	
	
	
rp <- sample(1:rr,2)
cp <- sample(1:cc,2)

a <- mat[rp[1],cp[1]]
d <- mat[rp[2],cp[2]]+1
b <- mat[rp[1],cp[2]]-1
c <- mat[rp[2],cp[1]]-1

if( a >= 0 & b >= 0 & c >= 0 & d >= 0) { mat[rp[1],cp[1]] <- a
	mat[rp[1],cp[2]] <- b
	mat[rp[2],cp[1]] <- c
	mat[rp[2],cp[2]] <- d}
	
	simlist[i] <- chisq.test(mat)$statistic
	}
	hist(simlist,prob=T)
	curve(dchisq(x,9),0,120,add=T)