gibbs <- function(tr) {
	p = 1/2
bb <- numeric(tr)
gg <- numeric(tr)
gg[1]  <- 1+ rgeom(1,p)
bb[1] <- rbeta(1,1,gg[1])
for (i in 2:tr) {
	gg[i] <- 1 + rgeom(1, bb[i-1]) 
	bb[i] <- rbeta(1, 1, gg[i-1])
}
c(bb[tr],gg[tr])
}