## c10markov.R
## Simulating a Markov chain
## markov(matrix, starting state, # of steps)

 
 markov <- function(mat,start,n)   {
 	state <- start
 	k = dim(mat)[[1]]
 	   for (i in 2:(n+1)) 
    state <- sample(1:k,1,prob=mat[state,])
        return(state)
         }

weather <- matrix(c(1/6,1/3,1/2,1/8,1/8,3/4,1/3,1/6,1/2),nrow=3,byrow=T)

simlist<- replicate(10000,markov(weather,1,200))
table(simlist)/10000