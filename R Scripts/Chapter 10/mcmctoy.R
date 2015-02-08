## mcmctoy.R
pi <- c(0.1, 0.2, 0.3, 0.4)
 mcmc <- function(n) {
   current_state <- 0
   for (i in 1:n) {
     proposal <- (current_state + sample(c(-1,1),1)) %% 4
     accept <- pi[proposal+1]/pi[current_state+1]
     if (runif(1) < accept) current_state <- proposal  }
   current_state }
 replicate(20,mcmc(100))
 trials <- 10000
 simlist <- replicate(trials,mcmc(100))
 table(simlist)/trials
 
 
 f <- function(x) {
 	(exp(-4*(x-3)^2) + exp(-40*(x-1)^2))/2
 }
optimize <- function(n) {
   current_state <- 3
   for (i in 1:n) {
     proposal <- runif(1,0,6)
          accept <- f(proposal)/f(current_state)
          
     if (runif(1) < accept) current_state <- proposal  }
   current_state }
 trials <- 100000
 simlist <- replicate(trials,optimize(80))
mean(simlist)
 
