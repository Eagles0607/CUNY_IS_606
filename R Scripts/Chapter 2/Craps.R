### Craps.R

craps <- function()
{
roll <- sum( sample(1:6,2,replace=T) )
if (roll == 2 || roll == 3 || roll == 12) out <- 0 else
   { if (roll == 7 || roll == 11) out <- 1  else 
   	{ roll2 <- 1
   		while (roll2 != 7 && roll2 != roll )
   		{
   	roll2 <- sum(sample(1:6,2, replace = T))
   	if (roll2 == 7) out <- 0
   	if (roll2 == roll) out <- 1
   }}
   }
   	return(out)
   }

     
