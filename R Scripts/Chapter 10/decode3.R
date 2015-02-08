# decode.R

mat <- read.table("ShakeCount.txt",header=F)
mat <- read.table("AustenCount.txt",header=F)
logmat <- log(mat + 1)
str <- c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z"," ")

oldcode <- "presidents generally do not like to admit mistakes so it was interesting when obama owned up to one during an interview with charlie rose on cbs last summer it was the job of the president obama said to tell a story to the american people that gives them a sense of unity and purpose and optimism and it was on this score that he had fallen short"
oldcode <- "the theory of probabilities is at bottom nothing but common sense reduced to calculus it enables us to appreciate with exactness that which accurate minds feel with a sort of instinct for which often they are unable to account it is remarkable that a science which began with the consideration of games of chance should have become the most important object of human knowledge"
oldcode <- "the theory of probabilities is at bottom nothing but common sense reduced to calculus it enables us to appreciate with exactness that which accurate minds feel with a sort of instinct for which often they are unable to account"
keep <- "i think you are begging the question said haydock and i can see looming ahead one of those terrible exercises in probability where six men have white hats and six men have black hats and you have to work it out by mathematics how likely it is that the hats will get mixed up and in what proportion if you start thinking about things like that you would go round the bend let me assure you of that agatha christie"
oldcode <- "obama never expressly said that tax rates on top earners must return to the higher levels of the bill clinton era leading to speculation that he was willing to soften the core position of his election campaign to get a grand debt deal with republicans i am not wedded to every detail of my plan i am open to compromise he said"
oldcode <- keep
code_length <- nchar(oldcode)
vec0 <- 1:code_length
c1 <- code_length - 1
vec1 <- 1:c1
oldcode <- dec(oldcode,sample(1:26))
keep1 <- oldcode
tw <- 1:26
func <- 1:27
for (i in 1:4000) {
	p <- sample(tw,2)
	old <- func
	func[p[1]] <- old[p[2]]
	func[p[2]] <- old[p[1]]
	newcode <- dec(oldcode,func)
	p1 <- cost(dec(oldcode,old))
	p2 <- cost (newcode)
	if (runif(1) > exp(p2-p1)^1.3) func <- old
	if ((i %%  200) == 0) print(dec(oldcode,func))
}

 cost <- function(code) {
 	#code_length <- nchar(code)
 	p <- 0
 	for (i in vec1){
 		p <- p+ logmat[grep(substr(code,i,i),str),grep(substr(code,i+1,i+1),str)]
 	}
 p}
 
 ### need translator here
 dec <- function(code,func) {  # p
      out <- code
    #  tt <- nchar(out)
          for (i in vec0) {
      ct <- grep(substr(code,i,i),str)
      if (ct < 27) substr(out,i,i) <- str[func[ ct ]] }
          out }
