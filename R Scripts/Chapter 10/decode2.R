# decode.R

mat <- read.table("ShakeCount.txt",header=F)
mat <- read.table("AustenCount.txt",header=F)
logmat <- log(mat + 1)
str <- c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z"," ")

 score <- function(code) {  # Computes the score of a decoded message
 	p <- 0
 	for (i in vec1){
 		p <- p+ logmat[grep(substr(code,i,i),str),grep(substr(code,i+1,i+1),str)]	}
            p}
 
 dec <- function(code,func) {  # Decrypts code according to func
      out <- code
    #  tt <- nchar(out)
          for (i in vec0) {
      ct <- grep(substr(code,i,i),str)
      if (ct < 27) substr(out,i,i) <- str[func[ ct ]] }
          out }

message <- "i know why you are here neo i know what you have been doing why you hardly sleep why you live alone and why night after night you sit by your computer you are looking for him i know because i was once looking for the same thing and when he found me he told me i was not really looking for him i was looking for an answer it is the question that drives us neo it is the question that brought you here you know the question just as i did what is the matrix the answer is out there neo and it is looking for you and it will find you if you want it to"

message <- "coincidences in general are great stumbling blocks in the way of that class of thinkers who have been educated to know nothing of the theory of probabilities that theory to which the most glorious objects of human research are indebted for the most glorious of illustrations edgar allen poe the murders in the rue morgue morpheus this is your last chance after this there is no turning back you take the blue pill the story ends you wake up in your bed and believe whatever you want to believe you take the red pill you stay in wonderland and i show you how deep the rabbit hole goes"


code_length <- nchar(message)
vec0 <- 1:code_length
c1 <- code_length - 1
vec1 <- 1:c1
codemess <- dec(message,sample(1:26))
keep <- codemess
tw <- 1:26
func <- 1:27
for (i in 1:7000) {
	p <- sample(tw,2)
	old <- func
	func[p[1]] <- old[p[2]]
	func[p[2]] <- old[p[1]]
	p1 <- score(dec(codemess,old))
	p2 <- score (dec(codemess,func))
	if (runif(1) > exp(p2-p1)) func <- old
	if ((i %%  100) == 0) print(c(i,dec(codemess,func)))
}
