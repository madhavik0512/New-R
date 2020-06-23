corr <- function(directory, threshhold=0){
  coml <- complete(directory, 1:332)
  g <- coml[coml$nobs > threshhold, "id"]
  old <- getwd()
  setwd(directory)
  corrcases <- numeric(length(g))
  j <- 1
  for(i in g){
    if(i <= 9){
      f <- paste0("00",i)
    } else if(i >= 10 && i < 100){
      f <- paste0("0", i)
    } else {
      f <- paste0(i)
    }
    t <- paste0(f,".csv")
    play <- read.csv(t)
    c <- complete.cases(play)
    corrcases[j] <- cor(play[c,"sulfate"],play[c,"nitrate"])
    j <- j+1
  }
  setwd(old)
  corrcases
}cr <- corr("specdata", 5000)