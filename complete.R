complete <- function(directory = "specdata",id = 1:332){
  old <- getwd()
  setwd(directory)
  rowcomplete <- numeric(length(id))
  j <- 1
  for(i in id){
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
    rowcomplete[j] <- nrow(play[c,])
    j <- j+1
  }
  comp <- data.frame(id = id, nobs = rowcomplete)
  setwd(old)
  comp
}
