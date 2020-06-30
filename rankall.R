rankall <- function(outcome, num = "best"){
  out <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  if(!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) stop("invalid outcome")
  
  num1 <- function(){
    if(outcome == "heart attack") b <- 11
    else if(outcome == "heart failure") b <- 17
    else b <- 23
  }
  out <- out[,c(2, num1(), 7)]
  out[,2] <- as.numeric(out[,2])
  s <- split(out, out$State)
  if(num == "best") m <- 1
  else if(num == "worst") m <- "worst"
  else m<- num
  
  ranker <- function(t){
    
    t <- t[!(is.na(t[,2])),]
    if(m == "worst") m <- length(t[,2])
    if(m > length(t[,2])) {
      f <- data.frame(Hospital.Name = "<NA>", State = t$State[1])
      return(f)
    }
    t <- t[order(t[,2], t[,1]),]
    rank <- seq_len(length(t[,2]))
    t$rank <- rank
    f <- t[t$rank == m,c(1,3)]
    return(f)
  }
  z <- data.frame(t(sapply(s, ranker)))
  colnames(z) <- c("hospital", "state")
  z
}