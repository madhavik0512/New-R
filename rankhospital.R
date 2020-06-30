rankhospital <- function(state, outcome, num = "best"){
  out <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  if(!(state %in% out[,"State"])) stop("invalid state")
  if(!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) stop("invalid outcome")
  
  num1 <- function(){
    if(outcome == "heart attack") b <- 11
    else if(outcome == "heart failure") b <- 17
    else b <- 23
  }
  out1 <- out[out$State == state,c(2, num1())]
  out1[,2] <- as.numeric(out1[,2])
  out1 <- out1[!(is.na(out1[,2])),]
  
  if(num == "best") t <- 1
  else if(num == "worst") t <- length(out1[,2])
  else t <- num
  if(t > length(out1[,2])) return(NA)
  
  out1 <- out1[order(out1[,2], out1[,1]),]
  rank <- seq_len(length(out1[,2]))
  out1$rank <- rank
  out1[out1$rank == t,1]
}