best <- function(state, outcome){
  out <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  if(!(state %in% out[,"State"])) stop("invalid state")
  if(!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) stop("invalid outcome")
  
  num <- function(){
    if(outcome == "heart attack") b <- 11
    else if(outcome == "heart failure") b <- 17
    else b <- 23
  }
  out1 <- out[out$State == state,c(2, num())]
  out1[,2] <- as.numeric(out1[,2])
  min_val <- min(out1[,2], na.rm = TRUE)
  good <- out1[out1[,2] == min_val, "Hospital.Name"]
  good1 <- sort(good)
  good1[1]
}