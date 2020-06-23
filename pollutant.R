pollutantmean <- function(directory = "specdata", pollutant, id = 1:332){
  old <- getwd()
  setwd(directory)
  means <- 0
  rowcount <- 0
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
      means <- means + sum(play[[pollutant]], na.rm = TRUE)
      not <- is.na(play[[pollutant]])
      count <- length(play[[pollutant]][!not])
      rowcount <- rowcount + count
  }
  win <- means/rowcount
  setwd(old)
  win
}

