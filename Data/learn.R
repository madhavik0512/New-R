n <- 5
raise <- function(n){
  n <- 4
  x <- 7
  pow(x)
}
pow <- function(x){
  x^n
} 

y <- 10
f <- function(x){
   y <- 2
   g <- function(x){
         x*y
  }   
   y*2 + g(x)
}

g <- 1:10
if(g>5){
  g <- 0
}

i <- 5
g <- if(i<3){
   NA
} else {
  10
}