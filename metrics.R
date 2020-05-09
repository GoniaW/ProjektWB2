mse <- function(x,y){
  mean((as.integer(x)-as.integer(y))^2)
}

acc1 <- function(x,y){
  mean(as.numeric(x) == as.numeric(y))
}

acc2 <- function(x,y){
  mean(abs((as.numeric(x)-as.numeric(y)) <= 1))
}

