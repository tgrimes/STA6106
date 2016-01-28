
simulation <- function(g, seed, n = 1000000000) {
  y <- runif(n, 0, 1)
  sum <- sum(g(y))
  sum/n
}

g <- function(x) {
  return (1 - x)*x / (1 - 2*x + 2*x^2)^2
}

#system.time(result <- simulation(g, seed))
#Results:
#    user  system elapsed 
#  47.043  61.185 376.509 

simulation2 <- function(g, seed, n = 10^9) {
  rep <- 1
  if(n/10^7 > 1) {
    rep <- n/10^7
    n <- 10^7
  }
  
  total <- 0
  for(i in 1:rep) {
    y <- runif(n, 0, 1)
    total <- total + sum(g(y))
  }
  
  total/(n*rep)
}

results <- simulation2(g, seed = 10)