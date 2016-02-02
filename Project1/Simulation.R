#Tyler Grimes
#Project #1
#STA 6106


#Congruential generator with a = 7^5, c = 0, and m = 2^31-1.
#
Random <-  setRefClass("Random", 
                       fields = list(seed = "numeric", xn = "numeric", a = "numeric",
                                     c = "numeric", m = "numeric"),
                       methods = list(
                         initialize = function(seed, a = 16807, c = 0, m = 21474836475) {
                           xn <<- seed
                           a <<- a
                           c <<- c
                           m <<- m
                         },
                         unif = function(n = 1) {
                           vars <- numeric(n)
                           for(i in 1:n) {
                             xn <<- (a * xn + c) %% m
                             vars[i] <- xn/m
                           }
                           vars
                         }))

#This function performs the Kolmogorov Smirnov test on the first n random
#numbers generated from our generator with each seed provided. It returns
#a list containing n, the number of seeds the passed and failed, and the
#actual seed #'s for the seeds that failed.
sim <- function(n, seeds, PRINT_UPDATES = TRUE) {
  if(PRINT_UPDATES) {
    print(paste("Begin Kolmogorov Smirnov test for" length(seeds), "seeds."))
    print(paste("Sampling first n =", n, "numbers from generator."))
    print(paste("Start time ", format(Sys.time(), "%X")))
  }
  #Simulation:
  #
  numseeds <- length(seeds) #Number of seeds to consider.
  crit.value <- 1.6276/sqrt(n)
  count <- 0 #Counts number of iterations
  update <- ceiling(numseeds/10) #Print updates in 10% increments.
  pass <- 0
  fail <- 0 #Counts number of tests failed.
  failed <- numeric(1 + 0.02*numseeds) #This will contain the failed seeds. 
  for(seed in seeds) {
    count <- count + 1
    rand <- Random$new(seed = seed) #Create a random number generator
    # with the appropriate seed.
    x <- rand$unif(n) #Draw a random sample of size n.
    Sn <- ecdf(x) #Create emperical cdf function.
    #This is supx(Sn(x) - F(x)); our test statistic:
    t <- max(c(max(Sn(x) - x), max(Sn(x-0.0001) - (x - 0.0001)) 
    if(t > crit.value) { #Tests if t > 1.6276/sqrt(n), in which case the sample failed.
      fail <- fail + 1
      failed[fail] <- seed
    } 
    
    if((count %% update) == 0 && PRINT_UPDATES) {
      print(paste("...", count, " of ", numseeds, " seeds tested. ", 
                  format(Sys.time(), "%X"), sep = ""))
    }
  }
  
  list(n = n, pass = numseeds - fail, fail = fail, failed = failed)
}


#This function will plot the empirical cdf of the first n numbers generated
#from our generator with the given seed.
graph <- function(seed, n) {
  points <- sort(Random$new(seed = seed)$unif(n))
  y <- (ecdf(points))(points)
  plot(x = points, y = y, col = "red", type = "s",
       ylab = "Cumulative relative frequency", xlab = "x",
       main = paste("Empirical cdf for \nseed", seed, "with n =", n))
  abline(a = 0, b = 1, col = "black")
}


#We perform the Kolmogorov Smirnov test for the first 100000 seeds, starting 
#with seed 1454301633. The first round uses sample size n = 10^4 The seeds that
#fail this round are tested again with n = 10^5. And those that fail the second
#round are tested a third time with n = 10^6. The results of each round are
#saved in the list "results".
results <- list()
results[[1]] <- sim(10^3, 1454301633:(1454301633 + 100000 - 1))
for(i in 2:3) {
  n = 10^(i+2)
  results[[i]] <- sim(n, (results[[i-1]]$failed)[1:results[[i-1]]$fail])
}


#Print out the results of each round. Give the sample size, number of failed seeds
#and print out some of those failed seeds.
for(r in results) {
  print(paste("Sample size n = ", r$n, " had ", r$fail, " failed seeds.", sep = ""))
  print("  Failed seeds (first 20):")
  print(r$failed[1:min(r$fail, 20)])
}


#Show empirical distribution of seeds that passed the K.S. test, and seeds that
#failed. Show these for tests with sample sizes n = 10^4 and n = 10^5.
par(mfrow=c(2,2))
graph(1454301633, 1000) #"Good" seed.
graph(1454301633, 100000) #"Good" seed.

graph(1454302563, 1000) #"Bad" seed.
graph(1454302563, 100000) #"Bad" seed.

