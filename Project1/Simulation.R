#Tyler Grimes
#Project #1
#STA 6106


#Congruential generator with a = 7^5, c = 0, and m = 2^31-1.
#
Random <-  setRefClass("Random", 
                       fields = list(seed = "numeric", xn = "numeric"),
                       methods = list(
                         unif = function(n = 1) {
                           vars <- numeric(n)
                           for(i in 1:n) {
                             xn <<- (16807 * xn) %% 21474836475
                             vars[i] <- xn/21474836475
                           }
                           vars
                         }))

#Simulation:
#
n <- 1000 #Sample size.
numseeds <- 20000 #Number of seeds to consider.
seeds <- seq(1, 21474836475, ceiling(21474836475/numseeds)) #Seeds to consider.
crit.value <- 1.6276/sqrt(n)
pass <- 0 #Counts number of tests passed.
for(seed in seeds) {
  rand <- Random$new(seed = seed, xn = seed) #Create a random number generator
                                             # with the appropriate seed.
  
  x <- rand$unif(n) #Draw a random sample of size n.
  Sn <- ecdf(x) #Create emperical cdf function.
  t <- max(Sn(x) - x) #This is supx(Sn(x) - F(x)); our test statistic.
  if(t < crit.value) { #Tests if t < 1.6276/sqrt(n).
    pass <- pass + 1
  }
  if(pass %% 1000 == 0) {
    print(paste(pass, "", numseeds))
  }
}

#Results:
#
printf(paste("Percentage of seeds that passed the test:", pass/numseeds))
