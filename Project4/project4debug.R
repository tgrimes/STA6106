library("ggplot2")
library("gridExtra")

set.seed(1259789)

#Simulation parameters.
k <- 2      #Number of mixtures to apply in EM algorithm
n <- 50     #Sample size
MAX_ITER <- 100     #Maximum number of iterations for EM.
EPSILON <- 10^(-6)

#Graph parameters.
binwidth <- c(1/10, 1/2, 1/5)

#Define parameters for mixture model.
mu <- c(0, 0)
sigma <- c(1, 3)
p <- c(0.4, 0.6)

#rmix returns n observations from the mixture.
rmix <- function(n = 1) {
  data <- data.frame(x = vector("numeric", n),
                     dist = vector("numeric", n))
  for(i in 1:n) {
    u <- runif(1)
    for(j in 1:length(p)) {
      if(u <= p[j]) {
        data$x[i] <- rnorm(1, mu[j], sigma[j])
        data$dist[i] <- j
        break
      } else {
        u <- u - p[j]
      }
    }
  }
  return(data)
}

mean_mix <- function() {
  sum(p*mu)
}

var_mix <- function() {
  sum(p*(mu^2 + sigma^2)) - mean_mix()^2
}

EM_mix <- function(x, k = 2, mu_hat_0 = NULL, sigma_hat_0 = NULL, p_hat_0 = NULL) {
  #Store estimates from each iteration.
  mu_hat <- matrix(0, nrow = MAX_ITER + 1, ncol = k)
  sigma_hat <- matrix(0, nrow = MAX_ITER + 1, ncol = k)
  p_hat <- matrix(0, nrow = MAX_ITER + 1, ncol = k)
  log_likelihood <- matrix(0, nrow = MAX_ITER + 1, ncol = 1)
  
  iter <- 1
  #Use first "iteration"" to initialize paramters.
  if(is.null(mu_hat_0)) {
    mu_hat[iter, ] <- quantile(x, seq(0, 1, by = 1/(k - 1)))
  } else {
    mu_hat[iter, ] <- mu_hat_0
  }
  if(is.null(sigma_hat_0)) {
    sigma_hat[iter, ] <- rep(1, k)
  } else {
    sigma_hat[iter, ] <- sigma_hat_0
  }
  if(is.null(p_hat_0)) {
    p_hat[iter, ] <- rep(1/k, k)
  } else {
    p_hat[iter, ] <- p_hat_0
  }
  log_likelihood[iter, ] <- -Inf
  z <- NULL
  
  iter <- 2
  #In each iteration, do the following.
  EM_mix_iterate <- function(data) {
    #E-step
    z <<- t(sapply(1:n, function(i) {
      den <- sum(sapply(1:k, function(j) { 
        p_hat[iter - 1, j]*dnorm(x[i], mu_hat[iter - 1, j], sigma_hat[iter - 1, j])
      }))
      sapply(1:k, function(j) { 
        p_hat[iter - 1, j] * dnorm(x[i], mu_hat[iter - 1, j], sigma_hat[iter - 1, j]) 
      }) / den
    }))
    
    #M-step
    N <- apply(z, 2, sum)
    mu_hat[iter, ] <<- (t(z) %*% x)/N
    sigma_hat[iter, ] <<- sqrt(sapply(1:k, function(j) { 
      z[, j] %*% (x - mu_hat[iter, j])^2 / N[j] 
    }))
    p_hat[iter, ] <<- N/n
    
    #Check for convergence.
    log_likelihood[iter, ] <<- sum(sapply(1:n, function(i) { 
      log(sum(sapply(1:k, function(j) { 
        p_hat[iter, j]*dnorm(x[i], mu_hat[iter, j], sigma_hat[iter, j] ) 
      })))
    }))
    
    iter <<- iter + 1
  }
  
  plots <- list(ggplot(data.frame(x = x), aes(x)) + 
                  geom_histogram(binwidth = binwidth[2], alpha = 0.4, position = "identity"))
  
  #Start the iterative process.
  while(iter <= MAX_ITER) {
    EM_mix_iterate()
    plots <- c(plots, list(ggplot(data.frame(x = x, z = apply(z, 1, function(x) which(x == max(x)))), 
                                  aes(x, fill = factor(z))) + 
                             geom_histogram(binwidth = binwidth[2], alpha = 0.4, position = "identity")))
    if(abs(log_likelihood[iter - 1] - log_likelihood[iter - 2]) < EPSILON) {
      break
    }
  }
  
  return(list(mu_hat = mu_hat[1:(iter - 1), ], sigma_hat = sigma_hat[1:(iter - 1), ], 
              p_hat = p_hat[1:(iter - 1), ], log_lik = log_likelihood[1:(iter - 1)], 
              x = x, z = z, iterations = iter - 1, plots = plots))
}

#Simulation parameters.
k <- 2      #Number of mixtures to apply in EM algorithm
n <- 50     #Sample size
MAX_ITER <- 1000     #Maximum number of iterations for EM.
EPSILON <- 10^(-10)

#Graph parameters.
binwidth <- c(1/10, 1/2, 1/5)

#Define parameters for mixture model.
mu <- c(0, 3)
sigma <- c(4, 4)
p <- c(0.4, 0.6)

#Simulation to obtain distribution of EM estimates.
m <- 200     #Number of simulations
estimates <- list(p_hat = matrix(0, nrow = m, ncol = k), 
                  mu_hat = matrix(0, nrow = m, ncol = k),
                  sigma_hat = matrix(0, nrow = m, ncol = k))
perc_correct <- vector("numeric", m)

#Perform the simulation.
for(i in 1:m) {
  obs <- rmix(n)
  results <- EM_mix(obs$x, k)
  classification <- apply(results$z, 1, function(x) which(x == max(x)))
  estimates$p_hat[i, ] <- results$p_hat[results$iterations, ]
  estimates$mu_hat[i, ] <- results$mu_hat[results$iterations, ]
  estimates$sigma_hat[i, ] <- results$sigma_hat[results$iterations, ]
  perc_correct[i] <- mean(obs$dist == classification)
}


#Plot the distributions of the estimators.
plots <- list()
plots[[1]] <- ggplot(data.frame(p_hat = c(estimates$p_hat[, 1:k]), distribution = factor(rep(1:k, each = m))),
                     aes(p_hat, fill = distribution)) +
  geom_histogram(binwidth = binwidth[1], alpha = 0.4, position = "identity") +
  geom_vline(data = data.frame(x = p, distribution = factor(1:k)), aes(xintercept = x, color = distribution))
plots[[2]] <- ggplot(data.frame(mu_hat = c(estimates$mu_hat[, 1:k]), distribution = factor(rep(1:k, each = m))),
                     aes(mu_hat, fill = distribution)) +
  geom_histogram(binwidth = binwidth[2], alpha = 0.4, position = "identity") +
  geom_vline(data = data.frame(x = mu, distribution = factor(1:k)), aes(xintercept = x, color = distribution))
plots[[3]] <- ggplot(data.frame(sigma_hat = c(estimates$sigma_hat[, 1:k]), distribution = factor(rep(1:k, each = m))),
                     aes(sigma_hat, fill = distribution)) +
  geom_histogram(binwidth = binwidth[3], alpha = 0.4, position = "identity") +
  geom_vline(data = data.frame(x = sigma, distribution = factor(1:k)), aes(xintercept = x, color = distribution))
plots[[4]] <- ggplot(data.frame(perc_correct = perc_correct), aes(perc_correct)) +
  geom_histogram(binwidth = 1/20, alpha = 0.4)

layout <- rbind(c(1, 2), c(3, 4), rep(5, 2))
g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
lheight <- sum(legend$height)
gl <- lapply(plots, function(x) x + theme(legend.position="none"))

grid.arrange(grobs = c(gl, list(legend)), layout_matrix = layout,
             heights = grid::unit.c(rep((unit(1, "npc") - lheight)*(1/(nrow(layout)-1)),nrow(layout)-1), lheight))


#Create confidence intervals
CI_p1 = c(max(0, mean(estimates$p_hat[, 1]) - 1.96*sd((estimates$p_hat[, 1]))), 
          min(1, mean(estimates$p_hat[, 1]) + 1.96*sd((estimates$p_hat[, 1]))))
CI_p2 = c(max(0, mean(estimates$p_hat[, 2]) - 1.96*sd((estimates$p_hat[, 2]))), 
          min(1, mean(estimates$p_hat[, 2]) + 1.96*sd((estimates$p_hat[, 2]))))
CI_cp = c(max(0, mean(perc_correct) - 1.96*sd((perc_correct))), 
          min(1, mean(perc_correct) + min(1, 1.96*sd((perc_correct)))))
