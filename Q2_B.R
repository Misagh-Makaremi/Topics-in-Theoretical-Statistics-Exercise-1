library(stats4)

# generate samples
inverse_cdf <- function(u, a, b) {
  (1 - (1 - u)^(1 / b))^(1 / a)
}

# Log-likelihood function
loglik_fun <- function(params, x) {
  a <- params[1]
  b <- params[2]
  
  if (a <= 0 || b <= 0) return(Inf)
  
  log_vals <- log(a) + log(b) + (a - 1) * log(x) + (b - 1) * log(1 - x^a)
  
  return(-sum(log_vals))  # minimization
}

# Simulation
simulate_estimates <- function(a_true, b_true, n, reps = 10000) {
  ests <- matrix(NA, nrow = reps, ncol = 2)
  
  for (i in 1:reps) {
    u <- runif(n)
    x <- inverse_cdf(u, a_true, b_true)
    
    opt <- tryCatch(
      optim(c(1, 1), loglik_fun, x = x, method = "L-BFGS-B", lower = c(0.01, 0.01)),
      error = function(e) NULL
    )
    
    if (!is.null(opt) && opt$convergence == 0) {
      ests[i, ] <- opt$par
    }
  }
  
  colnames(ests) <- c("a_hat", "b_hat")
  return(colMeans(na.omit(ests)))
}

# Run simulations
set.seed(123)
results <- data.frame()

param_sets <- list(
  list(a = 2, b = 1),
  list(a = 1.5, b = 1.5)
)

sample_sizes <- c(1000, 5000)

for (p in param_sets) {
  for (n in sample_sizes) {
    est <- simulate_estimates(p$a, p$b, n)
    results <- rbind(results, data.frame(
      True_a = p$a,
      True_b = p$b,
      n = n,
      Estimated_a = round(est[1], 4),
      Estimated_b = round(est[2], 4)
    ))
  }
}

print(results)