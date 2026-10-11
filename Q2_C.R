library(stats4)

data <- scan("leaves data set.txt")

# summary and plot histogram
summary(data)
hist(data, breaks = 30, main = "Histogram of Leaves Data", col = "lightgreen", xlab = "x")

# Log-likelihood Function
loglik_fun <- function(params, x) {
  a <- params[1]
  b <- params[2]
  
  # invalid parameters
  if (a <= 0 || b <= 0) return(Inf)
  
  log_vals <- log(a) + log(b) + (a - 1) * log(x) + (b - 1) * log(1 - x^a)
  
  return(-sum(log_vals))  # minimization
}

# MLE Estimation
start_vals <- c(1, 1)

# Optimization
opt <- optim(start_vals, loglik_fun, x = data, method = "L-BFGS-B", lower = c(0.01, 0.01))

# Results
if (opt$convergence == 0) {
  est_a <- opt$par[1]
  est_b <- opt$par[2]
  
  cat("Estimated parameters:\n")
  cat("a =", round(est_a, 4), "\n")
  cat("b =", round(est_b, 4), "\n")
} else {
  cat("Optimization did not converge. Try different starting values.\n")
}