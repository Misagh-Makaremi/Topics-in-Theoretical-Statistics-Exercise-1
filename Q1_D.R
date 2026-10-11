library(MASS)

simulate_and_estimate <- function(alpha, beta, n) {
  # Weibull(scale = beta^2, shape = alpha)
  scale <- beta^2
  y <- rweibull(n, shape = alpha, scale = scale)
  
  # Fit Weibull using MLE
  fit <- fitdistr(y, "weibull")
  
  # Extract estimates
  alpha_hat <- fit$estimate["shape"]
  lambda_hat <- fit$estimate["scale"]
  beta_hat <- sqrt(lambda_hat)  # lambda = beta^2
  
  return(c(alpha_hat, beta_hat))
}

# Simulation Settings
sample_sizes <- c(100, 200, 500, 1000)
params <- list(
  c(alpha = 1.5, beta = 0.5),
  c(alpha = 0.5, beta = 1)
)

# Simulations and result table
results <- data.frame()

for (p in params) {
  for (n in sample_sizes) {
    est <- simulate_and_estimate(p["alpha"], p["beta"], n)
    results <- rbind(results, data.frame(
      True_Alpha = p["alpha"],
      True_Beta = p["beta"],
      n = n,
      Estimated_Alpha = round(est[1], 4),
      Estimated_Beta = round(est[2], 4)
    ))
  }
}

print(results)