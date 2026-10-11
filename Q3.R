# FGM copula density
copula_density <- function(u, v, alpha) {
  1 + alpha * (1 - 2 * u) * (1 - 2 * v)
}

# Marginal CDF and PDF
F_q2 <- function(x, a, b) {
  1 - (1 - x^a)^b
}

f_q2 <- function(x, a, b) {
  a * b * x^(a - 1) * (1 - x^a)^(b - 1)
}

# Joint PDF
joint_density <- function(x, y, a, b, alpha) {
  u <- F_q2(x, a, b)
  v <- F_q2(y, a, b)
  fx <- f_q2(x, a, b)
  fy <- f_q2(y, a, b)
  
  copula_density(u, v, alpha) * fx * fy
}

# plot settings
a <- 1.5
b <- 1.5
alpha <- 0.8

x_vals <- seq(0.001, 0.999, length.out = 100)
y_vals <- seq(0.001, 0.999, length.out = 100)

z <- outer(x_vals, y_vals, Vectorize(function(x, y) joint_density(x, y, a, b, alpha)))

# Plot
persp(x_vals, y_vals, z, theta = 30, phi = 25, col = "lightblue",
      xlab = "x", ylab = "y", zlab = "Joint Density",
      main = expression(paste("Joint PDF with FGM Copula (", alpha, " = 0.8)")))