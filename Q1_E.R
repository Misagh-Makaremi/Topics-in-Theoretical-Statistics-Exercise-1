library(ggplot2)

# parameters
params <- data.frame(
  alpha = c(2, 1, 0.5, 0.5),
  beta  = c(0.5, 1, 0.5, 1.5)
)

# y values
y_vals <- seq(0, 5, length.out = 500)

# Store all densities
plot_data <- data.frame()

# Densities for parameters
for (i in 1:nrow(params)) {
  alpha <- params$alpha[i]
  beta <- params$beta[i]
  lambda <- beta^2
  f_y <- dweibull(y_vals, shape = alpha, scale = lambda)
  
  temp <- data.frame(
    y = y_vals,
    density = f_y,
    label = paste0("α = ", alpha, ", β = ", beta)
  )
  
  plot_data <- rbind(plot_data, temp)
}

# Plot
ggplot(plot_data, aes(x = y, y = density, color = label)) +
  geom_line(size = 1.2) +
  labs(title = "Weibull Densities for Different α and β",
       x = "y", y = "Density", color = "Parameters") +
  theme_minimal() +
  theme(legend.position = "top")