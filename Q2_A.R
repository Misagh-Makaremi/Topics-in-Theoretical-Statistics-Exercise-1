library(ggplot2)

# Parameter combinations
params <- data.frame(
  a = c(1, 1, 2, 0.5, 2, 0.5),
  b = c(1, 2, 1, 2, 0.5, 0.5)
)

# values between 0 and 1
x_vals <- seq(0.001, 0.999, length.out = 500)

plot_data <- data.frame()

# Compute pdf
for (i in 1:nrow(params)) {
  a <- params$a[i]
  b <- params$b[i]
  
  f_x <- a * b * x_vals^(a - 1) * (1 - x_vals^a)^(b - 1)
  
  temp <- data.frame(
    x = x_vals,
    density = f_x,
    label = paste0("a = ", a, ", b = ", b)
  )
  
  plot_data <- rbind(plot_data, temp)
}

# Plot densities
ggplot(plot_data, aes(x = x, y = density, color = label)) +
  geom_line(size = 1.2) +
  labs(title = "Densities for Various (a, b) Values",
       x = "x", y = "Density", color = "Parameters") +
  theme_minimal() +
  theme(legend.position = "top")
