set.seed(100)

beta <- 1.5
n <- 1000

# If U ~ Uniform(0,1), then Y = β * sqrt(-log(1 - U)) ~ Rayleigh(β)
u <- runif(n)
y <- beta * sqrt(-log(1 - u))

# For Rayleigh, E[Y] = β * sqrt(pi / 2)
# β_hat = mean(Y) / sqrt(pi / 2)
beta_hat <- mean(y) / sqrt(pi / 2)

beta_hat