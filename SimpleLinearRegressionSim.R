######## 
# Simple Linear Regression OLS Estimator Accuracy
##
# By: Jacob Hofer
########

# Initialize simulation values
k <- 1000
x <- seq(from = -100, to = 100, length.out = 200)
n <- length(x)

#Set up dataframe
sim_results <- data.frame(
  beta0 = numeric(k), beta1 = numeric(k), sigma = numeric(k), 
  beta0hat = numeric(k), beta1hat = numeric(k), 
  beta0_error = numeric(k), beta1_error = numeric(k),
  snr_min = numeric(k), snr_median = numeric(k), snr_max = numeric(k)
)

# Simulation loop
for (i in 1:k) {
  
  # Generate true parameters
  beta0 <- runif(1, min = -10, max = 10)
  beta1 <- runif(1, min = -10, max = 10)
  sigma <- runif(1, min = 1, max = 10)
  
  # Generate response values
  y <- beta0 + beta1 * x + rnorm(n, mean = 0, sd = sigma)
  
  # Fitting slr model
  fit <- lm(y ~ x)
  beta0hat <- coef(fit)[1]
  beta1hat <- coef(fit)[2]
  
  # percent error
  beta0_error <- abs((beta0hat - beta0) / beta0) * 100
  beta1_error <- abs((beta1hat - beta1) / beta1) * 100
  
  # snr at min median and max
  snr_min <- (beta0 + beta1 * min(x)) / sigma^2
  snr_median <- (beta0 + beta1 * median(x)) / sigma^2
  snr_max <- (beta0 + beta1 * max(x)) / sigma^2
  
  sim_results[i, ] <- c(beta0, beta1, sigma, beta0hat, beta1hat, beta0_error, beta1_error, snr_min, snr_median, snr_max)
}

p1 <- ggplot(sim_results, aes(x = snr_min, y = beta0_error)) +
  geom_point(alpha = 0.5) +
  labs(title = "Beta0 Error vs SNR (Min)", x = "SNR (Min)", y = "Beta0 Error")

p2 <- ggplot(sim_results, aes(x = snr_min, y = beta1_error)) +
  geom_point(alpha = 0.5) +
  labs(title = "Beta1 Error vs SNR (Min)", x = "SNR (Min)", y = "Beta1 Error")


p3 <- ggplot(sim_results, aes(x = snr_median, y = beta0_error)) +
  geom_point(alpha = 0.5) +
  labs(title = "Beta0 Error vs SNR (Median)", x = "SNR (Median)", y = "Beta0 Error")

p4 <- ggplot(sim_results, aes(x = snr_median, y = beta1_error)) +
  geom_point(alpha = 0.5) +
  labs(title = "Beta1 Error vs SNR (Median)", x = "SNR (Median)", y = "Beta1 Error")


p5 <- ggplot(sim_results, aes(x = snr_max, y = beta0_error)) +
  geom_point(alpha = 0.5) +
  labs(title = "Beta0 Error vs SNR (Max)", x = "SNR (Max)", y = "Beta0 Error")

p6 <- ggplot(sim_results, aes(x = snr_max, y = beta1_error)) +
  geom_point(alpha = 0.5) +
  labs(title = "Beta1 Error vs SNR (Max)", x = "SNR (Max)", y = "Beta1 Error")


p1; p2; p3; p4; p5; p6