######## 
# Simple Linear Regression OLS Estimator Accuracy
##
# By: Jacob Hofer
########
library(tidyverse)

# Initialize simulation values
k <- 1000
x <- seq(from = -100, to = 100, length.out = 200)
n <- length(x)

#Set up dataframe


# Generating values

#set parameters
beta0 = 2
beta1 = 4
exp.median.y <- 2 + 4*median(x)
exp.min.y <- 2 + 4*min(x)
exp.max.y <- 2 + 4*max(x)

#for the 4 signal:1 noise case
#take sqrt so sigma^2 becomes sigma, as required by rnorm
sigma.median41 <- sqrt(abs(exp.median.y)/4)
sigma.min41 <- sqrt(abs(exp.min.y)/4)
sigma.max41 <- sqrt(abs(exp.max.y)/4)
y.median41 <- 2 + 4*x + rnorm(n, 0, sigma.median41)
y.min41 <- 2 + 4*x + rnorm(n, 0, sigma.min41)
y.max41 <- 2 + 4*x  + rnorm(n,0,sigma.max41)

#for the 1:1 case
sigma.median11 <- sqrt(abs(exp.median.y))
sigma.min11 <- sqrt(abs(exp.min.y))
sigma.max11 <- sqrt(abs(exp.max.y))
y.median11 <- 2 + 4*x + rnorm(n, 0, sigma.median11)
y.min11 <- 2 + 4*x + rnorm(n, 0, sigma.min11)
y.max11 <- 2 + 4*x  + rnorm(n,0,sigma.max11)


#for the 1 signal:4 noise case
sigma.median14 <- sqrt(abs(exp.median.y)*4)
sigma.min14 <- sqrt(abs(exp.min.y)*4)
sigma.max14 <- sqrt(abs(exp.max.y)*4)
y.median14 <- 2 + 4*x + rnorm(n, 0, sigma.median14)
y.min14 <- 2 + 4*x + rnorm(n, 0, sigma.min14)
y.max14 <- 2 + 4*x  + rnorm(n,0,sigma.max14)

fitmedian41 <- lm(y.median41 ~ x)
beta0hat <- coef(fitmedian41)[1]
beta1hat <- coef(fitmedian41)[2]
beta0errormedian41 <- (abs((beta0hat - beta0)/beta0)) * 100
beta1errormedian41 <- (abs((beta1hat - beta1)/beta1)) * 100

fitmin41 <- lm(y.min41 ~ x)
beta0hat <- coef(fitmin41)[1]
beta1hat <- coef(fitmin41)[2]
beta0errormin41 <- (abs((beta0hat - beta0)/beta0)) * 100
beta1errormin41 <- (abs((beta1hat - beta1)/beta1)) * 100

fitmax41 <- lm(y.max41 ~ x)
beta0hat <- coef(fitmax41)[1]
beta1hat <- coef(fitmax41)[2]
beta0errormax41 <- (abs((beta0hat - beta0)/beta0)) * 100
beta1errormax41 <- (abs((beta1hat - beta1)/beta1)) * 100

fitmedian11 <- lm(y.median11 ~ x)
beta0hat <- coef(fitmedian11)[1]
beta1hat <- coef(fitmedian11)[2]
beta0errormedian11 <- (abs((beta0hat - beta0)/beta0)) * 100
beta1errormedian11 <- (abs((beta1hat - beta1)/beta1)) * 100

fitmin11 <- lm(y.min11 ~ x)
beta0hat <- coef(fitmin11)[1]
beta1hat <- coef(fitmin11)[2]
beta0errormin11 <- (abs((beta0hat - beta0)/beta0)) * 100
beta1errormin11 <- (abs((beta1hat - beta1)/beta1)) * 100

fitmax11 <- lm(y.max11 ~ x)
beta0hat <- coef(fitmax11)[1]
beta1hat <- coef(fitmax11)[2]
beta0errormax11 <- (abs((beta0hat - beta0)/beta0)) * 100
beta1errormax11 <- (abs((beta1hat - beta1)/beta1)) * 100

fitmedian14 <- lm(y.median14 ~ x)
beta0hat <- coef(fitmedian14)[1]
beta1hat <- coef(fitmedian14)[2]
beta0errormedian14 <- (abs((beta0hat - beta0)/beta0)) * 100
beta1errormedian14 <- (abs((beta1hat - beta1)/beta1)) * 100

fitmin14 <- lm(y.min14 ~ x)
beta0hat <- coef(fitmin14)[1]
beta1hat <- coef(fitmin14)[2]
beta0errormin14 <- (abs((beta0hat - beta0)/beta0)) * 100
beta1errormin14 <- (abs((beta1hat - beta1)/beta1)) * 100

fitmax14 <- lm(y.max14 ~ x)
beta0hat <- coef(fitmax14)[1]
beta1hat <- coef(fitmax14)[2]
beta0errormax14 <- (abs((beta0hat - beta0)/beta0)) * 100
beta1errormax14 <- (abs((beta1hat - beta1)/beta1)) * 100


errors <- tibble(
  SNR = c(rep("4:1", 3), rep("1:1", 3), rep("1:4", 3)),
  section = rep(c("min(x)", "median(x)", "max(x)"), 3),
  beta0_error = c(beta0errormin41, beta0errormedian41, beta0errormax41, 
                  beta0errormin11, beta0errormedian11, beta0errormax11, 
                  beta0errormin14, beta0errormedian14, beta0errormax14),
  beta1_error = c(beta1errormin41, beta1errormedian41, beta1errormax41, 
                  beta1errormin11, beta1errormedian11, beta1errormax11, 
                  beta1errormin14, beta1errormedian14, beta1errormax14)
)

# Reshape the data into long format for ggplot
errors_long <- errors %>%
  pivot_longer(cols = c(beta0_error, beta1_error),
               names_to = "error_type", 
               values_to = "error_value")

ggplot(errors_long, aes(x = section, y = error_value, fill = error_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ SNR) +
  scale_fill_manual(values = c("beta0_error" = "blue", "beta1_error" = "red")) +
  labs(x = "E(Y_i|____)", y = "Error (%)", fill = "Error Type") +
  theme_minimal()

