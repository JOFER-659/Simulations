######## 
# Simple Linear Regression OLS Estimator Accuracy
##
# By: Jacob Hofer
########
library(tidyverse)

# Initialize simulation values
k <- 1000
x <- seq(from = 1, to = 10, length.out = 200)
n <- length(x)

# store error values for each iteration
#first beta0
beta0errormin41 <- numeric(k)
beta0errormax41 <- numeric(k)
beta0errormedian41 <- numeric(k)
beta0errormin11 <- numeric(k)
beta0errormax11 <- numeric(k)
beta0errormedian11 <- numeric(k)
beta0errormin14 <- numeric(k)
beta0errormax14 <- numeric(k)
beta0errormedian14 <- numeric(k)

#now beta1
beta1errormin41 <- numeric(k)
beta1errormax41 <- numeric(k)
beta1errormedian41 <- numeric(k)
beta1errormin11 <- numeric(k)
beta1errormax11 <- numeric(k)
beta1errormedian11 <- numeric(k)
beta1errormin14 <- numeric(k)
beta1errormax14 <- numeric(k)
beta1errormedian14 <- numeric(k)


# Generating values

for (i in 1:k){
#set parameters (0 not be included here, if zero is included you can get inflated percent errors)
beta0 = runif(1, 2, 10)
beta1 = runif(1, 2, 10)
exp.median.y <- beta0 + beta1*median(x)
exp.min.y <- beta0 + beta1*min(x)
exp.max.y <- beta0 + beta1*max(x)

#for the 4 signal:1 noise case
#take sqrt so sigma^2 becomes sigma, as required by rnorm
sigma.median41 <- sqrt(abs(exp.median.y)/4)
sigma.min41 <- sqrt(abs(exp.min.y)/4)
sigma.max41 <- sqrt(abs(exp.max.y)/4)
y.median41 <- beta0 + beta1*x + rnorm(n, 0, sigma.median41)
y.min41 <- beta0 + beta1*x + rnorm(n, 0, sigma.min41)
y.max41 <- beta0 + beta1*x  + rnorm(n,0,sigma.max41)

#for the 1:1 case
sigma.median11 <- sqrt(abs(exp.median.y))
sigma.min11 <- sqrt(abs(exp.min.y))
sigma.max11 <- sqrt(abs(exp.max.y))
y.median11 <- beta0 + beta1*x + rnorm(n, 0, sigma.median11)
y.min11 <- beta0 + beta1*x + rnorm(n, 0, sigma.min11)
y.max11 <- beta0 + beta1*x  + rnorm(n,0,sigma.max11)


#for the 1 signal:4 noise case
sigma.median14 <- sqrt(abs(exp.median.y)*4)
sigma.min14 <- sqrt(abs(exp.min.y)*4)
sigma.max14 <- sqrt(abs(exp.max.y)*4)
y.median14 <- beta0 + beta1*x + rnorm(n, 0, sigma.median14)
y.min14 <- beta0 + beta1*x + rnorm(n, 0, sigma.min14)
y.max14 <- beta0 + beta1*x  + rnorm(n,0,sigma.max14)

#Fit models and calculate percent errors
#first, median 4:1
fitmedian41 <- lm(y.median41 ~ x)
beta0hat <- coef(fitmedian41)[1]
beta1hat <- coef(fitmedian41)[2]
beta0errormedian41[i] <- (abs((beta0hat - beta0)/beta0)) * 100
beta1errormedian41[i] <- (abs((beta1hat - beta1)/beta1)) * 100

#min 4:1
fitmin41 <- lm(y.min41 ~ x)
beta0hat <- coef(fitmin41)[1]
beta1hat <- coef(fitmin41)[2]
beta0errormin41[i] <- (abs((beta0hat - beta0)/beta0)) * 100
beta1errormin41[i] <- (abs((beta1hat - beta1)/beta1)) * 100

#max 4:1
fitmax41 <- lm(y.max41 ~ x)
beta0hat <- coef(fitmax41)[1]
beta1hat <- coef(fitmax41)[2]
beta0errormax41[i] <- (abs((beta0hat - beta0)/beta0)) * 100
beta1errormax41[i] <- (abs((beta1hat - beta1)/beta1)) * 100

#median 1:1
fitmedian11 <- lm(y.median11 ~ x)
beta0hat <- coef(fitmedian11)[1]
beta1hat <- coef(fitmedian11)[2]
beta0errormedian11[i] <- (abs((beta0hat - beta0)/beta0)) * 100
beta1errormedian11[i] <- (abs((beta1hat - beta1)/beta1)) * 100

#min 1:1
fitmin11 <- lm(y.min11 ~ x)
beta0hat <- coef(fitmin11)[1]
beta1hat <- coef(fitmin11)[2]
beta0errormin11[i] <- (abs((beta0hat - beta0)/beta0)) * 100
beta1errormin11[i] <- (abs((beta1hat - beta1)/beta1)) * 100

#max 1:1
fitmax11 <- lm(y.max11 ~ x)
beta0hat <- coef(fitmax11)[1]
beta1hat <- coef(fitmax11)[2]
beta0errormax11[i] <- (abs((beta0hat - beta0)/beta0)) * 100
beta1errormax11[i] <- (abs((beta1hat - beta1)/beta1)) * 100

#median 1:4
fitmedian14 <- lm(y.median14 ~ x)
beta0hat <- coef(fitmedian14)[1]
beta1hat <- coef(fitmedian14)[2]
beta0errormedian14[i] <- (abs((beta0hat - beta0)/beta0)) * 100
beta1errormedian14[i] <- (abs((beta1hat - beta1)/beta1)) * 100

#min 1:4
fitmin14 <- lm(y.min14 ~ x)
beta0hat <- coef(fitmin14)[1]
beta1hat <- coef(fitmin14)[2]
beta0errormin14[i] <- (abs((beta0hat - beta0)/beta0)) * 100
beta1errormin14[i] <- (abs((beta1hat - beta1)/beta1)) * 100

#max 1:4
fitmax14 <- lm(y.max14 ~ x)
beta0hat <- coef(fitmax14)[1]
beta1hat <- coef(fitmax14)[2]
beta0errormax14[i] <- (abs((beta0hat - beta0)/beta0)) * 100
beta1errormax14[i] <- (abs((beta1hat - beta1)/beta1)) * 100
}

#calculate averages
#beta0
avgbeta0errormin41 <- mean(beta0errormin41)
avgbeta0errormax41 <- mean(beta0errormax41)
avgbeta0errormedian41 <- mean(beta0errormedian41)
avgbeta0errormin11 <- mean(beta0errormin11)
avgbeta0errormax11 <- mean(beta0errormax11)
avgbeta0errormedian11 <- mean(beta0errormedian11)
avgbeta0errormin14 <- mean(beta0errormin14)
avgbeta0errormax14 <- mean(beta0errormax14)
avgbeta0errormedian14 <- mean(beta0errormedian14)

#now beta1
avgbeta1errormin41 <- mean(beta1errormin41)
avgbeta1errormax41 <- mean(beta1errormax41)
avgbeta1errormedian41 <- mean(beta1errormedian41)
avgbeta1errormin11 <- mean(beta1errormin11)
avgbeta1errormax11 <- mean(beta1errormax11)
avgbeta1errormedian11 <- mean(beta1errormedian11)
avgbeta1errormin14 <- mean(beta1errormin14)
avgbeta1errormax14 <- mean(beta1errormax14)
avgbeta1errormedian14 <- mean(beta1errormedian14)

errors <- tibble(
  SNR = c(rep("SNR 4:1", 3), rep("SNR 1:1", 3), rep("SNR 1:4", 3)),
  section = rep(c("E(y_i|min(x))", "E(y_i|median(x))", "E(y_i|max(x))"), 3),
  beta0_error = c(avgbeta0errormin41, avgbeta0errormedian41, avgbeta0errormax41, 
                  avgbeta0errormin11, avgbeta0errormedian11, avgbeta0errormax11, 
                  avgbeta0errormin14, avgbeta0errormedian14, avgbeta0errormax14),
  beta1_error = c(avgbeta1errormin41, avgbeta1errormedian41, avgbeta1errormax41, 
                  avgbeta1errormin11, avgbeta1errormedian11, avgbeta1errormax11, 
                  avgbeta1errormin14, avgbeta1errormedian14, avgbeta1errormax14)
)

# Reshape the data into long format
errors_long <- errors %>%
  pivot_longer(cols = c(beta0_error, beta1_error),
               names_to = "error_type", 
               values_to = "error_value")

ggplot(errors_long, aes(x = section, y = error_value, fill = error_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ SNR) +
  scale_fill_manual(values = c("beta0_error" = "blue", "beta1_error" = "red")) +
  labs(x = "Sigma Calculated with respect to...", y = "Error (%)", fill = "Error Type") +
  theme_minimal()

