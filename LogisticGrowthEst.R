#####
# Simulating and estimating parameters of logistic growth model
####
# BY: Jacob Hofer
#####

library(tidyverse)

#First lets simulate data (keep time small because of potential for overflow)
N0 <- 5
r <- 1.02
K <- 2000
time <- 10
std <-15
t <- seq(from = 0, to = time, length.out = time*10)
y <- (N0 * exp(r*t))/(1-(N0/K)+((N0/K)*exp(r*t)))+rnorm(length(t), mean = 0, sd = std)
dat <- data.frame(t,y)


# Now we estimate our parameters using nonlinear regression (put start estimates close to originals)

est <- nls(y ~ (N0 * exp(r*t))/(1-(N0/K)+((N0/K)*exp(r*t))), data = dat, start = list(N0 = 15, r = 2, K = 150))
summary(est)
confint(est)


# Create plot of our model and data
predictions <- predict(est, newdata = t)
preddf<- data.frame(pred = predictions, time = t)
ggplot(dat, aes(x = t, y = y)) + geom_point() + geom_line(aes(x = time, y = predictions), data =preddf, col = 'red')
