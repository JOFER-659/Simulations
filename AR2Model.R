#### Libraries

library(tidyverse)

#### Giving True Parameters

beta0 <- 5
beta1 <- 1.7
beta2 <- -0.9
t<- 1:10000
y <- vector(mode = 'double', length = length(t))

# parameters for random error
sd <- 3
mean <- 0

#### Generating Data

y[1] <- beta0 + rnorm(1, mean, sd)
y[2] <- beta0 + y[1]*beta1 + rnorm(1, mean, sd)
for (i in 3:length(t)){
  y[i] <- beta0 + y[i-1]*beta1 + y[i-2]*beta2 + rnorm(1, mean, sd)
}

dat <- tibble(y = y, t = t)
ggplot(dat, aes(x = t, y = y)) + geom_line()

tsdat <- ts(data = y, start = 1)
acf(tsdat)
pacf(tsdat)

# Estimate the beta parameters (note beta0 is not estimated because ar
# removes the mean of the series)
ar(tsdat, order.max = 2, method = "ols")



         