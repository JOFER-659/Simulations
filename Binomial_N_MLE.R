#####
# Exploration of maximum likelihood of binomial parameter n
#####
# BY: Jacob Hofer
#####

#First lets look at the parameter p, probability of success

# Load necessary libraries
library(tidyverse)

#set seed if desired
#set.seed(1)

# Lets say we have some data from a binomial distribution
#observered data

#true value of p parametes
p <- 0.3
#true values of n parameter (size)
n <- 20
# number of observations
m <- 2000
#determines how many lik.n to calculate out to (also upper bound of x axis in plot)
tuner <- 60

#generate random binomial data based on specified parameters
x <- rbinom(n = m, size = n, prob = p)


#the likelihood function is then
lik.int <- numeric(length(x))
lik.n<-numeric()
for (i in max(x):tuner){
  for (j in 1:length(x)){
    # use log likelihood for numerical stability
    lik.int[j] <- log(choose(i, x[j]))+(x[j]*log(p))+((i-x[j])*log(1-p))
  }
  lik.n[i-(max(x)-1)] <- sum(lik.int)
}

#prep data for plotting
xaxis <- c(max(x):tuner)
yaxis <- lik.n
themax<-xaxis[which.max(lik.n)]
dat <- data.frame(xaxis = xaxis, yaxis = yaxis)

#plot the likelihood
ggplot(dat, aes(x = xaxis, y = yaxis)) +
  geom_point() + 
  xlim(max(x), tuner) + 
  geom_vline(aes(xintercept = themax, color = "Maximum Likelihood"), 
             linetype = "dashed") + 
  scale_color_manual(name = "Legend", values = c("Maximum Likelihood" = "red")) +
  labs(title = "Log-Likelihood for Binomial Parameter", x = "n", y = "Log-Likelihood") +
  theme_minimal()
