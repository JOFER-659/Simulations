######## 
# Simple Linear Regression OLS Estimator Accuracy
##
# By: Jacob Hofer
########

##Library Packages
library(tidyverse)

## k is the number of sims to run
k <- 1000
mat <- data.frame(beta0 = numeric(), beta1 = numeric(), sigma = numeric(), beta0hat = numeric(), beta1hat = numeric(), beta0error = numeric(), beta1error = numeric())
x <- seq(from = -100, to = 100, length.out = 200)
n <- length(x)
# Create for loop to run simulations
for (i in 1:k){
  
#Create true beta0
mat[i,1] <- runif(1, min = -10, max = 10)

#Create true beta1
mat[i,2] <- runif(1, min = -10, max = 10)

#Creaete true sigma
mat[i,3] <- runif(1, min = 1, max = 10)

#Generate y values
y <- mat[i,1] + mat[i,2]*x + rnorm(n = n, mean = 0, sd = mat[i,3])

#fit the model
fit <- summary(lm(y ~ x))

#store OLS estimates (first is beta0, second is beta1)
mat[i,4] <- fit$coefficients[1,1]
mat[i,5] <- fit$coefficients[2,1]

##Calculate the percent error (again, first is beta0 and second is beta1)
mat[i,6] <- abs(mat[i,4] - mat[i,1])
mat[i,7] <- abs(mat[i,5] - mat[i,2])
}

p1 <- ggplot(mat, aes(x = sigma, y = beta0error)) + geom_point()
p2 <- ggplot(mat, aes(x = sigma, y = beta1error)) + geom_point()
p3 <- ggplot(mat, aes(x = beta0error)) + geom_histogram()
p4 <- ggplot(mat, aes(x = beta1error)) + geom_histogram()
