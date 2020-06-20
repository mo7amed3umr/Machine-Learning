source('data.R')

#Predictor variable
x <- as.matrix(training_set[, 1:16])
x <- cbind(rep(1, nrow(training_set)), x)

#response variable
y <- as.matrix(training_set$Risk1Yr)

#Sigmoid function
sigmoid <- function(z) {
  g <- 1 / (1+exp(-z))
  return(g)
}

#log likelihood fin
LL <- function(theta, x, y) {
  -sum(
  y*(x %*% theta - log(1+exp(x %*% theta)))
    + (1-y)*(-log(1 + exp(x %*% theta)))
  ) 
}

theta0 <- rep(0, ncol(x))

optim_sigmoid <- optim(theta0, LL,
                       x=x, y=y, method = 'BFGS')
theta <- optim_sigmoid$par


matrix_test_set = as.matrix(test_set[,-17])
matrix_test_set = cbind(rep(1, nrow(matrix_test_set)), matrix_test_set)

test = c()
for (i in 1:nrow(matrix_test_set)){
test[i] =1/(1+exp(-sum(theta*matrix_test_set[i,])))
}
pred <- ifelse(test > 0.5,1,0)
















