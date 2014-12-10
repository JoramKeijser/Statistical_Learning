theta.fit <- function(x, y) {
  lm(y ~ x)
}
theta.predict <- function(fit, x) {
  cbind(1,x)%*%fit$coef         
}

performCV <- function(Train) {   
  CVmodel <- crossval(Train[, -dim(Train)[2]], Train[, dim(Train)[2]], theta.fit, theta.predict, ngroup = 5 )
  # CV error
  prediction <- CVmodel$cv.fit
  actual <- Train[, dim(Train)[2]]
  partitionErrors <- rep(0, 5)
  for( k in 1:5) {
    partitionErrors[k] <- sum((prediction[CVmodel$groups[[k]]] - actual[CVmodel$groups[[k]]])^2)
  }
  CVerr <- mean(partitionErrors)
  return(CVerr)
}


# subset selection
install.packages("leaps")
library(leaps)
subModels <- regsubsets(Train$MEDV ~ ., data = Train, nvmax = 13, nbest = 10)
CVerrs <- rep(0, length(summary(subModels)$which))
for( k in 1:length(CVerrs)) {
  subset <- Train[, summary(subModel)$which[k,]]
  CVerrs[k] <- performCV(subset)
}


set.seed(123)
theta.fit <- function(x, y) {
  lm(y ~ x)
}
theta.predict <- function(fit, x) {
  cbind(1,x)%*%fit$coef         
}

# Method for performing the 5-fold cross-validation
performCV <- function(Data) {   
  CVmodel <- crossval(Data[, -dim(Data)[2]], Data[, dim(Data)[2]], 
                      theta.fit, theta.predict, ngroup = 5 )
  # CV error
  prediction <- CVmodel$cv.fit
  actual <- Data[, dim(Data)[2]]
  partitionErrors <- rep(0, 5)
  for( k in 1:5) {
    partitionErrors[k] <- sum((prediction[CVmodel$groups[[k]]] - actual[CVmodel$groups[[k]]])^2)
  }
  CVerr <- mean(partitionErrors)
  return(CVerr)
}

