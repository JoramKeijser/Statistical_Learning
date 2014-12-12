performCV <- function(Data, K){
  # Divide data in K folds
  folds <- sample.int(K, size = dim(Data)[[1]], replace = TRUE)
  # Vector will contain mean error of every fold
  errors <- rep(0, 5)
  for(k in 1:K) {
    # test on fold k, train on other folds:
    test <- Data[folds == k, ]
    train <- Data[folds != k, ]   
    NB <- naiveBayes(party ~., data = train)
    pred <- predict(NB, newdata = test)
    errors[k] <- mean(pred != test[, "party"])
  }
  return(mean(errors)) #averaged over all folds
}
result <- performCV(Train, 5)

## Subset selection
subModels <- regsubsets(x = Train[, -1], y = Train[, 1])
errors <- rep(0, dim(summary(subModels)$which)[1])
for( k in 1:length(errors)) {
  select <- as.logical(summary(subModels)$which[k,])
  select[1] <- TRUE
  subset <- Train[, select]
  errors[k] <- performCV(subset, 5)
}
plot(errors/65, xlab = 'Number of features', ylab = 'mean error')
summary(subModels)$which[which(errors == min(errors)), ]