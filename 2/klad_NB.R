## Naive Bayes
install.packages("e1071", repos = "http://cran.xl-mirror.nl/")
library("e1071")

###
set.seed(123)
trainIndex <- sample.int(dim(Data)[1], size = round(dim(Data)[1]/2))
Train <- Data[trainIndex, ]
Test <- Data[-trainIndex, ]
alpha = c(1, 0.1, 10)
lossNB <- data.frame(matrix(ncol = length(alpha), nrow = dim(Train)[1]))
colnames(lossNB) <- alpha
rownames(lossNB) <- 1:dim(Train)[1]
for(n in 1:dim(Train)[1]) { 
  for( k in 1:length(alpha)) {
    tempTrain <- Train[1:n, ]
    NB <- naiveBayes(party ~., data = tempTrain, alpha = alpha[k])
    prediction <- predict(NB, newdata = Test)
    lossNB[n, k] <- mean(Test$party != prediction)
  }
}

# subset selection
calcLoss <- function(xTrain, yTrain, x, y){ 
  NB <- naiveBayes(xTrain, yTrain, laplace = 1)
  prediction <- predict(NB, newdata = x)
  loss <- mean(prediction != y)
  return(loss)
}
xTrain <- Train[,"physician"]
yTrain <- Train[, "party"]
x <- Test[,"physician"]
y <- Test[, "party"]
calcLoss(xTrain, yTrain, x, y)

# OF
NB <- naiveBayes(party ~., data = Train, alpha = 1)
prediction <- predict(NB, newdata = Test)
mean(Test$party != prediction) #o.11
#
subset <- c("party", "physician", "dut.free", "education", "synfuels")
B <- naiveBayes(party ~., data = Train[,subset], alpha = 1)
prediction <- predict(NB, newdata = Test[, subset])
mean(Test$party != prediction) #o.06