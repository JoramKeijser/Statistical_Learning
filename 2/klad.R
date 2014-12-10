## Download data
attributes <- c("class", "handicap", "water", "adoption", 
                "physician", "el-salvador", "religious", 
                "sattelite", "nicaraguan", "missile", "immigration", 
                "synfuels", "education", "superfund", "crime", "duty-free", 
                "export")
Data <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/voting-records/house-votes-84.data", 
                   sep = ',', col.names = attributes, na.strings = "?")

# number of missing
sum(is.na(Data))
## per topic
plot(apply(is.na(Data), 2, sum), type = 'h')
text(apply(is.na(Data), 2, sum), attributes, pos = c(rep(3, 16), 2), col = "red")
# per congress man
plot(apply(is.na(Data), 1, sum), type = 'h')
# fix missing values:
install.packages("randomForest")
library("randomForest")
## replace missing with most frequent value in column
Data <- na.roughfix(Data)
Data

## Exploratory.
# division republican/democrat
REP <- Data$class == "republican"
DEM <- Data$class == "democrat"
sum(REP)
sum(DEM)
mean(Data == "y")
mean(Data[REP, ] == "y")
mean(Data[DEM, ] == "y")
meanVotes <- data.frame(matrix(ncol = dim(Data)[2], nrow = 2))
names(meanVotes) <- attributes
meanVotes[1,1] <- "Republican"
meanVotes[2,1] <- "Democrat"
for( k in 2:dim(Data)[2] ) {
  meanVotes[1, k] <- mean(Data[REP, k] == "y")
  meanVotes[2, k] <- mean(Data[DEM, k] == "y")
}

## plot mean number of yes's per party
par(las=2) # make label text perpendicular to axis
par(mar=c(5,8,4,2)) # increase y-axis margin.
barplot(as.matrix(meanVotes[,-1]), beside = TRUE, col = c("red", "blue"), horiz=T)
View(sort(apply(meanVotes, 2, function(x){ max(x)/min(x)}), decreasing = TRUE))


# Split data
trainIndex <- sample.int(dim(Data)[1], size = round(dim(Data)[1]/2))
Train <- Data[trainIndex, ]
Test <- Data[-trainIndex, ]
# Geen overlap:
anyDuplicated(rbind(Train$row.names, Test$row.names), MARGIN = 1)
