# Creates a simple random forest classifier with accuracy of 93.47%

library(randomForest)
library(readr)

set.seed(123)

numTrain <- 10000
numTrees <- 25

train <- read.csv("../DIGIT/train.csv")
test <- read.csv("../DIGIT/test.csv")

rows <- sample(1:nrow(train), numTrain)
labels <- as.factor(train[rows,1])
train <- train[rows,-1]

rf <- randomForest(train, labels, xtest=test, ntree=numTrees)
predictions <- data.frame(ImageId=1:nrow(test), Label=levels(labels)[rf$test$predicted])


write.csv(predictions, "rf_benchmark.csv") 
