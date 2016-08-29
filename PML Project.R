######################################
## PML - FINAL PROJECT
## 
######################################

## What you should submit

## The goal of your project is to predict the manner in which they did the exercise. 
## This is the "classe" variable in the training set. You may use any of the other variables to predict with. 
## You should create a report describing how you built your model, how you used cross validation, 
## what you think the expected out of sample error is, and why you made the choices you did. 
## You will also use your prediction model to predict 20 different test cases.

trainset <- read.csv('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv', na.strings=c("","NA", "#DIV/0!"), header=T)
testset <- read.csv('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv', na.strings=c("","NA", "#DIV/0!"), header=T)

# see error percentage 
NAPercent <- round(colMeans(is.na(trainset)), 2)
table(NAPercent)

# find index of the complete columns minus the first 
index <- which(NAPercent==0)[-1]
# subset the data
train <- trainset[, index]
test <- testset[, index]
# looking at the structure of the data for the first 10 columns
str(train[, 1:10])

# subset the data
train <- train[, -(1:6)]
test <- test[, -(1:6)]
# convert all numerical data to numeric class
for(i in 1:(length(train)-1)){
  train[,i] <- as.numeric(train[,i])
  test[,i] <- as.numeric(test[,i])
}


# split train data set
inTrain <- createDataPartition(y=trainset$classe,p=0.8, list=FALSE)
trainData <- train[inTrain,]
validation <- train[-inTrain,]
# print out the dimentions of the 3 data sets
rbind(trainData = dim(trainData), validation = dim(validation), test = dim(test))


# load randomForest package
library(randomForest)
# run the random forest algorithm on the training data set
rfFit <- randomForest(classe~., data = trainData, method ="rf", prox = TRUE)
rfFit
# use model to predict on validation data set
rfPred <- predict(rfFit, validation)
# predicted result
confusionMatrix(rfPred, validation$classe)

# run the generalized boosted regression model
gbmFit <- train(classe~., data = trainData, method ="gbm", verbose = FALSE)
gbmFit
# use model to predict on validation data set
gbmPred <- predict(gbmFit, validation)
# predicted result
confusionMatrix(gbmPred, validation$classe)

# apply random forest model to test set
predict(rfFit, test)



