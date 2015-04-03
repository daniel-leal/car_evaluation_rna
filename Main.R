library("nnet")
library("lattice")
library("ggplot2")
library("caret")

# Load Data
data <- read.table("../car_evaluation/car.csv", header = T, sep = ",")

# Obtaining summary
summary(data)

# Shuffle the data and split it into two equal data frames so we can have a training and a testing data set:
set.seed(1234)
data <- data[sample(nrow(data)), ] # shuffle
split <- floor(nrow(data) / 2) # divide
dataTrain <- data[0:split, ] # get Training data set
dataTest <- data[(split+1):nrow(data), ] # get Test data set
resultModel <- multinom(result~., data=dataTrain, maxit = 500, trace=T)

# Find more influential variables
mostImportantVariables <- varImp(resultModel)
mostImportantVariables$Variables <- row.names(mostImportantVariables)
mostImportantVariables <- mostImportantVariables[order(-mostImportantVariables$Overall),]
print(head(mostImportantVariables))

preds1 <- predict(resultModel, type="probs", newdata=dataTest)
head(preds1)
preds2 <- predict(resultModel, type="class", newdata=dataTest)
head(preds2)

# Check the accuracy of the model
postResample(dataTest$result,preds2)

totalAccuracy <- c()
cv <- 10
cvDivider <- floor(nrow(data) / (cv+1))

for (cv in seq(1:cv)) {
  # assign chunk to data test
  dataTestIndex <- c((cv * cvDivider):(cv * cvDivider + cvDivider))
  dataTest <- data[dataTestIndex,]
  # everything else to train
  dataTrain <- data[-dataTestIndex,]
  
  resultModel <- multinom(result~., data=dataTrain, maxit=50, trace=T) 
  
  pred <- predict(resultModel, newdata=dataTest, type="class")
  
  #  classification error
  cv_ac <- postResample(dataTest$result, pred)[[1]]
  print(paste('Current Accuracy:',cv_ac,'for CV:',cv))
  totalAccuracy <- c(totalAccuracy, cv_ac)
}

plot(totalAccuracy, xlab="Predict")
print(paste('Mean of total accuracy:', mean(totalAccuracy)))

