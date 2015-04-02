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