library(dplyr)
library(ggplot2)

#### 1. Load Data ####
df <- read.csv(
  file="~/Documents/data-science-for-marketing/ch.8/data/WA_Fn-UseC_-Marketing-Customer-Value-Analysis.csv", 
  header=TRUE
)

#### 2. Variable Encoding ####

## 2.1. Response Variable: Response
df$Engaged <- as.integer(df$Response) - 1
mean(df$Engaged)

## 2.2. Categorical Features
categoricalVars = c(
  'Sales.Channel', 'Vehicle.Size', 'Vehicle.Class', 'Policy', 'Policy.Type',
  'EmploymentStatus', 'Marital.Status', 'Education', 'Coverage', 'Gender'
)

encodedDF <- model.matrix(~.-1, df[categoricalVars])

## 2.3. Continuous Features
continuousFeatures <- c(
  'Customer.Lifetime.Value', 'Income', 'Monthly.Premium.Auto',
  'Months.Since.Last.Claim', 'Months.Since.Policy.Inception',
  'Number.of.Open.Complaints', 'Number.of.Policies', 'Total.Claim.Amount'
)

encodedDF <- cbind(encodedDF, df[continuousFeatures])


#### 3. Training & Testing ####

# install.packages('caTools')
library(caTools)

sample <- sample.split(df$Customer, SplitRatio = .7)

trainX <- as.matrix(subset(encodedDF, sample == TRUE))
trainY <- as.double(as.matrix(subset(df$Engaged, sample == TRUE)))

testX <- as.matrix(subset(encodedDF, sample == FALSE))
testY <- as.double(as.matrix(subset(df$Engaged, sample == FALSE)))

## 3.1. Building Random Forest Model

# - Training
# install.packages('randomForest')
library(randomForest)

rfModel <- randomForest(x=trainX, y=factor(trainY), ntree=200, maxnodes=24)

# - Individual Tree Predictions
getTree(rfModel, 1)
predict(rfModel, trainX, predict.all=TRUE)$individual

# - Feature Importances
importance(rfModel)

## 3.2. Evaluating Models

inSamplePreds <- as.double(predict(rfModel, trainX)) - 1
outSamplePreds <- as.double(predict(rfModel, testX)) - 1

# - Accuracy, Precision, and Recall
inSampleAccuracy <- mean(trainY == inSamplePreds)
outSampleAccuracy <- mean(testY == outSamplePreds)
print(sprintf('In-Sample Accuracy: %0.4f', inSampleAccuracy))
print(sprintf('Out-Sample Accuracy: %0.4f', outSampleAccuracy))

inSamplePrecision <- sum(inSamplePreds & trainY) / sum(inSamplePreds)
outSamplePrecision <- sum(outSamplePreds & testY) / sum(outSamplePreds)
print(sprintf('In-Sample Precision: %0.4f', inSamplePrecision))
print(sprintf('Out-Sample Precision: %0.4f', outSamplePrecision))

inSampleRecall <- sum(inSamplePreds & trainY) / sum(trainY)
outSampleRecall <- sum(outSamplePreds & testY) / sum(testY)
print(sprintf('In-Sample Recall: %0.4f', inSampleRecall))
print(sprintf('Out-Sample Recall: %0.4f', outSampleRecall))

# - ROC & AUC
# install.packages('ROCR')
library(ROCR)

inSamplePredProbs <- as.double(predict(rfModel, trainX, type='prob')[,2])
outSamplePredProbs <- as.double(predict(rfModel, testX, type='prob')[,2])

pred <- prediction(outSamplePredProbs, testY)
perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
auc <- performance(pred, measure='auc')@y.values[[1]]

plot(
  perf, 
  main=sprintf('Random Forest Model ROC Curve (AUC: %0.2f)', auc), 
  col='darkorange', 
  lwd=2
) + grid()
abline(a = 0, b = 1, col='darkgray', lty=3, lwd=2)



