library(dplyr)
library(tidyr)
library(readxl)

#### 1. Load Data ####
df <- read_excel(
  path="~/Documents/data-science-for-marketing/ch.11/data/WA_Fn-UseC_-Telco-Customer-Churn.xlsx"
)

#### 2. Date Analysis & Preparation ####
df <- df %>% drop_na()

apply(df, 2, function(x) length(unique(x)))

ggplot(df %>% group_by(gender) %>% summarise(Count=n()), aes(x=gender, y=Count)) +
  geom_bar(width=0.5, stat="identity") +
  ggtitle('') +
  xlab("Gender") +
  ylab("Count") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(df %>% group_by(InternetService) %>% summarise(Count=n()), aes(x=InternetService, y=Count)) +
  geom_bar(width=0.5, stat="identity") +
  ggtitle('') +
  xlab("Internet Service") +
  ylab("Count") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(df %>% group_by(PaymentMethod) %>% summarise(Count=n()), aes(x=PaymentMethod, y=Count)) +
  geom_bar(width=0.5, stat="identity") +
  ggtitle('') +
  xlab("Payment Method") +
  ylab("Count") +
  theme(plot.title = element_text(hjust = 0.5))

# Binary & Continuous Vars
sampleDF <- df %>%
  select(tenure, MonthlyCharges, TotalCharges, gender, Partner, Dependents, PhoneService, PaperlessBilling, Churn) %>%
  mutate(
    # transforming continuous vars
    tenure=(tenure - mean(tenure))/sd(tenure),
    MonthlyCharges=(log(MonthlyCharges) - mean(log(MonthlyCharges)))/sd(log(MonthlyCharges)),
    TotalCharges=(log(TotalCharges) - mean(log(TotalCharges)))/sd(log(TotalCharges)),
    # encoding binary categorical vars
    gender=gender %>% as.factor() %>% as.numeric() - 1,
    Partner=Partner %>% as.factor() %>% as.numeric() - 1,
    Dependents=Dependents %>% as.factor() %>% as.numeric() - 1,
    PhoneService=PhoneService %>% as.factor() %>% as.numeric() - 1,
    PaperlessBilling=PaperlessBilling %>% as.factor() %>% as.numeric() - 1,
    Churn=Churn %>% as.factor() %>% as.numeric() - 1
  )

summary(df[,c("tenure", "MonthlyCharges", "TotalCharges")])
apply(df[,c("tenure", "MonthlyCharges", "TotalCharges")], 2, sd)

summary(sampleDF[,c("tenure", "MonthlyCharges", "TotalCharges")])
apply(sampleDF[,c("tenure", "MonthlyCharges", "TotalCharges")], 2, sd)

# Dummy vars
# install.packages('dummies')
library(dummies)

sampleDF <- cbind(sampleDF, dummy(df$MultipleLines, sep="."))
names(sampleDF) = gsub("sampleDF", "MultipleLines", names(sampleDF))

sampleDF <- cbind(sampleDF, dummy(df$InternetService, sep="."))
names(sampleDF) = gsub("sampleDF", "InternetService", names(sampleDF))

sampleDF <- cbind(sampleDF, dummy(df$OnlineSecurity, sep="."))
names(sampleDF) = gsub("sampleDF", "OnlineSecurity", names(sampleDF))

sampleDF <- cbind(sampleDF, dummy(df$OnlineBackup, sep="."))
names(sampleDF) = gsub("sampleDF", "OnlineBackup", names(sampleDF))

sampleDF <- cbind(sampleDF, dummy(df$DeviceProtection, sep="."))
names(sampleDF) = gsub("sampleDF", "DeviceProtection", names(sampleDF))

sampleDF <- cbind(sampleDF, dummy(df$TechSupport, sep="."))
names(sampleDF) = gsub("sampleDF", "TechSupport", names(sampleDF))

sampleDF <- cbind(sampleDF, dummy(df$StreamingTV, sep="."))
names(sampleDF) = gsub("sampleDF", "StreamingTV", names(sampleDF))

sampleDF <- cbind(sampleDF, dummy(df$StreamingMovies, sep="."))
names(sampleDF) = gsub("sampleDF", "StreamingMovies", names(sampleDF))

sampleDF <- cbind(sampleDF, dummy(df$Contract, sep="."))
names(sampleDF) = gsub("sampleDF", "Contract", names(sampleDF))

sampleDF <- cbind(sampleDF, dummy(df$PaymentMethod, sep="."))
names(sampleDF) = gsub("sampleDF", "PaymentMethod", names(sampleDF))


#### 3. Train & Test Set Split ####
library(caTools)

sample <- sample.split(sampleDF$Churn, SplitRatio = .7)

train <- as.data.frame(subset(sampleDF, sample == TRUE))
test <- as.data.frame(subset(sampleDF, sample == FALSE))

trainX <- as.matrix(train[,names(train) != "Churn"])
trainY <- train$Churn
testX <- as.matrix(test[,names(test) != "Churn"])
testY <- test$Churn


#### 4. Aritificial Neural Network (ANN) with Keras ####
install.packages("devtools")
devtools::install_github("rstudio/tensorflow")
library(tensorflow)
install_tensorflow()

devtools::install_github("rstudio/keras")
library(keras)
install_keras()


model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 16, kernel_initializer = "uniform", activation = 'relu', input_shape=ncol(sampleDF)-1) %>% 
  layer_dense(units = 8, kernel_initializer = "uniform", activation = 'relu') %>%
  layer_dense(units = 1, kernel_initializer = "uniform", activation = 'sigmoid') %>% 
  compile(
    optimizer = 'adam',
    loss      = 'binary_crossentropy',
    metrics   = c('accuracy')
  )

history <- model %>% fit(
  trainX, 
  trainY, 
  epochs = 50, batch_size = 100, 
  validation_split = 0.2
)

# Evaluating ANN model
inSamplePreds <- as.double(model %>% predict_classes(trainX))
outSamplePreds <- as.double(model %>% predict_classes(testX))

# - Accuracy, Precision, and Recall
inSampleAccuracy <- mean(trainY == inSamplePreds)
outSampleAccuracy <- mean(testY == outSamplePreds)

inSamplePrecision <- sum(inSamplePreds & trainY) / sum(inSamplePreds)
outSamplePrecision <- sum(outSamplePreds & testY) / sum(outSamplePreds)

inSampleRecall <- sum(inSamplePreds & trainY) / sum(trainY)
outSampleRecall <- sum(outSamplePreds & testY) / sum(testY)


print(sprintf('In-Sample Accuracy: %0.4f', inSampleAccuracy))
print(sprintf('Out-Sample Accuracy: %0.4f', outSampleAccuracy))
print(sprintf('In-Sample Precision: %0.4f', inSamplePrecision))
print(sprintf('Out-Sample Precision: %0.4f', outSamplePrecision))
print(sprintf('In-Sample Recall: %0.4f', inSampleRecall))
print(sprintf('Out-Sample Recall: %0.4f', outSampleRecall))


# - ROC & AUC
library(ROCR)

outSamplePredProbs <- as.double(predict(model, testX))

pred <- prediction(outSamplePredProbs, testY)
perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
auc <- performance(pred, measure='auc')@y.values[[1]]

plot(
  perf, 
  main=sprintf('Model ROC Curve (AUC: %0.2f)', auc), 
  col='darkorange', 
  lwd=2
) + grid()
abline(a = 0, b = 1, col='darkgray', lty=3, lwd=2)




