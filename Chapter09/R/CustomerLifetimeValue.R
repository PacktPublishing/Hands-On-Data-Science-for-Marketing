library(dplyr)
library(readxl)
library(ggplot2)

#### 1. Load Data ####
df <- read_excel(
  path="~/Documents/data-science-for-marketing/ch.9/data/Online Retail.xlsx", 
  sheet="Online Retail"
)

#### 2. Date Clean-Up ####

# ignore negative quantity
dim(df)
df <- df[which(df$Quantity > 0),]
dim(df)

# remove records with NA
df <- na.omit(df)
dim(df)

# excluding incomplete month
sprintf("Date Range: %s ~ %s", min(df$InvoiceDate), max(df$InvoiceDate))
dim(df)
df <- df[which(df$InvoiceDate < '2011-12-01'),]
dim(df)

# total sales
df$Sales <- df$Quantity * df$UnitPrice

# per order data
ordersDF <- df %>% 
  group_by(CustomerID, InvoiceNo) %>% 
  summarize(Sales=sum(Sales), InvoiceDate=max(InvoiceDate))

#### 3. Date Analysis ####

# order amount & frequency summary
summaryDF <- ordersDF %>%
  group_by(CustomerID) %>%
  summarize(
    SalesMin=min(Sales), SalesMax=max(Sales), SalesSum=sum(Sales), SalesAvg=mean(Sales), SalesCount=n(),
    InvoiceDateMin=min(InvoiceDate), InvoiceDateMax=max(InvoiceDate), 
    PurchaseDuration=as.double(floor(max(InvoiceDate)-min(InvoiceDate))),
    PurchaseFrequency=as.double(floor(max(InvoiceDate)-min(InvoiceDate)))/n()
  )

# customers with repeat purchases
dim(summaryDF)
summaryDF <- summaryDF[which(summaryDF$PurchaseDuration > 0),]
dim(summaryDF)

salesCount <- summaryDF %>% 
  group_by(SalesCount) %>% 
  summarize(Count=n())

ggplot(salesCount[1:19,], aes(x=SalesCount, y=Count)) +
  geom_bar(width=0.5, stat="identity") +
  ggtitle('') +
  xlab("Sales Count") +
  ylab("Count") +
  theme(plot.title = element_text(hjust = 0.5))

summary(summaryDF$SalesCount)
summary(summaryDF$SalesAvg)

hist(
  summaryDF$PurchaseFrequency, 
  breaks=20,
  xlab='avg. number of days between purchases',
  ylab='count',
  main=''
)

summary(summaryDF$PurchaseDuration)
summary(summaryDF$PurchaseFrequency)


#### 4. Predicting 3-Month CLV ####

## 4.1. Data Prep ##

# group data into every 3 months
library(lubridate)

ordersDF$Quarter = as.character(round_date(ordersDF$InvoiceDate, '3 months'))

dataDF <- ordersDF %>%
  group_by(CustomerID, Quarter) %>%
  summarize(SalesSum=sum(Sales), SalesAvg=mean(Sales), SalesCount=n())

dataDF$Quarter[dataDF$Quarter == "2012-01-01"] <- "Q1"
dataDF$Quarter[dataDF$Quarter == "2011-10-01"] <- "Q2"
dataDF$Quarter[dataDF$Quarter == "2011-07-01"] <- "Q3"
dataDF$Quarter[dataDF$Quarter == "2011-04-01"] <- "Q4"
dataDF$Quarter[dataDF$Quarter == "2011-01-01"] <- "Q5"

# building sample set
# install.packages('reshape2')
library(reshape2)

salesSumFeaturesDF <- dcast(
  dataDF[which(dataDF$Quarter != "Q1"),], 
  CustomerID ~ Quarter, 
  value.var="SalesSum"
)
colnames(salesSumFeaturesDF) <- c("CustomerID", "SalesSum.Q2", "SalesSum.Q3", "SalesSum.Q4", "SalesSum.Q5")

salesAvgFeaturesDF <- dcast(
  dataDF[which(dataDF$Quarter != "Q1"),], 
  CustomerID ~ Quarter, 
  value.var="SalesAvg"
)
colnames(salesAvgFeaturesDF) <- c("CustomerID", "SalesAvg.Q2", "SalesAvg.Q3", "SalesAvg.Q4", "SalesAvg.Q5")

salesCountFeaturesDF <- dcast(
  dataDF[which(dataDF$Quarter != "Q1"),], 
  CustomerID ~ Quarter, 
  value.var="SalesCount"
)
colnames(salesCountFeaturesDF) <- c("CustomerID", "SalesCount.Q2", "SalesCount.Q3", "SalesCount.Q4", "SalesCount.Q5")

featuresDF <- merge(
  merge(salesSumFeaturesDF, salesAvgFeaturesDF, by="CustomerID"),
  salesCountFeaturesDF, by="CustomerID"
)
featuresDF[is.na(featuresDF)] <- 0

responseDF <- dataDF[which(dataDF$Quarter == "Q1"),] %>% 
  select(CustomerID, SalesSum)
colnames(responseDF) <- c("CustomerID", "CLV_3_Month")

sampleDF <- merge(featuresDF, responseDF, by="CustomerID", all.x=TRUE)
sampleDF[is.na(sampleDF)] <- 0

summary(sampleDF$CLV_3_Month)

## 4.2. Regression Models ##

# train/test set split
library(caTools)

sample <- sample.split(sampleDF$CustomerID, SplitRatio = .8)

train <- as.data.frame(subset(sampleDF, sample == TRUE))[,-1]
test <- as.data.frame(subset(sampleDF, sample == FALSE))[,-1]

# Linear Regression model
regFit <- lm(CLV_3_Month ~ ., data=train)

summary(regFit)

## 4.3. Evaluation ##
train_preds <- predict(regFit, train)
test_preds <- predict(regFit, test)

# R-squared
# install.packages('miscTools')
library(miscTools)

inSampleR2 <- rSquared(train$CLV_3_Month, resid=train$CLV_3_Month - train_preds)
outOfSampleR2 <- rSquared(test$CLV_3_Month, resid=test$CLV_3_Month - test_preds)

sprintf('In-Sample R-Squared: %0.4f', inSampleR2)
sprintf('Out-of-Sample R-Squared: %0.4f', outOfSampleR2)

# Median Absolute Error
inSampleMAE <- median(abs(train$CLV_3_Month - train_preds))
outOfSampleMAE <- median(abs(test$CLV_3_Month - test_preds))

sprintf('In-Sample MAE: %0.4f', inSampleMAE)
sprintf('Out-of-Sample MAE: %0.4f', outOfSampleMAE)

# Actual vs. Predicted Scatter Plot
plot(
  train$CLV_3_Month, 
  train_preds, 
  xlab='actual', 
  ylab='predicted', 
  main='In-Sample Actual vs. Predicted'
)
abline(a=0, b=1)

plot(
  test$CLV_3_Month, 
  test_preds, 
  xlab='actual', 
  ylab='predicted', 
  main='Out-of-Sample Actual vs. Predicted'
)
abline(a=0, b=1)




