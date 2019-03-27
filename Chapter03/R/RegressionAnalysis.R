library(dplyr)
library(ggplot2)

# Load data
df <- read.csv(
  file="~/Documents/data-science-for-marketing/ch.3/data/WA_Fn-UseC_-Marketing-Customer-Value-Analysis.csv", 
  header=TRUE, 
  sep=","
)

#### 1. Engagement Rate ####

# Encode Response as 0s and 1s
df$Engaged <- as.integer(df$Response) - 1

engagementRate <- df %>% 
  group_by(Engaged) %>% 
  summarise(Count=n()) %>%
  mutate(Percentage=Count/nrow(df)*100.0)

engagementRate

# Transpose
transposed <- t(engagementRate)

colnames(transposed) <- engagementRate$Engaged
transposed <- transposed[-1,]
transposed

#### 2. Renewal Offer Type ####
renewalOfferType <- df %>% 
  group_by(Engaged, Type=Renew.Offer.Type) %>% 
  summarise(Count=n())

renewalOfferType

# pie chart
ggplot(renewalOfferType, aes(x="", y=Count, fill=Type)) + 
  geom_bar(width=1, stat = "identity", position=position_fill()) +
  geom_text(aes(x=1.25, label=Count), position=position_fill(vjust = 0.5)) +
  coord_polar("y") +
  facet_wrap(~Engaged) +
  ggtitle('Renwal Offer Type (0: Not Engaged, 1: Engaged)') +
  theme(
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    plot.title=element_text(hjust=0.5),
    legend.position='bottom'
  )

#### 3. Sales Channel ####
salesChannel <- df %>% 
  group_by(Engaged, Channel=Sales.Channel) %>% 
  summarise(Count=n())

salesChannel

# pie chart
ggplot(salesChannel, aes(x="", y=Count, fill=Channel)) + 
  geom_bar(width=1, stat = "identity", position=position_fill()) +
  geom_text(aes(x=1.25, label=Count), position=position_fill(vjust = 0.5)) +
  coord_polar("y") +
  facet_wrap(~Engaged) +
  ggtitle('Sales Channel (0: Not Engaged, 1: Engaged)') +
  theme(
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    plot.title=element_text(hjust=0.5),
    legend.position='bottom'
  )

#### 4. Total Claim Amount ####
ggplot(df, aes(x="", y=Total.Claim.Amount)) + 
  geom_boxplot() +
  facet_wrap(~Engaged) +
  ylab("Total Claim Amount") +
  xlab("0: Not Engaged, 1: Engaged") +
  ggtitle("Engaed vs. Not Engaged: Total Claim Amount") +
  theme(plot.title=element_text(hjust=0.5))

# without outliers
ggplot(df, aes(x="", y=Total.Claim.Amount)) + 
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(limits = quantile(df$Total.Claim.Amount, c(0.1, 0.9))) +
  facet_wrap(~Engaged) +
  ylab("Total Claim Amount") +
  xlab("0: Not Engaged, 1: Engaged") +
  ggtitle("Engaed vs. Not Engaged: Total Claim Amount") +
  theme(plot.title=element_text(hjust=0.5))

#### 5. Income ####

# boxplot
ggplot(df, aes(x="", y=Income)) + 
  geom_boxplot() +
  facet_wrap(~Engaged) +
  ylab("Income") +
  xlab("0: Not Engaged, 1: Engaged") +
  ggtitle("Engaed vs. Not Engaged: Income") +
  theme(plot.title=element_text(hjust=0.5))

# summary statistics
incomeDescription <- df %>% 
  group_by(Engaged) %>% 
  summarise(
    Min=min(Income), Q1=quantile(Income, 0.25), 
    Median=median(Income), Q3=quantile(Income, 0.75),
    Max=max(Income)
  )

incomeDescription


#### 6. Regression Analysis ####
# summary statistics per column
summary(df)
# get data types of each column
sapply(df, class)

## 6.1. Continuous Variables ##

# get numeric columns
continuousDF <- select_if(df, is.numeric)
colnames(continuousDF)

# Fit regression model with continuous variables
logit.fit <- glm(Engaged ~ ., data = continuousDF, family = binomial)
summary(logit.fit)


## 6.2. Categorical Variables ##

# a. Education
# Fit regression model with Education factor variables
logit.fit <- glm(Engaged ~ factor(Education), data = df, family = binomial)
summary(logit.fit)

# b. Education + Gender
# Fit regression model with Education & Gender variables
logit.fit <- glm(Engaged ~ factor(Education) + factor(Gender), data = df, family = binomial)
summary(logit.fit)


## 6.3. Continuous & Categorical Variables ##

continuousDF$Gender <- factor(df$Gender)
continuousDF$Education <- factor(df$Education)

# Fit regression model with Education & Gender variables
logit.fit <- glm(Engaged ~ ., data = continuousDF, family = binomial)
summary(logit.fit)

