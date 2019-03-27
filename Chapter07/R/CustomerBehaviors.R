library(dplyr)
library(ggplot2)

#### 1. Load Data ####
df <- read.csv(
  file="~/Documents/data-science-for-marketing/ch.7/data/WA_Fn-UseC_-Marketing-Customer-Value-Analysis.csv", 
  header=TRUE
)

# Encode engaged customers as 0s and 1s
df$Engaged <- as.integer(df$Response) - 1

#### 2. Analytics on Engaged Customers ####

## - Overall Engagement Rates ##
engagementRate <- df %>% group_by(Response) %>%
  summarise(Count=n())  %>%
  mutate(EngagementRate=Count/nrow(df)*100.0)

ggplot(engagementRate, aes(x=Response, y=EngagementRate)) +
  geom_bar(width=0.5, stat="identity") +
  ggtitle('Engagement Rate') +
  xlab("Engaged") +
  ylab("Percentage (%)") +
  theme(plot.title = element_text(hjust = 0.5)) 


## - Engagement Rates by Offer Type ##
engagementRateByOfferType <- df %>% 
  group_by(Renew.Offer.Type) %>%
  summarise(Count=n(), NumEngaged=sum(Engaged))  %>%
  mutate(EngagementRate=NumEngaged/Count*100.0)

ggplot(engagementRateByOfferType, aes(x=Renew.Offer.Type, y=EngagementRate)) +
  geom_bar(width=0.5, stat="identity") +
  ggtitle('Engagement Rates by Offer Type') +
  xlab("Offer Type") +
  ylab("Engagement Rate (%)") +
  theme(plot.title = element_text(hjust = 0.5)) 


## - Offer Type & Vehicle Class ##
engagementRateByOfferTypeVehicleClass <- df %>% 
  group_by(Renew.Offer.Type, Vehicle.Class) %>%
  summarise(NumEngaged=sum(Engaged))  %>%
  left_join(engagementRateByOfferType[,c("Renew.Offer.Type", "Count")], by="Renew.Offer.Type") %>%
  mutate(EngagementRate=NumEngaged/Count*100.0)

ggplot(engagementRateByOfferTypeVehicleClass, aes(x=Renew.Offer.Type, y=EngagementRate, fill=Vehicle.Class)) +
  geom_bar(width=0.5, stat="identity", position = "dodge") +
  ggtitle('Engagement Rates by Offer Type & Vehicle Class') +
  xlab("Offer Type") +
  ylab("Engagement Rate (%)") +
  theme(plot.title = element_text(hjust = 0.5)) 


## - Engagement Rates by Sales Channel ##
engagementRateBySalesChannel <- df %>% 
  group_by(Sales.Channel) %>%
  summarise(Count=n(), NumEngaged=sum(Engaged))  %>%
  mutate(EngagementRate=NumEngaged/Count*100.0)

ggplot(engagementRateBySalesChannel, aes(x=Sales.Channel, y=EngagementRate)) +
  geom_bar(width=0.5, stat="identity") +
  ggtitle('Engagement Rates by Sales Channel') +
  xlab("Sales Channel") +
  ylab("Engagement Rate (%)") +
  theme(plot.title = element_text(hjust = 0.5)) 


## - Sales Channel & Vehicle Size ##
engagementRateBySalesChannelVehicleSize <- df %>% 
  group_by(Sales.Channel, Vehicle.Size) %>%
  summarise(NumEngaged=sum(Engaged))  %>%
  left_join(engagementRateBySalesChannel[,c("Sales.Channel", "Count")], by="Sales.Channel") %>%
  mutate(EngagementRate=NumEngaged/Count*100.0)

ggplot(engagementRateBySalesChannelVehicleSize, aes(x=Sales.Channel, y=EngagementRate, fill=Vehicle.Size)) +
  geom_bar(width=0.5, stat="identity", position = "dodge") +
  ggtitle('Engagement Rates by Sales Channel & Vehicle Size') +
  xlab("Sales Channel") +
  ylab("Engagement Rate (%)") +
  theme(plot.title = element_text(hjust = 0.5)) 


## - Engagement Rates by Months Since Policy Inception ##
engagementRateByPolicyAge <- df %>% 
  group_by(Months.Since.Policy.Inception) %>%
  summarise(Count=n(), NumEngaged=sum(Engaged))  %>%
  mutate(EngagementRate=NumEngaged/Count*100.0)

ggplot(engagementRateByPolicyAge, aes(x=Months.Since.Policy.Inception, y=EngagementRate)) +
  geom_line() +
  ylab("Engagement Rate (%)") +
  xlab("Months Since Policy Inception") +
  ggtitle("Engagement Rates by Months Since Policy Inception") +
  theme(plot.title=element_text(hjust=0.5))


#### 3. Customer Segmentation by CLV & Months Since Inception ####
summary(df$Customer.Lifetime.Value)
summary(df$Months.Since.Policy.Inception)

clv_encode_fn <- function(x) {if(x > median(df$Customer.Lifetime.Value)) "High" else "Low"}
df$CLV.Segment <- sapply(df$Customer.Lifetime.Value, clv_encode_fn)

policy_age_encode_fn <- function(x) {if(x > median(df$Months.Since.Policy.Inception)) "High" else "Low"}
df$Policy.Age.Segment <- sapply(df$Months.Since.Policy.Inception, policy_age_encode_fn)

ggplot(
  df[which(df$CLV.Segment=="High" & df$Policy.Age.Segment=="High"),], 
  aes(x=Months.Since.Policy.Inception, y=log(Customer.Lifetime.Value))
) +
  geom_point(color='red') +
  geom_point(
    data=df[which(df$CLV.Segment=="High" & df$Policy.Age.Segment=="Low"),], 
    color='orange'
  ) +
  geom_point(
    data=df[which(df$CLV.Segment=="Low" & df$Policy.Age.Segment=="Low"),], 
    color='green'
  ) +
  geom_point(
    data=df[which(df$CLV.Segment=="Low" & df$Policy.Age.Segment=="High"),], 
    color='blue'
  ) +
  ggtitle('Segments by CLV and Policy Age') +
  xlab("Months Since Policy Inception") +
  ylab("CLV (in log scale)") +
  theme(plot.title = element_text(hjust = 0.5)) 

engagementRateBySegment <- df %>% 
  group_by(CLV.Segment, Policy.Age.Segment) %>%
  summarise(Count=n(), NumEngaged=sum(Engaged))  %>%
  mutate(EngagementRate=NumEngaged/Count*100.0)

ggplot(engagementRateBySegment, aes(x=CLV.Segment, y=EngagementRate, fill=Policy.Age.Segment)) +
  geom_bar(width=0.5, stat="identity", position = "dodge") +
  ggtitle('Engagement Rates by Customer Segments') +
  ylab("Engagement Rate (%)") +
  theme(plot.title = element_text(hjust = 0.5)) 

