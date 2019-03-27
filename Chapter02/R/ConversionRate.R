library(dplyr)  # Data Wrangling and Manipulation
library(ggplot2)

conversionsDF <- read.csv(
  file="~/Documents/data-science-for-marketing/ch.2/data/bank-additional-full.csv", 
  header=TRUE, 
  sep=";"
)

# Shape of conversionsDF
dim(conversionsDF)
# Quick look at conversionsDF
head(conversionsDF)

# Encode conversions as 0s and 1s
conversionsDF$conversion <- as.integer(conversionsDF$y) - 1
tail(conversionsDF)

#### 1. Aggregate Conversion Rate ####
sprintf("total conversions: %i out of %i", sum(conversionsDF$conversion), nrow(conversionsDF))
sprintf("conversion rate: %0.2f%%", sum(conversionsDF$conversion)/nrow(conversionsDF)*100.0)

#### 2. Conversion Rates by Number of Contacts ####
conversionsByNumContact <- conversionsDF %>% 
  group_by(NumContact=campaign) %>% 
  summarise(TotalCount=n(), NumConversions=sum(conversion)) %>%
  mutate(ConversionRate=NumConversions/TotalCount*100.0)

head(conversionsByNumContact, 10)

# line chart
ggplot(data=head(conversionsByNumContact, 10), aes(x=NumContact, y=ConversionRate)) +
  geom_line() +
  ggtitle('Conversion Rates by Number of Contacts') +
  xlab("Number of Contacts") +
  ylab("Conversion Rate (%)") +
  ylim(c(0, 15)) +
  theme(plot.title = element_text(hjust = 0.5))


#### 3. Conversion Rates by Age ####

# a. by age
conversionsByAge <- conversionsDF %>% 
  group_by(Age=age) %>% 
  summarise(TotalCount=n(), NumConversions=sum(conversion)) %>%
  mutate(ConversionRate=NumConversions/TotalCount*100.0)

head(conversionsByAge)

# line chart
ggplot(data=conversionsByAge, aes(x=Age, y=ConversionRate)) +
  geom_line() +
  ggtitle('Conversion Rates by Age') +
  xlab("Age") +
  ylab("Conversion Rate (%)") +
  theme(plot.title = element_text(hjust = 0.5))

# b. by age groups
conversionsByAgeGroup <- conversionsDF %>% 
  group_by(AgeGroup=cut(age, breaks= seq(20, 70, by = 10)) ) %>% 
  summarise(TotalCount=n(), NumConversions=sum(conversion)) %>%
  mutate(ConversionRate=NumConversions/TotalCount*100.0)

conversionsByAgeGroup$AgeGroup <- as.character(conversionsByAgeGroup$AgeGroup)
conversionsByAgeGroup$AgeGroup[6] <- "70+"

# bar chart
ggplot(conversionsByAgeGroup, aes(x=AgeGroup, y=ConversionRate)) +
  geom_bar(width=0.5, stat="identity") +
  ggtitle('Conversion Rates by Age Groups') +
  xlab("Age") +
  ylab("Conversion Rate (%)") +
  theme(plot.title = element_text(hjust = 0.5)) 

#### 4. Conversions vs. Non-Conversions ####

# 4.1. Marital Status
conversionsByMaritalStatus <- conversionsDF %>% 
  group_by(Marital=marital, Conversion=conversion) %>% 
  summarise(Count=n())

conversionsByMaritalStatus

# pie chart
ggplot(conversionsByMaritalStatus, aes(x="", y=Count, fill=Marital)) + 
  geom_bar(width=1, stat = "identity", position=position_fill()) +
  geom_text(aes(x=1.25, label=Count), position=position_fill(vjust = 0.5)) +
  coord_polar("y") +
  facet_wrap(~Conversion) +
  ggtitle('Marital Status (0: Non Conversions, 1: Conversions)') +
  theme(
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    plot.title=element_text(hjust=0.5),
    legend.position='bottom'
  )

# 4.2. Education
conversionsByEducation <- conversionsDF %>% 
  group_by(Education=education, Conversion=conversion) %>% 
  summarise(Count=n())

conversionsByEducation

# pie chart
ggplot(conversionsByEducation, aes(x="", y=Count, fill=Education)) + 
  geom_bar(width=1, stat = "identity", position=position_fill()) +
  geom_text(aes(x=1.25, label=Count), position=position_fill(vjust = 0.5)) +
  coord_polar("y") +
  facet_wrap(~Conversion) +
  ggtitle('Education (0: Non Conversions, 1: Conversions)') +
  theme(
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    plot.title=element_text(hjust=0.5),
    legend.position='bottom'
  )

# 4.3. Last Contact Duration
conversionsDF$duration <- conversionsDF$duration / (60*60)

ggplot(conversionsDF, aes(x="", y=duration)) + 
  geom_boxplot() +
  facet_wrap(~conversion) +
  ylab("duration (in hours)") +
  xlab("0: Non-Conversion, 1: Conversion") +
  ggtitle("Conversion vs. Non-Conversions: Last Contact Duration") +
  theme(plot.title=element_text(hjust=0.5))

#### 5. Conversions by Age Groups & Marital Status ####
conversionsByAgeMarital <- conversionsDF %>% 
  group_by(AgeGroup=cut(age, breaks= seq(20, 70, by = 10)), Marital=marital) %>% 
  summarise(Count=n(), NumConversions=sum(conversion)) %>%
  mutate(TotalCount=sum(Count)) %>%
  mutate(ConversionRate=NumConversions/TotalCount)

conversionsByAgeMarital$AgeGroup <- as.character(conversionsByAgeMarital$AgeGroup)
conversionsByAgeMarital$AgeGroup[is.na(conversionsByAgeMarital$AgeGroup)] <- "70+"

# bar chart
ggplot(conversionsByAgeMarital, aes(x=AgeGroup, y=ConversionRate, fill=Marital)) + 
  geom_bar(width=0.5, stat="identity", position="dodge") +
  ylab("Conversion Rate (%)") +
  xlab("Age") +
  ggtitle("Conversion Rates by Age and Marital Status") +
  theme(plot.title=element_text(hjust=0.5))

# stacked bar chart
ggplot(conversionsByAgeMarital, aes(x=AgeGroup, y=ConversionRate, fill=Marital)) + 
  geom_bar(width=0.5, stat="identity", position="stack") +
  ylab("Conversion Rate (%)") +
  xlab("Age") +
  ggtitle("Conversion Rates by Age and Marital Status") +
  theme(plot.title=element_text(hjust=0.5))
  
