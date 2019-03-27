library(dplyr)
library(ggplot2)

# install.packages('rattle')
library(rattle)	

# install.packages('rpart')
library(rpart) # used to build decision tree

# install.packages('rpart.plot')
library(rpart.plot)

#### 1. Load Data ####
df <- read.csv(
  file="~/Documents/data-science-for-marketing/ch.4/data/bank-full.csv", 
  header=TRUE, 
  sep=";"
)

# Encode conversions as 0s and 1s
df$conversion <- as.integer(df$y) - 1

head(df)

#### 2. Data Analysis ####

# column names
colnames(df)

#### 2.1. Conversion Rate ####
sprintf("total conversions: %i out of %i", sum(df$conversion), nrow(df))
sprintf("conversion rate: %0.2f%%", sum(df$conversion)/nrow(df)*100.0)

#### 2.2. Conversion Rates by Marital Status ####
conversionsByMarital <- df %>% 
  group_by(Marital=marital) %>% 
  summarise(Count=n(), NumConversions=sum(conversion)) %>%
  mutate(ConversionRate=NumConversions/Count*100.0)

conversionsByMarital

ggplot(conversionsByMarital, aes(x=Marital, y=ConversionRate)) +
  geom_bar(width=0.5, stat="identity") +
  ggtitle('Conversion Rates by Marital Status') +
  xlab("Marital Status") +
  ylab("Conversion Rate (%)") +
  theme(plot.title = element_text(hjust = 0.5)) 

#### 2.2. Conversion Rates by Job ####
conversionsByJob <- df %>% 
  group_by(Job=job) %>% 
  summarise(Count=n(), NumConversions=sum(conversion)) %>%
  mutate(ConversionRate=NumConversions/Count*100.0)

conversionsByJob

ggplot(conversionsByJob, aes(x=Job, y=ConversionRate)) +
  geom_bar(width=0.5, stat="identity") +
  coord_flip() +
  ggtitle('Conversion Rates by Job') +
  xlab("Job") +
  ylab("Conversion Rate (%)") +
  theme(plot.title = element_text(hjust = 0.5)) 

#### 2.3. Default Rates by Conversions ####
defaultByConversion <- df %>% 
  group_by(Default=default, Conversion=conversion) %>% 
  summarise(Count=n())

defaultByConversion

ggplot(defaultByConversion, aes(x="", y=Count, fill=Default)) + 
  geom_bar(width=1, stat = "identity", position=position_fill()) +
  geom_text(aes(x=1.25, label=Count), position=position_fill(vjust = 0.5)) +
  coord_polar("y") +
  facet_wrap(~Conversion) +
  ggtitle('Default (0: Non Conversions, 1: Conversions)') +
  theme(
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    plot.title=element_text(hjust=0.5),
    legend.position='bottom'
  )


#### 2.4. Bank Balance by Conversions ####
ggplot(df, aes(x="", y=balance)) + 
  geom_boxplot() +
  facet_wrap(~conversion) +
  ylab("balance") +
  xlab("0: Non-Conversion, 1: Conversion") +
  ggtitle("Conversion vs. Non-Conversions: Balance") +
  theme(plot.title=element_text(hjust=0.5))

ggplot(df, aes(x="", y=balance)) + 
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(limits = c(-2000, 5000)) +
  facet_wrap(~conversion) +
  ylab("balance") +
  xlab("0: Non-Conversion, 1: Conversion") +
  ggtitle("Conversion vs. Non-Conversions: Balance") +
  theme(plot.title=element_text(hjust=0.5))

#### 2.5. Conversions by Number of Contacts ####
conversionsByNumContacts <- df %>% 
  group_by(Campaign=campaign) %>% 
  summarise(Count=n(), NumConversions=sum(conversion)) %>%
  mutate(ConversionRate=NumConversions/Count*100.0)

conversionsByNumContacts

ggplot(conversionsByNumContacts, aes(x=Campaign, y=ConversionRate)) +
  geom_bar(width=0.5, stat="identity") +
  ggtitle('Conversion Rates by Number of Contacts') +
  xlab("Number of Contacts") +
  ylab("Conversion Rate (%)") +
  theme(plot.title = element_text(hjust = 0.5)) 

#### 3. Encoding Categorical Variables ####
rapply(df, function(x) length(unique(x)))

#### 3.1. encoding 'month' ####
# unique values
unique(df$month)
# convert to numbers
months = lapply(month.abb, function(x) tolower(x))
months
# test
match(unique(df$month), months)

# encode
df$month <- match(df$month, months)
# check
df %>% 
  group_by(month) %>% 
  summarise(Count=n())

#### 3.2. encoding job, housing, marital ####
df$job <- factor(df$job)
df$housing <- factor(df$housing)
df$marital <- factor(df$marital)


#### 4. Fitting Decision Trees ####

# grow tree 
fit <- rpart(
  conversion ~ age + balance + campaign + previous + housing + job + marital,
  method="class", 
  data=df,
  control=rpart.control(maxdepth=4, cp=0.0001)
)

# plot tree 
fancyRpartPlot(fit)
