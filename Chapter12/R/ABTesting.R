library(dplyr)
library(readxl)
library(ggplot2)

#### 1. Load Data ####
df <- read_excel(
  path="~/Documents/data-science-for-marketing/ch.12/data/WA_Fn-UseC_-Marketing-Campaign-Eff-UseC_-FastF.xlsx"
)

#### 2. Date Analysis ####

# - total sales
summary(df$SalesInThousands)

salesPerPromo <- df %>% 
  group_by(Promotion) %>%
  summarise(Sales=sum(SalesInThousands))

salesPerPromo

ggplot(salesPerPromo, aes(x="", y=Sales, fill=Promotion)) + 
  geom_bar(width=1, stat = "identity", position=position_fill()) +
  geom_text(aes(x=1.25, label=Sales), position=position_fill(vjust = 0.5), color='white') +
  coord_polar("y") +
  ggtitle('sales distribution across different promotions')

# - market size
df %>% 
  group_by(MarketSize) %>%
  summarise(Count=n())

marketSizePerPromo <- df %>% 
  group_by(Promotion, MarketSize) %>%
  summarise(Count=n())

marketSizePerPromo

ggplot(marketSizePerPromo, aes(x=Promotion, y=Count, fill=MarketSize)) + 
  geom_bar(width=0.5, stat="identity", position="dodge") +
  ylab("Count") +
  xlab("Promotion") +
  ggtitle("breakdowns of market sizes across different promotions") +
  theme(plot.title=element_text(hjust=0.5))

ggplot(marketSizePerPromo, aes(x=Promotion, y=Count, fill=MarketSize)) + 
  geom_bar(width=0.5, stat="identity", position="stack") +
  ylab("Count") +
  xlab("Promotion") +
  ggtitle("breakdowns of market sizes across different promotions") +
  theme(plot.title=element_text(hjust=0.5))


# - store age
summary(df$AgeOfStore)

overallAge <- df %>%
  group_by(AgeOfStore) %>%
  summarise(Count=n())

overallAge

ggplot(overallAge, aes(x=AgeOfStore, y=Count)) + 
  geom_bar(width=0.5, stat="identity") +
  ylab("Count") +
  xlab("Store Age") +
  ggtitle("overall distributions of age of store") +
  theme(plot.title=element_text(hjust=0.5))

AgePerPromo <- df %>% 
  group_by(Promotion, AgeOfStore) %>%
  summarise(Count=n())

AgePerPromo

ggplot(AgePerPromo, aes(x=AgeOfStore, y=Count, fill=Promotion)) + 
  geom_bar(width=0.5, stat="identity", position="dodge2") +
  ylab("Count") +
  xlab("Store Age") +
  ggtitle("distributions of age of store") +
  theme(plot.title=element_text(hjust=0.5))

tapply(df$AgeOfStore, df$Promotion, summary)


# - week number
df %>% 
  group_by(Week) %>%
  summarise(Count=n())

weekPerPromo <- df %>% 
  group_by(Week, Promotion) %>%
  summarise(Count=n())

weekPerPromo

ggplot(weekPerPromo, aes(x="", y=Count, fill=Promotion)) + 
  geom_bar(width=1, stat = "identity", position=position_fill()) +
  geom_text(aes(x=1.25, label=Count), position=position_fill(vjust = 0.5), color='white') +
  coord_polar("y") +
  facet_wrap(~Week) +
  ggtitle('distribution across different weeks')


#### 3. Statistical Significance ####

# Promotion 1 vs. 2
promo_1 <- df[which(df$Promotion == 1),]$SalesInThousands
promo_2 <- df[which(df$Promotion == 2),]$SalesInThousands

mean_1 <- mean(promo_1)
mean_2 <- mean(promo_2)
std_1 <- sd(promo_1)
std_2 <- sd(promo_2)
n_1 <- length(promo_1)
n_2 <- length(promo_2)

df_1_2 <- n_1 + n_2 - 2

t_val <- (
  mean_1 - mean_2
) / sqrt(
  (std_1**2/n_1 + std_2**2/n_2)
)

p_val <- 2 * pt(t_val, df_1_2, lower=FALSE)

# - using t.test
t.test(
  promo_1, 
  promo_2
)

# Promotion 1 vs. 3
promo_1 <- df[which(df$Promotion == 1),]$SalesInThousands
promo_3 <- df[which(df$Promotion == 3),]$SalesInThousands

mean_1 <- mean(promo_1)
mean_3 <- mean(promo_3)
std_1 <- sd(promo_1)
std_3 <- sd(promo_3)
n_1 <- length(promo_1)
n_3 <- length(promo_3)
df_1_3 <- n_1 + n_3 - 2

t_val <- (
  mean_1 - mean_3
) / sqrt(
  (std_1**2/n_1 + std_3**2/n_3)
)

p_val <- 2 * pt(t_val, df_1_3, lower=FALSE)

# - using t.test
t.test(
  promo_1, 
  promo_3
)




