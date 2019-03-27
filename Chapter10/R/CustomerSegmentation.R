library(dplyr)
library(readxl)
library(ggplot2)
#### 1. Load Data ####
df <- read_excel(
  path="~/Documents/data-science-for-marketing/ch.10/data/Online Retail.xlsx", 
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

# per customer data
customerDF <- df %>% 
  group_by(CustomerID) %>% 
  summarize(TotalSales=sum(Sales), OrderCount=length(unique(InvoiceDate))) %>%
  mutate(AvgOrderValue=TotalSales/OrderCount)

rankDF <- customerDF %>%
  mutate(TotalSales=rank(TotalSales), OrderCount=rank(OrderCount, ties.method="first"), AvgOrderValue=rank(AvgOrderValue))

normalizedDF <- rankDF %>%
  mutate(TotalSales=scale(TotalSales), OrderCount=scale(OrderCount), AvgOrderValue=scale(AvgOrderValue))

# check for normalization - mean of 0 & std of 1
summary(normalizedDF)
sapply(normalizedDF, sd)

#### 3. Customer Segmentation via K-Means Clustering ####

cluster <- kmeans(normalizedDF[c("TotalSales", "OrderCount", "AvgOrderValue")], 4)

# cluster centers
cluster$centers
# cluster labels
normalizedDF$Cluster <- cluster$cluster

normalizedDF %>% group_by(Cluster) %>% summarise(Count=n())

ggplot(normalizedDF, aes(x=AvgOrderValue, y=OrderCount, color=Cluster)) +
  geom_point()

ggplot(normalizedDF, aes(x=TotalSales, y=OrderCount, color=Cluster)) +
  geom_point()

ggplot(normalizedDF, aes(x=TotalSales, y=AvgOrderValue, color=Cluster)) +
  geom_point()


# Selecting the best number of cluster
library(cluster)

for(n_cluster in 4:8){
  cluster <- kmeans(normalizedDF[c("TotalSales", "OrderCount", "AvgOrderValue")], n_cluster)
  
  silhouetteScore <- mean(
    silhouette(
      cluster$cluster, 
      dist(normalizedDF[c("TotalSales", "OrderCount", "AvgOrderValue")], method = "euclidean")
    )[,3]
  )
  print(sprintf('Silhouette Score for %i Clusters: %0.4f', n_cluster, silhouetteScore))
}

# Interpreting customer segments
cluster <- kmeans(normalizedDF[c("TotalSales", "OrderCount", "AvgOrderValue")], 4)
normalizedDF$Cluster <- cluster$cluster
# count per cluster
normalizedDF %>% group_by(Cluster) %>% summarise(Count=n())
# cluster centers
cluster$centers

# High value cluster summary
summary(customerDF[which(normalizedDF$Cluster == 4),])

highValueCustomers <- unlist(
  customerDF[which(normalizedDF$Cluster == 4),'CustomerID'][,1], use.names = FALSE
)

df[which(df$CustomerID %in% highValueCustomers),] %>%
  group_by(Description) %>%
  summarise(Count=n()) %>%
  arrange(desc(Count))


