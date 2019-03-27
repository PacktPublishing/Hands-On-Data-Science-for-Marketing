library(dplyr)
library(readxl)

#### 1. Load Data ####
df <- read_excel(
  path="~/Documents/research/data-science-marketing/ch.6/data/Online Retail.xlsx", 
  sheet="Online Retail"
)

# ignore cancel orders
df <- df[which(df$Quantity > 0),]

#### 2. Data Preparation ####

## 2.1. Handle NaNs in CustomerID field

# there are 133,361 records with no CustomerID
sum(is.na(df$CustomerID))
# sneak peek at records with no CustomerID
head(df[which(is.na(df$CustomerID)),])

# current DataFrame shape
dim(df)

# remove records with NA
df <- na.omit(df)
dim(df)

## 2.2. Customer-Item Matrix
# install.packages("reshape2")
library(reshape2)

customerItemMatrix <- dcast(
  df, CustomerID ~ StockCode, value.var="Quantity"
)
# 0-1 encode 
encode_fn <- function(x) {as.integer(x > 0)}
customerItemMatrix <- customerItemMatrix %>% 
  mutate_at(vars(-CustomerID), funs(encode_fn))

#### 3. Collaborative Filtering ###
# install.packages("coop")
library(coop)

## 3.1. User-based Collaborative Filtering

# User-to-User Similarity Matrix
userToUserSimMatrix <- cosine(
  as.matrix(
    # excluding CustomerID column
    t(customerItemMatrix[, 2:dim(customerItemMatrix)[2]])
  )
)
colnames(userToUserSimMatrix) <- customerItemMatrix$CustomerID

# Making Recommendations
top10SimilarCustomersTo12350 <- customerItemMatrix$CustomerID[
  order(userToUserSimMatrix[,"12350"], decreasing = TRUE)[1:11]
]

itemsBoughtByA <- customerItemMatrix[
  which(customerItemMatrix$CustomerID == "12350"),
]
itemsBoughtByA <- colnames(customerItemMatrix)[which(itemsBoughtByA != 0)]

itemsBoughtByB <- customerItemMatrix[
  which(customerItemMatrix$CustomerID == "17935"),
]
itemsBoughtByB <- colnames(customerItemMatrix)[which(itemsBoughtByB != 0)]

itemsToRecommendToB <- setdiff(itemsBoughtByA, itemsBoughtByB)
itemsToRecommendToB

itemsToRecommendToBDescriptions <- unique(
  df[
    which(df$StockCode %in% itemsToRecommendToB), 
    c("StockCode", "Description")
    ]
)
itemsToRecommendToBDescriptions <- itemsToRecommendToBDescriptions[
  match(itemsToRecommendToB, itemsToRecommendToBDescriptions$StockCode),
]


## 3.2. Item-based Collaborative Filtering

# Item-to-Item Similarity Matrix
itemToItemSimMatrix <- cosine(
  as.matrix(
    # excluding CustomerID column
    customerItemMatrix[, 2:dim(customerItemMatrix)[2]]
  )
)

# Making Recommendations
top10SimilarItemsTo23166 <- colnames(itemToItemSimMatrix)[
  order(itemToItemSimMatrix[,"23166"], decreasing = TRUE)[1:11]
]
top10SimilarItemsTo23166

top10SimilarItemDescriptions <- unique(
  df[
    which(df$StockCode %in% top10SimilarItemsTo23166), 
    c("StockCode", "Description")
  ]
)
top10SimilarItemDescriptions <- top10SimilarItemDescriptions[
  match(top10SimilarItemsTo23166, top10SimilarItemDescriptions$StockCode),
]



