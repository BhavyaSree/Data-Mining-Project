# set working directory and load data into R

setwd("/Users/ram/Desktop/Datamining") 

install.packages("openxlsx")
library(openxlsx)
install.packages("caret")
library(caret)

data=read.xlsx("Online-Retail.xlsx", 1)

# Structure of the data
str(data)

# Change the type of InvoiceDate
data$InvoiceDate <- as.POSIXct(data$InvoiceDate * (60*60*24),
                               origin="1899-12-30", tz="GMT")

str(data)
head(data)

#### Pre-Processing ############################################################

# Check for the sum of Null values in all columns
colSums(is.na(data))
## Have Null values in CustomerID and Description columns.
## As CustomerID is uniqueID, it cannot be replaced with any meaning value

# Omit the rows with Null values in CustomerID.
retail_data <- data %>%
  drop_na(CustomerID)

# Check for Null values
colSums(is.na(retail_data))

str(retail_data)

# Summary of retail data
summary(retail_data)

## Remove unrelated and return transactions
retail_data <- retail_data[retail_data$Quantity >= 0,]
retail_data <- retail_data[retail_data$UnitPrice > 0,]

nrow(retail_data)

colSums(is.na(retail_data))

summary(retail_data)

## Customer Segmentation - calcualte RFM#######################################################

# RFM method for customer segmentation
# Recency: Number of days since last purchase by the Customer
# Frequency: Total number of transactions made by the Customer
# Monetary: Total amount spent by the Customer

## Recency
max_date <- max(retail_data$InvoiceDate)
max_date

# Difference between the InvoiceDate and max_date
retail_data$time_delta <- as.numeric(difftime(as.Date(max_date), as.Date(retail_data$InvoiceDate), units="days"))
head(retail_data)

# minimum time_delta (maxdate - recent InvoiceDate) of every customer
Recency <- retail_data %>%
              select(CustomerID, time_delta) %>%
              group_by(CustomerID) %>%
              slice(which.min(time_delta))

# Frequency
# Group the data by customerId and get the count of invoiceNo
Frequency <- retail_data %>%
                select(CustomerID, InvoiceNo) %>%
                group_by(CustomerID) %>%
                summarize(count = n())

## Monetry
# Group the data by CustomerID and get the sum of amount spent 

# Calculate total price i.e., quantity*unitprice
retail_data$Total_Price <- (retail_data$Quantity)*(retail_data$UnitPrice)

Monetry <- retail_data %>%
                select(CustomerID, Total_Price) %>%
                group_by(CustomerID) %>%
                summarize(Total = sum(Total_Price))

RF <- merge(Recency, Frequency, by='CustomerID')
RFM <- merge(RF, Monetry, by='CustomerID')
colnames(RFM) <- c('CustomerID', 'Recency', 'Frequency', 'Monetry')

str(RFM)
summary(RFM)

## Pre-processing RFM data for clustering #######################################

# Check for outliers
boxplot(RFM[-1], main="Boxplot for Recency, Frequency, Monetry of Customers")

# Remove outliers
Recency_outliers <- boxplot(RFM$Recency, plot=FALSE)$out
RFM_data <- RFM[-which(RFM$Recency %in% Recency_outliers),]

Frequency_outliers <- boxplot(RFM$Frequency, plot=FALSE)$out
RFM_data <- RFM[-which(RFM$Frequency %in% Frequency_outliers),]

Monetry_outliers <- boxplot(RFM$Monetry, plot=FALSE)$out
RFM_data <- RFM[-which(RFM$Monetry %in% Monetry_outliers),]

# Structure of RFM_data
str(RFM_data)
# 3911

# Scale the Numerical Variables
vars <- c('Recency','Frequency','Monetry')
Clust_Data <- data.frame(RFM_data[-1])
Clust_Data[vars] <- lapply(RFM_data[vars], scale)
head(Clust_Data)

## K-Means Clustering #########################################################

# Silhouette method
set.seed(123)
fviz_nbclust(Clust_Data, kmeans, method = "silhouette")
# From the silhouette method, the optimal number of clusters is 4

# Compute K-Means with k as 4
set.seed(101)
KM_Model1 <- kmeans(Clust_Data, 4, nstart = 25)
# nstart=25 will generate 25 random centroids and choose the best one for algorithm.

install.packages('factoextra')
library(factoextra)

fviz_cluster(KM_Model1, data=Clust_Data, 
             geom="point",
             ellipse.type = "convex",
             ggtheme = theme_bw())

# Elbow method
set.seed(123)
fviz_nbclust(Clust_Data, FUNcluster = kmeans, method='wss')
# From the elbow method, at k=6, the total within sum of squares will be less.

set.seed(101)
KM_Model2 <- kmeans(Clust_Data, 6, nstart = 25)

fviz_cluster(KM_Model2, data=Clust_Data, 
             geom="point",
             ellipse.type = "convex",
             ggtheme = theme_bw())

# Sum of squares
KM_Model1$tot.withinss
KM_Model2$tot.withinss

KM_Model1$betweenss
KM_Model2$betweenss

KM_Model2$size

## Evaluation ##################################################################

## Use KNN classification to evaluate the Clustering model

vars <- c('Recency', 'Frequency', 'Monetry')

# At K=4
KM_data1 <- data.frame(Clust_Data, KM_Model1$cluster)
names(KM_data1)[names(KM_data1)=='KM_Model1.cluster'] <- 'Cluster'
head(KM_data1)

x <- KM_data1[vars]
y <- KM_data1$Cluster

set.seed(123)
KNN_Model1 <- train(x,y, 'knn', trControl=trainControl(method='cv', number=10),
                   tuneGrid=expand.grid(k=1:10))
print(KNN_Model1)
# The R-squared value at K=4 is 0.95

# At K=6 Clusters
KM_data2 <- data.frame(Clust_Data, KM_Model2$cluster)
names(KM_data2)[names(KM_data2)=='KM_Model2.cluster'] <- 'Cluster'
head(KM_data2)

x <- KM_data2[vars]
y <- KM_data2$Cluster

set.seed(101)
KNN_Model2 <- train(x,y, 'knn', trControl=trainControl(method='cv', number=10),
                   tuneGrid=expand.grid(k=1:10))
print(KNN_Model2)
# The R-squared value at K=3 is 0.974 with RMSE as 0.2462569
# Clustering is good fit at K as 6 Clusters

KM_data <- data.frame(Clust_Data, KM_Model2$cluster)
names(KM_data)[names(KM_data)=='KM_Model2.cluster'] <- 'Cluster'
head(KM_data)

KM_Model2$size

KM_data %>%
  group_by(Cluster) %>%
  summarise(count=n())

## Hierarchical Clustering #####################################################

# calculate distance between vectors of Clust_Data
d <- dist(Clust_Data, method='euclidean')

HC_model <- hclust(d, method='ward.D2')
plot(HC_model)
rect.hclust(HC_model, k=6, border="green")

groups <- cutree(HC_model, k=6)
HC_data <- data.frame(Clust_Data, groups)
head(HC_data)

HC_data %>%
  group_by(groups) %>%
  summarise(count=n())

## Evaluation ##################################################################

## Using KNN classification to evaluate the model

vars <- c('Recency', 'Frequency', 'Monetry')
x <- HC_data[vars]
y <- HC_data$groups

set.seed(101)
KNN_Model_HC <- train(x,y, 'knn', trControl=trainControl(method='cv', number=10),
                      tuneGrid=expand.grid(k=1:10))
print(KNN_Model_HC)

# The Rsquared value of the model is 0.9871551 at K=1 with RMSE=0.14

## Conclusion ##################################################################

RFM_data['Cluster'] <- HC_data$groups
head(RFM_data)

summary(RFM_data)

customers_1 <- tail(RFM_data$CustomerID[RFM_data$Cluster == 1])
filter(RFM_data, RFM_data$CustomerID %in% customers_1)
# Average Recency and less Frequency and less Monetary 

customers_2 <- tail(RFM_data$CustomerID[RFM_data$Cluster == 2])
filter(RFM_data, RFM_data$CustomerID %in% customers_2)
# Average Recency, Average Frequency and more Monetary

customers_3 <- head(RFM_data$CustomerID[RFM_data$Cluster == 3])
filter(RFM_data, RFM_data$CustomerID %in% customers_3)
# Not Recent and less Frequent Visitors with less Monetary

customers_4 <- tail(RFM_data$CustomerID[RFM_data$Cluster == 4])
filter(RFM_data, RFM_data$CustomerID %in% customers_4)
# Recent and less Frequent visitors with less Monetary. 

customers_5 <- head(RFM_data$CustomerID[RFM_data$Cluster == 5])
filter(RFM_data, RFM_data$CustomerID %in% customers_5)
# Recent visitors, with Average Frequency and less than Average Monetary

customers_6 <- head(RFM_data$CustomerID[RFM_data$Cluster == 6])
filter(RFM_data, RFM_data$CustomerID %in% customers_6)
# Recent, Frequent visitors with High Monetary.
# Best Customers

Final_data <- data.frame(RFM_data[c('CustomerID', 'Cluster')])

Final_data %>%
  group_by(Cluster) %>%
  summarise(count=n())

head(Final_data)

#################################################################################
# Association Rules #############################################################

str(data)

colSums(is.na(data))

# Remove rows with Description as Null
rules_data <- data %>%
              drop_na(Description)
summary(rules_data)

#remove the unrelated and return transactions
rules_data <- rules_data[rules_data$Quantity >= 0,]
rules_data <- rules_data[rules_data$UnitPrice > 0,]

str(rules_data)

length(unique(rules_data$StockCode))
length(unique(rules_data$Description))

head(rules_data$Description)

# trim the Description
rules_data$Description <- str_trim(rules_data$Description)

# Check for Special characters
rules_data[grepl('[!#$%&*+;<=>?@[]^`|~]', rules_data$Description),]

# Check for Description in lower case
head(rules_data[grepl('[:lower:]', rules_data$Description),])

head(rules_data$Description)
# Remove the data with description as Manual
rules_data <- rules_data[rules_data$Description != 'Manual',]

df <- data.frame(rules_data %>%
  select(StockCode, Description) %>%
  group_by(StockCode, Description) %>%
  count(StockCode, Description))

x <- df$StockCode[duplicated(df$StockCode)]
sort(df$Description[df$StockCode %in% x])

rules_data[grep('c$', rules_data$InvoiceNo),]

rules_data %>%
  ifelse(Description == "16 PC CUTLERY SET PANTRY DESIGN", "16 PIECE CUTLERY SET PANTRY DESIGN",
  ifelse(Description == "3 TRADITIONAl BISCUIT CUTTERS  SET", "3 TRADITIONAL COOKIE CUTTERS  SET",
  ifelse(Description == "ANTIQUE SILVER TEA GLASS ETCHED", "ANTIQUE SILVER T-LIGHT GLASS",
  iflese(Description == "ASS COL CIRCLE MOBILE", "ASSORTED COLOURED CIRCLE MOBILE",
  iflese(Description %in% c("BAKING MOULD CHOCOLATE CUP CAKES", "BAKING MOULD CUPCAKE CHOCOLATE"), "BAKING MOULD CHOCOLATE CUPCAKES",
  ifelse(Description == "BAKING MOULD TOFFEE CUP  CHOCOLATE", "BAKING MOULD TOFFEE CUP CHOCOLATE",
  ifelse(Description == "BLUE FELT HANGING HEART W FLOWER", "BLUE FELT HANGING HEART WITH FLOWER",
  ifelse(Description == "BREAD BIN, DINER STYLE, MINT", "BREAD BIN DINER STYLE MINT",
  ifelse(Description == "BREAD BIN, DINER STYLE, IVORY", "BREAD BIN DINER STYLE IVORY",
  ifelse(Description == "BUNDLE OF 3 RETRO EXERCISE BOOKS", "BUNDLE OF 3 RETRO NOTE BOOKS",
  ifelse(Description == "CHARLOTTE BAG ALPHABET  DESIGN", "CHARLOTTE BAG VINTAGE ALPHABET",
  ifelse(Description == "CLASSIC CROME BICYCLE BELL", "CLASSIC CHROME BICYCLE BELL",
  ifelse(Description == "CLASSIC GLASS SWEET JAR", "CLASSIC GLASS COOKIE JAR",
  ifelse(Description == "COLOUR GLASS. STAR T-LIGHT HOLDER", "COLOURED GLASS STAR T-LIGHT HOLDER",
  ifelse(Description == "CORDIAL JUG", "CORDIAL GLASS JUG",
  ifelse(Description == "DECORATION , WOBBLY CHICKEN, METAL", "DECORATION WOBBLY CHICKEN",
  ifelse(Description == "DECORATION , WOBBLY RABBIT , METAL", "DECORATION WOBBLY RABBIT METAL",
  ifelse(Description == "DOILEY BISCUIT TIN", "DOILEY STORAGE TIN",
  ifelse(Description == "DOLLCRAFT GIRL AMELIE", "DOLLCRAFT GIRL AMELIE KIT",
  ifelse(Description == "DOLLY GIRL MINI RUCKSACK", "DOLLY GIRL MINI BACKPACK",
  ifelse(Description == "DOORMAT VINTAGE LEAVES DESIGN", "DOORMAT VINTAGE LEAF",
  ifelse(Description == "DOORKNOB CERAMIC IVORY", "DRAWER KNOB CERAMIC IVORY",
  ifelse(Description == "DOORKNOB CRACKED GLAZE BLUE" , "DRAWER KNOB CRACKLE GLAZE BLUE",
  ifelse(Description == "DOORKNOB CRACKED GLAZE GREEN" , "DRAWER KNOB CRACKLE GLAZE GREEN",
  ifelse(Description == "DOORKNOB CRACKED GLAZE PINK" , "DRAWER KNOB CRACKLE GLAZE PINK",
  ifelse(Description == "DOORKNOB CRACKED GLAZE IVORY" , "DRAWER KNOB CRACKLE GLAZE IVORY",
  ifelse(Description == "ELEPHANT, BIRTHDAY CARD,", "ELEPHANT BIRTHDAY CARD",
  ifelse(Description %in% c("FLOWER FAIRY,5 SUMMER B'DRAW LINERS", "FLOWER FAIRY 5 DRAWER LINERS"),"FLOWER FAIRY 5 SUMMER DRAW LINERS",
  ifelse(Description == "FLOWER PURPLE CLOCK W/SUCKER", "FLOWER PURPLE CLOCK WITH SUCKER",
  ifelse(Description == "FOLDING MIRROR IVORY", "FOLDING BUTTERFLY MIRROR IVORY",
  ifelse(Description == "FOLDING MIRROR RED", "FOLDING BUTTERFLY MIRROR RED",
  ifelse(Description == "FOLDING MIRROR HOT PINK","FOLDING BUTTERFLY MIRROR HOT PINK",
  ifelse(Description == "FOOD COVER WITH BEADS , SET 2 SIZES", "FOOD COVER WITH BEADS SET 2"),
  ifelse(Description == "FRYING PAN RED POLKADOT", "FRYING PAN RED RETROSPOT",
  ifelse(Description == "GIN AND TONIC DIET METAL SIGN", "GIN + TONIC DIET METAL SIGN",
  ifelse(Description == "GOLD M.O.P. ORBIT NECKLACE" ,"GOLD M PEARL  ORBIT NECKLACE",
  ifelse(Description == "GYMKHANNA TREASURE BOOK BOX", "GYMKHANA TREASURE BOOK BOX",
  ifelse(Description == "HEART TRELLIS TRIPLE T-LIGHT HOLDER","HANGING JAM JAR T-LIGHT HOLDERS", 
  ifelse(Description == "HEN HOUSE W CHICK STANDING","HEN HOUSE WITH CHICK STANDING",
  ifelse(Description == "HOME SWEEET HOME 3 PEG HANGER", "HOME SWEET HOME 3 PEG HANGER",
  ifelse(Description == "HOT WATER BOTTLE BABUSHKA LARGE", "HOT WATER BOTTLE BABUSHKA",
  ifelse(Description == "IVORY PANTRY HANGING LAMP" ,"IVORY CAFE HANGING LAMP",
  ifelse(Description == "JARDIN ETCHED GLASS BUTTER DISH","JARDIN ETCHED GLASS CHEESE DISH",
  ifelse(Description == "JUMBO BAG SCANDINAVIAN PAISLEY","JUMBO BAG SCANDINAVIAN BLUE PAISLEY",
         ))) )))   ))))))))))))))))))))))))))))))))))

rules_data$StockCode[(rules_data$Description == "JUMBO BAG SCANDINAVIAN PAISLEY")]
