# Load retail data into R session
install.packages("openxlsx")
library(openxlsx)

data=read.xlsx("Online Retail.xlsx", 1)

# Structure of the data
str(data)
# Change the type of InvoiceDate
data$InvoiceDate <- as.POSIXct(data$InvoiceDate * (60*60*24),
                               origin="1899-12-30", tz="GMT") 
head(data)

#### Pre-Processing ############################################################

# Check for the sum of Null values in all columns
colSums(is.na(data))
## Have Null values in CustomerID and Description columns.
## As CustomerID is uniqueID, it cannot be replaced with any meaning value

# Omit the rows with Null values in CustomerID.
library(tidyr)
retail_data <- data %>%
  drop_na(CustomerID)

# Check for Null values
colSums(is.na(retail_data))

# Summary of retail data
summary(retail_data)

## Remove unrelated and return transactions
retail_data <- retail_data[retail_data$Quantity >= 0,]
retail_data <- retail_data[retail_data$UnitPrice > 0,]

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
head(retail_data$time_delta, 2)

installed.packages('dplyr')
library(dplyr)

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
head(RFM)
summary(RFM)


## RFM data re-processing ######################################################

# Check for outliers
boxplot(RFM[-1], main="Boxplot for RFM values")

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

install.packages('factoextra')
library(factoextra)

# Silhouette Score
set.seed(123)
fviz_nbclust(Clust_Data, kmeans, method = "silhouette")
# From the silhouette method, the optimal number of clusters is 4

# Compute K-Means with k as 4
set.seed(101)
KM_Model1 <- kmeans(Clust_Data, 4, nstart = 25)
# nstart=25 will generate 25 random centroids and choose the best one for algorithm.

fviz_cluster(KM_Model1, data=Clust_Data, 
             geom="point",
             ellipse.type = "convex",
             ggtheme = theme_bw())

# Size of Clusters
KM_Model1$size
# Center of clusters
KM_Model1$centers

# Elbow method
set.seed(123)
fviz_nbclust(Clust_Data, kmeans, method='wss')
# From the elbow method, at k=3, the total within sum of squares will be less.

# Compute K-Means with k as 3
set.seed(101)
KM_Model2 <- kmeans(Clust_Data, 3, nstart = 25)
# nstart=25 will generate 25 random centroids and choose the best one for algorithm.

fviz_cluster(KM_Model2, data=Clust_Data, 
             geom="point",
             ellipse.type = "convex",
             ggtheme = theme_bw())

# Size of Clusters
KM_Model2$size
# Center of clusters
KM_Model2$centers

## Evaluation ##################################################################
## Use KNN classification to evaluate the Clustering model
vars <- c('Recency', 'Frequency', 'Monetry')
# At K=4
KM_data1 <- data.frame(Clust_Data, KM_Model1$cluster)
names(KM_data1)[names(KM_data1)=='KM_Model1.cluster'] <- 'Cluster'
head(KM_data1, 1)

KM_data1$Cluster <- factor(KM_data1$Cluster)
x <- KM_data1[vars]
y <- KM_data1$Cluster

library(caret)
set.seed(123)
KNN_Model1 <- train(x,y, 'knn', trControl=trainControl(method='cv', number=10),
                   tuneGrid=expand.grid(k=1:10))
print(KNN_Model1)

# At K=3 Clusters
KM_data2 <- data.frame(Clust_Data, KM_Model2$cluster)
names(KM_data2)[names(KM_data2)=='KM_Model2.cluster'] <- 'Cluster'
head(KM_data2,1)

KM_data2$Cluster <- factor(KM_data2$Cluster)
x <- KM_data2[vars]
y <- KM_data2$Cluster

set.seed(123)
KNN_Model2 <- train(x,y, 'knn', trControl=trainControl(method='cv', number=10),
                   tuneGrid=expand.grid(k=1:10))
print(KNN_Model2)

#KM_data <- data.frame(Clust_Data, KM_Model2$cluster)
#names(KM_data)[names(KM_data)=='KM_Model2.cluster'] <- 'Cluster'
#head(KM_data)

#KM_Model2$size

#KM_data %>%
#  group_by(Cluster) %>%
#  summarise(count=n())

## Hierarchical Clustering #####################################################

# Silhouette Score
set.seed(123)
fviz_nbclust(Clust_Data, hcut, method = "silhouette")
# From the silhouette method, the optimal number of clusters is 2

# calculate distance between vectors of Clust_Data
d <- dist(Clust_Data, method='euclidean')

HC_model1 <- hclust(d, method='ward.D2')
plot(HC_model)
rect.hclust(HC_model1, k=2, border="green")

groups <- cutree(HC_model1, k=2)
HC_data1 <- data.frame(Clust_Data, groups)
head(HC_data1, 1)

HC_data1 %>%
  group_by(groups) %>%
  summarise(count=n())

# Elbow method
set.seed(123)
fviz_nbclust(Clust_Data, hcut, method='wss')
# From the elbow method, at k=3, the total within sum of squares will be less.

HC_model2 <- hclust(d, method='ward.D2')
plot(HC_model2)
rect.hclust(HC_model2, k=3, border="green")

groups <- cutree(HC_model2, k=3)
HC_data2 <- data.frame(Clust_Data, groups)
head(HC_data2, 1)

HC_data2 %>%
  group_by(groups) %>%
  summarise(count=n())


## Evaluation ##################################################################
## Using KNN classification to evaluate the Hierarchical clustering result
vars <- c('Recency', 'Frequency', 'Monetry')

# At K=2
HC_data1$groups <- factor(HC_data1$groups)
x <- HC_data1[vars]
y <- HC_data1$groups

set.seed(101)
KNN_Model_HC1 <- train(x,y, 'knn', trControl=trainControl(method='cv', number=10),
                      tuneGrid=expand.grid(k=1:10))
print(KNN_Model_HC1)

# At K=3
HC_data2$groups <- factor(HC_data2$groups)
x <- HC_data2[vars]
y <- HC_data2$groups

set.seed(101)
KNN_Model_HC2 <- train(x,y, 'knn', trControl=trainControl(method='cv', number=10),
                       tuneGrid=expand.grid(k=1:10))
print(KNN_Model_HC2)

## Conclusion ##################################################################

RFM_data['Cluster'] <- HC_data1$groups
# head(RFM_data)
summary(RFM_data)

customers_1 <- head(RFM_data$CustomerID[RFM_data$Cluster == 1])
filter(RFM_data, RFM_data$CustomerID %in% customers_1)

customers_2 <- head(RFM_data$CustomerID[RFM_data$Cluster == 2])
filter(RFM_data, RFM_data$CustomerID %in% customers_2)
# Best Customers

Final_data <- data.frame(RFM_data[c('CustomerID', 'Cluster')])

Final_data %>%
  group_by(Cluster) %>%
  summarise(count=n())

head(Final_data)

#################################################################################
# Association Rules #############################################################
# Pre-Processing ################################################################
colSums(is.na(data))
library(tidyr)

# Remove rows with Description as Null
rules_data <- data %>%
              drop_na(Description)
summary(rules_data)

#remove the unrelated and return transactions
rules_data <- rules_data[rules_data$Quantity >= 0,]
rules_data <- rules_data[rules_data$UnitPrice > 0,]

# str(rules_data)

length(unique(rules_data$StockCode))
length(unique(rules_data$Description))

head(rules_data$Description, 3)

# trim the Description
library(stringr)
rules_data$Description <- str_trim(rules_data$Description)

# Check for Special characters
rules_data[grepl('[!#$%&*+;<=>?@[]^`|~]', rules_data$Description),]
# Check for Description in lower case
head(rules_data[grepl('[:lower:]', rules_data$Description),])

# Remove the data with description as Manual
rules_data <- rules_data[rules_data$Description != 'Manual',]

# Identify the StockCodes having two descriptions and use the descriptions to process the multi descriptions
df <- data.frame(rules_data %>%
  select(StockCode, Description) %>%
  group_by(StockCode, Description) %>%
  count(StockCode, Description))

x <- df$StockCode[duplicated(df$StockCode)]
sort(df$Description[df$StockCode %in% x])

rules_data$Description <- ifelse(rules_data$Description == "16 PC CUTLERY SET PANTRY DESIGN", "16 PIECE CUTLERY SET PANTRY DESIGN",
ifelse(rules_data$Description == "3 TRADITIONAl BISCUIT CUTTERS  SET", "3 TRADITIONAL COOKIE CUTTERS  SET",
ifelse(rules_data$Description == "ASS COL CIRCLE MOBILE", "ASSORTED COLOURED CIRCLE MOBILE",
ifelse(rules_data$Description %in% c("BAKING MOULD CHOCOLATE CUP CAKES", "BAKING MOULD CUPCAKE CHOCOLATE"), "BAKING MOULD CHOCOLATE CUPCAKES",
ifelse(rules_data$Description == "BAKING MOULD TOFFEE CUP  CHOCOLATE", "BAKING MOULD TOFFEE CUP CHOCOLATE", 
ifelse(rules_data$Description == "BLUE FELT HANGING HEART W FLOWER", "BLUE FELT HANGING HEART WITH FLOWER",
ifelse(rules_data$Description == "BREAD BIN, DINER STYLE, MINT", "BREAD BIN DINER STYLE MINT",
ifelse(rules_data$Description == "BREAD BIN, DINER STYLE, IVORY", "BREAD BIN DINER STYLE IVORY",
ifelse(rules_data$Description == "BUNDLE OF 3 RETRO EXERCISE BOOKS", "BUNDLE OF 3 RETRO NOTE BOOKS",
ifelse(rules_data$Description == "CHARLOTTE BAG ALPHABET  DESIGN", "CHARLOTTE BAG VINTAGE ALPHABET",
ifelse(rules_data$Description == "CLASSIC CROME BICYCLE BELL", "CLASSIC CHROME BICYCLE BELL",
ifelse(rules_data$Description == "CLASSIC GLASS SWEET JAR", "CLASSIC GLASS COOKIE JAR",
ifelse(rules_data$Description == "COLOUR GLASS. STAR T-LIGHT HOLDER", "COLOURED GLASS STAR T-LIGHT HOLDER",
ifelse(rules_data$Description == "CORDIAL JUG", "CORDIAL GLASS JUG",
ifelse(rules_data$Description == "DECORATION , WOBBLY CHICKEN, METAL", "DECORATION WOBBLY CHICKEN",
ifelse(rules_data$Description == "DECORATION , WOBBLY RABBIT , METAL", "DECORATION WOBBLY RABBIT METAL",
ifelse(rules_data$Description == "DOILEY BISCUIT TIN", "DOILEY STORAGE TIN",
ifelse(rules_data$Description == "DOLLCRAFT GIRL AMELIE", "DOLLCRAFT GIRL AMELIE KIT",
ifelse(rules_data$Description == "DOLLY GIRL MINI RUCKSACK", "DOLLY GIRL MINI BACKPACK",
ifelse(rules_data$Description == "DOORMAT VINTAGE LEAVES DESIGN", "DOORMAT VINTAGE LEAF",
ifelse(rules_data$Description == "DOORKNOB CERAMIC IVORY", "DRAWER KNOB CERAMIC IVORY",
ifelse(rules_data$Description == "DOORKNOB CRACKED GLAZE BLUE" , "DRAWER KNOB CRACKLE GLAZE BLUE",
ifelse(rules_data$Description == "DOORKNOB CRACKED GLAZE GREEN" , "DRAWER KNOB CRACKLE GLAZE GREEN",
ifelse(rules_data$Description == "DOORKNOB CRACKED GLAZE PINK" , "DRAWER KNOB CRACKLE GLAZE PINK",
ifelse(rules_data$Description == "DOORKNOB CRACKED GLAZE IVORY" , "DRAWER KNOB CRACKLE GLAZE IVORY",
ifelse(rules_data$Description == "ELEPHANT, BIRTHDAY CARD,", "ELEPHANT BIRTHDAY CARD",
ifelse(rules_data$Description %in% c("FLOWER FAIRY,5 SUMMER B'DRAW LINERS", "FLOWER FAIRY 5 DRAWER LINERS"),"FLOWER FAIRY 5 SUMMER DRAW LINERS",
ifelse(rules_data$Description == "FLOWER PURPLE CLOCK W/SUCKER", "FLOWER PURPLE CLOCK WITH SUCKER",
ifelse(rules_data$Description == "FOLDING MIRROR IVORY", "FOLDING BUTTERFLY MIRROR IVORY",
ifelse(rules_data$Description == "FOLDING MIRROR RED", "FOLDING BUTTERFLY MIRROR RED",
ifelse(rules_data$Description == "FOLDING MIRROR HOT PINK","FOLDING BUTTERFLY MIRROR HOT PINK", 
ifelse(rules_data$Description == "FOOD COVER WITH BEADS , SET 2 SIZES", "FOOD COVER WITH BEADS SET 2",
ifelse(rules_data$Description == "FRYING PAN RED POLKADOT", "FRYING PAN RED RETROSPOT",
ifelse(rules_data$Description == "GIN AND TONIC DIET METAL SIGN", "GIN + TONIC DIET METAL SIGN",
ifelse(rules_data$Description == "GOLD M.O.P. ORBIT NECKLACE" ,"GOLD M PEARL  ORBIT NECKLACE",
ifelse(rules_data$Description == "GYMKHANNA TREASURE BOOK BOX", "GYMKHANA TREASURE BOOK BOX", 
ifelse(rules_data$Description == "HEN HOUSE W CHICK STANDING","HEN HOUSE WITH CHICK STANDING",
ifelse(rules_data$Description == "HOME SWEEET HOME 3 PEG HANGER", "HOME SWEET HOME 3 PEG HANGER",
ifelse(rules_data$Description == "HOT WATER BOTTLE BABUSHKA LARGE", "HOT WATER BOTTLE BABUSHKA",
ifelse(rules_data$Description == "IVORY PANTRY HANGING LAMP" ,"IVORY CAFE HANGING LAMP",
ifelse(rules_data$Description == "JARDIN ETCHED GLASS BUTTER DISH","JARDIN ETCHED GLASS CHEESE DISH",
ifelse(rules_data$Description == "JUMBO BAG SCANDINAVIAN PAISLEY","JUMBO BAG SCANDINAVIAN BLUE PAISLEY",
ifelse(rules_data$Description == "JUMBO BAG VINTAGE DOILEY","JUMBO BAG VINTAGE DOILY",
ifelse(rules_data$Description == "LARGE CAKE TOWEL, CHOCOLATE SPOTS","LARGE CAKE TOWEL CHOCOLATE SPOTS",
ifelse(rules_data$Description == "LARGE JEWELLERY STAND","LARGE DECO JEWELLERY STAND", 
ifelse(rules_data$Description == "LARGE PICTURE FRAME","LARGE PARLOUR PICTURE FRAME", 
ifelse(rules_data$Description == "LUNCH BAG RED SPOTTY","LUNCH BAG RED RETROSPOT",  
ifelse(rules_data$Description == "LUNCH BAG VINTAGE DOILEY","LUNCH BAG VINTAGE DOILY", rules_data$Description ))))))))))))))))))))))))))))))))))))))))))))))))

rules_data$Description <- ifelse(rules_data$Description == "MARIE ANTOIENETT TRINKET BOX GOLD", "MARIE ANTOINETTE TRINKET BOX GOLD",
ifelse(rules_data$Description == "MEDIUM PARLOUR FRAME", "MEDIUM PARLOUR PICTURE FRAME",
ifelse(rules_data$Description == "MINITURE ANTIQUE ROSE HOOK IVORY", "MINIATURE ANTIQUE ROSE HOOK IVORY",
ifelse(rules_data$Description == "MINT DINER CLOCK", "MINT DINER WALL CLOCK", 
ifelse(rules_data$Description %in% c("MISTLETOE HEART WREATH CREAM", "MISELTOE HEART WREATH WHITE"),"MISELTOE HEART WREATH CREAM", 
ifelse(rules_data$Description == "MONEY BOX FIRST ADE DESIGN", "MONEY BOX FIRST AID DESIGN",
ifelse(rules_data$Description == "N0 SINGING METAL SIGN", "NO SINGING METAL SIGN",
ifelse(rules_data$Description == "PACK 3 BOXES BIRD PANETTONE", "PACK 3 BOXES BIRD PANNETONE", 
ifelse(rules_data$Description == "PACK 3 BOXES CHRISTMAS PANETTONE", "PACK 3 BOXES CHRISTMAS PANNETONE",
ifelse(rules_data$Description == "PACK OF 12 DOILEY TISSUES","PACK OF 12 VINTAGE DOILY TISSUES",
ifelse(rules_data$Description == "PACK OF 6 PANETTONE GIFT BOXES","PACK OF 6 PANNETONE GIFT BOXES",
ifelse(rules_data$Description == "PAPER LANTERN 5 POINT STAR MOON 30", "PAPER LANTERN 5 POINT STAR MOON",
ifelse(rules_data$Description == "PARTY CONES CANDY DECORATION","PARTY CONES CANDY TREE DECORATION",
ifelse(rules_data$Description == "PEG BAG APPLE DESIGN","PEG BAG APPLES DESIGN",
ifelse(rules_data$Description == "PICNIC BASKET WICKER 60 PIECES", "PICNIC BASKET WICKER SMALL",
ifelse(rules_data$Description == "PLAYING CARDS VINTAGE DOILEY","PLAYING CARDS VINTAGE DOILY",
ifelse(rules_data$Description == "RETO LEAVES MAGNETIC SHOPPING LIST","RETRO LEAVES MAGNETIC NOTEPAD",
ifelse(rules_data$Description == "ROCOCO WALL MIROR" ,"ROCOCO WALL MIRROR WHITE",
ifelse(rules_data$Description == "RUSTIC STRAWBERRY JAMPOT LARGE","RUSTIC STRAWBERRY JAM POT LARGE",
ifelse(rules_data$Description == "RUSTIC STRAWBERRY JAMPOT SMALL","RUSTIC STRAWBERRY JAM POT SMALL",
ifelse(rules_data$Description == "SET 10 LIGHTS NIGHT OWL", "SET 10 NIGHT OWL LIGHTS",
ifelse(rules_data$Description %in% c("SET 12 COLOUR PENCILS DOILEY", "SET 12 COLOURING PENCILS DOILEY"), "SET 12 COLOURING PENCILS DOILY",
ifelse(rules_data$Description %in% c("SET 36 COLOUR PENCILS DOILEY", "SET 36 COLOURING PENCILS DOILEY"), "SET 36 COLOURING PENCILS DOILY",
ifelse(rules_data$Description == "SET 8 CANDLES VINTAGE DOILEY","SET 8 CANDLES VINTAGE DOILY",
ifelse(rules_data$Description == "SET OF 12 T-LIGHTS VINTAGE DOILEY","SET OF 12 T-LIGHTS VINTAGE DOILY",
ifelse(rules_data$Description == "SET OF 4 KNICK KNACK TINS DOILEY","SET OF 4 KNICK KNACK TINS DOILY",        
ifelse(rules_data$Description == "SET OF 4 KNICK KNACK TINS LEAF","SET OF 4 KNICK KNACK TINS LEAVES", 
ifelse(rules_data$Description == "SET/5 RED RETROSPOT LID GLASS BOWLS","SET/5 RED SPOTTY LID GLASS BOWLS",
ifelse(rules_data$Description == "SILVER/BLACK ORBIT NECKLACE", "SILVER AND BLACK ORBIT NECKLACE",
ifelse(rules_data$Description == "SILVER/MOP ORBIT NECKLACE", "SILVER M.O.P. ORBIT NECKLACE",
ifelse(rules_data$Description == "SMALL DECO JEWELLERY STAND","SMALL JEWELLERY STAND",
ifelse(rules_data$Description == "SMALL PARLOUR FRAME","SMALL PARLOUR PICTURE FRAME",
ifelse(rules_data$Description == "SMALL POP BOX FUNKY MONKEY","SMALL POP BOX,FUNKY MONKEY",
ifelse(rules_data$Description == "SMOKEY GREY COLOUR D.O.F. GLASS", "SMOKEY GREY COLOUR GLASS",
ifelse(rules_data$Description == "SPACE BOY CHILDRENS CUP", "SPACEBOY CHILDRENS CUP",
ifelse(rules_data$Description == "SPACEBOY MINI RUCKSACK", "SPACEBOY MINI BACKPACK",
ifelse(rules_data$Description == "SQUARECUSHION COVER PINK UNION FLAG", "SQUARECUSHION COVER PINK UNION JACK",
ifelse(rules_data$Description == "STORAGE TIN VINTAGE DOILEY","STORAGE TIN VINTAGE DOILY",
ifelse(rules_data$Description == "STRAWBERRY CERAMIC TRINKET BOX","STRAWBERRY CERAMIC TRINKET POT",
ifelse(rules_data$Description == "SWEETHEART CAKESTAND 3 TIER","SWEETHEART 3 TIER CAKE STAND", 
ifelse(rules_data$Description == "SWISS ROLL TOWEL, PINK  SPOTS","SWISS ROLL TOWEL PINK  SPOTS", 
ifelse(rules_data$Description == "TRAVEL CARD WALLET RETRO PETALS", "TRAVEL CARD WALLET VINTAGE LEAF",
ifelse(rules_data$Description == "TUMBLER, BAROQUE", "TUMBLER BAROQUE",
ifelse(rules_data$Description == "TUMBLER, NEW ENGLAND", "TUMBLER NEW ENGLAND",
ifelse(rules_data$Description == "TUSCAN VILLA DOVECOTE", "TUSCAN VILLA DOVECOTE BIRD FEEDER",
ifelse(rules_data$Description == "VINTAGE  2 METRE FOLDING RULER", "VINTAGE  2 METER FOLDING RULER",
ifelse(rules_data$Description == "VINTAGE ENGRAVED HEART", "VINTAGE EMBOSSED HEART",
ifelse(rules_data$Description == "VIPPASSPORT COVER", "VIP PASSPORT COVER",  rules_data$Description ))))))))))))))))))))))))))))))))))))))))))))))))

rules_data$Description <- ifelse(rules_data$Description == "WALL ART BICYCLE SAFTEY", "WALL ART BICYCLE SAFETY",
ifelse(rules_data$Description == "WALL ART,ONLY ONE PERSON", "WALL ART ONLY ONE PERSON",
ifelse(rules_data$Description == "WHITE WIRE PLANT POT HOLDER", "WHITE HEARTS WIRE PLANT POT HOLDER",
ifelse(rules_data$Description == "WHITE METAL LANTERN", "WHITE MOROCCAN METAL LANTERN",
ifelse(rules_data$Description == "WOODLAND MINI RUCKSACK", "WOODLAND MINI BACKPACK", 
ifelse(rules_data$Description == "WRAP RED DOILEY", "WRAP RED VINTAGE DOILY"  ,
ifelse(rules_data$Description == "WRAP VINTAGE PETALS  DESIGN", "WRAP VINTAGE LEAF DESIGN",
ifelse(rules_data$Description == "ZINC PLANT POT HOLDER" ,"ZINC HEARTS PLANT POT HOLDER",
ifelse(rules_data$Description == "ZINC  STAR T-LIGHT HOLDER", "ZINC STAR T-LIGHT HOLDER",
ifelse(rules_data$Description == "ZINC T-LIGHT HOLDER STAR LARGE", "ZINC T-LIGHT HOLDER STARS LARGE", rules_data$Description ))))))))))

str(rules_data)
# 529782

## Formatting the data ################################################################

library(plyr)
# Use ddply function to get all the items bought together in a row separated by ,.
# To get the items bought together, get Description by grouping the data on InvoiceNo.
Association_data <- ddply(rules_data,c("InvoiceNo"),
                            function(x)paste(x$Description,
                            collapse = ","))
str(Association_data)

Association_data$InvoiceNo <- NULL
colnames(Association_data) <- c("items")
Association_data$items <- as.factor(Association_data$items)

str(Association_data)

# To view the transactional_data in the better format
write.csv(Association_data, "transactional_data.csv", quote=FALSE, row.names = FALSE)

## Building Association rules ########################################################

# convert data to transactional data
library(arules)
transactional_data <- read.transactions('transactional_data.csv', format = 'basket', sep=',', quote="")

rules.001 <- apriori(transactional_data, parameter=list(support=0.005, confidence = 0.8))

rules.02 <- apriori(transactional_data, parameter=list(support=0.02, confidence = 0.1))

inspect(rules.02)

rules.01 <- apriori(transactional_data, parameter=list(support=0.01, confidence = 1.0))

inspect(sort(rules.01, by='confidence'))

rules.009 <- apriori(transactional_data, parameter=list(support=0.009, confidence = 1.0))

inspect(sort(rules.009, by='lift'))

rules <- sort(rules.009, by='lift')

# Prune the Redundant Rules ####################################################

# Check for the rules which are subset of other rules
subset_matrix <- is.subset(rules,rules)
subset_matrix

# Assigning FALSE to diagonal line (as every rule is a subset of it's own, neglect diagonal positions)
subset_matrix[lower.tri(subset.matrix, diag=T)] <- F
subset_matrix

# Check the item sets which are redudant and which are not
redundant_check <- apply(subset.matrix, 2, any)
redundant_check

rules.pruned <- rules[!redundant]
inspect(rules.pruned)
        
### Evaluate the rules ########################################################

# We evaluate the association rules with support, confidence, lift values
interestMeasure(rules.pruned, c("support", "confidence", "lift"), transactional_data)

inspect(sort(rules.pruned, by='support'))

install.packages("arulesViz")
library(grid)
library(arulesViz)

plot(rules.pruned, method="graph")
