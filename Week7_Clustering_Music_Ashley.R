#############################################
#                                           #
# Name:       Ashley Music                  #
# Date:       08/10/2020                    #
# Subject:    NN                            #
# Class:      BDAT 625                      #
# File Name:                                #
# Week7_Clustering_Music_Ashley             # 
#                                           #
#############################################

setwd("C:/Users/aggie/Desktop/Data Mining")
rm(list = ls())

library(readr)
EastWestAirlinesCluster <- read_csv("EastWestAirlinesCluster.csv")
View(EastWestAirlinesCluster)
EastWestAirlines <- as.data.frame(EastWestAirlinesCluster)



# Apply hierarchical clustering with Euclidean distance and Ward's method. Make sure to normalize the data first. How many clusters appear?


#set row names to ID #
row.names(EastWestAirlines) <- EastWestAirlines[,1]


#removing the ID column 
EastWestAirlines  <- EastWestAirlines[,-1]
head(EastWestAirlines)



#normalize the input variables 
EastWestAirlines.norm <- sapply(EastWestAirlines, scale)
head(EastWestAirlines.norm)

#add row names to normalized data 
row.names(EastWestAirlines.norm)<- row.names(EastWestAirlines)

#Compute normalized distance based on input variables 
d.norm <- dist(EastWestAirlines.norm, method = "euclidean")


#Running hierarchical clustering and generating dendrogram 
# Best cluster is 5
hc1 <- hclust(d.norm, method = "ward.D")
plot(hc1, cex = 0.6, hang = -1)

memb <- cutree(hc1, k = 5)
table(memb)


row.names(EastWestAirlines.norm) <- paste(memb, ":", row.names(EastWestAirlines), sep = "")
heatmap(as.matrix(EastWestAirlines.norm), Colv = NA, hclustfun = hclust, col = rev(paste("gray", 1:99, sep = "")))

# Compare the cluster centroid to characterize the different clusters, and try to give each cluster a label.
d.norm.centroid <- dist(EastWestAirlines.norm, method = "euclidean")
hc2 <- hclust(d.norm.centroid, method = "centroid")
plot(hc2, cex = 0.6, hang = -1)

memb2 <- cutree(hc2, k = 6)
table(memb2)


# Check the stability of the clusters, by removing a random 5% of the data (by taking a random sample of 95% of the records), and repeat the analysis. Does the same picture emerge?
set.seed(111)
train.index <- sample(row.names(EastWestAirlines.norm), 0.95*dim(EastWestAirlines.norm)[1])
train.df <- EastWestAirlines.norm[train.index, ]
head(train.df)




d.norm.2 <- dist(train.df, method = "euclidean")
d.norm.2


hc3 <- hclust(d.norm.2, method = "ward.D")
plot(hc3, cex = 0.6, hang = -1)

memb3 <- cutree(hc3, k = 5)
table(memb3)


hc4 <- hclust(d.norm.2, method = "centroid")
plot(hc4, cex = 0.6, hang = -1)

memb4 <- cutree(hc4, k = 5)
table(memb4)


# Use k-means clustering with the number of clusters that you found above. Does the same picture emerge?

km <- kmeans(train.df, 5)
print(km)


km$cluster
head(km$cluster, 10)
km$centers

install.packages("factoextra")
library(factoextra)

fviz_cluster(list(data = train.df, cluster = memb3 ))


# Tell me: Which clusters would you target for offers, and what types of offers would you target to customers in that cluster?
#Cluster 1 and 5