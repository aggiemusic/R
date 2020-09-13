#############################################
#                                           #
# Author:     Ashley Music                  #
# Date:       04/19/2020                    #
# Subject:    Project 7                     #
# Class:      BDAT 640                      #
# Section:    01W 20/SP2                    #         
# Instructor: Chris Shannon                 #
# File Name:  Project7_Music_Ashley.R       #
#                                           #
#############################################



# 1.1. Load the dataset CreditCards.csv into memory.
library(readr)
CreditCards <- read_csv("C:/Users/aggie/Desktop/Forecasting and Modeling/CreditCards.csv")
View(CreditCards)
library(cluster)
library(factoextra)


# 2.1 Perform the k-means cluster analysis  
# a. Remove the first column: CUST_ID since it doesn't provide any info for cluster.
CC <- CreditCards[,-1]
head(CC)
#scale the data 
scaledCC <- scale(CC)



# b. Determine the optimal number of clusters. Justify your answer. 
set.seed(1)
fviz_nbclust(scaledCC, kmeans, iter.max = 1000, method = "gap_stat")



# c. Perform k-means clustering using the optimal number of clusters.
km.res <- kmeans(scaledCC, 7, nstart = 25)



# d. Visualize the clusters in different colors.
fviz_cluster(km.res, data = scaledCC, geom = "point")



