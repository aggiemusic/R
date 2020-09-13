#############################################
#                                           #
# Author:     Ashley Music                  #
# Date:       03/13/2020                    #
# Subject:    Project 1                     #
# Class:      BDAT 640                      #
# Section:    01W 20/SP2                    #         
# Instructor: Chris Shannon                 #
# File Name:  Project1_Music_Ashley.R       #
#                                           #
#############################################

# 1.1  - Read the dataset into R
#   Call the loaded data Boston 
library(readr)
boston <- read_csv("Boston.csv")
View(boston)

# 1.2 Make sure that you have the directory set 
#     to the correct location for the data.
setwd("C:/Users/aggie/Desktop/Forecasting and Modeling")


#2.1 How many rows are in the data frame?
# Answer: 506
nrow(boston)

#2.2 How many columns are in the data frame? 
# Answer: 14 
ncol(boston)

# 2.3 What do the rows and columns represent?
# Answer: Each row represents a real estate suburb in Boston. 
# Each column represents characteristics for each suburb
# such as: (crim) crime rate, (rm) avg # of rooms, (tax) property tax rate per 10K
#(ptratio) pupil-teacher ratio by town

# 3.1 Select the 1st, 10th, and 500th row  
#  with columns tax and medv 
#  Answer: 
# A tibble: 3 x 2
#tax  medv
#1   296  24  (1st)
#2   311  18.9 (10th)
#3   391  17.5 (500th)
boston[c(1, 10, 500),c("tax","medv")]

#4.1 Look at the data using cor function. 
#Are any of the predictors associated with per capita crime rate? 
#If so, explain the relationship based on correlation coefficents.
#Below code transforms the boston data to a log since not all of the data is linear
boston.log <- boston
boston.log$crim <- log(boston.log$crim)
cor(boston.log)
round(cor(boston.log,boston.log$crim),3)
#Answer
#Strong positive Crime and Indus   .731 (proportion of non-retail business acres per town)
#Strong positive Crim and Nox  .789 (nitrogen oxides concentration per parts per 10 mil) 
#Strong positive Crim and Rad .853  (proximity to a radial highway) 
#Strong positive Crim and Tax .828 (full value property tax rate per $10K)
#Moderate negative Crim and dis -.682 (mean distances to 5 Boston employment centers.) 
####So this means as crime goes up the mean distance to an employment center goes down


#5.1 Make some pairwise scatterplots of the predictors, crim, rad, tax, indus, and lstat 
#Describe your findings.
pairsPlot = boston.log[, c("crim","rad", "tax", "indus", "lstat")]
pairs(pairsPlot)
#Answer: We see lots of flat and horizonal lines suggesting the linear correlation is not defined.
#Strongest linear correlation appears to be between tax and indus.
#and lstat and crim 


#6.1 Do any of the suburbs of Boston appear to have particularly high crime rates
#by looking at the histogram of crim? 
crimeRate = boston$crim
hist(crimeRate, labels = TRUE)
#transformed data - creating a non-linear scale for this type of data
#The number on the axis represents the log number. 
boston.log$crim <- exp(log(boston.log$crim))
hist(boston.log$crim, labels = TRUE)
#Yes, the histogram does show that some suburbs are impacted by crime
#6.2 What is the range of crime? 
range(boston$crim)
#min crime rate is .00632  and max crime rate is 88.9762

#7.1 How many suburbs bound the Charles River?
charlesRiver = boston$chas
hist(charlesRiver, labels = TRUE)  
#using a histogram to count and adding labels. 1 is the dummy variable for the Charles River.
#There are 35 suburbs that bound the river 


#8.1 What is the median pupil-teach ratio? 
median(ptratio) 
#Answer: 19.05 is the median pupil to teacher ratio 
mean(ptratio)
#Answer: 18.45 is the mean pupil to teacher ratio 

#9.1 - How many suburbs average more than 7 rooms per dwelling and more than 8? 
#using a histogram to count and adding labels to count
rooms = boston$rm
hist(rooms, labels = TRUE)
#  Answer: 64 suburbs average more than 7 rooms per dwelling
#  Answer: 13 suburbs average more than 8 rooms per dwelling

#9.2 - Comment on the suburbs that average more than 8 rooms per dwelling
#The histogram shows that in the bucket for rooms between 8-9 we see 13 suburbs listed. 
#This is just a little less than the suburbs that have 4-5 rooms per dwelling. 
#It is more than suburbs with 2 rooms per dwelling, but a lot less than suburbs with 6-7 rooms. 
#using subset we can look at the individual characteristics for suburbs with more than 8 rooms.
more_than_8 <- subset(boston, rm >= 8)
more_than_8
#The crime rate is generally low (most under 1). 
#Only 2 bound the river.
#The pupil to teacher ratio is as low as 13 and as high as 20.2. 
#Appears that tax rate per $10K is pretty similar
#With the outlier being 666 for the last suburb in the list.

#10.1 - Convert chas to a factor 
boston$chas = factor(boston$chas)

#10.2 - Boxplot chas vs medv 
plot(boston$chas, boston$medv, xlab = "chas", ylab = "medv")

#10.3 - Are houses around the Charles River more expensive?
# Answer: Yes, homes that bound the river are more expensive (on average)












