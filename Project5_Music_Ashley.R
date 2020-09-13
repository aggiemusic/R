#############################################
#                                           #
# Author:     Ashley Music                  #
# Date:       04/12/2020                    #
# Subject:    Project 5                     #
# Class:      BDAT 640                      #
# Section:    01W 20/SP2                    #         
# Instructor: Chris Shannon                 #
# File Name:  Project5_Music_Ashley.R       #
#                                           #
#############################################


# 1.1 
# Load the dataset bike.csv into memory. 
library(readr)
setwd("C:/Users/aggie/Desktop/Forecasting and Modeling")
Bike <- read_csv("Bike.csv")
View(Bike)
BikePairs <- Bike[ ,-1]
pairs(BikePairs)
#season, holiday, workingday, weather appear to be factors based on plots
#creating factors 
Bike$season = factor(Bike$season, levels = c(1,2,3,4), labels = c("Spring", "Summer", "Fall", "Winter"))
Bike$holiday = factor(Bike$holiday, levels = c(0,1), labels = c("No_Holiday", "Holiday"))
Bike$workingday = factor(Bike$workingday, levels = c(0,1), labels = c("No_Work", "Working_Day"))
Bike$weather = factor(Bike$weather, levels = c(1,2,3,4), labels = c("Very_Good", "Good", "Fair", "Poor"))

# 1.2 Split the data into a training set containing 2/3 of the original data 
# Test set containing remaining 1/3 of the original data. 

train <- sample(1:nrow(Bike),
                2*nrow(Bike)/3)
test <- (1:nrow(Bike))[-train]



# 2.1 Build a tree model using function tree(). 
# A. The response is count, predictors are season, holiday, workingday, temp, atemp, humidity, windspeed, casual, and registered.
library(tree)
set.seed(1)
tree.bike = tree(count ~ season + holiday + workingday + temp + atemp + humidity + windspeed + casual + registered, Bike, subset = train)
summary(tree.bike)
plot(tree.bike)
text(tree.bike, pretty = 0)

# B. Perform cross-validation to choose the best tree by calling cv.tree(). 
cv.bike = cv.tree(tree.bike)

# C. Plot the model results of b) and determine the best size of the optimal tree.
plot(cv.bike$size, cv.bike$dev, type =  "b")
# The optimal size is 4.  This is the bend in the elbow. 
# It has a low error but is not as complicated as trees with more nodes (5-8).

# D. Prune the tree by calling prune.tree() function with the best size found in c).
prune.bike = prune.tree(tree.bike, best = 4)

# E. Plot the best tree model.
plot(prune.bike)
text(prune.bike, pretty = 0)

# F. Compute the test error using the test data set.
# Answer: Test error is 4682.476

yhat = predict(prune.bike, newdata = Bike[test, ])
Bike.test = Bike[test, "count"]
mean((yhat - Bike.test$count)^2)


# 3.1 Build a random forest model using function randomForest() 
library(randomForest)
# A. The response is count and the predictors are season, holiday, workingday, temp, atemp, humidity, windspeed, casual, and registered.
set.seed(1)
rf.bike = randomForest(count ~ season + holiday + workingday + temp + atemp + humidity + windspeed + casual + registered, Bike, subset = train, importance = TRUE) 
rf.bike

# B. Compute the test error using the test data set.
# Answer: Test error is 108.2742
yhat.rf = predict(rf.bike, newdata = Bike[test, ])
Bike.test = Bike[test, "count"]
mean((yhat.rf - Bike.test$count) ^ 2)

# C. Extract variable importance measure using importance() function.
importance(rf.bike)

# D. Plot the variable importance using function varImpPlot(). Which are the top 2 important predictors in this model?
# Answer: Registered and casual are the two most important variables. 
varImpPlot(rf.bike)









