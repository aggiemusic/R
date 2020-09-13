#############################################
#                                           #
# Author:     Ashley Music                  #
# Date:       04/13/2020                    #
# Subject:    Project 6                     #
# Class:      BDAT 640                      #
# Section:    01W 20/SP2                    #         
# Instructor: Chris Shannon                 #
# File Name:  Project6_Music_Ashley.R       #
#                                           #
#############################################

# 1.1 Load the dataset bike.csv into memory. 
# Convert holiday to a factor using factor() function. 
# Split the data into training set containing 2/3 of the original data 
# Test set containing remaining 1/3 of the original data

library(readr)
Bike <- read_csv("C:/Users/aggie/Desktop/Forecasting and Modeling/Bike.csv")
View(Bike)
library(e1071)
Bike$holiday = factor(Bike$holiday, levels = c(0,1), labels = c("No_Holiday", "Holiday"))
Bike$workingday = factor(Bike$workingday, levels = c(0,1), labels = c("No_Work", "Working_Day"))
Bike$season = factor(Bike$season, levels = c(1,2,3,4), labels = c("Spring", "Summer", "Fall", "Winter"))


set.seed(1)
train <- sample(1:nrow(Bike),
                2*nrow(Bike)/3)



# 2.1 Build a support vector machine model. 
# A. Response is holiday and the predictors are: season, workingday, casual, and registered. 
# Use svm() function with radial kernel and gamma=10 and cost = 100.
set.seed(1)
svm.fit = svm(holiday ~ season + workingday + casual + registered, data = Bike[train,], kernel = "radial", gamma = 10, cost = 100)
summary(svm.fit)

# B. Perform a grid search to find the best model with potential cost: 1, 10, 50, 100  
# potential gamma: 1, 3, and 5 and using radial kernel and training dataset.
set.seed(1)
tune.out = tune(svm, holiday ~ season + workingday + casual + registered, data = Bike[train,], kernel = "radial", 
                ranges = list(cost = c(1, 10, 50, 100), gamma = c(1, 3, 5)))

# C. Print out the model results. What's the best model parameters?
# Answer: The best model parameters are cost of 50 and gamma of 3 
summary(tune.out)

# D. Forecast holiday using the test dataset and the best model found in c).
set.seed(1)
svm.fit2 = svm(holiday ~ season + workingday + casual + registered, data = Bike[-train,], kernel = "radial", gamma = 1, cost = 100)
summary(svm.fit2)

# E. Get the true observations of holiday in the test dataset.

trueObservation = Bike[-train, "holiday"]



# F. Compute the test error by constructing the confusion matrix. Is it a good model?
# Answer: Yes, this is a good model as shown by the confusion matrix. 
# This model has a 97.13% accuracy, 50% sensitivity and 97.36% specificity 
pred = predict(tune.out$best.model, newdata = Bike[-train, ])
table(trueObservation$holiday, pred)


TN = 3530
TP = 7
FN = 2
FP = 90
accuracy = (TP + TN)/(TN + TP + FN + FP)
accuracy 
sensitivity = TP/(TP + FN)
sensitivity
specificity = TN / (TN + FP)
specificity














                         
