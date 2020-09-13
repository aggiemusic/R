#############################################
#                                           #
# Author:     Ashley Music                  #
# Date:       04/05/2020                    #
# Subject:    Project 4                     #
# Class:      BDAT 640                      #
# Section:    01W 20/SP2                    #         
# Instructor: Chris Shannon                 #
# File Name:  Project4_Music_Ashley.R       #
#                                           #
#############################################


# 1.1 1.	Read the dataset in Boston.csv into R. 
# Call the loaded data Boston. 
# Make sure that you have the directory set to the correct location for the data.
library(readr)
Boston <- read_csv("Boston.csv")
View(Boston)
setwd("C:/Users/aggie/Desktop/Forecasting and Modeling")


# 2.1 The response is nox and the predictor is dis. 
# Use the poly() function to fit a cubic polynomial regression to predict nox using dis.
# Report the regression output.

lm3 =lm(nox ~ poly(dis,3), data = Boston)
summary(lm3)
# Answer: 
# The cubic polynomial regression shows signicance for all models 
# The p-value for the t value is less than .05 

# 3.1 Predict nox using dis as a predictor. 
# Use models from degree 5, degree 4, and degree 3, and degree 2 polynomial regression. 
# Perform cross-validation using caret package to select the optimal degree for the polynomial. 
# Justify your answer.
# Answer: 
# The best degree to choose is 3. This is due to the lowest RSME of .0619
library(caret)
train_control = trainControl(method = "CV", number = 10)
set.seed(1)

cv2 = train(nox ~ poly(dis,2), data = Boston, trControl = train_control, method = "lm")
print(cv2)

cv3 = train(nox ~ poly(dis,3), data = Boston, trControl = train_control, method = "lm")
print(cv3)

cv4 = train(nox ~ poly(dis,4), data = Boston, trControl = train_control, method = "lm")
print(cv4)

cv5 = train(nox ~ poly(dis,5), data = Boston, trControl = train_control, method = "lm")
print(cv5)


# 4.1 Perform the following GAM analysis.
# Predict nox using a smoothing spline of degree 3 in dis and a smoothing spline of degree 2 in medv.
# Predict nox using a smoothing spline of degree 2 in dis and a smoothing spline of degree 1 in medv.
# Perform anova analysis. Recommend the best model and justify your answer.
# Answer: 
# The best model is gam2. The pvalue for this model is significant and less than .05. 
# ANOVA shows significant deviance in the two models.   

library(gam)
gam1 = gam(nox ~ s(dis,3) + s(medv, 2), data = Boston)
summary(gam1)
gam2 = gam(nox ~ s(dis,2) + s(medv, 1), data = Boston)
summary(gam2)
anova(gam2, gam1)











