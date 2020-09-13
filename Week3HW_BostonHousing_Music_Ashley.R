#############################################
#                                           #
# Name:       Ashley Music                  #
# Date:       07/13/2020                    #
# Subject:    Predict Botson Housing        #
# Class:      BDAT 625                      #
# File Name:                                #
# Week3HW_BostonHousing__Music_Ashley.R     #
#                                           #
#############################################


# 1.Tell me why should the data be partitioned into training and validation sets? What will the training set be used for? What will the validation set be used for?
#Answer: Data should be partitioned so that we can run the algorithm on the training set and then run the algorithm again on the validation set to see how well the algorithm performed. 


# 2. Fit a multiple linear regression model to the median house price (MEDV) 
# as a function of CRIM, CHAS, and RM. 
# Write the equation for predicting the median house price from the predictors in the model.
# Answer: Price = Constant + a*CRIM + b*CHAS + c*RM or Price = 8.94021 + a * -0.24603 + b * 12.11572 + c * 1.77385


library(readr)
BostonHousing <- read_csv("C:/Users/aggie/Desktop/Data Mining/BostonHousing.csv")
View(BostonHousing)

#Splitting into 60% of the data for training and 40% for testing 
testBoston <- BostonHousing[1:303,] 
trainBoston <- BostonHousing[304:506,]

boston.lm <- lm(MEDV ~ CRIM + CHAS + RM, data = trainBoston)
options(scipen = 999)
summary(boston.lm)

library(forecast)
boston.lm.pred <- predict(boston.lm, testBoston)
options(scipen = 999, digits = 0)
some.residuals <- testBoston$MEDV[1:20] - boston.lm.pred[1:20]
data.frame("Predicted" = boston.lm.pred[1:20], "Actual" = testBoston$MEDV[1:20], "Residual" = some.residuals)
accuracy(boston.lm.pred, testBoston$MEDV)


# 3.Using the estimated regression model, what median house price is predicted for a tract in the Boston area that does not bound the Charles River, has a crime rate of 0.1, and where the average number of rooms per house is 6?
# Answer: Price = 8.94021 + 0.1 * -0.24603 + 0 * 12.11572 + 6 * 1.77385
# Price = $139,293


# 4.1 Which predictors are likely to be measuring the same thing among the 13 predictors?
# Discuss the relationships among INDUS, NOX, and TAX.
# LSAT, RAD, DIS and CRIM might be related in this data. 
# INDUS, NOX and TAX seem to imply that there the home is close to a business or industrial plant vs residential. INDUS is the proportion of nonretail business acres per town and NOX is the amount of nitirc oxcide concentration. TAX is about the full-value tax property. You would expect that these three predictors might be similar. 


# 4.2 Compute the correlation table for the 12 numerical predictors and search for highly correlated pairs. These have potential redundancy and can cause multicollinearity. Choose which ones to remove based on the above table.
# Answer: I would remove RAD and TAX because they are highly correlated to CRIM
# Answer: I would remove NOX and LSAT because they are highly correlated with INDUS

removeCATMEDV <- BostonHousing[-c(14)]

View(removeCATMEDV)

cor(removeCATMEDV)
pairs(removeCATMEDV)


#creating new training and test sets with those predictors removed 
bostonRemoved <- BostonHousing[-c(5,9,10,12,14)]
View(bostonRemoved)

trainBoston2 <- bostonRemoved[1:303,]
testBoston2 <- bostonRemoved[304:506,]


# Use stepwise regression with the three options (backward, forward, both) to reduce the remaining predictors as follows:
# Run stepwise on the training set. 
# Choose the top model from each stepwise run. 
# Then use each of these models separately to predict the validation set. 
# Compare RMSE, MAPE, and mean error, as well as lift charts. 
# Finally, describe the best model.
# Answer: The best model has predictors Age, RM, PTRATIO and DIS based off the stepwise selection. 
# These predictors have the highest significance according to all of the regression tests.

boston.lm2 <- lm(MEDV ~ ., data = trainBoston2)
options(scipen = 999)
summary(boston.lm2)



#Backwards stepwise 
boston.stepback <- step(boston.lm2, direction = "backward")
summary(boston.stepback)
# Top model for backwards run is with predictors RM, AGE, DIS, PTRATIO per the significant codes 
boston.lm.stepback.pred <- predict(boston.stepback, testBoston2) 
accuracy(boston.lm.stepback.pred, testBoston2$MEDV)




#Forward stepwise 
boston.lm.null <- lm(MEDV ~ 1, data = trainBoston2)
boston.lm.step <- step(boston.lm.null, scope = list(lower=boston.lm.null, upper = boston.lm2), direction = "forward")
summary(boston.lm.step)
#Best model based on forward stepwise is RM, PTRATIO, AGE and DIS based off the significance
bostonlm.step.pred3 <- predict(boston.lm.step, testBoston2)
accuracy(bostonlm.step.pred3, testBoston2$MEDV)




boston.lm.step2 <- step(boston.lm2, direction = "both")
summary(boston.lm.step2)
#Best model based on forward stepwise is RM, AGE, DIS and PTRATIO based off the significance
boston.lm.step2.pred <- predict(boston.lm.step2, testBoston2)
accuracy(boston.lm.step2.pred, testBoston2$MEDV)




#Lift Chart 
pred_v <- predict(boston.lm2, newdata = testBoston2)
install.packages("gains")
library(gains)
gain <- gains(testBoston2$MEDV[!is.na(pred_v)], pred_v[!is.na(pred_v)])

price <- testBoston2$MEDV[!is.na(testBoston2$MEDV)]

plot(c(0,gain$cume.pct.of.total*sum(price))~c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative Price", main="Lift Chart", type="l")




# Creating a LM with all of the predictors to run stepwise on
boston.lm.all <-lm(MEDV ~ ., data = trainBoston)
options(scipen = 999)
summary(boston.lm.all)

#stepback 
boston.stepback.all <- step(boston.lm.all, direction = "backward")
summary(boston.stepback)
# Top model for backwards run is with predictors RM, AGE, DIS and PTRATIO per the significant codes 
boston.lm.stepback.pred.all <- predict(boston.stepback.all, testBoston) 
accuracy(boston.lm.stepback.pred.all, testBoston$MEDV)


#Forward stepwise 
boston.lm.null.all <- lm(MEDV ~ 1, data = trainBoston)
boston.lm.step.all <- step(boston.lm.null.all, scope = list(lower=boston.lm.null.all, upper = boston.lm.all), direction = "forward")
summary(boston.lm.step.all)
#Best model based on forward stepwise is LSTAT, CRIM, DIS, CHAS, NOX and RAD  based off the significance
bostonlm.step.pred3.all <- predict(boston.lm.step, testBoston)
accuracy(bostonlm.step.pred3.all, testBoston$MEDV)


#Both 
boston.lm.step2.all <- step(boston.lm, direction = "both")
summary(boston.lm.step2.all)
#Best model based on forward stepwise is  based off the significance
boston.lm.step2.pred.all <- predict(boston.lm.step2.all, testBoston)
accuracy(boston.lm.step2.pred.all, testBoston$MEDV)


pred_v <- predict(boston.lm.all, newdata = testBoston)
install.packages("gains")
library(gains)
gain <- gains(testBoston$MEDV[!is.na(pred_v)], pred_v[!is.na(pred_v)])

price <- testBoston$MEDV[!is.na(testBoston$MEDV)]

plot(c(0,gain$cume.pct.of.total*sum(price))~c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative Price", main="Lift Chart", type="l")

