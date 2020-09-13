#############################################
#                                           #
# Author:     Ashley Music                  #
# Date:       03/23/2020                    #
# Subject:    Project 1                     #
# Class:      BDAT 640                      #
# Section:    01W 20/SP2                    #         
# Instructor: Chris Shannon                 #
# File Name:  Project3_Music_Ashley.R       #
#                                           #
#############################################

# Part 1 

# 1.1 Load the dataset mtcars.xlsx into memory 
library(readxl)
mtcars <- read_excel("mtcars.xlsx")
View(mtcars)

# and convert column am to a factor using factor() function.
mtcars$am = factor(mtcars$am, levels = c(0,1), labels = c("automatic", "manual"))





# 2.1 Split the data into training set and test set. 
# The training set contains the first 35 observations 
# Answer: 
train <- mtcars[1:35, ]
train
# the test set containing the remaining observations.
#Answer: 
test <- mtcars[36:41, ]
test



# 3.1 Build a logistic regression model 
# with the response is am and the predictors are mpg, cyl, hp, and wt 
# using glm() function
attach(mtcars)
glm.fit <- glm(am ~ wt + hp + cyl + mpg, data= mtcars, family = binomial) 
summary(glm.fit)
# Answer: 
# The p-value for the z-value only shows to be significant for wt (.0302) 
# in this regression model at least one regression coefficient is not equal to 0.



# 4.1 Compute the test error on the test data set using a confusion matrix. 
# Computing the test error of the test data 
pred <- predict(glm.fit, newdata = test, type = "response")
# Transforming the values into 0 and 1 
length(test)
pred.values <- rep(0,12)
pred.values[pred > 0.5] <- 1

#Creating the confusion matrix 
resp <- factor(pred.values, levels = c(0,1), labels = c("N", "Y"))
confusion <- table(data.frame(Predicted = resp, Reference = test$am))
confusion 

#Accuracy: 
Accuracy <- (confusion[1,1] + confusion[2,2])/sum(confusion)
Accuracy
#The confusion matrix shows 100% accuracy 

#Sensitivity: 
sens <- (confusion[2,2]) / (confusion[2,2] + confusion[2,1])
sens
# Shows 100% sensitivity 

# Specificity: 
spes <- (confusion[1,1]) / (confusion[1,1] + confusion[1,2])
spes
# Shows 100% specificity 


# Is it a good model based on test error?
# Answer: 
# Based on the test error this is a good model. Shows 100% accuracy, specificity and sensitivity. 




# Part 2 

Bike <- read.csv("C:/Users/aggie/Desktop/Forecasting and Modeling/Bike.csv")
View(Bike)

# 1.1 Build a linear model to forecast number of total rentals (count) 
# using potential predictors, season, holiday, workingday, weather, atemp, and registered.
# First, converting to factors for season, holiday, working day and weather? 
Bike$season = factor(Bike$season, levels = c(1,2,3,4), labels = c("Spring", "Summer", "Fall", "Winter"))
Bike$holiday = factor(Bike$holiday, levels = c(0,1), labels = c("No_Holiday", "Holiday"))
Bike$workingday = factor(Bike$workingday, levels = c(0,1), labels = c("No_Work", "Working_Day"))
Bike$weather = factor(Bike$weather, levels = c(1,2,3,4), labels = c("Very_Good", "Good", "Fair", "Poor"))


#Now, creating the lm with glm
bike2 <- Bike[ , c(2,3,4,5,7,11,12)]
bike2

glm.fit.bike <- glm(count ~ ., data = bike2)
summary(glm.fit.bike)

# Answer: 
# The p-values for the t-test show to be significant and less than .5 for all but weatherPoor (.931) 
# At least one of the regression coefficients is not equal to 0. 


# 2.1 Perform best subset selection using bestglm() function based on BIC. What’s the best model based on BIC?
library(leaps)
library(bestglm)
select.cols <- c("season", "holiday", "workingday", "weather", "atemp", "registered", "count")
bestglm.1 <- bestglm(bike2[,select.cols], IC = "BIC")
min(bestglm.1$BestModels$Criterion)
# The BIC reports a 77533.39 as the best model 
summary(bestglm.1)


#3.1 Compute the test error of the best model based on BIC using LOOCV.
loocv <- LOOCV(bike2[,c("season", "holiday", "workingday", "weather", "atemp", "registered")], 
               bike2[,"count"])
MSE.loocv <- loocv[1]
MSE.loocv
# Answer: 
# The test error using LOOCV is 1231.477 (MSE)


# 4.1 Calculate the test error of the best model based on BIC using 10-fold CV.

# Creating a model matrix to use CV 
myformula <- formula(paste("~", paste(select.cols, collapse = "+")))
mm<- model.matrix(myformula, data = bike2)[,-1]
mm <- as.data.frame(mm)
mm

cvhtf <- CVHTF(mm[,c(1,2,3,4,5,6,7,8,9,10)],
  mm[,c(11)], K = 10, family = gaussian)
cvhtf
MSE.cvhtf <- cvhtf[1]
MSE.cvhtf

#Answer: 
# The test error using 10 fold CV is 1232.385(MSE)


# 5.1 Perform best subset selection using bestglm() function based on CV. What’s the best model based on CV?
library(leaps)
library(bestglm)

bestglmcv <- bestglm(mm, IC = "CV")
bestglmcv
bestglmcv$Subsets  #10 has astrick 
bestglmcv$Subsets[10,c("CV")] #1240.906
summary(bestglmcv$BestModel)
# The best model is 10 with a 1240.906




#6.1 Perform the backward stepwise selection using stepAIC() function. What’s the best model?
library(MASS)
fit.c <- glm(count ~ ., data = bike2)
fit.best <- stepAIC(fit.c, scope = list(lower = ~ 1), direction = "backward")
summary(fit.best)
#Pseudo R2 
with(fit.best, 1 - deviance/null.deviance) 

# Answer: 
# The p-values for the t-value all show significant expect weater poor. 
# All p-values are less than .5. At least one of the regression 
# coefficients is not equal to 0. The R2 is strong at .962. 
                    



                    









