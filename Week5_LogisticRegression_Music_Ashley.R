#############################################
#                                           #
# Name:       Ashley Music                  #
# Date:       07/28/2020                    #
# Subject:    Predict Botson Housing        #
# Class:      BDAT 625                      #
# File Name:                                #
# Week5_LogisticRegression_HW_Music_Ashley  # 
#                                           #
#############################################
setwd("C:/Users/aggie/Desktop/Data Mining")
rm(list = ls())

library(readr)
eBayAuctions <- read_csv("eBayAuctions.csv")
View(eBayAuctions)
eBayAuctions <- as.data.frame(eBayAuctions)


# Create dummy variables for the categorical predictors. 
# These include Category (18 categories), Currency (USD, GBP, Euro), EndDay (Monday-Sunday), and Duration (1, 3, 5, 7, or 10 days). 
library(dummies)
eBayDummy <-  dummy.data.frame(eBayAuctions, names = c("Category", "currency", "endDay", "Duration"), sep = ".")
names(eBayDummy)
head(eBayDummy)


# Partition the data into training and validation sets.
set.seed(2)
train.index <- sample(c(1:dim(eBayDummy)[1]), dim(eBayDummy)[1]*0.6)  
train.df <- eBayDummy[train.index, ]
valid.df <- eBayDummy[-train.index, ]
dim(train.df)
dim(valid.df)
View(train.df)
View(valid.df)




# Create pivot tables for the mean of the binary outcome (Competitive?) as a function of the various categorical variables (use the original variables, not the dummies).  
# Use the information in the tables to reduce the number of dummies that will be used in the model. 

?prob.table

prop.table(table(eBayAuctions$`Competitive?`, eBayAuctions$Category, eBayAuctions$currency, eBayAuctions$Duration, eBayAuctions$endDay), margin = 1)

#The majority of the categories can be combined. I would combine Computer and Electronics, Coins/Stamps and Collectibles, Pottery/Glass and Antique/Art/Craft 


# Run a logistic model with all predictors as above, excluding price.
train.df.noprice <- train.df[, -c(35)]
valid.df.noprice <- valid.df[ , -c(35)]
View(valid.df.noprice)

logit.reg <- glm(`Competitive?` ~ ., data = train.df.noprice, family = "binomial" )
options(scipen = 999)
summary(logit.reg)

#Running predictions on the model with cost excluded 
logit.reg.pred <-predict(logit.reg, valid.df.noprice[, -36], type = "response")
data.frame(actual = valid.df$`Competitive?`[1:5], predicted = logit.reg.pred[1:5])


#Running the glm on the full model that includes cost 
logit.full <-glm(`Competitive?` ~., data = train.df, family = "binomial")
options(scipen = 999)
summary(logit.full)
#Running the predictions for full model that includes cost 
logit.reg.pred.2 <-predict(logit.full, valid.df[, -37], type = "response")
data.frame(actual = valid.df$`Competitive?`[1:5], predicted = logit.reg.pred.2[1:5])


# Use stepwise selection (use function step() in the stats package or function stepAIC() in the MASS package)
# and an exhaustive search (use function glmulti() in package glmulti to find the model with the best fit to the training data.
# Which predictors are used?
eBay.stepback <- step(logit.full, direction = "both")
summary(eBay.stepback)
install.packages('glmulti')
library(glmulti)
library(leaps)
library(rJava)
?glmulti


# exhaustive.search <- glmulti(y = train.df$`Competitive?`, xr = train.df[ , c(1:34, 36)]  , data = train.df, method = "h", fitfunction = "glm", family = binomial)
# 
# search <- glmulti(logit.full, data = train.df)
# 
search <- glmulti(logit.full, data = train.df)
# Error in glmulti(y = `Competitive?` ~ `Category.Antique/Art/Craft` + Category.Automotive +  : 
#                    formal argument "data" matched by multiple actual arguments
# 


# Use stepwise selection and an exhaustive search to find the model with the lowest predictive error rate (use the validation data). Which predictors are used? 
logit.full.valid <-glm(`Competitive?` ~., data = valid.df, family = "binomial")
options(scipen = 999)
summary(logit.full.valid)

eBay.stepback.valid <- step(logit.full.valid , direction = "both")
summary(eBay.stepback.valid )





