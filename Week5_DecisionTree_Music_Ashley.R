#############################################
#                                           #
# Name:       Ashley Music                  #
# Date:       07/28/2020                    #
# Subject:    Predict Botson Housing        #
# Class:      BDAT 625                      #
# File Name:                                #
# Week5_DecisionTree_HW_Music_Ashley        # 
#                                           #
#############################################
setwd("C:/Users/aggie/Desktop/Data Mining")
rm(list = ls())
library(readr)
FlightDelays <- read_csv("C:/Users/aggie/Desktop/Data Mining/FlightDelays.csv")
View(FlightDelays)

FlightDelays <- as.data.frame(FlightDelays)



# Transform variable day of week (DAY_WEEK) info a categorical variable.
FlightDelays$DAY_WEEK <- factor(FlightDelays$DAY_WEEK)

# Bin the scheduled departure time into eight bins (in R use function cut()).
options(scipen=999)
FlightDelays$CRS_DEP_TIME <- cut(FlightDelays$CRS_DEP_TIME, 8, include.lowest = FALSE, dig.lab = 8 )

# Use these and all other columns as predictors (excluding DAY_OF_MONTH).
#Also excluding actual departure time, FL Num and Tail Num.  
selected.var <- c(1:2, 4:6,8:10,13)
colnames(FlightDelays)

# Partition the data into training and validation sets.
train.index <- sample(c(1:dim(FlightDelays)[1]), dim(FlightDelays)[1]*0.6)
train.df <- FlightDelays[train.index, selected.var]
valid.df <- FlightDelays[-train.index, selected.var]
dim(train.df)
dim(valid.df)

# Fit a classification tree to the flight delay variable using all the relevant predictors. 
install.packages('rpart')
install.packages('adabag')
library(rpart)
library(adabag) 
library(caret)
library(ggplot2)
install.packages('rpart.plot')
library(rpart.plot)


default.ct <- rpart(`Flight Status` ~ . , data = train.df, method = "class")
prp(default.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10)


# Use a pruned tree with maximum of 8 levels, setting cp = 0.001. Express the resulting tree as a set of rules.
library(caret)


#I have tried prune three different ways. Appears to work best with prune 
FlightPruned <- rpart(`Flight Status` ~ ., data = train.df, method = "class", maxdepth = 8) 
printcp(FlightPruned)

pruned.ct <- prune(FlightPruned, cp = 0.001)
prp(pruned.ct, type = 1,
    box.col=ifelse(pruned.ct$frame$var == "<leaf>", 'gray', 'white')) 

#Decision Rules: 
summary(pruned.ct)
print(pruned.ct)


# Tell me:  If you needed to fly between DCA and EWR on a Monday at 7:00 AM, would you be able to use this tree? What other information would you need? Is it available in practice? What information is redundant?
# No, I would not be able to use this tree. I would need to know the date the flight was departing in order to use this tree. 


# Fit the same tree as you did initially, this time excluding the Weather predictor. Display both the pruned and unpruned tree. You will find that the pruned tree contains a single terminal node. 


#Full Tree 
train.df.2 <- train.df[ , -7]

default.ct.2 <- rpart(`Flight Status` ~ . , data = train.df.2, method = "class")
prp(default.ct.2, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10)



#Pruned Tree 
set.seed(1)
default.ct.2.p <- rpart(`Flight Status` ~ ., data = train.df.2, method = "class", maxdepth = 8)  
printcp(default.ct.2.p)

pruned.noweather <- prune(default.ct.2.p , cp = 0.001 )
prp(pruned.noweather, type = 1,
    box.col=ifelse(pruned.ct$frame$var == "<leaf>", 'gray', 'white'))





