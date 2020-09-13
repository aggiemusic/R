#############################################
#                                           #
# Name:       Ashley Music                  #
# Date:       07/21/2020                    #
# Subject:    Predict Botson Housing        #
# Class:      BDAT 625                      #
# File Name:                                #
# Week4HW_Accidents_Music_Ashley            # 
#                                           #
#############################################
setwd("C:/Users/aggie/Desktop/Data Mining")
rm(list = ls())
library(readr)
AccidentsFull <- read_csv("C:/Users/aggie/Desktop/Data Mining/AccidentsFull.csv")
View(AccidentsFull)

accidents.df <- data.frame(AccidentsFull)
install.packages('e1071')
library(e1071)

#Converting MSX_SEV_IR to a factor with three levels. 0 is no injury and 1 and 2 are injury 
# accidents.df$MAX_SEV_IR  <- factor(accidents.df$MAX_SEV_IR ,    
#                                    levels=c(0,1, 2),
#                                    labels=c("No_Injury", "Injury", "Injury"))
# View(accidents.df)



accidents.df$INJURY<- ifelse(accidents.df$MAX_SEV_IR>0,"Yes","No")
injury_probs_table<- table(accidents.df$INJURY)
injury_probs_table

#Running for the full table 
total<- nrow(accidents.df)
Prob_Yes<- 21462/total
Prob_No<- 20721/total 
Prob_Yes
Prob_No
Compare<-cbind(Prob_Yes,Prob_No)
Compare



# Tell me, if an accident has just been reported and no further information is available, what should the prediction be? 
# (INJURY = Yes or No?) Why?
# I would classify this as no injury because out of the first 12 records we only see 3 with an injury and 9 with no injury. , but if you look at the full data set, then it looks like the probability of an accident is slightly higher. 


View(accidents.df[1:12, ])


# Select the first 12 records in the dataset and look only at the response (INJURY) and the two predictors WEATHER_R and TRAF_CON_R. Then:

col_names<-names(accidents.df)
accidents.df[,col_names]<-lapply(accidents.df[,col_names],factor)

accidents.subset<-accidents.df[1:12,c("INJURY","WEATHER_R","TRAF_CON_R")]




# Create a pivot table that examines INJURY as a function of the two predictors for these 12 records. 
#Use all three variables in the pivot table as rows/columns.
prop.table(table(accidents.subset$INJURY, accidents.subset$WEATHER_R, accidents.subset$TRAF_CON_R), margin = 1)




# Compute the exact Bayes conditional probabilities of an injury (INJURY = Yes) given the six possible combinations of the predictors.

# Probability (Injury=Yes|Weather_R=1, TraF_Con=0)
# .666%


# Probability (Injury=Yes|Weather_R=1, Traf_Con=1)
# 0% 


# Probability (Injury=Yes|Weather_R=1, Traf_Con=2)
#0%


# Probability (Injury=Yes|Weather_R=2, Traf_Con=0)
# .333%


# Probability (Injury=Yes| Weather_R=2, Traf_Con=1)
# 0%


# Probability (Injury=Yes|Weather_R=2, Traf_Con=2)
# 0%


# Classify the 12 accidents using these probabilities and a cutoff of 0.5.  
# Record 1 - .66% chance of injury 
# Record 2 - .33% chance of injury 
# Record 3 - 0% change of injury 
# Record 4 - 0% chance of injury 
# Record 5 - .66% chance of injury  
# Record 6 - .33 % chance of injury 
# Record 7 - .33% chance of injury 
# Record 8 - .66 % chance of injury 
# Record 9 - .33% chance of injury 
# Record 10 - .33% chance of injury
# Record 11 - .33% chance of injury
# Record 12 - 0% chance of injury 

# Compute manually the naive Bayes conditional probability of an injury given WEATHER_R = 1 and TRAF_CON_R = 1.
# 0% chance of injury 


# Then, Run a naive Bayes classifier on the 12 records and two predictors using R. Check the model output to obtain probabilities and classifications for all 12 records. 



accidents.nb <- naiveBayes(INJURY ~ WEATHER_R + TRAF_CON_R , data = accidents.subset)
accidents.nb

# Compare this to the exact Bayes classification. Are the resulting classifications equivalent? Is the ranking (= ordering) of observations equivalent?
# Yes, we have such a small number of records that the classifications are equivalent. 



# Partition the data into training (60%) and validation (40%).
set.seed(111)
train.index <- sample(row.names(accidents.df), 0.6*dim(accidents.df)[1])  
valid.index <- setdiff(row.names(accidents.df), train.index)  
train.df <- accidents.df[train.index, ]
valid.df <- accidents.df[valid.index, ]




# Tell me, assuming that no information or initial reports about the accident itself are available at the time of prediction (only location characteristics, weather conditions, etc.), 
# which predictors can we include in the analysis? (Use the Data_Codes sheet.)
# I think we should use HOUR_I_R (rush hour), ALCOHOL_I (alchol involved),WRK_ZONE (was it a work zone), INT_HWY (on the interstate), LGTCON_I_R (light conditions, dark, light,etc. ), MAN_COL_I (type of collision), SPD_LIM (Speed limit), SUR_CON (road surface conditions)  

# Run a naive Bayes classifier on the complete training set with the relevant predictors (and INJURY as the response). Note that all predictors are categorical. Show the confusion matrix. 
accidents.nb.2 <- naiveBayes(INJURY ~ HOUR_I_R + ALCHL_I + WRK_ZONE + INT_HWY + LGTCON_I_R + MANCOL_I_R + SPD_LIM + SUR_COND + WEATHER_R , data = train.df)
accidents.nb.2 
head(train.df)


library(caret)

# training
pred.class.train <- predict(accidents.nb.2, newdata = train.df)
confusionMatrix(pred.class.train , train.df$INJURY)

# validation
pred.class.valid <- predict(accidents.nb.2, newdata = valid.df)
confusionMatrix(pred.class.valid, valid.df$INJURY)




# What is the overall error for the validation set? What is the percent improvement relative to the naive rule (using the validation set)? Examine the conditional probabilities output.
# The confusion matrix shows a 53% accuracy
pred.prob <- predict(accidents.nb.2, newdata = valid.df, type = "raw")
## predict class membership
pred.class <- predict(accidents.nb.2, newdata = valid.df)
pred.prob
pred.class

# Why do we get a probability of zero for P(INJURY = No ∣ SPD_LIM = 5)?
# Speed limit of 5 is probably not fast enought to cause injuries. Speed limit of 5 is probably not fast enought to cause injuries. There we so few injuries with speed limit of 5.  

# What is the percent improvement relative to the naive rule (using the validation set)?
# We see .49% predicted as no and .50% predicted as yes 
library(gains)
gain <- gains(ifelse(valid.df$INJURY=="No",1,0), pred.prob[,1], groups=100)

plot(c(0,gain$cume.pct.of.total*sum(valid.df$INJURY=="No"))~c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(valid.df$INJURY=="No"))~c(0, dim(valid.df)[1]), lty=2)



# Why is it after examining the conditional probabilities output, do we get a probability of zero for P(INJURY = No ∣ SPD_LIM = 5)?
# Speed limit of 5 is probably not fast enought to cause injuries. There we so few injuries with speed limit of 5. 
#   



  