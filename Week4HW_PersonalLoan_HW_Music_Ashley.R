#############################################
#                                           #
# Name:       Ashley Music                  #
# Date:       07/21/2020                    #
# Subject:    Predict Botson Housing        #
# Class:      BDAT 625                      #
# File Name:                                #
# Week4HW_PersonalLoan_HW_Music_Ashley      # 
#                                           #
#############################################



library(class)
library(caret)

rm(list = ls())
library(readr)
UniversalBank <- read_csv("C:/Users/aggie/Desktop/Data Mining/UniversalBank.csv")
View(UniversalBank)


#Removing ID and Zip Code and putting the response variable last.
ub.Subset <- UniversalBank[c('Age', 'Experience', 'Income', 'Family', 'CCAvg','Education','Mortgage', "Securities Account", 'CD Account', 'Online', 'CreditCard', "Personal Loan")] 
head(ub.Subset)

ub.df <- data.frame(ub.Subset)
head(ub.df)



#Creating dummy variable for Education 
library(dummies)
ub.dummy <-  dummy.data.frame(ub.df, names = c("Education"), sep = ".")
names(ub.dummy)
head(ub.dummy)


#Creating the new customer 
new.customer <- data.frame(Age = 40, Experience = 10, Income = 84, Family = 2, CCAvg = 2, Education_1 = 0, Education_2 = 1, Education_3 = 0, Mortgage = 0, 'Securities Account' = 0, 'CD Account' = 0, Online = 1,  'Credit Card' = 1)
View(new.customer)


#Partitioning the data 
set.seed(111)
train.index <- sample(row.names(ub.dummy), 0.6*dim(ub.dummy)[1])
valid.index <- setdiff(row.names(ub.dummy), train.index)

train.df <- ub.dummy[train.index, ]
valid.df <- ub.dummy[valid.index, ]
View(train.df)
View(valid.df)
#Personal Loan is cloumn 14 





train.norm.df <- train.df 
valid.norm.df <- valid.df 
bank.norm.df <- ub.dummy 
new.norm.customer <- new.customer  


#normalizing the data. I am normalizing age, experience, income, family, CCAgg (1:5) and Mortgage (9) 
norm.values <- preProcess(train.df[ , c(1:5,9)], method = c("center", "scale") )
train.norm.df[ , c(1:5,9)] <- predict(norm.values, train.df[ , c(1:5,9)])
valid.norm.df[ , c(1:5,9)]<- predict(norm.values, valid.df[ , c(1:5,9)])
bank.norm.df[ , c(1:5,9)] <- predict(norm.values, ub.dummy[ , c(1:5,9)])
new.norm.customer[ , c(1:5,9)] <- predict(norm.values, new.norm.customer[ , c(1:5,9)] ) 

library(class)
nn <- knn(train = train.norm.df[ , 1:13], test = new.norm.customer, cl = train.norm.df[, 14], k = 1 )   #Specify the columns 1:13, you have to exclude the predictor 'Personal Loan' in train = , then you use only that predictor in the cl =  
row.names(train.df)[attr(nn, "nn.index")]

nn.n <- knn(bank.norm.df[, 1:13], test = new.norm.customer, cl = bank.norm.df[,14], k = 1)
row.names(bank.norm.df)[attr(nn.n, "nn.n index")]
nn.n


#This customer would be classified as not accepting the loan as indicated by the 0. 



#Determining the best K.  

library(caret)
# Error: `data` and `reference` should be factors with the same levels.

# # initialize a data frame with two columns: k, and accuracy.
#  accuracy.df <- data.frame(k = seq(1, 14, 1), accuracy = rep(0, 14))
# 
#  for(i in 1:14) {
#   knn.pred <- knn(train.norm.df[, 1:13], valid.norm.df[, 1:13], 
#                  cl = train.norm.df[, 14], k = i)
#  accuracy.df[i, 2] <- confusionMatrix(table(knn.pred, valid.norm.df$Personal.Loan)) 
#  }
#  accuracy.df
#  




i = 1 
k.optm = 1 
for (i in 1:20){
  knn.mod <- knn(train = train.norm.df[, 1:13], test = valid.norm.df[,1:13], cl = train.norm.df[,14], k = i)
  k.optm[i] <- 100*sum(valid.norm.df[,14] == knn.mod)/NROW(valid.norm.df[,14])
  k=i
  cat(k, '=', k.optm[i],'\n') 
}

#Best k is 3 at 95.20

plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level") 


# Show the confusion matrix for the validation data that results from using the best k.
# Need to use the validation data for test =  valid.nor.df[,1:13] and not the customer we created before. We are just checking the accuracy when compared to the whole data set.
# The previous error was because we were only using a dataset with one row (the customer we were trying to predict)

knn.pred.1 <- knn(train = train.norm.df[ , 1:13], test = valid.norm.df[,1:13], cl = train.norm.df[, 14], k = 3 )
knn.pred.1

#Manually creating a table to make sure it will work

xtab = table(knn.pred.1, valid.norm.df$Personal.Loan)
xtab

#Using confusion Matrix to get all the variables 
library(caret)
confusionMatrix(table(knn.pred.1, valid.norm.df$Personal.Loan))




# Consider the following customer: 
# Age = 40, Experience = 10, Income = 84, Family = 2, CCAvg = 2, Education_1 = 0, Education_2 = 1, Education_3 = 0, Mortgage = 0, Securities Account = 0, CD Account = 0, Online = 1 and Credit Card = 1. 

new.customer.df.2 <- data.frame(Age = 40, Experience = 10, Income = 84, Family = 2, CCAvg = 2, Education_1 = 0, Education_2 = 1, Education_3 = 0, Mortgage = 0,'Securities Account'= 0,'CD Account'= 0, Online = 1, 'Credit Card'= 1)
new.norm.customer.2 <- new.customer.df.2
new.norm.customer.2[ , c(1:5,9)] <- predict(norm.values, new.norm.customer.2[ , c(1:5,9)] ) 

# Classify the above customer using the best k.

nn.2 <- knn(train = bank.norm.df[ , 1:13], test = new.norm.customer.2, cl = bank.norm.df[, 14], k = 3 )   
row.names(bank.norm.df)[attr(nn.2, "nn.2 index")]
nn.2

# Customer is classified again as not accepting the loan. This is not surprising  considering the low acceptance rate of 9%



# Repartition the data, this time into training, validation, and test sets (50% : 30% : 20%).
# This came from an R-Blog site on creating 3 partions. Previously, I've only done two partitions. 

fractionTraining <- 0.50
fractionValidation <- 0.30
fractionTest <- 0.20

samplesizeTraining <- floor(fractionTraining * nrow(bank.norm.df))
samplesizeValidation <- floor(fractionValidation * nrow(bank.norm.df))
samplesizeTest <- floor(fractionTest * nrow(bank.norm.df))
indicesTraining <- sort(sample(seq_len(nrow(bank.norm.df)), size = samplesizeTraining))
indicesNotTraining <- setdiff(seq_len(nrow(bank.norm.df)), indicesTraining)
indicesValidation <- sort(sample(indicesNotTraining, size = samplesizeValidation))
indicesTest <- setdiff(indicesNotTraining, indicesValidation)

dfTraining <- bank.norm.df[indicesTraining, ]
dfValidation <- bank.norm.df[indicesValidation, ]
dfTest <- bank.norm.df[indicesTest, ]

# Apply the k-NN method with the k chosen above


knn.3 <- knn(train = dfTraining[ , 1:13], test = dfTest[, 1:13], cl = dfTraining[, 14], k = 3 )
#Manually creating a table to make sure it will work
xtab2 = table(knn.3, dfTest$Personal.Loan )
xtab2
### Getting a length error so I'm checking which one I need to test against the knn.3  
# dfTest is 1000 and so is knn.3  so these are what I need to create the confusion matrix with 
length(knn.3)
length(dfValidation$Personal.Loan)
length(dfTest$Personal.Loan)
length(dfTraining$Personal.Loan)

#Using confusion Matrix to get all the variables for the test set 
confusionMatrix(table(knn.3, dfTest$Personal.Loan))


#Now looking at the confusion matrix for the validation sets. 
knn.4 <- knn(train = dfTraining[ , 1:13], test = dfValidation[, 1:13], cl = dfTraining[, 14], k = 3 )
length(knn.4)

confusionMatrix(table(knn.4, dfValidation$Personal.Loan))


# Comment on the differences and their reason.
# Confusion Matrix for the validation set is slightly higher (97% accuracy) because it's a smaller dataset 



