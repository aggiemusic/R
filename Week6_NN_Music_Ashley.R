#############################################
#                                           #
# Name:       Ashley Music                  #
# Date:       08/03/2020                    #
# Subject:    NN                            #
# Class:      BDAT 625                      #
# File Name:                                #
# Week6_NN_HW_Music_Ashley                  # 
#                                           #
#############################################
setwd("C:/Users/aggie/Desktop/Data Mining")
rm(list = ls())

library(class)
library(caret)
library(neuralnet)
library(readr)
ToyotaCorolla <- read_csv("C:/Users/aggie/Desktop/Data Mining/ToyotaCorolla(1).csv")
View(ToyotaCorolla)


# Remember to first scale the numerical predictor and outcome variables to a 0-1 scale (use function preprocess() with method = "range"-see Chapter 7) 
# and convert categorical predictors to dummies.
Toyota.DF <- ToyotaCorolla[ , c(3:4, 7:9, 12,14,17,19,21,25:26,28,30,34,39)]
Toyota.DF <- as.data.frame(Toyota.DF)
View(Toyota.DF)

#Converting Fuel_Type to dummy 
library(dummies)
Toyota.dummy <-  dummy.data.frame(Toyota.DF, names = c("Fuel_Type"), sep = ".")
View(Toyota.dummy)


set.seed(111)
train.index <- sample(row.names(Toyota.dummy), 0.6*dim(Toyota.dummy)[1])
valid.index <- setdiff(row.names(Toyota.dummy), train.index)

train.df <- Toyota.dummy[train.index, ]
valid.df <- Toyota.dummy[valid.index, ]
View(train.df)
View(valid.df)


#Price is col 1 

train.norm.df <- train.df 
valid.norm.df <- valid.df 
toyota.norm.df <- Toyota.dummy 
 


#Scaling the data using PreProcess. 
# I am scaling Price [,1] Age_08_04 [, 2] , KM [ ,3], HP [, 7], Doors [, 9], Quarterly_Tax [, 10], Guarentee_Period [ , 12] 
norm.values <- preProcess(train.df[ , c(1:3, 7, 9:10, 12)], method = "range" )

train.norm.df[ , c(1:3, 7, 9:10, 12)] <- predict(norm.values, train.df[ , c(1:3, 7, 9:10, 12)])
valid.norm.df[ , c(1:3, 7, 9:10, 12)]<- predict(norm.values, valid.df[ , c(1:3, 7, 9:10, 12)])
toyota.norm.df[ , c(1:3, 7, 9:10, 12)] <- predict(norm.values, Toyota.dummy [ , c(1:3, 7, 9:10, 12)])

head(train.norm.df)


# Fit a neural network model to the data. Use a single hidden layer with 2 nodes.
# Use predictors Age_08_04, KM, Fuel_Type, HP, Automatic, Doors, Quarterly_Tax, Mfr_Guarantee, Guarantee_Period, Airco, Automatic_airco, CD_Player, Powered_Windows, Sport_Model, and Tow_Bar.
nn <- neuralnet(Price ~ Age_08_04 + KM + Fuel_Type.CNG + Fuel_Type.Diesel + Fuel_Type.Petrol + HP + Automatic + Doors + Quarterly_Tax + Mfr_Guarantee + Guarantee_Period + Airco + Automatic_airco + CD_Player
                + Powered_Windows + Sport_Model + Tow_Bar,
                data = train.norm.df, hidden = 2)


nn$weights
plot(nn, rep = "best")

# Record the RMS error for the training data and the validation data. 
#RMS for training data is 0.001416009
training.predict.1 <- compute(nn, train.norm.df[ , c("Age_08_04", "KM",  "Fuel_Type.CNG" , "Fuel_Type.Diesel", "Fuel_Type.Petrol", "HP",  "Automatic",  "Doors" , "Quarterly_Tax" , "Mfr_Guarantee" , "Guarantee_Period",  "Airco" , "Automatic_airco" , "CD_Player",
                                                "Powered_Windows" , "Sport_Model" , "Tow_Bar") ])
obser.train <- train.norm.df$Price
mean((obser.train - training.predict.1$net.result)^2)



#RMS for test data is 0.001559039
test.predict.1<- compute(nn, valid.norm.df[ , c("Age_08_04", "KM",  "Fuel_Type.CNG" , "Fuel_Type.Diesel", "Fuel_Type.Petrol", "HP",  "Automatic",  "Doors" , "Quarterly_Tax" , "Mfr_Guarantee" , "Guarantee_Period",  "Airco" , "Automatic_airco" , "CD_Player",
                                                                       "Powered_Windows" , "Sport_Model" , "Tow_Bar") ])
obser.test <- valid.norm.df$Price
mean((obser.test - test.predict.1$net.result)^2)



# Repeat the process, changing the number of hidden layers and nodes to {single layer with 5 nodes}, {two layers, 5 nodes in each layer}.
#Single layer, 5 nodes
nn.one.five <- neuralnet(Price ~ Age_08_04 + KM + Fuel_Type.CNG + Fuel_Type.Diesel + Fuel_Type.Petrol + HP + Automatic + Doors + Quarterly_Tax + Mfr_Guarantee + Guarantee_Period + Airco + Automatic_airco + CD_Player
                + Powered_Windows + Sport_Model + Tow_Bar,
                data = train.norm.df, hidden = 5)

# RMS for Training data for single layer, 5 nodes 
training.predict.2 <- compute(nn.one.five, train.norm.df[ , c("Age_08_04", "KM",  "Fuel_Type.CNG" , "Fuel_Type.Diesel", "Fuel_Type.Petrol", "HP",  "Automatic",  "Doors" , "Quarterly_Tax" , "Mfr_Guarantee" , "Guarantee_Period",  "Airco" , "Automatic_airco" , "CD_Player",
                                                     "Powered_Windows" , "Sport_Model" , "Tow_Bar") ])
obser.train <- train.norm.df$Price
mean((obser.train - training.predict.2$net.result)^2)

#RMS for Test data for single layer, 5 nodes 
test.predict.2<- compute(nn.one.five, valid.norm.df[ , c("Age_08_04", "KM",  "Fuel_Type.CNG" , "Fuel_Type.Diesel", "Fuel_Type.Petrol", "HP",  "Automatic",  "Doors" , "Quarterly_Tax" , "Mfr_Guarantee" , "Guarantee_Period",  "Airco" , "Automatic_airco" , "CD_Player",
                                                "Powered_Windows" , "Sport_Model" , "Tow_Bar") ])
obser.test <- valid.norm.df$Price
mean((obser.test - test.predict.2$net.result)^2)




#2 Layers, 5 nodes 
nn.two.five <- neuralnet(Price ~ Age_08_04 + KM + Fuel_Type.CNG + Fuel_Type.Diesel + Fuel_Type.Petrol + HP + Automatic + Doors + Quarterly_Tax + Mfr_Guarantee + Guarantee_Period + Airco + Automatic_airco + CD_Player
                         + Powered_Windows + Sport_Model + Tow_Bar,
                         data = train.norm.df, hidden = c(5,5))

# RMS for Training data for two layer, 5 nodes 
training.predict.3 <- compute(nn.two.five, train.norm.df[ , c("Age_08_04", "KM",  "Fuel_Type.CNG" , "Fuel_Type.Diesel", "Fuel_Type.Petrol", "HP",  "Automatic",  "Doors" , "Quarterly_Tax" , "Mfr_Guarantee" , "Guarantee_Period",  "Airco" , "Automatic_airco" , "CD_Player",
                                                              "Powered_Windows" , "Sport_Model" , "Tow_Bar") ])
obser.train <- train.norm.df$Price
mean((obser.train - training.predict.3$net.result)^2)

#RMS for Test data for two layer, 5 nodes 
test.predict.3<- compute(nn.two.five, valid.norm.df[ , c("Age_08_04", "KM",  "Fuel_Type.CNG" , "Fuel_Type.Diesel", "Fuel_Type.Petrol", "HP",  "Automatic",  "Doors" , "Quarterly_Tax" , "Mfr_Guarantee" , "Guarantee_Period",  "Airco" , "Automatic_airco" , "CD_Player",
                                                         "Powered_Windows" , "Sport_Model" , "Tow_Bar") ])
obser.test <- valid.norm.df$Price
mean((obser.test - test.predict.3$net.result)^2)


