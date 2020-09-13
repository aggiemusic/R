#############################################
#                                           #
# Author:     Ashley Music                  #
# Date:       04/20/2020                    #
# Subject:    Project 8                     #
# Class:      BDAT 640                      #
# Section:    01W 20/SP2                    #         
# Instructor: Chris Shannon                 #
# File Name:  Project8_Music_Ashley.R       #
#                                           #
#############################################


# 1.1 	1. Load the dataset wine.csv into memory.
library(readr)
wine <- read_csv("C:/Users/aggie/Desktop/Forecasting and Modeling/wine.csv")
View(wine)


# 2.1	Preprocess the inputs 
# a. Standardize the inputs using the scale() function.
wine.scaled <- scale(wine)

# b. Convert the standardized inputs to a data frame using the as.data.frame() function.
wine.scaled <- as.data.frame(wine.scaled)

# c. Split the data into a training set containing 3/4 of the original data 
# test set containing the remaining 1/4 of the original data.
set.seed(1)
index <-sample(1:nrow(wine.scaled), 0.75*nrow(wine.scaled))
train <- wine.scaled[index, ]
test <- wine.scaled[-index, ]


# 3.1 Build a neural networks model
library(neuralnet)
# a. The response is quality and the inputs are: volatile.acidity, density, pH, and alcohol. 
# Please use 1 hidden layer with 1 neuron.
set.seed(1)
nn.model.wine <- neuralnet(quality ~ volatile.acidity + density + pH + alcohol, data = train, hidden = 1)

# b. Plot the neural networks.
plot(nn.model.wine)

# c. Forecast the wine quality in the test dataset.
predict.nn.wine <- compute(nn.model.wine, test[ , c("volatile.acidity", "density", "pH", "alcohol")])

# d. Get the observed wine quality of the test dataset.
observ.test.wine <- test$quality

# e. Compute test error (MSE).
# Answer: The MSE is 0.701
mean((observ.test.wine - predict.nn.wine$net.result)^2)




