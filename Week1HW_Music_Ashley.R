#############################################
#                                           #
# Name:       Ashley Music                  #
# Date:       06/29/2020                    #
# Subject:    Week 1 H.W                    #
# Class:      BDAT 625                      #
# File Name:  Week1HW_Music_Ashley.R        #
#                                           #
#############################################

rm(list = ls())

library(readr)
tC.df <- as.data.frame(toyotaCorolla <- read_csv("C:/Users/aggie/Desktop/Data Mining/ToyotaCorolla.csv"))

View(tC.df)
head(tC.df)
dim(tC.df)
summary(tC.df)
#looking at correlations but removing ID # and brand name of car
pairData <- toyotaCorrola.dummy.DF[-c(1:2)]
View(pairData)
cor(pairData)
pairs(pairData)
str(tC.df)



#Part 1 
#The dataset has two categorical attributes, Fuel Type and Metallic.
#Use R's functions to transform categorical data into dummies.

#using model matrix 
xtotal <- model.matrix(~ 0 + Fuel_Type + Color, data = tC.df)
xtotal <- as.data.frame(xtotal)
t(t(names(xtotal)))
head(xtotal)

#using the dummies package
library(dummies)
toyotaCorrola.dummy.DF <-  dummy.data.frame(tC.df, names = c("Fuel_Type", "Color"), sep = ".")
names(toyotaCorrola.dummy.DF)
View(toyotaCorrola.dummy.DF)



#Part 2 
#Select all the variables and use default values for the random seed and partitioning percentages 
#for training (50%), validation (30%), and test (20%) sets.

set.seed(1)

train.rows <- sample(rownames(toyotaCorrola.dummy.DF), dim(tC.df)[1]*0.5)
valid.rows <- sample(setdiff(rownames(toyotaCorrola.dummy.DF), train.rows),
                     dim(toyotaCorrola.dummy.DF)[1]*0.3)
test.rows <- setdiff(rownames(toyotaCorrola.dummy.DF), union(train.rows, valid.rows))
train.data <- toyotaCorrola.dummy.DF[train.rows, ]
valid.data <- toyotaCorrola.dummy.DF[valid.rows, ]
test.data <- toyotaCorrola.dummy.DF[test.rows, ]






