#############################################
#                                           #
# Name:       Ashley Music                  #
# Date:       07/06/2020                    #
# Subject:    Week 2.2 H.W                  #
# Class:      BDAT 625                      #
# File Name:  Week2.2HW__Music_Ashley.R     #
#                                           #
#############################################
library(readr)
Universities <- read_csv("C:/Users/aggie/Desktop/Data Mining/Universities.csv")
View(Universities)


# Part 1 
# Remove all categorical variables. 
universities.df <- Universities[-c(1:2)]
View(universities.df)
# Then remove all records with missing numerical measurements from the dataset.
sum(is.na(universities.df))
complete.cases(universities.df)
cleanUniversities.df <- universities.df[complete.cases(universities.df), ]
str(cleanUniversities.df)
View(cleanUniversities.df)

# Conduct a principal components analysis on the cleaned data and comment on the results. 
# Answer: Yes, the data should be normalized.
# We can see that for the first component it's a dummy variable (Public or Private).
# We can also see that the 3 and 4 components are actually % of students from the top 10 and 25%. 
# These variables are measured in different units so normalizing would be beneficial. 

# Should the data be normalized? Discuss what characterizes the components you consider key.
# Answer: The 1st and 2nd components are key because they represent 56% and 36% of the variance in the data.  

pcs <- prcomp(cleanUniversities.df)
summary(pcs)


# Part 2 
library(readr)
tC.df <- as.data.frame(toyotaCorolla <- read_csv("C:/Users/aggie/Desktop/Data Mining/ToyotaCorolla.csv"))
View(tC.df)

# Identify the categorical variables.
# Fuel Type and Color are  categorical variables. 

# Explain the relationship between a categorical variable and the series of binary dummy variables derived from it.
# Answer: A categorical variable can take a few different forms and is usually finite. 
# Like gender, year, occupation, type, etc. 
# A dummy variable takes this categorical variable and changes that value to either 0 or 1.
# A 0 means that it does not have that category and a 1 means that it does. 
# For example, with color we could look at "purple" and if the car is purple it would have a 1 as a dummy variable in a column called color_purple. 
# It greatly increases the number of columns in your data. 


# State how many dummy binary variables are required to capture the information in a categorical variable with N categories?
# You would make N - 1 dummy variables for the categorical variables. 

  
# Use R to convert the categorical variables in this dataset into dummy variables, and explain in words, for one record, the values in the derived binary dummies.
# For color we now have 10 columns in the data set.
# One column for each color. The dummy variable is either a 0 or a 1.
# For example, if we looked at color.red we would either see a 1 if the car is red or a 0 if the car is not red. 

library(dummies)
toyotaCorrola.dummy.DF <-  dummy.data.frame(tC.df, names = c("Fuel_Type", "Color"), sep = ".")
names(toyotaCorrola.dummy.DF)
View(toyotaCorrola.dummy.DF)




# Use R to produce a correlation matrix and matrix plot. Comment on the relationships among variables.

#removing model and ID for correlation analysis
noModel <- toyotaCorrola.dummy.DF[-c(1:2)]
View(noModel)
noModelNoColor <- noModel[-c(13:22)]

# Answer: We can see a negative correlation between price and age. 
# As age goes up, price goes down. HP and price have a slightly positive relationship.
# Weight and price are also positively correlated.
# Color does not seem to have a very high correlation to price, which I thought would be stronger. 

#correlation matrix 
cor(noModel)
#matrix plot 
pairs(noModelNoColor)






