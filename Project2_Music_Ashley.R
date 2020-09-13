#############################################
#                                           #
# Author:     Ashley Music                  #
# Date:       03/16/2020                    #
# Subject:    Project 2                     #
# Class:      BDAT 640                      #
# Section:    01W 20/SP2                    #         
# Instructor: Chris Shannon                 #
# File Name:  Project2_Music_Ashley.R       #
#                                           #
#############################################

#importing the mtcars dataset 
library(readxl)
Mtcars <- read_excel("mtcars.xlsx")
View(Mtcars)

# 1.1 Use the lm() function to perform a simple linear regression 
# with the response mpg and the predictor hp.
lm.fit = lm(mpg ~ hp, data = Mtcars)
summary(lm.fit)

# 2.1 Is there a relationship between the target mpg and predator hp?
# Answer: Yes, the pvalue for the F-test is very small (2.723e -09)and less than 5%. 
# The regression coefficient is not equal to 0. 
# There is a relationshiop between mpg and hp. 

# 3.1 How strong is the relationship between the response and predictor?
# Answer: There is a moderate relationship based on r2. 
# adjusted r2 is .5904 and multiple R-squared is .6006


# 4.1 Is the relationship between mpg and hp positive or negative?
# Answer:The relationship is negative. 
# Predictor HP estimate is a negative number -0.0645

# 5.1 What is the predicted mpg associated with a horsepower (hp) of 100? 
# Answer: 22.843
confint(lm.fit)
predict(lm.fit, data.frame(hp = c(100)), interval = "confidence", level = .95)
# 5.2 What’s the 95% confidence interval for the predicted mpg?
# Answer: 21.53% - 24.16% 

# 6.1 Plot the response and the predictor 
# and add the regression line using abline().
plot(Mtcars$hp, Mtcars$mpg, xlab = "HP", ylab = "MPG")
abline(lm.fit, lwd = 3 ,col = "red" )

# 7.1 Perform a multiple linear regression with mpg as the response
# and the predictors cyl, disp, hp, wt, vs, and gear. 
# First converting cyl, vs and gear to factors
Mtcars$cylf <- factor(Mtcars$cyl, levels = c(4,6,8), labels = c("4cyl", "6cyl", "8cyl"))
Mtcars$vsf <- factor(Mtcars$vs, levels = c(0,1), labels = c("V-shaped","Straight"))
Mtcars$gearf <- factor(Mtcars$gear, levels = c(3,4,5), labels = c("3gear", "4gear", "5gear"))
#checking to see if there are any variables that should be transformed 
# due to skewness. Wt appears to be very slightly right skwewed, but does not 
# appear to be so skewed that it needs transformed. 
pairsP = Mtcars[, c("mpg", "hp", "disp", "cyl", "wt", "vs", "gear")]
pairs(pairsP)
#Now preforming the linear regression 
lm.fit2 = lm(mpg ~  hp + disp + cylf + wt + vsf + gearf, data = Mtcars)

# 7.2 Print out the results using summary() function.
summary(lm.fit2)


# 8.1 Is there a relationship between the predictors and the response?
# Answer: Yes, the F-statistic for the  p-value is very low so there is a relationship 
# between at least one of the predictors and the response
# At least one of the regression coefficients is less than 0.  

# 9.1 Which predictors appears to have 
# a statistically significant relationship to the response? 
# Answer: Based on the p-value for the t-test is lower than 5% 
# wt (.00012) and hp (.00557) 
# have a statistically significant relationship to the response mpg.

# 10.1 Use * symbols to fit linear regression models 
# with interaction effects between hp and wt.
# Does this interaction appear to be statistically significant?
lm.fit3 = lm(mpg ~ hp*wt, data = Mtcars)
summary(lm.fit3)
# Answer: Yes, this interaction does appear to be statistically significant
# based on the p-value for the t-test being .000362 which is less than 5%
# There is evidence to conclude that there is a significant interaction
# relationship between hp and wt. You should take this interaction
# effect into consideration with interpreting the effects between
# mpg and hp and weight. 



