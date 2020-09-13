#############################################
#                                           #
# Name:       Ashley Music                  #
# Date:       08/03/2020                    #
# Subject:    NN                            #
# Class:      BDAT 625                      #
# File Name:                                #
# Week6_Association_HW_Music_Ashley         # 
#                                           #
#############################################
setwd("C:/Users/aggie/Desktop/Data Mining")
rm(list = ls())
install.packages('arules')
install.packages('recommenderlab')

library(arules)
library(recommenderlab)




library(readr)
CourseRatings <- read_csv("CourseRatings.csv")
View(CourseRatings)

# Consider a user-based collaborative filter. This requires computing correlations between all student pairs. For which students is it possible to compute correlations with E.N.? Compute them.
#EN and LN 
EN <- (4+4+4+3)/4
LN <- (4+3+2+4+2)/5
DS <- (4+2+4)/3



#EN and LN's correlations. They both liked SQL, R Prog and Regression. Cor = .8703. Pretty similar
EN.LN <- (4-3.75)*(4-3)+(4-3.75)*(4-3)+(3-3.75)*(2-3)
EN.Squared <- (4-3.75)^2 + (4-3.75)^2 + (3-3.75)^2 
En.Sqrt <- sqrt(EN.Squared)
LN.Squared <- (4-3)^2+ (4-3)^2 +(2-3)^2
LN.Sqrt <- sqrt(LN.Squared)

EN.LN.Cor <- EN.LN / (En.Sqrt * LN.Sqrt)




#EN and DS. They both like SQL, R Prog and DM in R. Cor of .003535512
EN.DS <- (4-3.75)*(4-3.33)+(4-3.75)*(2-3.33)+(4-3.75)*(4-3.33)
EN.Sq.2 <- (4-3.75)^2 + (4-3.75)^2 +(4-3.75)^2 
EN.Sqrt.2 <- sqrt(EN.Sq.2)
DS.Squared <- (4-3.33)^2 + (2-3.33)^2 + (4-3.33)^2 
DS.Sqrt <- sqrt(DS.Squared)

EN.DS.Cor <- EN.DS / (EN.Sqrt.2 * DS.Sqrt)




#Cosine similarities between EN and LN. Cosine is .64639 
Cosine.num <- 4*4+4*4+3*2
Cosine.dem.en <- 4^2 + 4^2 + 4^3
Cosine.dem.en.sqrt <- sqrt(Cosine.dem.en)
Cosine.dem.ln <- 4^2 + 4^2 + 2^2 
Cosine.dem.ln.sqrt <- sqrt(Cosine.dem.ln)

Cosine <- Cosine.num / (Cosine.dem.en.sqrt*Cosine.dem.ln.sqrt)


# Calculate two course pair correlations and report the results.

#SQL (3.4 Average) and Spatial (3.5 Average). Cor is .8424235

SQL <- (4+3+2+4+4)/5 
Spatial <- (4+2+4+4+4+3)/6

SQL.Spatial <- (3-3.4)*(4-3.5)+(2-3.4)*(2-3.5)+(4-3.4)*(4-3.5)
SQL.Sq.2 <- (3-3.4)^2 + (2-3.4)^2 +(4-3.4)^2 
SQL.Sq.2<- sqrt(SQL.Sq.2)
Spatial.Squared <- (4-3.5)^2 + (2-3.5)^2 + (4-3.5)^2 
Spatial.Squared.2 <- sqrt(Spatial.Squared)

SQL.Spatial.Cor  <- SQL.Spatial / (SQL.Sq.2 * Spatial.Squared.2 )

#SQL and Python (3.5 Average). Cor of -.9805807
Python <- (3+4)/2

SQL.Python <- (4-3.4)*(3-3.5)+(3-3.4)*(4-3.5)

SQL.Sq.2.3 <- (4-3.4)^2 + (3-3.4)^2 
SQL.Sq.2.3  <- sqrt(SQL.Sq.2.3 )

Python.Squared <- (3-3.5)^2 + (4-3.5)^2 
Python.Squared.2 <- sqrt(Python.Squared)

SQL.Python.Cor  <- SQL.Python  / (SQL.Sq.2.3 * Python.Squared.2 )





# 15 users [1:15,1] , 9 courses to rate [1:15, 2:10]
# Not all users have given recomendations. 
CourseRatings <- as.matrix(CourseRatings)

r <- as(CourseRatings, "realRatingMatrix")
EN <- CourseRatings[ 4, ]
EN <- as.matrix(EN)
EN.Rec <- as(EN, "realRatingMatrix")





Course.Rec.item <- Recommender(r, "IBCF")
pred.2 <- predict(Course.Rec.item, r, type = "ratings")

as(pred.2, "matrix")



pred.3 <- predict(Course.Rec.item , EN.Rec, type = "ratings")
as(pred.3, "matrix")










