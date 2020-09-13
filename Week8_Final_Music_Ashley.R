#############################################
#                                           #
# Name:       Ashley Music                  #
# Date:       08/14/2020                    #
# Subject:    Final                         #
# Class:      BDAT 625                      #
# File Name:                                #
# Week8_Final_Music_Ashley                  # 
#                                           #
#############################################
rm(list = ls())
setwd("C:/Users/aggie/Desktop/Data Mining")


library(readr)
Bankruptcy <- read_csv("Bankruptcy.csv")
View(Bankruptcy)


#Pre-Processing the data. Removing NO and YR
Bankruptcy.df <- as.data.frame(Bankruptcy[,-c(1,3)])
View(Bankruptcy.df)


#Converting D to factor with 0 as not bankrupt and 1 as bankrupt 
Bankruptcy.df$D <- factor(Bankruptcy.df$D)
str(Bankruptcy.df)

#Removing D so I can perform PCA 
pca.Bankrupt <- Bankruptcy.df[,-1]
#Checking for missing data. None found.
sum(is.na(pca.Bankrupt))


#Running PCA 
pairs(pca.Bankrupt)

pca <- prcomp(pca.Bankrupt)
summary(pca)
plot(pca)
biplot(pca)
str(pca)
#PCA shows us that the PC1 and PC2 account for the most variance in the data set. Those should be the principal components that are selected. 
#PC1 accounts for 73% of the variance and PC2 accounts for 26% 


#Using Random Forrest to select variables 
 
library(randomForest)
library(caret)
fit_rf <- randomForest(D ~ ., data = Bankruptcy.df)
importance(fit_rf)
varImp(fit_rf)
varImpPlot(fit_rf)

#Based on RF I will select R's 9, 24, 21, 18, 15, 14, 17, 23




#Visualising the data with some counts of important variables. 
ggplot(data = Bankruptcy.df)+
  geom_bar(mapping = aes(x = D, y = R9), stat = "identity")

ggplot(data = Bankruptcy.df)+
  geom_bar(mapping = aes(x = D, y = R24), stat = "identity")

ggplot(data = Bankruptcy.df)+
  geom_bar(mapping = aes(x = D, y = R21), stat = "identity")

ggplot(data = Bankruptcy.df)+
  geom_bar(mapping = aes(x = D, y = R18), stat = "identity")



#Side-by-Side boxplots of all variables and response 

par(mfcol = c(5,5))
boxplot(Bankruptcy.df$R1 ~ Bankruptcy.df$D, xlab = "Bankruptcy", ylab = "R1")
boxplot(Bankruptcy.df$R2 ~ Bankruptcy.df$D, xlab = "Bankruptcy", ylab = "R2")
boxplot(Bankruptcy.df$R3 ~ Bankruptcy.df$D, xlab = "Bankruptcy", ylab = "R3")
boxplot(Bankruptcy.df$R4 ~ Bankruptcy.df$D, xlab = "Bankruptcy", ylab = "R4")
boxplot(Bankruptcy.df$R5 ~ Bankruptcy.df$D, xlab = "Bankruptcy", ylab = "R5")
boxplot(Bankruptcy.df$R6 ~ Bankruptcy.df$D, xlab = "Bankruptcy", ylab = "R6")
boxplot(Bankruptcy.df$R7 ~ Bankruptcy.df$D, xlab = "Bankruptcy", ylab = "7")
boxplot(Bankruptcy.df$R8 ~ Bankruptcy.df$D, xlab = "Bankruptcy", ylab = "R8")
boxplot(Bankruptcy.df$R9 ~ Bankruptcy.df$D, xlab = "Bankruptcy", ylab = "R9")
boxplot(Bankruptcy.df$R10 ~ Bankruptcy.df$D, xlab = "Bankruptcy", ylab = "R10")
boxplot(Bankruptcy.df$R11 ~ Bankruptcy.df$D, xlab = "Bankruptcy", ylab = "R11")
boxplot(Bankruptcy.df$R12 ~ Bankruptcy.df$D, xlab = "Bankruptcy", ylab = "R12")
boxplot(Bankruptcy.df$R13 ~ Bankruptcy.df$D, xlab = "Bankruptcy", ylab = "R13")
boxplot(Bankruptcy.df$R14 ~ Bankruptcy.df$D, xlab = "Bankruptcy", ylab = "R14")
boxplot(Bankruptcy.df$R15 ~ Bankruptcy.df$D, xlab = "Bankruptcy", ylab = "R15")
boxplot(Bankruptcy.df$R16 ~ Bankruptcy.df$D, xlab = "Bankruptcy", ylab = "R16")
boxplot(Bankruptcy.df$R17 ~ Bankruptcy.df$D, xlab = "Bankruptcy", ylab = "R17")
boxplot(Bankruptcy.df$R18 ~ Bankruptcy.df$D, xlab = "Bankruptcy", ylab = "R18")
boxplot(Bankruptcy.df$R19 ~ Bankruptcy.df$D, xlab = "Bankruptcy", ylab = "R19")
boxplot(Bankruptcy.df$R20 ~ Bankruptcy.df$D, xlab = "Bankruptcy", ylab = "R20")
boxplot(Bankruptcy.df$R21 ~ Bankruptcy.df$D, xlab = "Bankruptcy", ylab = "R21")
boxplot(Bankruptcy.df$R22 ~ Bankruptcy.df$D, xlab = "Bankruptcy", ylab = "R22")
boxplot(Bankruptcy.df$R23 ~ Bankruptcy.df$D, xlab = "Bankruptcy", ylab = "R23")
boxplot(Bankruptcy.df$R24 ~ Bankruptcy.df$D, xlab = "Bankruptcy", ylab = "R24")



#Making the boxplots of important variables with fill color and labels. 

library(ggplot2)
my.plot <- ggplot(data = Bankruptcy.df, aes(y = R9, x = D, fill = D))
my.plot <- my.plot + geom_boxplot()
my.plot <- my.plot + ggtitle("Distribution of Bankruptcy and R9")
my.plot <- my.plot + ylab("R9") + xlab("Bankrupt vs. No Bankrupt")
my.plot



my.plot2 <- ggplot(data = Bankruptcy.df, aes(y = R24, x = D, fill = D))
my.plot2 <- my.plot2 + geom_boxplot()
my.plot2 <- my.plot2 + ggtitle("Distribution of Bankruptcy and R24")
my.plot2 <- my.plot2 + ylab("R24") + xlab("Bankrupt vs. No Bankrupt")
my.plot2

my.plot3 <- ggplot(data = Bankruptcy.df, aes(y = R21, x = D, fill = D))
my.plot3 <- my.plot3 + geom_boxplot()
my.plot3 <- my.plot3 + ggtitle("Distribution of Bankruptcy and R21")
my.plot3 <- my.plot3 + ylab("R21") + xlab("Bankrupt vs. No Bankrupt")
my.plot3

my.plot4 <- ggplot(data = Bankruptcy.df, aes(y = R18, x = D, fill = D))
my.plot4 <- my.plot4 + geom_boxplot()
my.plot4 <- my.plot4 + ggtitle("Distribution of Bankruptcy and R21")
my.plot4 <- my.plot4 + ylab("R18") + xlab("Bankrupt vs. No Bankrupt")
my.plot4


my.plot5 <- ggplot(data = Bankruptcy.df, aes(y = R15, x = D, fill = D))
my.plot5 <- my.plot5 + geom_boxplot()
my.plot5 <- my.plot5 + ggtitle("Distribution of Bankruptcy and R21")
my.plot5 <- my.plot5 + ylab("R15") + xlab("Bankrupt vs. No Bankrupt")
my.plot5

my.plot6 <- ggplot(data = Bankruptcy.df, aes(y = R14, x = D, fill = D))
my.plot6 <- my.plot6 + geom_boxplot()
my.plot6 <- my.plot6 + ggtitle("Distribution of Bankruptcy and R21")
my.plot6 <- my.plot6 + ylab("R14") + xlab("Bankrupt vs. No Bankrupt")
my.plot6

my.plot7 <- ggplot(data = Bankruptcy.df, aes(y = R17, x = D, fill = D))
my.plot7 <- my.plot7 + geom_boxplot()
my.plot7 <- my.plot7 + ggtitle("Distribution of Bankruptcy and R21")
my.plot7 <- my.plot7 + ylab("R17") + xlab("Bankrupt vs. No Bankrupt")
my.plot7

my.plot8 <- ggplot(data = Bankruptcy.df, aes(y = R23, x = D, fill = D))
my.plot8 <- my.plot8  + geom_boxplot()
my.plot8 <- my.plot8  + ggtitle("Distribution of Bankruptcy and R21")
my.plot8 <- my.plot8  + ylab("R23") + xlab("Bankrupt vs. No Bankrupt")
my.plot8



#Creating training and test sets. Training 80% and test set 20%  
set.seed(1234)
train <- sample(nrow(Bankruptcy.df), nrow(Bankruptcy.df) * 0.8)
Bankrupt.train <- Bankruptcy.df[train, ]
Bankrupt.test <- Bankruptcy.df[-train, ]
table(Bankrupt.train$D)
table(Bankrupt.test$D)
nrow(Bankrupt.train)
nrow(Bankrupt.test)


#Creating a GLM model 
library(forecast)

#With the following classifiers: R9, R24, R21, R18, R15, R14, R17, R23 selected from Random Forest above
Bankrupt.GLM <- glm(D ~ R9 + R24 + R21 + R18 + R15 + R14 + R17 + R23, data = Bankrupt.train, family = "binomial")
summary(Bankrupt.GLM)


#Predicting with the training set
cutoff <- 0.5
preds <- predict(Bankrupt.GLM, type = "response")
predicted.glm <- preds > cutoff
predicted.glm <- as.numeric(predicted.glm)


#confusion matrix for the training set 
table(Bankrupt.train$D, predicted.glm, dnn = c("Truth", "Predicted"))

#Converting back to factor to do confusion matrix
test <- as.factor(predicted.glm)
confusionMatrix(Bankrupt.train$D, test)


#Predicting for the test set 
cutoff <- 0.5
preds2 <- predict(Bankrupt.GLM, Bankrupt.test, type = "response")
predicted.glm2 <- preds2 > cutoff
predicted.glm2 <- as.numeric(predicted.glm2)


#Confusion Matrix with the test set 
table(Bankrupt.test$D, predicted.glm2, dnn = c("Truth", "Predicted"))

#Converting back to factor to do confusion matrix
ref2 <- as.factor(predicted.glm2)
confusionMatrix(Bankrupt.test$D, ref2)


#Full GLM shows that R16, R17, R18, R23 and R24 are of importance.
Bankrupt.glm.full <- glm(D ~., data = Bankrupt.train, family = "binomial")
summary(Bankrupt.glm.full)


#Now will try a model with the inputs found from the full GLM model and compare to the model I built with variables of importance from RF 

#GLM with variables that were important from full model 
Bankrupt.GLM.Vars <- glm(D ~ R16 + R17 + R18 + R23 +R24, data = Bankrupt.train, family = "binomial")
summary(Bankrupt.GLM.Vars)


#Predicting with the training set for GLM identified inputs 
cutoff <- 0.5
preds3 <- predict(Bankrupt.GLM.Vars, Bankrupt.train, type = "response")
predicted.glm.vars <- preds3 > cutoff
predicted.glm.vars <- as.numeric(predicted.glm.vars)


#confusion matrix for the training set for GLM identified inputs 
table(Bankrupt.train$D, predicted.glm.vars, dnn = c("Truth", "Predicted"))

#Converting back to factor to do confusion matrix
test2 <- as.factor(predicted.glm.vars)
confusionMatrix(Bankrupt.train$D, test2)


#Predicting for the test set 
cutoff <- 0.5
preds4 <- predict(Bankrupt.GLM.Vars, Bankrupt.test, type = "response")
predicted.glm4 <- preds4 > cutoff
predicted.glm4 <- as.numeric(predicted.glm4)


#Confusion Matrix with the test set 
table(Bankrupt.test$D, predicted.glm4 , dnn = c("Truth", "Predicted"))

#Converting back to factor to do confusion matrix
ref3 <- as.factor(predicted.glm4)
confusionMatrix(Bankrupt.test$D, ref3)




#KNN algorithm - With all predictors 

bankrupt.train.target <- Bankruptcy.df[train, 1 ]
bankrupt.test.target <- Bankruptcy.df[-train, 1]

library(class)

knn <- knn(Bankrupt.train, Bankrupt.test, cl = bankrupt.train.target, k = 10 )

tab <- table(knn, bankrupt.test.target)
accuracy <- function(x){sum(diag(x)/sum(rowSums(x))) * 100 }
accuracy(tab)


 #KNN with predictors from RF: R9 + R24 + R21 + R18 + R15 + R14 + R17 + R23
Bankrupt.subset <- Bankruptcy.df  [ c("D", "R9", "R24", "R21", "R18", "R15", "R14", "R17", "R23")]
head(Bankrupt.subset)


set.seed(1234)
train2 <- sample(nrow(Bankrupt.subset), nrow(Bankrupt.subset) * 0.8)
Bankrupt.train.subset <- Bankrupt.subset[train2, ]
Bankrupt.test.subset <- Bankrupt.subset[-train2, ]

knn2 <- knn(Bankrupt.train.subset, Bankrupt.test.subset, cl = bankrupt.train.target, k = 10)
tab2 <- table(knn2, bankrupt.test.target)
accuracy(tab2)

