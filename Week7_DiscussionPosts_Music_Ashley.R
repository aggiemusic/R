#############################################
#                                           #
# Name:       Ashley Music                  #
# Date:       08/12/2020                    #
# Subject:    NN                            #
# Class:      BDAT 625                      #
# File Name:                                #
# Week7_DiscussionPosts_Music_Ashley        # 
#                                           #
#############################################

setwd("C:/Users/aggie/Desktop/Data Mining")
rm(list = ls())
library(tm)



# Load the above file into R and create a label vector.

corp <- Corpus(ZipSource("AutoAndElectronics.zip", recursive = T))
label <- c(rep(1, 1000), rep(0, 1000))


# Preprocess the documents. Explain what would be different if you did not perform the "stemming" step.
# Answer: We would have potentially duplicate terms and that might impact the analysis. 

#Toenization - dividing the text into seperate tokens. 
corp <- tm_map(corp, stripWhitespace) 
corp <- tm_map(corp, removePunctuation) 
corp <- tm_map(corp, removeNumbers) 


#Stopwords - Removing the frequently occuring terms like I, Me, myself, we, etc. 
corp <- tm_map(corp, removeWords, stopwords("english"))

library(SnowballC)
install.packages("SnowballC")
#Stemming - Reducing different varients of words 
corp <- tm_map(corp, stemDocument) 


tdm <- TermDocumentMatrix(corp)
tfidf <- weightTfIdf(tdm)




# Use the lsa package from R to create 10 concepts. Explain what is different about the concept matrix, as opposed to the TF-IDF matrix.
#Answer: Concept matrix -  This is a way to reduce the concepts, 
# similar to the PCA it finds the principal components that are linear combo's of the original  and a subset of them will serve as new variables to replace the original  ones.  
# TF-IDF method - this measures the importance of a term in a document or the number of times that term appears in the document. 
# TF-DF is looking for frequent occurance of rare terms in the document. 
# It gives high values for docs with high frequency for terms that are rare and almost 0 values for terms that are absent from a doc. 


install.packages("lsa")
library(lsa)

lsa.tfidf <- lsa(tfidf, dim = 10)

words.df <- as.data.frame(as.matrix(lsa.tfidf$dk)) 
head(words.df)


# Using this matrix, fit a predictive model (different from the model presented in the chapter illustration) to classify documents as autos or electronics. 
# Compare its performance to that of the model presented in the chapter illustration.

training <- sample(c(1:2000), 0.6*2000)
trainData = cbind(label = label[training], words.df[training,])
head(trainData)

#Running the glm 
reg <- glm(label ~ ., data = trainData, family = 'binomial')
summary(reg)

validData = cbind(label = label[-training], words.df[-training,])
pred <- predict(reg, newdata = validData, type = "response")


library(caret)

#turning predictions and labels into factors to use for confusion matrix. 
pred.labels <- factor(label[-training])
pred.factor <- factor(ifelse(pred>0.5, 1, 0))

#Creating the confusion matrix
confusionMatrix(pred.factor, pred.labels)

#SVM - I will predict the labels (0 or 1) with a Support Vector Machine. 

library(e1071)
#Turning response into a factor to use for SVM.
trainData$label <- factor(trainData$label)

#Fitting the model 
svm.fit <- svm(label ~., data = trainData)


summary(svm.fit)

#Creating predictions 
pred.4 <- predict(svm.fit, newdata = validData)

#Generating a confusion matrix. 
confusionMatrix(pred.4, pred.labels)










