#############################################
#                                           #
# Name:       Ashley Music                  #
# Date:       08/10/2020                    #
# Subject:    NN                            #
# Class:      BDAT 625                      #
# File Name:                                #
# Week_TextMining_Music_Ashley              # 
#                                           #
#############################################
setwd("C:/Users/aggie/Desktop/Data Mining")
rm(list = ls())

install.packages('tm')
library(tm)
library(NLP)
library()


corp <- Corpus(ZipSource("AutoElectronics.zip", recursive = T))

