#############################################
#                                           #
# Name:       Ashley Music                  #
# Date:       07/06/2020                    #
# Subject:    Laptops in London             #
# Class:      BDAT 625                      #
# File Name:  Week2.1HW__Music_Ashley.R     #
#                                           #
#############################################

rm(list = ls())
library(readr)
LaptopSalesJanuary2008 <- read_csv("C:/Users/aggie/Desktop/Data Mining/LaptopSalesJanuary2008.csv")
LapTopSalesJan2008 <- as.data.frame(LaptopSalesJanuary2008)

View(LapTopSalesJan2008)

StorePostCode <- LapTopSalesJan2008$`Store Postcode` 
RetailPrice <- LapTopSalesJan2008$`Retail Price`

# Part 1 
# 1.1 Create a bar chart, showing the average retail price by store. 
# Which store has the highest average?  NI7 6 QA 
# Which has the lowest? E2 0RY 

#get average(mean) retail by store 
plot.data <- aggregate(RetailPrice, by = list(StorePostCode), FUN = mean)
names(plot.data)<- c("PostalCode", "AvgSales")   
library(ggplot2)
ggplot(plot.data)+geom_bar(aes(x = PostalCode, y = AvgSales), stat = "identity")


# 1.2 Create side-by-side boxplots of retail price by store, to better compare retail prices across stores. 
# Now compare the prices in the two stores from (a). 
# Does there seem to be a difference between their price distributions?
# Answer: Store E2 0RY has the lowest min retail prices than almost all the stores
#(with the exception of W4 3PH, but they still have a higher average price than E2 ORY).
# Store NI7 6 QA has a higher min price than almost all other stores,
# but it is almost tied with stores KT2 5AU and E7 8NW for the highest retail price. 
# Interestingly, store E2 0RY has some high selling outliers

par(mfcol= c(1,4))
ggplot(LapTopSalesJan2008) + geom_boxplot(aes(x = as.factor(StorePostCode), y = RetailPrice)) + xlab("PostalCode")


# Part 2 
# 2.1 
library(readr)
LaptopSales <- read_csv("C:/Users/aggie/Desktop/Data Mining/LaptopSales.csv", 
                        col_types = cols(Date = col_datetime(format = "%m/%d/%Y %H:%M")))
View(LaptopSales)

# At what price are the laptops actually selling? 
# Answer: Most frequently selling price is 450-500

hist(LaptopSales$`Retail Price`, xlab = "Price")


# Does price change with time? 
# Answer: Yes, price changes over time We can see it has peaks around September (8) 
# It is also high around December. It has lows around March.

install.packages('forecast', dependencies = TRUE)
library(forecast)
sales.ts <- ts(LaptopSales$`Retail Price`, start = c(2008, 1), end = c(2008, 12), freq = 12)
plot(sales.ts, xlab = "Year", ylab = "Sales")




# Are prices consistent across retail outlets?
# Answer: Yes, prices appear to be consistent across the different stores.
# We do see a few outliers (I see 5), but they are not too far off. 
StorePostCode2 <- LaptopSales$`Store Postcode`
RetailPrice2 <- LaptopSales$`Retail Price`
ggplot(LaptopSales) + geom_boxplot(aes(x = as.factor(StorePostCode2), y = RetailPrice2)) + xlab("PostalCode")




# How does price change with configuration?
# Answer: As the number of configurations goes up in this data, so does the price. 
# There are exceptions seen in this data. 
# The data almost seemed group into 6 distinct clusters.  

ggplot(LaptopSales, aes(x = `Retail Price`, y = Configuration))+ geom_point()







