# arules --	Provides the infrastructure for representing, manipulating and analyzing transaction data and patterns (frequent itemsets and association rules).
# arulesViz --	Extends package 'arules' with various visualization techniques for association rules and item-sets. The package also includes several interactive visualizations for rule exploration.
# tidyverse	-- The tidyverse is an opinionated collection of R packages designed for data science
# readxl --	Read Excel Files in R
# plyr --	Tools for Splitting, Applying and Combining Data
# ggplot2 --	Create graphics and charts
# knitr	-- Dynamic Report generation in R
# lubridate --	Lubridate is an R package that makes it easier to work with dates and times.



#install and load package arules
#install.packages("arules")
library(arules)
#install and load arulesViz
#install.packages("arulesViz")
library(arulesViz)
#install and load tidyverse
#install.packages("tidyverse")
library(tidyverse)
#install and load readxml
#install.packages("readxml")
library(readxl)
#install and load knitr
#install.packages("knitr")
library(knitr)
#load ggplot2 as it comes in tidyverse
library(ggplot2)
#install and load lubridate
#install.packages("lubridate")
library(lubridate)
#install and load plyr
#install.packages("plyr")
library(plyr)
library(dplyr)






# Attribute Information
# InvoiceNo: Invoice number. Nominal, a 6-digit integral number uniquely assigned to each transaction. If this code starts with letter 'c', it indicates a cancellation. +StockCode: Product (item) code. Nominal, a 5-digit integral number uniquely assigned to each distinct product.
# Description: Product (item) name. Nominal.
# Quantity: The quantities of each product (item) per transaction. Numeric.
# InvoiceDate: Invoice Date and time. Numeric, the day and time when each transaction was generated. Example from dataset: 12/1/2010 8:26
# UnitPrice: Unit price. Numeric, Product price per unit in sterling.
# CustomerID: Customer number. Nominal, a 5-digit integral number uniquely assigned to each customer.
# Country: Country name. Nominal, the name of the country where each customer resides.








#read excel into R dataframe
retail <- read_excel('C:/HenryHarvin/Dataset/Online_Retail.xlsx')
#complete.cases(data) will return a logical vector indicating which rows have no missing values. Then use the vector to get only rows that are complete using retail[,].
retail <- retail[complete.cases(retail), ]
#mutate function is from dplyr package. It is used to edit or add new columns to dataframe. Here Description column is being converted to factor column. as.factor converts column to factor column. %>% is an operator with which you may pipe values to another function or expression
retail %>% mutate(Description = as.factor(Description))

retail %>% mutate(Country = as.factor(Country))



#Converts character data to date. Store InvoiceDate as date in new variable
retail$Date <- as.Date(retail$InvoiceDate)
#Extract time from InvoiceDate and store in another variable
TransTime<- format(retail$InvoiceDate,"%H:%M:%S")
#Convert and edit InvoiceNo into numeric
InvoiceNo <- as.numeric(as.character(retail$InvoiceNo))


#Bind new columns TransTime and InvoiceNo into dataframe retail
cbind(retail,TransTime)
cbind(retail,InvoiceNo)

#get a glimpse of your data
glimpse(retail)

# Before applying MBA/Association Rule mining, we need to convert dataframe into transaction data 
# so that all items that are bought together in one invoice are in one row. 


# We need to do is group data in the retail dataframe either by CustomerID, CustomerID, and Date 
# we can also group data using InvoiceNo and Date. 
# We need this grouping and apply a function on it and store the output in another dataframe. 
# This can be done by ddply

library(plyr)
#ddply(dataframe, variables_to_be_used_to_split_data_frame, function_to_be_applied)
transactionData <- ddply(retail,c("InvoiceNo","Date"),
                         function(df1)paste(df1$Description,
                                            collapse = ","))
#The R function paste() concatenates vectors to character and separated results using collapse=[any optional charcater string ]. Here ',' is used


transactionData

#Next, as InvoiceNo and Date will not be of any use in the rule mining, you can set them to NULL.

#set column InvoiceNo of dataframe transactionData  
transactionData$InvoiceNo <- NULL
#set column Date of dataframe transactionData
transactionData$Date <- NULL
#Rename column to items
colnames(transactionData) <- c("items")
#Show Dataframe transactionData
transactionData

# This format for transaction data is called the basket format. 
# Next, you have to store this transaction data into a .csv (Comma Separated Values) file. 
# For this, write.csv()

write.csv(transactionData,"market_basket_transactions.csv", quote = FALSE, row.names = TRUE)

tr <- read.transactions('market_basket_transactions.csv', format = 'basket', sep=',')

getwd()

# The following line of code will take transaction data file "market_basket_transactions.csv"
# which is in basket format and convert it into an object of the transaction class

summary(tr)

#There are 22191 transactions (rows) and 30066 items (columns). 

# Element (itemset/transaction) length distribution: This is telling you how many transactions are there for 1-itemset, for 2-itemset and so on. 
# The first row is telling you a number of items and the second row is telling you the number of transactions.


# Create an item frequency plot for the top 20 items
if (!require("RColorBrewer")) {
  # install color package of R
  install.packages("RColorBrewer")
  #include library RColorBrewer
  library(RColorBrewer)
}
itemFrequencyPlot(tr,topN=20,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")

itemFrequencyPlot(tr,topN=20,type="relative",col=brewer.pal(8,'Pastel2'),main="Relative Item Frequency Plot")

#Generating Rules
# Min Support as 0.001, confidence as 0.8.
association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8,maxlen=10))



summary(association.rules) 

# shows the following:
# Parameter Specification: min_sup=0.001 and min_confidence=0.8 values with 10 items as max of items in a rule.
# 
# Total number of rules: The set of 49122 rules
# 
# Distribution of rule length: A length of 5 items has the most rules: 16424 and length of 2 items have the lowest number of rules:105
# 
# Summary of Quality measures: Min and max values for Support, Confidence and, Lift.
# 
# Information used for creating rules: The data, support, and confidence we provided to the algorithm.


inspect(association.rules[1:10])

# Using the above output, you can make analysis such as:
#   
# 100% of the customers who bought 'WOBBLY CHICKEN' also bought 'METAL'.
# 
# 100% of the customers who bought 'BLACK TEA' also bought SUGAR 'JARS'.

# Removing redundant rules
# You can remove rules that are subsets of larger rules. Use the code below to remove such rules:

subset.rules <- which(colSums(is.subset(association.rules, association.rules)) > 1) # get subset rules in vector

length(subset.rules)

subset.association.rules. <- association.rules[-subset.rules] # remove subset rules.


# which() returns the position of elements in the vector for which value is TRUE.
# 
# colSums() forms a row and column sums for dataframes and numeric arrays.
# 
# is.subset() Determines if elements of one vector contain all the elements of other

# Finding Rules related to given items
# Sometimes, you want to work on a specific product. 
# If you want to find out what causes influence on the purchase of item X you can use appearance option 
# in the apriori command. appearance gives us options to set LHS (IF part) and RHS (THEN part) of the rule.


metal.association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8),appearance = list(default="lhs",rhs="METAL"))

inspect(head(metal.association.rules))

# Similarly, to find the answer to the question Customers who bought METAL also bought
# .... you will keep METAL on lhs:


metal.association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8),appearance = list(lhs="METAL",default="rhs"))

inspect(head(metal.association.rules))


#Visualizing Association Rules

# Filter rules with confidence greater than 0.4 or 40%
subRules<-association.rules[quality(association.rules)$confidence>0.4]
#Plot SubRules
plot(subRules)

#The above plot shows that rules with high lift have low support.



#plot(rulesObject, measure, shading, method)

# rulesObject: the rules object to be plotted
# 
# measure: Measures for rule interestingness. Can be Support, Confidence, lift or combination of these depending upon method value.
# 
# shading: Measure used to color points (Support, Confidence, lift). The default is Lift.
# 
# method: Visualization method to be used (scatterplot, two-key plot, matrix3D).


plot(subRules,method="two-key plot")

# The two-key plot uses support and confidence on x and y-axis respectively. 
# It uses order for coloring. The order is the number of items in the rule.


#Individual Rule Representation
# This representation is also called as Parallel Coordinates Plot. 
# It is useful to visualized which products along with which items cause what kind of sales.

# the RHS is the Consequent or the item we propose the customer will buy;
# the positions are in the LHS where 2 is the most recent addition to our basket 
# and 1 is the item we previously had.


subRules2<-head(subRules, n=20, by="lift")
plot(subRules2, method="paracoord")

# Look at the topmost arrow. It shows that when I have 'CHILDS GARDEN SPADE PINK' and 'CHILDS GARDEN RAKE PINK' in my shopping cart, 
# I am likely to buy 'CHILDS GARDEN RAKE BLUE' along with these as well.

