#Apriori Algorithm

#Mining frequent items bought together

install.packages("arules")
install.packages("arulesViz")
install.packages("RColorBrewer")

library(arules)
library(arulesViz)
library(RColorBrewer)

data("Groceries")
str(Groceries)

#Groceries is internally divided into three slots: Data, itemInfo and itemsetInfo.

#The slot "Data" contains the dimensions, 
#dimension names and other numerical values of number of products sold by every transaction made.

head(Groceries@itemInfo, n=12)


inspect(Groceries)

summary(Groceries)

#Support is the basic probability of an event to occur. If we have an event to buy product A, 
#Support(A) is the number of transactions which includes A divided by total number of transactions.

#Confidence: The confidence of an event is the conditional probability of the occurrence; 
#the chances of A happening given B has already happened.

#Lift: This is the ratio of confidence to expected confidence.
#The lift value tells us how much better a rule is at predicting something than randomly guessing. 
#The higher the lift, the stronger the association.

rules <- apriori(Groceries,parameter = list(supp = 0.001, conf = 0.80))

#Top 10 strong rules
inspect(rules[1:10])

#if we buy Liquor and Red Wine, we are very likely to buy bottled beer.

#Build Histogram to depict how many times an item has occurred in our dataset as compared to the others.

arules::itemFrequencyPlot(
  Groceries,topN=20,col=brewer.pal(8,'Pastel2'),
  main='Relative Item Frequency Plot',
  type="relative",ylab="Item Frequency (Relative)")


#To boost sales of eggs I can place it beside my milk and vegetables.

plot(rules[1:20],
     
     method = "graph",
     
     control = list(type = "items"))

#The size of graph nodes is based on support levels and the colour on lift ratios. 

#Most of our transactions were consolidated around "Whole Milk".
#liquor and wine are very strongly associated so we must place these together.
#people who buy tropical fruits and herbs also buy rolls and buns.


#More Graphs
plot(rules[1:20],
     
     method = "paracoord",
     
     control = list(reorder = TRUE))


#More graphical representation to hover over each rule and see the Support, Confidence and Lift.
arulesViz::plotly_arules(rules)








