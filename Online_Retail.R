install.packages("ggplot2")
library(ggplot2)
#Use Case - Online Retail dataset
#Understanding Purchase behaviour
#Reading the dataset
#Chnange the format of Invoice date

Online_Retail_Dataset = read.csv("C:\\Deepak\\IIM\\Dataset\\Online Retail.csv")

#Dimension of dataset

dim(Online_Retail_Dataset)

#Number of Rows = 541909
#Number of columns = 8

#Check top 6 rows of dataset
head(Online_Retail_Dataset)





#Summarise the dataset volumns
str(Online_Retail_Dataset)

#To check if there is repeated customers
length(unique(Online_Retail_Dataset$CustomerID))

#Calculating number of rows having customer id missing
sum(is.na(Online_Retail_Dataset$CustomerID))

#Extracting only those rows where we have Customer ID
clean_retail_dataset <- subset(Online_Retail_Dataset, !is.na(Online_Retail_Dataset$CustomerID))

dim(clean_retail_dataset)

#Checking the frequency distribution of data country-wise
table(clean_retail_dataset$Country)

#Extracting only United Kingdom data from dataset
UK_data <- subset(clean_retail_dataset, Country == "United Kingdom")

dim(UK_data)

#Now we will start working on UK customers only

#Check number of unique invoices/transaction and unique customers

length(unique(UK_data$InvoiceNo))
length(unique(UK_data$CustomerID))



?grepl
# Identify returns
# grepl returns TRUE if a string contains the pattern, otherwise FALSE;
#A cancel invoice has invoice number like C547104
#Fixed = TRUE means check as it is 
UK_data$Return_Item <- grepl("C", UK_data$InvoiceNo, fixed=TRUE)

head(UK_data$Return_Item)

#We have added one more column in UK_data dataset for cancel invoice using $ sign

# 0 for Return or cancelation of invoice
#1 for purchase

UK_data$purchased_item <- ifelse(UK_data$Return_Item=="TRUE", 0, 1)

#If Return_Item value is TRUE assign 0 else assign 1

head(UK_data$purchased_item)

#Now starts the understanding of customer buying behaviour. 
#Lets study  Recency, Frequency and Monetory parameter

# Recency refers to the number of days that have elapsed 
# since the customer last purchased something. or when the customer visited the 
#store and made transaction. 

#Frequency refers to the number of invoices with purchases during the year for 
#each customer

# Monetary value is the amount that the customer spent during the year. 

#Creating customer dataset

List_of_customers <- as.data.frame(unique(UK_data$CustomerID))
names(List_of_customers)

#Changing the column name to customerid
names(List_of_customers) <- "CustomerID"

#Converting datet time to date only. 
as.Date(UK_data$InvoiceDate)


#Recency

UK_data$recency <- as.Date("2011-12-10") - as.Date(UK_data$InvoiceDate)



# remove returns and select only purchased items 
purchased <- subset(UK_data, purchased_item == 1)

head(purchased)

# Obtain number of days since most recent purchase
# geting customerid group by recency 
recency <- aggregate(recency ~ CustomerID, data=purchased, FUN=min, na.rm=TRUE)
#This means group recency by customerid. Min will let us know when last the customer visited the store


?aggregate

head(recency)
#Customer 12346 visited almost a year back and 12748 just visit the store
#The prupose of purchase is over, lets remove it
remove(purchased)

# merge or joining the recency data with main  customer data list

List_of_customers <- merge(List_of_customers, recency, by="CustomerID", all=TRUE, sort=TRUE)
#Now no need of recency so remove it. Customer dataset already have this information
remove(recency)

class(List_of_customers$recency)
#Converting to numeric data type
List_of_customers$recency <- as.numeric(List_of_customers$recency)

head(List_of_customers)

###Recency part over
########################################################################################
#Frequency Starts

customer.invoices <- subset(UK_data, select = c("CustomerID","InvoiceNo", "purchased_item"))
customer.invoices <- customer.invoices[!duplicated(customer.invoices), ]
customer.invoices <- customer.invoices[order(customer.invoices$CustomerID),]

head(customer.invoices)
#removing  
row.names(customer.invoices) <- NULL



# Number of invoices/year (purchases only) for each customerid
annual.invoices <- aggregate(purchased_item ~ CustomerID, data=customer.invoices, FUN=sum, na.rm=TRUE)
#Changing the name of purchase_item to frequency
names(annual.invoices)[names(annual.invoices)=="purchased_item"] <- "frequency"

# Add number  of invoices per customer to customers data
List_of_customers <- merge(List_of_customers, annual.invoices, by="CustomerID", all=TRUE, sort=TRUE)
#removing temporary dataset
remove(customer.invoices, annual.invoices)

range(List_of_customers$frequency)
table(List_of_customers$frequency)

# Remove customers who have not made any purchases in the past year, so removing 29 records
List_of_customers <- subset(List_of_customers, frequency > 0)

head(List_of_customers)


####################Frequency ends here

###############################
# Monetary Value of Customers #
###############################

# Total spent on each item and adding column Amount
UK_data$Amount <- UK_data$Quantity * UK_data$UnitPrice

# Aggregated total sales to customer
annual.sales <- aggregate(Amount ~ CustomerID, data=UK_data, FUN=sum, na.rm=TRUE)
#Changing name to monetory
names(annual.sales)[names(annual.sales)=="Amount"] <- "monetary"

# Add monetary value to customers dataset
List_of_customers <- merge(List_of_customers, annual.sales, by="CustomerID", all.x=TRUE, sort=TRUE)
remove(annual.sales)

# Identify customers with negative monetary value numbers. These are the customer who presumably returning purchases from the preceding year
hist(List_of_customers$monetary)
List_of_customers$monetary <- ifelse(List_of_customers$monetary < 0, 0, List_of_customers$monetary) # reset negative numbers to zero
hist(List_of_customers$monetary)


#Preprocess Data

# Log-transform positively-skewed variables
List_of_customers$recency.log <- log(List_of_customers$recency)
List_of_customers$frequency.log <- log(List_of_customers$frequency)
List_of_customers$monetary.log <- List_of_customers$monetary + 0.1 # can't take log(0), so add a small value to remove zeros
List_of_customers$monetary.log <- log(List_of_customers$monetary.log)

# Z-scores
List_of_customers$recency.z <- scale(List_of_customers$recency.log, center=TRUE, scale=TRUE)
List_of_customers$frequency.z <- scale(List_of_customers$frequency.log, center=TRUE, scale=TRUE)
List_of_customers$monetary.z <- scale(List_of_customers$monetary.log, center=TRUE, scale=TRUE)





names(List_of_customers)
#We need only normalised values
preprocessed <- List_of_customers[,8:10]
#Preparation for KNN starts
k <- 10 # specify the maximum number of clusters you want to try out

models <- data.frame(k=integer(),
                     tot.withinss=numeric(),
                     betweenss=numeric(),
                     totss=numeric(),
                     rsquared=numeric())

?kmeans
for (r in 1:k ) {
  
  print(r)
  
  # Run kmeans
  # nstart = number of initial configurations; the best one is used
  # $iter will return the iteration used for the final model
  output <- kmeans(preprocessed, centers = r)
  
  # Add cluster membership to customers dataset
  var.name <- paste("cluster", r, sep="_")
  List_of_customers[,(var.name)] <- output$cluster
  List_of_customers[,(var.name)] <- factor(List_of_customers[,(var.name)], levels = c(1:r))
  
  # Graph clusters
  cluster_graph <- ggplot(List_of_customers, aes(x = frequency.log, y = monetary.log))
  cluster_graph <- cluster_graph + geom_point(aes(colour = List_of_customers[,(var.name)]))
  colors <- c('red','orange','green3','deepskyblue','blue','darkorchid4','violet','pink1','tan3','black')
  cluster_graph <- cluster_graph + scale_colour_manual(name = "Cluster Group", values=colors)
  cluster_graph <- cluster_graph + xlab("Log-transformed Frequency")
  cluster_graph <- cluster_graph + ylab("Log-transformed Monetary Value of Customer")
  title <- paste("k-means Solution with", r, sep=" ")
  title <- paste(title, "Clusters", sep=" ")
  cluster_graph <- cluster_graph + ggtitle(title)
  print(cluster_graph)

  
  # # # Cluster centers in original metrics
  # library(plyr)
  #  print(title)
  #  cluster_centers <- ddply(List_of_customers, .(List_of_customers[,(var.name)]), summarize,
  #                           monetary=round(median(monetary),2),# use median b/c this is the raw, heavily-skewed data
  #                           frequency=round(median(frequency),1),
  #                           recency=round(median(recency), 0))
  #  names(cluster_centers)[names(cluster_centers)=="customers[, (var.name)]"] <- "Cluster"
  #  print(cluster_centers)
  #  cat("\n")
  #  cat("\n")
  
  # Collect model information
  models[r,("k")] <- r
  models[r,("tot.withinss")] <- output$tot.withinss # the sum of all within sum of squares
                                                    #It tells homogenity, lower the value good for cluster
  models[r,("betweenss")] <- output$betweenss #it is the mean of distances between cluster centers. 
                                              #this ratio, to be as higher as possible, 
                                              #since we would like to have heterogenous clusters.
  models[r,("totss")] <- output$totss # betweenss + tot.withinss
  models[r,("rsquared")] <- round(output$betweenss/output$totss, 3) # percentage of variance explained by cluster membership
  assign("models", models, envir = .GlobalEnv)
  #If we just have 1 cluster, then ... $betweenss = 0  and  $tot.withinss = $withinss  and  $totss = $tot.withinss
  remove(output, var.name, cluster_graph, title, colors)
  
}

head(List_of_customers)
(models)

remove(k)  


library(scales)

#we expect the within sum of squares ratio to be as lower as possible
ss_graph <- ggplot(models, aes(x = k, y = tot.withinss))
ss_graph <- ss_graph + geom_point() + geom_line()
ss_graph <- ss_graph + scale_x_continuous(breaks = 1:k)
ss_graph <- ss_graph + scale_y_continuous(labels = scales::comma)
ss_graph <- ss_graph + xlab("k (Number of Clusters)")
ss_graph <- ss_graph + ylab("Total Within SS")
ss_graph
