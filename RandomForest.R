getwd()

#To calculate accurate precision, recall and ROC
install.packages("precrec")
#General purpose statistic package
install.packages("e1071")
#For Random Forest
install.packages("randomForest")
#For plotting data
install.packages("rpart")
#For gradient boosting
install.packages("gbm")
#For data visualisation
install.packages("ggplot2")

#install.packages("psych")
#For data preparation and cleaning
install.packages("reshape2")

#Loading the packages in R environment
library(precrec)
library(e1071)
library(randomForest)
library(rpart)
#library(gbm)
library(ggplot2)
#library(psych)
library(reshape2)

#Reading the data from local machine. The data is in CSV file format
training_data <- read.csv('C:\\Deepak\\IIM\\Dataset\\Train_Churn.csv')

testing_data <- read.csv('C:\\Deepak\\IIM\\Dataset\\Test_Churn.csv')

## Remove three discrete numerical variables from train and test set
#train_data <- train_data[,setdiff(names(train_data),c('total.intl.calls', 'number.vmail.messages', 'number.customer.service.calls'))]

#test_data <- test_data[,setdiff(names(test_data),c('total.intl.calls', 'number.vmail.messages', 'number.customer.service.calls'))]

#Segregating the columns having categorical and contiuous data

#Preparing list of categorical columns
cat_col <- list('state', 'area.code', 'phone.number', 'international.plan', 'voice.mail.plan', 'Churn')

#Removing all categorical columns from the list using setdiff function
continuous_col <- setdiff(names(training_data),cat_col)



# --------------------------Exploratory Data Analysis--------------------------

#Creating frequency table of target variable

frequency_distribution <- table(training_data$Churn)

# Check the class distribution
print(frequency_distribution)

#Extract the numeric or continous columns
train_numeric <- training_data[,continuous_col]
head(train_numeric)

#The melt function takes data in wide format and stacks a set of columns into a single column of data

train_numeric_melt <- melt(train_numeric)
train_numeric_melt

#converting categorical to charachter
train_categorical <- training_data[,as.character(cat_col)]

for (i in 1:5){
  print(names(train_categorical)[i])
  #applying chisquare test 
  print(chisq.test(table(training_data$Churn, train_categorical[,i])))
}

# multi.hist(train_num, main = '', dcol=c('white','black'), 
#            dlty=c("solid", "solid"), bcol = 'white', density=TRUE)

#Calculate correlation
cor_matrix <- cor(train_numeric)

#reformating the data
melted_cormatrix <- melt(cor_matrix)

#ploting the correlation matrix
ggplot(data = melted_cormatrix, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + theme(
    axis.text.x = element_text(
      angle = 45, vjust = 1, size = 10, hjust = 1), 
    axis.title.x = element_blank(), axis.title.y = element_blank())

#Box Plot 
ggplot(train_numeric_melt, aes(x = 0, y = value)) +
  geom_boxplot() + facet_wrap(~variable, scales='free') + 
  theme(axis.title.x=element_blank())



# --------------------------------Data Preparation Functions-----------------------------

cleaning_data <- function(dataset, remove='None'){
  #loadining the data into another dataset
  cleaned_data <- dataset[,setdiff(names(dataset),c('state', 'phone.number', 'area.code'))]
  
  ##Removing charges column
  
  if (remove == 'charges'){
    cleaned_data <- cleaned_data[,setdiff(names(cleaned_data),c('total.night.charge', 'total.day.charge', 'total.intl.charge',
                                                                'total.eve.charge'))]} 
  
  ##Removing minutes column
  if (remove == 'minutes'){
    cleaned_data <- cleaned_data[,setdiff(names(cleaned_data),c('total.night.minutes', 'total.day.minutes', 'total.intl.minutes',
                                                                'total.eve.minutes'))]}
  
  #Converting Categorical to integer
  cleaned_data$Churn <- as.integer(cleaned_data$Churn)
  cleaned_data$international.plan <- as.integer(cleaned_data$international.plan)
  cleaned_data$voice.mail.plan <- as.integer(cleaned_data$voice.mail.plan)
  
  #Changing values of column churn because we need output in 0 and 1 format
  cleaned_data$Churn[cleaned_data$Churn == 1] <- 0
  cleaned_data$Churn[cleaned_data$Churn == 2] <- 1
  
  #Changing values of column international plan
  cleaned_data$international.plan[cleaned_data$international.plan == 1] <- 0
  cleaned_data$international.plan[cleaned_data$international.plan == 2] <- 1
  
  #Changing values of column voice mail
  cleaned_data$voice.mail.plan[cleaned_data$voice.mail.plan == 1] <- 0
  cleaned_data$voice.mail.plan[cleaned_data$voice.mail.plan == 2] <- 1
  
  #Converting churn column to factor
  cleaned_data$Churn <- as.factor(cleaned_data$Churn)
  
  #Return clean data
  return(cleaned_data)
}


#Removing Outliers
outlier_removal <- function(dataset, columns){
  #Copying the data from one dataset into another
  data <- dataset
  for (col in columns){
    print(col)
    #check if datatype as integer or numeric else converting to integer
    if (class(data[,col]) != 'integer' | class(data[,col]) != 'numeric'){
      data$col <- as.integer(data[,col])
    }
    #Applying percentile for IQR
    percentiles <- quantile(data[,col], c(.25, .75))
    #Because percentile is a list, we use [[]] to access the list data
    quantile_25 <- percentiles[[1]]
    quantile_75 <- percentiles[[2]]
    inter_quartile_range <- quantile_75 - quantile_25
    #setting a range for outliers
    #You can change it to any multiple. We have used 1.5
    min <- quantile_25 - (1.5*inter_quartile_range)
    max <- quantile_75 + (1.5*inter_quartile_range)
    print(paste(quantile_25,quantile_75,inter_quartile_range,min,max))
    #filer data which is outside outlier range. The outliers are removed from data
    data <- data[!(data[,col] < min | data[,i] > max), ]
    #print number of rows impacted
    print(nrow(data))
  }
  return(data)
}

#Scaling the data

standard_data <- function(dataset, source){
  
  # dataset : data frame to be transformed
  # source : data frame whose mean and standard deviation will be 
  # used to transform the 'data' mentioned above
  
  #comparing number of columns 
  if (ncol(dataset) != ncol(source)){
    print('datset and source should have equal number of columns')
    #Exit from function
    return
  }
  #calculating mean for each column
  columns_mean <- sapply(source, mean)
  print(columns_mean)
  #calculating standard deviation for each column
  columns_sd <- sapply(source, sd)
  print(columns_sd)
  for (i in 1:ncol(dataset)){
    #Applying standardise formula
    dataset[,i] <- (dataset[,i] - columns_mean[i])/columns_sd[i] 
  }
  return(dataset)
}




# -----------------------Preparing data for Predictive Modeling ----------------------- 

cleaned_training_data <- cleaning_data(training_data)



#names(cleaned_training_data)

# Dataset without any outliers
#clean_train <- outlier_removal(cleaned_training_data, continuous_col)

#Separating independent and dependent variable from training dataset

x_train <- cleaned_training_data[,!(names(cleaned_training_data) == 'Churn')]

y_train <- cleaned_training_data$Churn



cleaned_test_data <- cleaning_data(testing_data)

#Separating dependent variable from test data as well

x_test <- cleaned_test_data[,!(names(cleaned_test_data) == 'Churn')]

x_test <- standard_data(x_test, x_train)

x_test

y_test <- cleaned_test_data$Churn

#Except Churn column replace all column
cleaned_test_data[,!(names(cleaned_test_data) == 'Churn')] <- x_test


cleaned_test_data


x_train <- standard_data(x_train, x_train)

x_train


cleaned_training_data[,!(names(cleaned_training_data) == 'Churn')] <- x_train

cleaned_training_data



# ********************************Random Forest********************************

set.seed(1)

rf <- randomForest(Churn ~ ., data = cleaned_training_data)

#It will save probability result against customer churn 
rf_predictions_train_prob <- predict(rf, type = 'prob', x_train)[, 2]

#In case custmer churn probability higher tha .5 assign 1 else 0

rf_predictions_train <- ifelse(rf_predictions_train_prob > 0.5, 1, 0)

rf_predictions_train

#Calculate the accuracy of the trained model
rf_accuracy_train <- sum(rf_predictions_train == y_train)/nrow(cleaned_training_data)




#To calculate precision, recall and ROC
rf_average_precision_train <- evalmod(scores = rf_predictions_train_prob, labels = y_train)

#----------------------------------------------------------------------------------------

rf_predictions_test_prob <- predict(rf, x_test, type = 'prob')[, 2]

rf_predictions_test <- ifelse(rf_predictions_test_prob > 0.5, 1, 0)

rf_accuracy_test <- sum(rf_predictions_test == y_test)/nrow(cleaned_test_data)


rf_average_precision_test <- evalmod(scores = rf_predictions_test_prob, labels = y_test)

# **************************Precision-Recall Curves*****************************


labels <- (y_test)


scores <- join_scores(rf_predictions_test_prob)



mdat <- mmdata(scores, labels, modnames = c('Random Forest'), dsids = c(1))
curves <- evalmod(mdat)

autoplot(curves, 'PRC')

# ******Clubbing the results******

cleaned_test_data$rf_predictions_test <- rf_predictions_test

head(cleaned_test_data)



# ***********Print performance metrics of the best model************

sprintf('Training set accuracy of the best model i.e. Random Forest 
        is : %f', rf_accuracy_train)

sprintf('Test set accuracy of the best model i.e. Random Forest 
        is : %f', rf_accuracy_test)

sprintf('Average precision score of the best model i.e. Random Forest 
        on training set is : %f', attr(rf_average_precision_train, 'aucs')[2,4])

sprintf('Average precision score of the best model i.e. Random Forest 
        on test set is : %f', attr(rf_average_precision_test, 'aucs')[2,4])

# *************************************End**************************************

