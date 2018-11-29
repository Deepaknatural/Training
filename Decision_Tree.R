#Decision Tree

#Decision trees are versatile Machine Learning algorithm that can perform both classification and regression tasks.

#Import Data
#The purpose of this dataset is to predict which people are more likely to survive after the collision with the iceberg. 
#The dataset contains 13 variables and 1309 observations. 
#The dataset is ordered by the variable X.
install.packages("rpart.plot")	

set.seed(678)
path <- 'https://raw.githubusercontent.com/thomaspernet/data_csv_r/master/data/titanic_csv.csv'
titanic <-read.csv(path)
head(titanic)

#Last six records
tail(titanic)

#Shuffle the data to get sample of all classes of passengers
shuffle_index <- sample(1:nrow(titanic))
head(shuffle_index)

titanic <- titanic[shuffle_index, ]
head(titanic)

#Data Cleaning - Deleting NA

install.packages("dplyr")
library(dplyr)
# Drop variables
clean_titanic <- titanic %>%
  select(-c(home.dest, cabin, name, X, ticket)) %>% 
  #Convert to factor level
  mutate(pclass = factor(pclass, levels = c(1, 2, 3), labels = c('Upper', 'Middle', 'Lower')),
         survived = factor(survived, levels = c(0, 1), labels = c('Died', 'Survived'))) %>%
  na.omit()
glimpse(clean_titanic)


#Creating Training dataset
create_train_test <- function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample = c(1: total_row)
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}


data_train <- create_train_test(clean_titanic, 0.8, train = TRUE)
data_test <- create_train_test(clean_titanic, 0.8, train = FALSE)

dim(data_train)
dim(data_test)

#Frequency Table
prop.table(table(data_train$survived))


library(rpart.plot)

library(rpart)
#Training the model
fit <- rpart(survived~., data = data_train, method = 'class' )
rpart.plot(fit, extra = 106)

#install.packages("rattle")
#library(rattle)

library(RColorBrewer)



#install.packages("RGtk2")
#rattle()
fancyRpartPlot(fit)



#By default, rpart() function uses the Gini impurity measure to split the note. 
#The higher the Gini coefficient, the more different instances within the node.

#You can predict your test dataset. To make a prediction, you can use the predict() function. 
# predict(fitted_model, df, type = 'class')
# arguments:
#   - fitted_model: This is the object stored after model estimation. 
# - df: Data frame used to make the prediction
# - type: Type of prediction			
# - 'class': for classification			
# - 'prob': to compute the probability of each class			
# - 'vector': Predict the mean response at the node level

predict_unseen <-predict(fit, data_test, type = 'class')

print(predict_unseen)

#Confusion Metrics

table_mat <- table(data_test$survived, predict_unseen)
table_mat

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(accuracy_Test)


# rpart.control(minsplit = 20, minbucket = round(minsplit/3), maxdepth = 30)
# Arguments:
#   -minsplit: Set the minimum number of observations in the node before the algorithm perform a split
# -minbucket:  Set the minimum number of observations in the final note i.e. the leaf
# -maxdepth: Set the maximum depth of any node of the final tree. The root node is treated a depth 0


##Function approach to calculate accuracy
accuracy_tune <- function(fit) {
  predict_unseen <- predict(fit, data_test, type = 'class')
  table_mat <- table(data_test$survived, predict_unseen)
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  accuracy_Test
}

#Rule based controling
control <- rpart.control(minsplit = 4,
                         minbucket = round(5 / 3),
                         maxdepth = 3,
                         cp = 0)
#Again fit the model
tune_fit <- rpart(survived~., data = data_train, method = 'class', control = control)

#Check Accuracy
accuracy_tune(tune_fit)
