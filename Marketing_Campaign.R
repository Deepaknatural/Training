rm(list=ls(all=T))

install.packages("mlbench")
#### Setting working Directory
setwd("C:\\Deepak\\IIM\\Dataset")

######### Load Libraries ###############
library(ggplot2)
library(mlbench)
library(caret)

library(dummies)

library(e1071)  
############# Read the data  ##############
data = read.csv("bank.csv", header = T)
data = data[complete.cases(data), ]



#######  Missing  Value Analysis #############################

# creating dataframe with missing values
missing_val = data.frame(apply(data,2,function(x){sum(is.na(x))}))

#convert rows into columns
missing_val$Columns = row.names(missing_val)
row.names(missing_val)=NULL
names(missing_val)[1] =  "Missing_number"

View(missing_val)

########## To print unique valures##########
unique(data$education)

# Let us group "basic.4y", "basic.9y" and "basic.6y" together and call 
#them "basic".
data$education=as.character(data$education)
data[data$education=='basic.6y',"education"]='Basic'
data[data$education=='basic.9y',"education"]='Basic'
data[data$education=='basic.4y',"education"]='Basic'


#After grouping, this is the columns:

unique(data$education)

###############      Data exploration       ###################
library(data.table)
uniqueN(data$y)

# plotting bar plot for dependent variable
tbl <- with(data, table(y))
barplot(tbl, beside = TRUE, legend = TRUE)


# colored bar plot
library(ggplot2)
ggplot(as.data.frame(tbl), aes(factor(y), Freq, fill =y)) +geom_col(position = 'dodge')

#further exploration

count_no_sub = length(data[data['y']==0])
count_sub = length(data[data['y']==1])
pct_of_no_sub = count_no_sub/(count_no_sub+count_sub)
print(paste("percentage of no subscription is", pct_of_no_sub*100))
pct_of_sub = count_sub/(count_no_sub+count_sub)
print(paste("percentage of subscription", pct_of_sub*100))


#Our classes are imbalanced, and the ratio of no-subscription to subscription 
#instances is 89:11. Before we go ahead to balance the classes,







#Visualizations


#  First Plot for job

color=c('red','blue')
job_table=table(data$y,data$job)
barplot(job_table,col=color,legend=TRUE,ylab='Frequency of Purchase',xlab='Job',main='Purchase Frequency for Job Title')

# we can see from graph The frequency of purchase of the deposit depends a 
#great deal on the job title.Thus, the job title can be a good predictor of 
#the outcome variable.



# second plot for marital
color=c('red','blue')
job_table=table(data$y,data$marital)
barplot(job_table,col=color,legend=TRUE,ylab='Frequency of  Customers',xlab='Marital Status',main='Stacked Bar Chart of Marital Status vs Purchase')



# Third plot for education
color=c('red','blue')
job_table=table(data$y,data$education)
barplot(job_table,col=color,legend=TRUE,ylab='Frequency of  Customers',xlab='Education',main='Stacked Bar Chart of Education vs Purchase')

#Education seems a good predictor of the outcome variable.


# fourth plot for day of week
color=c('blue','green')
job_table=table(data$y,data$day_of_week)
barplot(job_table,col=color,legend=TRUE,ylab='Frequency of Purchase',xlab='Day of Week',main='Purchase Frequency for Day of Week')

#Day of week may not be a good predictor of the outcome.



# fifth plot for month
color=c('blue','green')
job_table=table(data$y,data$month)
barplot(job_table,col=color,legend=TRUE,ylab='Frequency of Purchase',xlab='Month',main='Purchase Frequency for Month')

#Month might be a good predictor of the outcome variable


# plot for poutcome
color=c('blue','green')
job_table=table(data$y,data$poutcome)
barplot(job_table,col=color,legend=TRUE,ylab='Frequency of Purchase',xlab='poutcome',main='pur_fre_pout_bar')

#Poutcome seems to be a good predictor of the outcome variable.



############ Creating dummuy Variables ##########

data.new <- dummy.data.frame(data, sep = "_")

# our final data columns
names(data.new)



########### Spliting data into train and test with 70-30  ##########

sample.data= sample(1:nrow(data.new),size= round(0.7*nrow(data.new)))
train= data.new[sample.data,]
test= data.new[-sample.data,]
train = train[complete.cases(train), ]

colnames(train)






################## Model Making using Logistic Regression #################
logit_model = glm(y~.,data=train, family = "binomial")


#summary of the model
summary(logit_model)

#predict using logistic regression
logit_Predictions = predict(logit_model, newdata = test, type = "response")



#convert prob
logit_Predictions = ifelse(logit_Predictions > 0.5, 1, 0)


##Evaluate the performance of classification model and making confusion matrix
ConfMatrix_RF = table(test$y, logit_Predictions)
confusionMatrix(ConfMatrix_RF)

#ROC Curve
#install.packages("pROC")
library('pROC')
plot(roc(test$y, predict(logit_model, test, type = "response")))








