#Logistic Regression
#Logistic regression is used to predict a class, 
#i.e., a probability. Logistic regression can predict a binary outcome accurately.

#the outcome is measured by the following probabilistic link function called sigmoid due to its S-shaped.:

#To convert a continuous flow into discrete value, 
#we can set a decision bound at 0.5. All values above this threshold are classified as 1


# The objective is to predict whether the annual income in dollar of an individual will exceed 50.000. 
#The dataset contains 46,033 observations and ten features:
#   
#   age: age of the individual. Numeric
# education: Educational level of the individual. Factor.
# marital.status: Marital status of the individual. Factor i.e. Never-married, Married-civ-spouse, ...
# gender: Gender of the individual. Factor, i.e. Male or Female
# income: Target variable. Income above or below 50K. Factor i.e. >50K, <=50K


library(dplyr)
data_adult <-  read.csv("https://raw.githubusercontent.com/thomaspernet/data_csv_r/master/data/data_adult.csv")

glimpse(data_adult)



# Step 1: Check continuous variables
# Step 2: Check factor variables
# Step 3: Feature engineering
# Step 4: Summary statistic
# Step 5: Train/test set
# Step 6: Build the model
# Step 7: Assess the performance of the model
# step 8: Improve the model

#Check what all are numeric data
continuous <-select_if(data_adult, is.numeric)


summary(continuous)

# Use the function select_if() from the dplyr library to select only the numerical columns

#Plot the distribution

library(ggplot2)
ggplot(continuous, aes(x = hours.per.week)) +
  geom_density(alpha = .5, fill = "#FF6666")


# quantile(variable, percentile)
# arguments:
#   -variable:  Select the variable in the data frame to compute the percentile
# -percentile:  Can be a single value between 0 and 1 or multiple value. If multiple, use this format:  `c(A,B,C, ...)
# - `A`,`B`,`C` and `...` are all integer from 0 to 1.

top_one_percent <- quantile(data_adult$hours.per.week, .99)
top_one_percent
#Compute the value of the 99 percent of the working time

data_adult_drop <-data_adult %>%
  filter(hours.per.week<top_one_percent)

dim(data_adult_drop)

##Standardize each column to improve the performance
# mutate_if(df, condition, funs(function))
# arguments:
#   -`df`: Data frame used to compute the function
# - `condition`: Statement used. Do not use parenthesis
# - funs(function):  Return the function to apply. Do not use parenthesis for the function

data_adult_rescale <- data_adult_drop %>%
  mutate_if(is.numeric, funs(as.numeric(scale(.))))
head(data_adult_rescale)

# Select categorical column
factor <- data.frame(select_if(data_adult_rescale, is.factor))

names(factor)

ncol(factor)
#The dataset contains 6 categorical variables

#Plotting 6 graphs

library(ggplot2)
# Create graph for each column
graph <- lapply(names(factor),
                function(x) 
                  ggplot(factor, aes(get(x))) +
                  geom_bar() +
                  theme(axis.text.x = element_text(angle = 90)))

graph



recast_data <- data_adult_rescale %>%
  select(-X) %>%
  mutate(education = factor(ifelse(education == "Preschool" | 
                     education == "10th" | education == "11th" | education == "12th" |
                    education == "1st-4th" | education == "5th-6th" | education == "7th-8th" | 
                      education == "9th", "dropout", 
                    ifelse(education == "HS-grad", "HighGrad", ifelse(education == "Some-college" | 
                  education == "Assoc-acdm" | education == "Assoc-voc", "Community",
                   ifelse(education == "Bachelors", "Bachelors", 
                    ifelse(education == "Masters" | education == "Prof-school", "Master", "PhD")))))))

table(recast_data$education)

recast_data %>%
  group_by(education) %>%
  summarize(average_educ_year = mean(educational.num),
            count = n()) %>%
  arrange(average_educ_year)


# Change level marry
recast_data <- recast_data %>%
  mutate(marital.status = factor(ifelse(marital.status == "Never-married" |
            marital.status == "Married-spouse-absent", "Not_married", 
            ifelse(marital.status == "Married-AF-spouse" | 
            marital.status == "Married-civ-spouse", "Married", ifelse(marital.status == "Separated" | 
            marital.status == "Divorced", "Separated", "Widow")))))

table(recast_data$marital.status)

##Summary Statistics

#statistics about  target variables

# Plot gender income
ggplot(recast_data, aes(x = gender, fill = income)) +
  geom_bar(position = "fill") +
  theme_classic()

#check if the origin of the individual affects their earning.
# Plot origin income
?geom_bar
ggplot(recast_data, aes(x = race, fill = income)) +
  geom_bar(position = "fill") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90))




# Plot distribution working time by education
ggplot(recast_data, aes(x = hours.per.week)) +
  geom_density(aes(color = education), alpha = 0.5) +
  theme_classic()

#The ANOVA test confirms the difference in average between groups.
anova <- aov(hours.per.week~education, recast_data)
summary(anova)



library(ggplot2)
ggplot(recast_data, aes(x = age, y = hours.per.week)) +
  geom_point(aes(color = income),
             size = 0.5) +
  stat_smooth(method = 'lm',
              formula = y~poly(x, 2),
              se = TRUE,
              aes(color = income)) +
  theme_classic()

abc <- recast_data$hours.per.week ~ poly(recast_data$age)
abc



# visualize the correlation between the variables. 
library(GGally)
# Convert data to numeric
corr <- data.frame(lapply(recast_data, as.integer))
# Plot the graph
ggcorr(corr,
method = c("pairwise", "spearman"),
nbreaks = 6,
hjust = 0.8,
label = TRUE,
label_size = 3,
color = "grey50")






###Train and Test sets

set.seed(1234)
create_train_test <- function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}
data_train <- create_train_test(recast_data, 0.8, train = TRUE)
data_test <- create_train_test(recast_data, 0.8, train = FALSE)
dim(data_train)

dim(data_test)
#Build the model
# 
# glm(formula, data=data, family=linkfunction()
#     Argument:
#       - formula:  Equation used to fit the model- data: dataset used
#     - Family:     - binomial: (link = "logit")			
#     - gaussian: (link = "identity")			
#     - Gamma:    (link = "inverse")			
#     - inverse.gaussian: (link = "1/mu^2")			
#     - poisson:  (link = "log")			
#     - quasi:    (link = "identity", variance = "constant")			
#     - quasibinomial:    (link = "logit")			
#     - quasipoisson: (link = "log")	


formula <- income~.
logit <- glm(formula, data = data_train, family = 'binomial')
summary(logit)


#The coefficient (or parameter estimate) for the variable age is 0.41118682.  
#This means that for a one-unit increase in age, 
#we expect a 0.41118682 increase in the log-odds of the dependent variable income, 
#holding all other independent variables constant.


lapply(logit, class)[1:3]

logit$coefficients

###Confusion Matrix
predict <- predict(logit, data_test, type = 'response')
?predict
# confusion matrix
table_mat <- table(data_test$income, predict > 0.5)
table_mat

table_mat_imbalance <- table(data_test$income)

table_mat_imbalance

#table_mat1 <- table(data_adult$income)
#table_mat1

##The model appears to suffer from one problem, it overestimates the number of false negatives.

###Accuracy check
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test

#TP = 1229
#TN = 6310
#FP = 1074
#FN = 495


#Precision=TP/(TP+FP)
#Recall=TP/(TP+FN)

precision <- function(matrix) {
  # True positive
  tp <- matrix[2, 2]
  # false positive
  fp <- matrix[1, 2]
  return (tp / (tp + fp))
}

recall <- function(matrix) {
  # true positive
  tp <- matrix[2, 2]# false positive
  fn <- matrix[2, 1]
  return (tp / (tp + fn))
}

prec <- precision(table_mat)
prec
rec <- recall(table_mat)
rec

#When the model says it is an individual above 50k, it is correct in only 54 percent of the case, 
#and can claim individuals above 50k in 72 percent of the case.

#It is impossible to have both a high precision and high recall.

#ROC 
install.packages("ROCR")
library(ROCR)
ROCRpred <- prediction(predict, data_test$income)
ROCRperf <- performance(ROCRpred, 'tpr', 'fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.1, 1.2))

