#Multiple Linear Regression
library(dplyr)
df <- mtcars %>%
  select(-c(am, vs, cyl, gear, carb))
glimpse(df)


#Syntax of Linear Regression
# lm(formula, data, subset)
# Arguments:
#   -formula: The equation you want to estimate	
# -data: The dataset used
# -subset: Estimate the model on a subset of the dataset

#Formula
model1 <- mpg~disp + hp + drat + wt

#Fitting Linear Regression
fit <- lm(model1, df)

#Result
fit

summary(fit)

#run the ANOVA test to estimate the effect of each feature on the variances
anova(fit)



# plot() function to show four graphs:
#   
#   - Residuals vs Fitted values
# 
# - Normal Q-Q plot: Theoretical Quartile vs Standardized residuals
# 
# - Scale-Location: Fitted values vs Square roots of the standardised residuals
# 
# - Residuals vs Leverage: Leverage vs Standardized residuals

par(mfrow=c(2,2))


plot(fit)


#Adding Factors
df <- mtcars %>%
  mutate(cyl = factor(cyl),
         vs = factor(vs),
         am = factor(am),
         gear = factor(gear),
         carb = factor(carb))


model2 <- mpg~cyl + vs + am + gear + carb
summary(lm(model2, df))


#Stepwise regression
#The purpose of this algorithm is to add and remove potential candidates in the models 
#and keep those who have a significant impact on the dependent variable. 


#Check Coorelation
# ggscatmat(df, columns = 1:ncol(df), corMethod = "pearson")
# arguments:
#   -df:  A matrix of continuous variables
# -columns: Pick up the columns to use in the function. By default, all columns are used
# -corMethod: Define the function to compute the correlation between variable. By default, the algorithm uses the Pearson formula

install.packages("ggplot2")
library(ggplot2)
library(GGally)

df <- mtcars %>%
  select(-c(am, vs, cyl, gear, carb))
ggscatmat(df, columns = 1: ncol(df))


install.packages("olsrr")
library(olsrr)


model <- mpg~.
fit <- lm(model, df)
fit
test <- ols_step_all_possible(fit)
plot(test)


##Selecting the most effective Independent Variable to run the model

ols_step_both_p(fit, pent = 0.1, prem = 0.3, details = FALSE)
# arguments:
#   -fit:  Model to fit. Need to use `lm()`before to run `ols_stepwise()
# -pent: Threshold of the p-value used to enter a variable into the stepwise model. By default, 0.1
# -prem: Threshold of the p-value used to exclude a variable into the stepwise model. By default, 0.3
# -details: Print the details of each step