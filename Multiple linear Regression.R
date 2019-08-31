

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

