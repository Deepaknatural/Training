#t test
# One Sampled - Two Sample

# t.test(x, y = NULL,
#        mu = 0, var.equal = FALSE)
# arguments:
#   - x : A vector to compute the one-sample t-test
# - y: A second vector to compute the two sample t-test
# - mu: Mean of the population- 
#var.equal: Specify if the variance of the two vectors are equal. 
#By default, set to `FALSE`

# rnorm(n, mean, sd)
# arguments
# - n: Number of observations to generate
# - mean: The mean of the distribution. Optional
# - sd: The standard deviation of the distribution. Optional


#One Sample t-test
#Softdrink company  Checking Sugar Content in Drink
#One bottle on an average contain 10gm sugar
#H0: The average level of sugar is equal to 10
#H1: The average level of sugar is different than 10

set.seed(123)
sugar_level <- rnorm(30, mean = 9.99, sd = 0.04)
head(sugar_level)

# H0 : mu = 10
t.test(sugar_level, mu = 10)	

#Answer



set.seed(123) 
sugar_cookie <- rnorm(30, mean = 9.99, sd = 0.04)
head(sugar_cookie)


#Paired t-Test
#The paired t-test, or dependant sample t-test, 
#is used when the mean of the treated group is computed twice.

#discount program
#statistical difference between the average sales of the shop before and after the program.
#This is called a paired t-test because the values of both vectors come from the same distribution 

#H0: No difference in mean
#H1: The two means are different

set.seed(123)
# sales before the program
sales_before <- rnorm(7, mean = 50000, sd = 50)
# sales after the program.This has higher mean
sales_after <- rnorm(7, mean = 50075, sd = 50)
# draw the distribution
t.test(sales_before, sales_after,var.equal = TRUE)

#Answer