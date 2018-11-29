#Functions in R
# Function is used to avoid repeating the same task, or reduce complexity.
# A function should be
#   written to carry out a specified a tasks
#   may or may not include arguments
#   contain a body
#   may or may not return one or more values.

#General Functions
#A stationary process allows constant mean, variance and autocorrelation over time. 
#This mainly improves the prediction of a time series. It can be easily done with the function diff()
#To ensure we all generate the same data, we use the set.seed() function with arbitrary values of 123
#If we don't use set.seed() function, we will all have different sequence of numbers.

set.seed(123)
## Create the data
x = rnorm(1000)

#For cumulative sum of numbers
ts <- cumsum(x)

## Stationary the serie
diff_ts <- diff(ts)
diff_ts
par(mfrow=c(1,2))
## Plot the series
plot(ts, type='l')
plot(diff(ts), type='l')

#Length of a vector 
#The length() function counts the number of rows in vector x
#In case of Matrix it gives number of columns
dt <- cars

## number columns
length(dt)

#HW - number of rows

#Generate a sequence with difference of 5
x_vector <- seq(45,100, by = 5)

#HW 
#logarithm
#exponential
#squared root
#factorial

#Statistical functions

speed <- dt$speed
speed
# Mean speed of cars dataset
mean(speed)
median(speed)
var(speed)
sd(speed)

#Standardize vector
head(scale(speed), 10)

# Quantile speed of cars dataset
quantile(speed)

summary(speed)
#range, IQR
range(speed)
IQR(speed)

#One argument function
square_function<- function(n) 
{
  # compute the square of integer `n`
  n^2
}  
# calling the function 
square_function(speed)

#delete function
rm(square_function)
#Check environment in R history
ls(environment())

#- Multi arguments function
times <- function(x,y) {
  x*y
}
times(2,4)

#HW - Addition, Subtraction, Division

#HW - Create a dataframe and normalise its columns using function



#Split dataset for train and test 
split_data <- function(df, train = TRUE){
  length<- nrow(df)
  total_row <- length *0.7
  split <- 1:total_row
  if (train ==TRUE){ 
    train_df <- df[split, ] 
    return(train_df)		
  } else {
    test_df <- df[-split, ] 
    return(test_df)		
  }
}

train <- split_data(airquality, train = TRUE)
dim(train)
test <- split_data(airquality, train = FALSE)
dim(test)



# Create vector quantiy
hours <-  10
# Create multiple condition statement
if (hours <10) {
  print('Not enough time spent for datascience study')
} else if (hours >= 10  & hours <= 20) {
  print('Average time spent for datascience study')
} else {
  print('Wao!, Very Nice')
}


##Loops
#For loop
#when we need to iterate over a list of elements or a range of numbers.
#Loop can be used to iterate over a list, data frame, vector, matrix or any other object. 

#For Loop over a list
fruit <- list(Basket = c('Apple', 'Orange', 'Passion fruit', 'Banana'), 
              Money = c(10, 12, 15), purchase = FALSE)
for (item  in fruit) 
{ 
  print(item)
}


#For Loop over a matrix
# Create a matrix
mat <- matrix(data = seq(10, 20, by=1), nrow = 6, ncol =2)

for (r in 1:nrow(mat))   
  for (c in 1:ncol(mat))  
    print(paste("Row", r, "and column",c, "have values of", mat[r,c]))  


#While loop
#Create a variable with value 1
begin <- 1

#Create the loop
while (begin <= 15){
  
  #See which we are  
  cat('This is loop number',begin)
  
  #add 1 to the variable begin after each loop
  begin <- begin+1
  print(begin)
}


##HW - You bought a stock at price of Rs 1000. 
#If the price goes below Rs 990, we want to short it. Otherwise, we keep it in our portfolio. 
#The price can fluctuate between -10 to +10 
 

##apply function
##The purpose of apply() is primarily to avoid explicit uses of loop constructs.
# apply(X, MARGIN, FUN)
# Here:
#   -x: an array or matrix
# -MARGIN:  take a value or range between 1 and 2 to define where to apply the function:
#   -MARGIN=1`: the manipulation is performed on rows
# -MARGIN=2`: the manipulation is performed on columns
# -MARGIN=c(1,2)` the manipulation is performed on rows and columns
# -FUN: tells which function to apply. 
# Built functions like mean, median, sum, min, max and even user-defined functions can be applied>


m1 <- matrix(C<-(1:10),nrow=5, ncol=6)
m1
a_m1 <- apply(m1, 1, sum)
a_m1


# laaply()function
# l in lapply() stands for list. 
# The output of lapply() is a list.
#HW - Output of apply() function ?

# lapply(X, FUN)
# Arguments:
#   -X: A vector or an object
# -FUN: Function applied to each element of x	

movies <- c("SPYDERMAN","BATMAN","VERTIGO","CHINATOWN")
movies_lower <-lapply(movies, tolower)
str(movies_lower)

#We can use unlist() to convert the list into a vector.
movies_lower <-unlist(lapply(movies,tolower))
str(movies_lower)

#sapply() function
#Return Vector
# sapply(X, FUN)
# Arguments:
#   -X: A vector or an object
# -FUN: Function applied to each element of x

dt <- cars
lmn_cars <- lapply(dt, min)
smn_cars <- sapply(dt, min)
lmn_cars
smn_cars

#sapply() function is more efficient than lapply() in the output returned 
#because sapply() store values direclty into a vector


above_ave <- function(x) {  
  ave <- mean(x) 
  
  return(x[x > ave])
}
dt_s<- sapply(dt, below_ave)
dt_l<- lapply(dt, below_ave)


##The function tapply() computes a measure (mean, median, min, max, etc..) 
##or a function for each factor variable in a vector.

data(iris)
tapply(iris$Sepal.Width, iris$Species, median)



