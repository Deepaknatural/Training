# Is used to comment the line in R
# cntrl+shift+c is used to 
# comment multiple lines in R


# R works with numerous data types, including
# Scalars
# Vectors (numerical, character, logical)
# Matrices
# Data frames
# Lists

# 
# 12.9 is a decimal value called numerics.
# 10 is a natural value called integers. Integers are also numerics.
# TRUE or FALSE is a Boolean value called logical.
# The value inside " " or ' ' are text (string). They are called characters.

# We can check the type of a variable with the class function

x <- 102
class(x)

y <- "Welcome to the session of R. Its an easy language to learn"
class(y)

z <- TRUE
class(z)


#Variables
x <- 46
y  <- 21
z = x-y
z

#Vector
#A vector is a one-dimensional array
# Create the vectors
vect_1 <- c(45, 12, 67,94)
vect_2 <- c(12, 4, 16, 19)
# Take the sum of A_vector and B_vector
sum_vect <- vect_1 + vect_2
# Print out total_vector
sum_vect



# Slice the first four rows of the vector
slice_vector <- c(1,5,7,9,11,13,7,8,9,10)
slice_vector[1:5]


# Faster way to create adjacent values
c(1:15)

#The logical statements in R are wrapped inside the []

# Create a vector from 1 to 15
logical_vector_create <- c(1:15)
logical_vector_create>5
logical_vector_create[(logical_vector_create>5)]
logical_vector_create[(logical_vector_create>4) & (logical_vector_create<12)]


# What is a Matrix?
# A matrix  is a combination of two or more vectors with the same data type.

matrix_test <-matrix(data=1:15, byrow = FALSE, nrow = 5 )
matrix_test

#Check byrow, ncol

# Print dimension of the matrix with dim()
dim(matrix_test)

#You can add a column to a matrix with the cbind() command.
matrix_test1 = cbind(matrix_test,c(1:5))
matrix_test1

matrix_test2=cbind(matrix_test,matrix_test1)
matrix_test2


#Slice a Matrix
#We can select elements one or many elements from a matrix by using the square brackets [ ]
# matrix_c[1,2] selects the element at the first row and second column.
# matrix_c[1:3,2:3] results in a matrix with the data on the rows 1, 2, 3 and columns 2, 3,
# matrix_c[,1] selects all elements of the first column.
# matrix_c[1,] selects all elements of the first row.

#What is a Data Frame?
# A data frame is a list of vectors which are of equal length. 
# A matrix contains only one type of data, 
# while a data frame accepts different data types (numeric, character, factor, etc.).

# Create a, b, c, d variables
a <- c(10,20,30,40)
b <- c('book', 'pen', 'textbook', 'bag')
c <- c(TRUE,FALSE,FALSE,TRUE)
d <- c(3.5, 22, 0.9, 71)
# Join the variables to create a data frame
df <- data.frame(a,b,c,d)
df

# Name the data frame
names(df) <- c('ID', 'items', 'In_store', 'price')
df

# Print the structure
str(df)

## Select row 1 in column 2
df[1,2]

## Select Rows 1 to 2
df[1:2,]


## Select Columns 1
df[,1]

## Select Rows 1 to 3 and columns 3 to 4
df[1:3, 3:4]

df[c('ID', 'In_store')]

#Append a Column to Data Frame


# Create a new vector
quantity <- c(13, 25, 78, 57)

# Add `quantity` to the `df` data frame
df$quantity <- quantity
df

###Note: The number of elements in the vector has to be equal to the no of elements in data frame. 

#Select a column of a data frame

# Select the column ID
df$ID

# subset(x, condition)
# arguments:
#   - x: data frame used to perform the subset
# - condition: define the conditional statement

subset(df,subset = quantity>20)


# List -- Its a collection of object

# Construct list with these vec, mat, and df:
sample_list <- list(logical_vector_create, matrix_test1, df)
sample_list

# 
# #Select elements from List
# #We need to use the [[]] to select an element in a list. 
# The value inside the double square bracket represents 
# the position of the item in a list we want to extract. 

sample_list[[3]]


#Access ibuild datasets in R

data()
data("cars")

copy_df <- cars
head(copy_df,5)
str(copy_df)

#sort the data in dataframe
# sort(x, decreasing = FALSE, na.last = TRUE):
#   Argument:
#   -x: A vector containing continuous or factor variable
# -decreasing: Control for the order of the sort method. By default, decreasing is set to `FALSE`.
# -na.last: Indicates whether the `NA` 's value should be put last or not
library(dplyr)

#rnorm(n, mean = , sd = ) 

data_frame <- tibble(  
  c1 = rnorm(50, 5, 1.5),   
  c2 = rnorm(50, 5, 1.5),  
  c3 = rnorm(50, 5, 1.5),
  c4 = rnorm(50, 5, 1.5), 	
  c5 = rnorm(50, 5, 1.5)
)
# Sort by c1 descending
df <-data_frame[order(-data_frame$c1),]
head(df)


