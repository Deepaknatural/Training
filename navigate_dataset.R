#Navigate Inside dataset

library(dplyr) 
PATH <- "https://raw.githubusercontent.com/thomaspernet/data_csv_r/master/data/travel_times.csv"
df <- read.csv(PATH)
glimpse(df)

#Check missing Values
sum(df$Comments =="")


#Select()
#We will begin with the select() verb. 
#We don't necessarily need all the variables, and a good practice is to select only the variables you find relevant.
# 
# - `select(df, A, B ,C)`: Select the variables A, B and C from df dataset.
# - `select(df, A:C)`: Select all variables from A to C from df dataset.
# - `select(df, -C)`: Exclude C from the dataset from df dataset.	

step_1_df <- select(df, -Comments)
dim(df)
dim(step_1_df)


table(step_1_df$GoingTo)

# Select observations
select_home <- filter(df, GoingTo == "Home")
dim(select_home)

select_home_wed <- filter(df, GoingTo == "Home" & DayOfWeek == "Wednesday")
dim(select_home_wed)
