###Reading and Writing files using R

PATH <- 'https://raw.githubusercontent.com/vincentarelbundock/Rdatasets/master/csv/datasets/mtcars.csv'
df <-read.csv(PATH, header =TRUE, sep = ',', stringsAsFactors =FALSE)
class(df$X)


##For Excel
install.packages("readxl")
library(readxl)

#to see all the available spreadsheets in the library
readxl_example()

readxl_example("clippy.xlsx")


# read_excel(PATH, sheet = NULL, range= NULL, col_names = TRUE)
# arguments:
#   -PATH: Path where the excel is located
# -sheet: Select the sheet to import. By default, all
# -range: Select the range to import. By default, all non-null cells
# -col_names: Select the columns to import. By default, all non-null columns

# Store the path of `datasets.xlsx`
example <- readxl_example("datasets.xlsx")
# Import the spreadsheet
df <- read_excel(example)
# Count the number of columns
length(df)

example <- readxl_example("datasets.xlsx")
excel_sheets(example)


example <- readxl_example("datasets.xlsx")
quake <- read_excel(example, sheet = "quakes")
quake_1 <-read_excel(example, sheet = 4)
identical(quake, quake_1)


# Read the first five row: with header
iris <-read_excel(example, n_max =5, col_names =TRUE)


# Read the first five row: without header
iris_no_header <-read_excel(example, n_max =5, col_names =FALSE)

iris_row_with_header <-read_excel(example, range =cell_cols(1:3), col_names=TRUE)

head(iris_row_with_header)

##Count number of missing values
iris_na <-read_excel(example, na ="setosa")
sum(is.na(iris_na))


#Avoid to name a dataset with blank spaces; 
#it can lead to interpreting as a separate variable. Alternatively, prefer to use '_' or '-.'

#Exclude Missing Values (NA)
#The na.omit() method from the dplyr library is a simple way to exclude missing observation. 
#Let's upload the data and verify the missing values.

PATH <- "https://raw.githubusercontent.com/thomaspernet/data_csv_r/master/data/titanic_csv.csv"
df_titanic <- read.csv(PATH, sep = ",")
# Return the column names containing missing observations
list_na <- colnames(df_titanic)[ apply(df_titanic, 2, anyNA) ]
list_na



library(dplyr)
# Exclude the missing observations
df_titanic_drop <-df_titanic %>%
  na.omit()		
dim(df_titanic_drop)

#Or

df_titanic_drop <-na.omit(df_titanic)	
dim(df_titanic_drop)


########Impute Missing Values

# Create mean
average_missing <- apply(df_titanic[,colnames(df_titanic) %in% list_na],
                         2,
                         mean,
                         na.rm =  TRUE)
average_missing



# Create a new variable with the mean and median
df_titanic_replace <- df_titanic %>%
  mutate(replace_mean_age  = ifelse(is.na(age), average_missing[1], age),
         replace_mean_fare = ifelse(is.na(fare), average_missing[2], fare))

#checking number of NA
sum(is.na(df_titanic_replace$age))

sum(is.na(df_titanic_replace$replace_mean_age))

##HW - Do the same for Median


##Exporting results to hard drive

##Check working Directory
directory <-getwd()
directory


library(dplyr)
df <-mtcars %>%
  select(mpg, disp, gear) %>%
  group_by(gear) %>%
  summarize(mean_mpg = mean(mpg), mean_disp = mean(disp))
df

write.csv(df, "table_car.csv")

#For Excel
install.packages("xlsx")
library(xlsx)
write.xlsx(df, "table_car.xlsx")
