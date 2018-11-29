#R has a library called dplyr to help in data transformation.
#It handles four types of joins similar to SQL
#Left_join()
# right_join()
# inner_join()
# full_join()

library(dplyr)
df_primary <- tribble(
  ~ID, ~y,
  "A", 23,
  "B", 45,
  "C", 56,
  "D", 49,
  "F", 84)
df_secondary <- tribble(
  ~ID, ~y,
  "A", 74,
  "B", 25,
  "C", 84,
  "D", 35,
  "E", 93)

left_join(df_primary, df_secondary, by ='ID')

right_join(df_primary, df_secondary, by ='ID')

inner_join(df_primary, df_secondary, by ='ID')

full_join(df_primary, df_secondary, by = 'ID')


#Multiple keys pairs

df_primary <- tribble(
  ~ID, ~year, ~items,
  "A", 2015,3,
  "A", 2016,7,
  "A", 2017,6,
  "B", 2015,4,
  "B", 2016,8,
  "B", 2017,7,
  "C", 2015,4,
  "C", 2016,6,
  "C", 2017,6)
df_secondary <- tribble(
  ~ID, ~year, ~prices,
  "A", 2015,9,
  "A", 2016,8,
  "A", 2017,12,
  "B", 2015,13,
  "B", 2016,14,
  "B", 2017,6,
  "C", 2015,15,
  "C", 2016,15,
  "C", 2017,13)

left_join(df_primary, df_secondary, by = c('ID', 'year'))


#Merge Dataframes

#Sometimes, we have data from multiple sources. 
#To perform an analysis, we need to merge two dataframes 
#together with one or more common key variables.

#To join two datasets, we can use merge() function.
# merge(x, y, by.x = x, by.y = y)
# Arguments:
#   -x: The origin data frame
# -y: The data frame to merge
# -by.x: The column used for merging in x data frame. Column x to merge on
# -by.y: The column used for merging in y data frame. Column y to merge on

# Create origin dataframe(

producers <- data.frame(   
  surname =  c("Spielberg","Scorsese","Hitchcock","Tarantino","Polanski"),    
  nationality = c("US","US","UK","US","Poland"),    
  stringsAsFactors=FALSE)

# Create destination dataframe
movies <- data.frame(    
  surname = c("Spielberg",
              "Scorsese",
              "Hitchcock",
              "Hitchcock",
              "Spielberg",
              "Tarantino",
              "Polanski"),    
  title = c("Super 8",
            "Taxi Driver",
            "Psycho",
            "North by Northwest",
            "Catch Me If You Can",
            "Reservoir Dogs",
            "Chinatown"),                
  stringsAsFactors=FALSE)
 

# Merge two datasets
m1 <- merge(producers, movies, by.x = "surname")
m1
dim(m1)

# Change name of ` movies ` dataframe
colnames(movies)[colnames(movies) == 'surname'] <- 'name'
# Merge with different key value
m2 <- merge(producers, movies, by.x = "surname", by.y = "name")
# Print head of the data
head(m2)
#Are the two datasets identical
identical(m1,m2)

#Quiz - Why the number of count of recordset returned was reduced by 1

#Partial Match
#With partial merging, it is possible to keep the rows with no matching rows in the other data frame. 
#HW  -- add a new producer in producer dataframe, without the movie references in movies data frame.
# Compare all.x= FALSE with all.x= TRUE
#Observe the results