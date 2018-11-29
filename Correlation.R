#Correlation
#In R, we can use the cor() function. It takes three arguments, and the method.

# cor(x, y, method)
# arguments:			
#   - x: First vector 
# - y: Second vector
# - method: The formula used to compute the correlation. Three string values: 
#   - "pearson"			
# - "kendall"			
# - "spearman"
# An optional argument can be added if the vectors contain missing value:- use = "complete.obs"

#BudgetUK dataset
# wfood: share food share spend
# wfuel: share fuel spend
# wcloth: budget share for clothing spend
# walc: share alcohol spend
# wtrans: share transport spend
# wother: share of other goods spend
# totexp: total household spend in pound
# income total net household income
# age: age of household
# children: number of children

library(dplyr)
PATH <-"https://raw.githubusercontent.com/thomaspernet/data_csv_r/master/data/british_household.csv"
data <-read.csv(PATH) %>%
  filter(income < 500) %>%
  mutate(log_income = log(income),
         log_totexp = log(totexp),
         children_fac = factor(children, order = TRUE, labels = c("No", "Yes"))) %>%
  select(-c(X, X.1, children, totexp, income))
glimpse(data)


cor(data$log_income, data$wfood, method = "pearson")

# The bivariate correlation is a good start, but we can get a broader picture with multivariate analysis. 
# A correlation with many variables is pictured inside a correlation matrix

# Note that, a correlation cannot be computed for factor variable. 
# We need to make sure we drop categorical feature before we pass the data frame inside cor().

# the last column of data is a factor level. We don't include it in the code

# cor(data): Display the correlation matrix
# round(data, 2): Round the correlation matrix with two decimals
# as.dist(): Shows the second half only
mat_1 <-as.dist(round(cor(data[,1:9]),2))
mat_1

install.packages("Hmisc")
library("Hmisc")
data_rcorr <-as.matrix(data[, 1: 9])
mat_2 <-rcorr(data_rcorr)


# The list object mat_2 contains three elements:
#   
# r: Output of the correlation matrix
# n: Number of observation
# P: p-value

p_value <-round(mat_2[["P"]], 3)
p_value

#Visualisation
install.packages("GGally")
library(GGally)


# ggcorr(df, method = c("pairwise", "pearson"),
#        nbreaks = NULL, digits = 2, low = "#3B9AB2",
#        mid = "#EEEEEE", high = "#F21A00",
#        geom = "tile", label = FALSE,
#        label_alpha = FALSE)
# arguments:			
#   - df: Dataset used
# - method: Formula to compute the correlation. By default, pairwise and Pearson are computed
# - nbreaks: Return a categorical range for the coloration of the coefficients. By default, no break and the color gradient is continuous 
# - digits: Round the correlation coefficient. By default, set to 2
# - low: Control the lower level of the coloration 
# - mid: Control the middle level of the coloration 
# - high: Control the high level of the coloration 
# - geom: Control the shape of the geometric argument. By default, "tile"
# - label: Boolean value. Display or not the label. By default, set to `FALSE`



# The legend of the graph shows a gradient color from - 1 to 1, 
# with hot color indicating strong positive correlation 
# and cold color, a negative correlation.

library(GGally)
ggcorr(data)

#Beautification of Graph
ggcorr(data,
       nbreaks = 6,
       low = "steelblue",
       label = TRUE,
       label_size = 3,
       color = "grey50",
       mid = "white",
       high = "darkred",
      )

#ggpairs
#It produces a graph in a matrix format. 

# ggpair(df, columns = 1: ncol(df), title = NULL,
#        upper = list(continuous = "cor"),
#        lower = list(continuous = "smooth"),
#        mapping = NULL)		
# arguments:			
#   - df: Dataset used
# - columns: Select the columns to draw the plot
# - title:  Include a title
# - upper: Control the boxes above the diagonal of the plot. Need to supply the type of computations or graph to return. 
#If continuous = "cor", we ask R to compute the correlation. Note that, the argument needs to be a list. 

# - Lower: Control the boxes below the diagonal. 
# - mapping: Indicates the aesthetic of the graph. For instance, we can compute the graph for different groups.

library(ggplot2)
ggpairs(data, columns = c("log_totexp", "log_income", "age", "wtrans"), 
        title = "Bivariate analysis of revenue expenditure by the British household", 
        upper = list(continuous = wrap("cor",size = 3)),
        lower = list(continuous = wrap("smooth", alpha = 0.3, size = 0.1)),
        mapping = aes(color = children_fac))