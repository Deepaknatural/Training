#Kmeans Clustering

getwd()
install.packages("dplyr")
library(dplyr)

PATH <-"D:/Dataset/Computers.csv"

df<- read.csv(PATH)

head(df,10)

df <- read.csv(PATH) %>%
  select(-c(X, cd, multi, premium))
glimpse(df)

summary(df)


#rescale the variables with the scale() function of the dplyr library.

rescale_df <- df %>%
  mutate(price_scal = scale(price),
         hd_scal = scale(hd),
         ram_scal = scale(ram),
         screen_scal = scale(screen),
         ads_scal = scale(ads),
         trend_scal = scale(trend)) %>%
  select(-c(price, speed, hd, ram, screen, ads, trend))

rescale_df

# kmeans(df, k)
# arguments:
#   -df: dataset used to run the algorithm
# -k: Number of clusters


# The list pc_cluster contains seven interesting elements:
# pc_cluster$cluster: Indicates the cluster of each observation
# pc_cluster$centers: The cluster centres
# pc_cluster$totss: The total sum of squares
# pc_cluster$withinss: Within sum of square. The number of components return is equal to `k`
# pc_cluster$tot.withinss: Sum of withinss
# pc_clusterbetweenss: Total sum of square minus Within sum of square
# pc_cluster$size: Number of observation within each cluster

##Optimal value of K
#Elbow mwthod is used the explain the variance by each cluster number


################################################################################3
# You can construct the elbow graph and find the optimal k as follow:
#   
# Step 1: Construct a function to compute the total within clusters sum of squares
# Step 2: Run the algorithm n times
# Step 3: Create a data frame with the results of the algorithm
# Step 4: Plot the results


#Step 1) Construct a function to compute the total within clusters sum of squares

kmean_withinss <- function(k) {
  cluster <- kmeans(rescale_df, k)
  return (cluster$tot.withinss)
}

#You can test the function with equals 5
kmean_withinss(2)

#Step 2 - Run the algorithm n times



# Set maximum cluster 
max_k <-20 
# Run algorithm over a range of k 
wss <- sapply(2:max_k, kmean_withinss)

elbow <-data.frame(2:max_k, wss)

library(ggplot2)

#Plot the graph to visualize where is the elbow point
# Plot the graph with gglop
ggplot(elbow, aes(x = X2.max_k, y = wss)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(1, 20, by = 1))

##8 is more optimal value of K from graph

#Examining the cluster
pc_cluster_2 <-kmeans(rescale_df, 10)


pc_cluster_2$cluster
pc_cluster_2$centers
pc_cluster_2$size

result <- cbind(df,pc_cluster_2$cluster)
tail(result)

write.csv(result,"D:\\temp\\cluster_result.csv")
