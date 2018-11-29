#Data Visualisation
#Scatter Plot

#ggplot2 package
#ggplot2 is very flexible, incorporates many themes and plot specification at a high level of abstraction. 

# ggplot(data, mapping=aes()) +
#   geometric object 
# 
# arguments: 
#   data: Dataset used to plot the graph
# mapping: Control the x and y-axis 
# geometric object: The type of plot you want to show. The most common object are:
#   
#   - Point: `geom_point()` 
# - Bar: `geom_bar()`
# - Line: `geom_line()` 
# - Histogram: `geom_histogram()`

#drat - rear axle ratio data


library(ggplot2)
ggplot(mtcars, aes(x = mpg, y = drat)) +
  geom_point(aes(color = factor(gear)))

ggplot(mtcars, aes(x = log(mpg), y = log(drat))) +
  geom_point(aes(color = factor(gear)))

#You can plot the fitted value of a linear regression.

my_graph <- ggplot(mtcars, aes(x = log(mpg), y = log(drat))) +
  geom_point(aes(color = factor(gear))) +
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE,
              size = 1)
my_graph

#You can add labels with labs()function.

# lab(title = "I am learning Data Visualisation")
# argument:
#   - title: Control the title. It is possible to change or add title with:			
#   - subtitle: Add subtitle below title			
# - caption: Add caption below the graph			
# - x: rename x-axis			
# - y: rename y-axis			
#Example:lab(title = "I am learning Data Visualisation", subtitle = "My first plot")

my_graph +
  labs(
    title = "I am learning Data Visualisation", subtitle = "My first plot"
  )

#Adding Extra Information

mean_mpg <- mean(mtcars$mpg)
my_graph + labs(
  title = paste("Plot Mile per hours and drat, in log. Average mpg is", mean_mpg)
)

#Renaming Axis
my_graph +
  labs(
    x = "Drat definition",
    y = "Mile per hours",
    color = "Gear",
    title = "Relation between Mile per hours and drat",
    subtitle = "Relationship break down by gear class",
    caption = "Learning Data Visualisation"
  )


#Themes used in Data Visualisation
# theme_bw()
# theme_light()
# theme_classis()
# theme_linedraw()
# theme_dark()
# theme_minimal()
# theme_gray()
# theme_void()

my_graph +
  theme_dark() +
  labs(
    x = "Drat definition, in log",
    y = "Mile per hours, in log",
    color = "Gear",
    title = "Relation between Mile per hours and drat",
    subtitle = "Relationship break down by gear class",
    caption = "Learning Data Visualisation"
  )
#Working Directory Check
directory <-getwd()
directory

#Saving the Graph
ggsave("my_first_Scatter_plot.png")



######Box Plot#######
##Box plot helps to visualize the distribution of the data by quartile and detect the presence of outliers##

library(dplyr)
library(ggplot2)
# Step 1
data_air <- airquality %>%
  
  #Step 2
  select(-c(Solar.R, Temp)) %>%
  
  #Step 3
  mutate(Month = factor(Month, order = TRUE, labels = c("May", "June", "July", "August", "September")), 
         
         #Step 4 
         day_cat = factor(ifelse(Day < 10, "Begin", ifelse(Day < 20, "Middle", "End"))))


glimpse(data_air)

#Removing NA
data_air_nona <-data_air %>% na.omit()	

# Create a Box Plot
box_plot <- ggplot(data_air_nona, aes(x = Month, y = Ozone , color = Month))
# Add the geometric object box plot
box_plot +
  geom_boxplot()

#If want to flip the side
box_plot +
  geom_boxplot()+
  coord_flip()


#Checking Outliers

box_plot +
  geom_boxplot(outlier.colour = "red",
               outlier.shape = 2,
               outlier.size = 3) +
  theme_classic()


#Plot Mean in Box Plot
box_plot +
  geom_boxplot() +
  stat_summary(fun.y = mean,
               geom = "point",
               size = 3,
               color = "steelblue") +
  theme_classic()



#Bar Chart
# ggplot(data, mapping = aes()) +
#   geometric object 
# 
# arguments: 
#   data: dataset used to plot the graph 
# mapping: Control the x and y-axis 
# geometric object: The type of plot you want to show. The most common objects are:
#   
#   - Point: `geom_point()`
# - Bar: `geom_bar()`
# - Line: `geom_line()`
# - Histogram: `geom_histogram()` 

library(ggplot2)
# Most basic bar chart
ggplot(mtcars, aes(factor(cyl),
                   fill = factor(cyl))) +
  geom_bar()
#to see all the colors available in R. There are around 650 colors.
grDevices::colors()	


#Add a group in the bars
library(dplyr)
# Step 1
data <- mtcars %>% 
  #Step 2
  mutate(am = factor(am, labels = c("auto", "man")),
         cyl = factor(cyl))


ggplot(data, aes(x = cyl, fill = am)) +
  geom_bar() +
  theme_classic()

##Side By Side Bar

# Bar chart side by side
ggplot(data, aes(x = cyl, fill = am)) +
  geom_bar(position = position_dodge()) +
  theme_classic()




