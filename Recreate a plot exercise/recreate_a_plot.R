library("ggplot2")
library("forcats")

# PLOT 1 ---------------------------------------------------------------------------------------------------------------------

# DATA MANIPULATION
# main variables needed for the plot dimension
cars <- mtcars
x <- cars$mpg
model <- rownames(cars)
# variable "model" in its reordered and reversed versions
# the two following variables are useful to make the code more readable
mm <- fct_reorder(model, x)
rmm <- fct_rev(mm)

# PLOT CREATION
# plots dimensions
# in y-axis, the reorder() function is needed to group the (already reordered and reversed) car models by number of cylinders
# the bars are filled with three colors, one for each type of cylinder
p1 <- ggplot(cars, aes(x = mpg,y = reorder(rmm, cyl), fill = factor(cyl))) + 
  # bar plot with custom gutter
  geom_bar(stat="identity", width = 0.7) +
  # custom x, y and legend labels
  labs(x="Miles per gallon", y = NULL) +
  guides(fill=guide_legend(title = "Number of \nCylinders")) + 
  # vector which contains the hexa colors of the bars
  scale_fill_manual(values = c("#AECDE1", "#3D76AF", "#BBDE93")) +
  # minimal theme with light background
  theme_minimal() +
  # top position of the legend
  theme(legend.position = "top")

ggsave("plot1.pdf")

# PLOT 2 ---------------------------------------------------------------------------------------------------------------------

# DATA MANIPULATION
# mean and z-score variables have been created to have a more readable code
# "z" is also needed for the x-axis dimention
# ("x" variable comes from the plot 1 data manipulation)
m <- mean(x)
z <- (x-m)/sd(x)
# more readable y-axis variable
rrmm <- fct_rev(reorder(rmm, z))
# new column "zscore" in "cars" data frame containing the values of the z-scores calculated in "z"
cars$zscore <- z
# new column "value" in "cars" data frame
cars$value <- "high"
cars$value[cars$zscore < 0] <- "low"
# more readable variable
v <- cars$value

# PLOT CREATION
# plot dimensions
# the bars are filled with two colors, one for each type of value of the "v" variable (referred to the value column)
p2 <- ggplot(cars, aes(x = z, y = rrmm, fill = factor(v))) + 
  # bar plot with custom and gradual transparency of the bars
  geom_bar(stat = "identity", width = 0.7, alpha = abs(z)) +
  # custom x, y and legend labels
  labs(x = "MPG z-score", y = "Car name") +
  guides(fill = guide_legend(title = "MPG \nGroup", reverse = TRUE)) +
  # vector which contains the hexa colors of the bars 
  scale_fill_manual(values = c("#FAD849", "#1A196B")) +
  # minimal theme with light background
  theme_minimal()

ggsave("plot2.pdf")
