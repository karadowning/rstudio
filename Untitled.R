#Create a data frame
data <- data.frame(
  Age = c(25,56,65,32,41,49),
  Income = c(49000,156000,99000,192000,39000,57000)
)

data

# Standardize the data
normalized_data <- scale(data)

library(knitr)
library(ggplot2)

# Scatter plot of Age vs. Income
ggplot(normalized_data, aes(x = Age, y = Income)) +
  geom_point() +
  labs(title = "Normalized Age vs. Income")

# Z-score normalization
normalized_data_z <- scale(data)

# Min-max scaling
normalized_data_minmax <- scale(data, center = FALSE, scale = apply(data, 2, max) - apply(data, 2, min))


# Scatter plot of Age vs. Income
ggplot(data, aes(x = Age, y = Income)) +
  geom_point() +
  labs(title = "Age vs. Income") +
  scale_x_continuous(breaks = seq(25, 70, by = 10), labels = c("25", "35", "45", "55", "65")) +
  scale_y_continuous(breaks = seq(0, 200000, by = 50000), labels = c("0", "50K", "100K", "150K", "200K"))