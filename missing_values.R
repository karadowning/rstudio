# install.packages("skimr")
# install.packages(dummies)
# install.packages("seminr")
remotes::install_github("https://github.com/cran/dummies.git")
library(compstatslib)
library(skimr)
library(dummies)
library(seminr)
library(naniar)
library(dplyr)
library(magrittr)

# Load the data:
housing.df <- read.csv("WestRoxbury.csv", header = TRUE)  # load data

# Investigate the data:
dim(housing.df)  # find the dimension of data frame
head(housing.df)  # show the first six rows
View(housing.df)  # show all the data in a new tab
skim(housing.df)

# Practice showing different subsets of the data:
housing.df[1:10, 1]  # show the first 10 rows of the first column only
housing.df[1:10, ]  # show the first 10 rows of each of the columns
housing.df[5, 1:10]  # show the fifth row of the first 10 columns
housing.df[5, c(1:2, 4, 8:10)]  # show the fifth row of some columns
housing.df[, 1]  # show the whole first column
housing.df$TOTAL.VALUE  # a different way to show the whole first column
housing.df$TOTAL.VALUE[1:10]  # show the first 10 rows of the first column
length(housing.df$TOTAL.VALUE)  # find the length of the first column
mean(housing.df$TOTAL.VALUE)  # find the mean of the first column
summary(housing.df)  # find summary statistics for each column

# Some descriptive stats
boxplot(housing.df[,1])
apply(housing.df[,-14],2, sd)
plot(density(housing.df[,2]))
round(cor(housing.df[,-14]), 2)
round(colMeans(housing.df[,-14]),2)
abline(v = mean(housing.df[,2]), col = "green")
abline(v = mean(housing.df[,2])+(c(-1,1)*sd(housing.df[,2])), lty = 2, col = "red")
plot(housing.df[,"LOT.SQFT"], housing.df[,"TOTAL.VALUE"])

# Now we look at some resampling: Lets take a random subset of 500 observations.
# What happens to our correlation matrix?
s <- sample(row.names(housing.df), 50)
round(cor(housing.df[,-14]), 2)
round(cor(housing.df[s,-14]),2)
round(cor(housing.df[1:500,-14]),2)

round(colMeans(housing.df[,-14]),2)
round(colMeans(housing.df[s,-14]),2)
round(colMeans(housing.df[1:500,-14]),2)

# Types of variables ----
# Bedrooms: ordinal??
boxplot(housing.df$BEDROOMS)
table(housing.df$BEDROOMS)

# Remodel: categorical??
barplot(housing.df$REMODEL)
table(housing.df$REMODEL)

# Total Value: ratio??
barplot(housing.df$TOTAL.VALUE)
table(housing.df$TOTAL.VALUE)

# Creating Dummies ----
# Use model.matrix() to convert all categorical variables in the data
# frame into a set of dummy variables. We must then turn the resulting data
# matrix back into a data frame for further work.
xtotal <- model.matrix(~ 0 + REMODEL, data = housing.df)
xtotal <- as.data.frame(xtotal)
names(xtotal) # check the names of the dummy variables
head(cbind(housing.df$REMODEL, xtotal))

# Outliers ----
# What do they do?
compstatslib::interactive_regression()

# How do we find them?
skim(housing.df)
boxplot(housing.df$TOTAL.VALUE)
plot(density(housing.df$TOTAL.VALUE))
abline(v = quantile(housing.df$TOTAL.VALUE, probs = c(0.001, 0.999)))
plot(scale(housing.df$TOTAL.VALUE))
boxplot(housing.df$YR.BUILT)

# Missing Values ----
# Load the data:
housing.df <- read.csv("WestRoxbury.csv", header = TRUE)  # load data
# Now lets use Little's test fopr MCAR:
# H0: MCAR
# H1: Not MCAR
naniar::mcar_test(housing.df[])
# Not missing completely at random (p < 0.001)

# To illustrate missing data procedures, we first convert a few entries for
# bedrooms to NA's. Then we impute these missing values using the median of the
# remaining values.
rows.to.missing <- sample(row.names(housing.df), 10)
housing.df[rows.to.missing,]$BEDROOMS <- NA
summary(housing.df$BEDROOMS)  # Now we have 10 NA's and the median of the
# remaining values is 3.

# Now lets use Little's test for MCAR:
naniar::mcar_test(housing.df)
# Missing completely at random (p > 0.01)

# replace the missing values using the median of the remaining values.
# use median() with na.rm = TRUE to ignore missing values when computing the median.
housing.df[rows.to.missing,]$BEDROOMS <- median(housing.df$BEDROOMS, na.rm = TRUE)
summary(housing.df$BEDROOMS)

# Standardize data ----
# Does it affect the shape?
plot(density(housing.df[,1]))
plot(density(scale(housing.df[,1])))
# No, just the SCALE.
bedrooms_zscaled <- scale(housing.df$BEDROOMS)
bedrooms_minmaxscales <- (housing.df$BEDROOMS - min(housing.df$BEDROOMS))/
                         (max(housing.df$BEDROOMS) - min(housing.df$BEDROOMS))
plot(density(bedrooms_zscaled), ylim = c(0,5))
lines(density(bedrooms_minmaxscales), col = "red")
# The results are quite different (or not), but serve the same purpose

# use set.seed() to get the same partitions when re-running the R code.
set.seed(1)

# Simple strategies for dimension reduction
boston.housing.df <- read.csv("BostonHousing.csv", header = TRUE)
head(boston.housing.df, 9)
summary(boston.housing.df)

# Or we can use dplyr and magrittr to create pivot tables:
housing.df %>%
  group_by(ROOMS, REMODEL) %>%
  summarize(count = n(),
            Value = mean(TOTAL.VALUE),
            Risk = sd(TOTAL.VALUE))

# Principal Components Analysis ----
decathlon <- read.csv(file = "decathlon_data.txt", sep="\t", header = TRUE)
round(cor(decathlon),2)
nick <- heatmap(cor(decathlon))

correlates <- decathlon[,c("X100m", "X110h", "X400m","X1500m")]
decathlon_eigen <- eigen(cor(correlates))
decathlon_eigen$values
sum(decathlon_eigen$values)
decathlon_eigen$values / sum(decathlon_eigen$values)
eigen_vector <- decathlon_eigen$vectors
rownames(eigen_vector) <- colnames(correlates)

dec_pca <- prcomp(decathlon, scale. = TRUE)

dec_pca$sdev
dec_pca$rotation
screeplot(dec_pca, type="lines")
abline(h = 1, lty = 4,lwd = 4, col = "green")

dec_pca$x
round(dec_pca$rotation, 2)
summary(dec_pca)

dec_scores <- dec_pca$x
plot(dec_scores, pch=19)
round(cor(dec_scores),2)
decathlon$PC1 <- dec_scores[, "PC1"]
decathlon$PC2 <- dec_scores[, "PC2"]
decathlon$PC3 <- dec_scores[, "PC3"]
decathlon$PC4 <- dec_scores[, "PC4"]

# End of Session 1 R Scripts ----

# Behind the door: ----

check <- function(sample, choice) {sample[choice] == 1}

run_game <- function() {
  ran <- sample(1:3,3)
  choice <- sample(c("one","two","three"),1)
  sample <- list(one = ran[1],
                 two = ran[2],
                 three = ran[3])
  ifelse (sample[choice] == 1,
          open <- sample(setdiff(names(sample), choice),1),
          open <- sample(setdiff(names(sample), c(choice,names(sample[sample ==1]))),1))
  return(check(sample, choice)[[1]])
}

change_game <- function() {
  ran <- sample(1:3,3)
  choice <- sample(c("one","two","three"),1)
  sample <- list(one = ran[1],
                 two = ran[2],
                 three = ran[3])
  ifelse (sample[choice] == 1,
          open <- sample(setdiff(names(sample), choice),1),
          open <- sample(setdiff(names(sample), c(choice,names(sample[sample ==1]))),1))
  new <- setdiff(c("one","two","three"), c(choice, open))
  return(check(sample, new)[[1]])
}

result <- c()
for (i in 1:100) {
  stay_same <- replicate(1000, run_game(), simplify = TRUE)
  change_choice <- replicate(1000, change_game(), simplify = TRUE)
  same <- sum(stay_same)
  change <- sum(as.vector(change_choice))
  result[i] <- ifelse(same > change, 0, 1)
  cat(i)
}
sum(result)
result

