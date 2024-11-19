library(FNN)
library(caret)
library(gains)
library(pROC)
library(RColorBrewer)

# Logit ----

bank.df <- read.csv("UniversalBank.csv")
bank.df <- bank.df[ , -c(1, 5)]  # Drop ID and zip code columns.
# treat Education as categorical (R will create dummy variables)
bank.df$Education <- factor(bank.df$Education, levels = c(1, 2, 3), 
                            labels = c("Undergrad", "Graduate", "Advanced/Professional"))

# partition data
set.seed(1234)
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)  
train.df <- bank.df[train.index, ]
valid.df <- bank.df[-train.index, ]

# run logistic regression
# use glm() (general linear model) with family = "binomial" to fit a logistic 
# regression.
logit.reg <- glm(Personal.Loan ~ ., data = train.df, family = "binomial") 
options(scipen=1)
summary(logit.reg)

# Now we can profile the outcome Personal Loan:
# First, we find out of what is the "status" of our observation: 
new_obs <- data.frame(Age = 35, 
                      Experience = 15, 
                      Income = 60, 
                      Family = 4,
                      CCAvg = 1.5,
                      Education = "Undergrad",
                      Mortgage = 1, 
                      Personal.Loan = 1, 
                      Securities.Account = 0, 
                      CD.Account = 1,
                      Online = 1,
                      CreditCard = 1)

new_obs2 <- data.frame(Age = 35, 
                      Experience = 15, 
                      Income = 60, 
                      Family = 4,
                      CCAvg = 1.5,
                      Education = "Graduate",
                      Mortgage = 1, 
                      Personal.Loan = 1, 
                      Securities.Account = 0, 
                      CD.Account = 1,
                      Online = 1,
                      CreditCard = 1)

p1 <- predict(logit.reg, newdata = new_obs, type = "response")
odds1 <- p1/(1-p1)
p2 <- predict(logit.reg, newdata = new_obs2, type = "response")
odds2 <- p2/(1-p2)
# now let's translate the summary object
exp(logit.reg$coefficients[7])

# so if we increase our data from "Undergrad" to "Graduate" you increase the odds by 
odds1 * exp(logit.reg$coefficients[7])

# use predict() with type = "response" to compute predicted probabilities. 
logit.reg.pred <- predict(logit.reg, valid.df[, -8], type = "response")

# first 5 actual and predicted records
data.frame(actual = valid.df$Personal.Loan[1:5], predicted = logit.reg.pred[1:5])

# Now lets calculate the data for the lift chart
gain <- gains(valid.df$Personal.Loan, logit.reg.pred, groups=10)

# plot lift chart
plot(c(0,gain$cume.pct.of.total*sum(valid.df$Personal.Loan))~c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(valid.df$Personal.Loan))~c(0, dim(valid.df)[1]), lty=2)

# compute deciles and plot decile-wise chart
heights <- gain$mean.resp/mean(valid.df$Personal.Loan)
midpoints <- barplot(heights, names.arg = gain$depth, ylim = c(0,9), 
                     xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")
# add labels to columns
text(midpoints, heights+0.5, labels=round(heights, 1), cex = 0.8)

# Performance Evaluation ---- 
# Now lets do some performance evaluation 

Actual_oos <- factor(as.numeric(valid.df$Personal.Loan), levels = c(0,1))
Actual_is <- factor(as.numeric(train.df$Personal.Loan), levels = c(0,1))
Fit <- logit.reg$fitted.values
Predict <- logit.reg.pred

# Let's choose a cutoff
accuracy.df <- data.frame(cutoff = seq(0, 1, 0.1), fit_accuracy = rep(0, 11), pred_accuracy = rep(0,11))

# calculate accuracy for each cutoff.
for(i in 1:11) {
  fit_cut <- factor(as.numeric(Fit > accuracy.df[i,1]), levels = c(0,1))
  pred_cut <- factor(as.numeric(Predict > accuracy.df[i,1]), levels = c(0,1))
  fit_cm <- confusionMatrix(fit_cut,  Actual_is)
  pred_cm <- confusionMatrix(pred_cut, Actual_oos)
  accuracy.df[i,2] <- fit_cm$overall[1]
  accuracy.df[i,3] <- pred_cm$overall[1]
}

plot(2:10, accuracy.df[2:10,2], col = "black", type = "l", ylim = c(0.9,1))
lines(2:10, accuracy.df[2:10,3], col = "blue", type = "l")
abline(v = c(5,6))
# So a classification cut-off of 0.5 makes sense.
# This is because the 

# Hierarchical Clustering ----
# Create some toy data
v1 <- c(1,2,4,7,5)
v2 <- c(1,1,5,7,7)
mat <- as.data.frame(cbind(v1,v2))

# Visualize it 
plot(mat)
text(v1,v2, 1:5, pos = 2)

# What is the distance between points 1 and 4 (manual calc)?
# Euclidean distance:
sqrt(6^2 + 6^2)

# We can use the dist() function to estimate all distances. 
round(dist(mat, diag = TRUE, method = "euclidean"),2)

# Let's create a heatmap with auto clustering
colnames(mat) <- c("A","B")
heatmap(t(mat), col=brewer.pal(9,"Blues"))

# Now lets load the utilities data set
utilities.df <- read.csv("Utilities.csv")

# set row names to the utilities column
row.names(utilities.df) <- utilities.df[,1]

# remove the utility column
utilities.df <- utilities.df[,-1]

# compute Euclidean distance
# (to compute other distance measures, change the value in method = )
d <- dist(utilities.df[1:6,], method = "euclidean")
heatmap(t(as.matrix(utilities.df)),col=brewer.pal(9,"Blues"))
plot(utilities.df$Sales, utilities.df$Fuel_Cost)
text(utilities.df$Sales, 
     utilities.df$Fuel_Cost,
     rownames(utilities.df),
     pos = 3)


# normalize input variables
utilities.df.norm <- sapply(utilities.df, scale)
colnames(utilities.df.norm)
# add row names: utilities
row.names(utilities.df.norm) <- row.names(utilities.df) 

heatmap(t(as.matrix(utilities.df.norm)),col=brewer.pal(9,"Blues"))

# compute normalized distance based on variables Sales and FuelCost
d.norm <- dist(utilities.df.norm[1:6,], method = "euclidean")

#### Figure 15.3
# compute normalized distance based on all 8 variables
d.norm <- dist(utilities.df.norm, method = "euclidean")

# in hclust() set argument method =  
# to "ward.D", "single", "complete", "average", "median", or "centroid"
hc1 <- hclust(d.norm, method = "single")
plot(hc1, hang = -1, ann = FALSE)
abline(h = c(1,1.5,2,2.5,3,3.5), lty = 2)
hc2 <- hclust(d.norm, method = "average")
plot(hc2, hang = -1, ann = FALSE)
abline(h = c(1,1.5,2,2.5,3,3.5), lty = 2)

# We can change the membership depending on the number of k:
memb <- cutree(hc1, k = 6)
memb
memb <- cutree(hc1, k = 3)
memb

# Kmeans clustering ----
# load and preprocess data 
utilities.df <- read.csv("Utilities.csv")
row.names(utilities.df) <- utilities.df[,1]
utilities.df <- utilities.df[,-1]

# normalized distance:
utilities.df.norm <- sapply(utilities.df, scale)
row.names(utilities.df.norm) <- row.names(utilities.df) 

# Now lets recall the membership by hierarchical clustering:
hc1 <- hclust(dist(utilities.df.norm), method = "average")
plot(hc1, hang = -1, ann = FALSE)
abline(h = c(1,1.5,2,2.5,3,3.5), lty = 2)
memb <- cutree(hc1, k = 3)
memb

plot(utilities.df$Sales, utilities.df$Fuel_Cost,
     pch = memb)
text(utilities.df$Sales, 
     utilities.df$Fuel_Cost,
     rownames(utilities.df),
     pos = 3)

# run kmeans algorithm 
set.seed(2)
km <- kmeans(utilities.df.norm,centers = 21)
km$betweenss
km$withinss
plot(utilities.df$Sales, utilities.df$Fuel_Cost,
     pch = km$cluster,
     col = km$cluster)
text(utilities.df$Sales, 
     utilities.df$Fuel_Cost,
     rownames(utilities.df),
     pos = 3,
     col = km$cluster)

km <- kmeans(utilities.df.norm, 4, nstart = 30)
plot(utilities.df$Sales, utilities.df$Fuel_Cost,
     pch = km$cluster,
     col = km$cluster)
text(utilities.df$Sales, 
     utilities.df$Fuel_Cost,
     rownames(utilities.df),
     pos = 3,
     col = km$cluster)

# centroids
km$centers

# Now lets consider a profile plot to evaluate the clusters
km_four <- kmeans(utilities.df.norm, 4, nstart = 30)

# plot an empty scatter plot
plot(c(0), xaxt = 'n', ylab = "", type = "l", 
     ylim = c(min(km_four$centers), max(km_four$centers)), xlim = c(0, 8))

# label x-axes
axis(1, at = c(1:8), labels = names(utilities.df))

# plot centroids
for (i in c(1:4)) {
  lines(km_four$centers[i,], lty = i, lwd = 2, col = ifelse(i %in% c(1, 3, 5),
                                                            "black", "dark grey"))
}
abline(h = 0,lty = 2)
# name clusters
text(x = 0.5, y = km_four$centers[, 1], labels = paste("Cluster", c(1:4)))


dist(km$centers)

# Choosing k 
results <- matrix(,nrow =21, ncol = 2)
for (i in 1:21) {
  km_loop <- kmeans(utilities.df.norm, i, nstart = 30)
  results[i,1] <- mean(km_loop$withinss)
  results[i,2] <- km_loop$betweenss
}

plot(1:21,results[,1])
lines(1:21, results[,1])
points(1:21,results[,2], col = "blue")
lines(1:21, results[,2], col = "blue")
