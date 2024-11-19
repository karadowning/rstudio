library(FNN)
library(skimr)
library(caret)
library(gains)

# KNN ----
# create a new variable and assign it a random vector of 1000 values


mower.df <- read.csv("RidingMowers.csv")
# set.seed(12345)

# Convert character vec to numeric vec
mower.df$Ownership[mower.df$Ownership == "Nonowner"] <- 0
mower.df$Ownership[mower.df$Ownership == "Owner"] <- 1

# Cut the data into training and validation sets
train.index <- sample(row.names(mower.df), 0.6*nrow(mower.df))  
valid.index <- setdiff(row.names(mower.df), train.index)  
train.df <- mower.df[train.index, ]
valid.df <- mower.df[valid.index, ]

## new household
new.df <- data.frame(Income = 60, Lot_Size = 20)

## scatter plot
# Visualize the data
plot(Lot_Size ~ Income, data=train.df, 
     pch=ifelse(train.df$Ownership=="1", 1, 3))
text(train.df$Income, train.df$Lot_Size, rownames(train.df), pos=4)
text(60, 20, "X")
legend("topleft", c("owner", "non-owner", "newhousehold"), pch = c(1, 3, 4))

# Lets change the dimensions
plot(Lot_Size ~ Income, data=mower.df, 
     pch=ifelse(train.df$Ownership=="1", 1, 3), 
     xlim = c(40,100), ylim = c(0,60))
text(mower.df$Income, mower.df$Lot_Size, rownames(train.df), pos=4)
text(60, 20, "X")
legend("topleft", c("owner", "non-owner", "newhousehold"), pch = c(1, 3, 4))

# Lets calculate the distances manually
round(dist(scale(rbind(mower.df[,1:2], new.df))),3)

# Lets create holders for the normalized data 
train.norm.df <- train.df
valid.norm.df <- valid.df
# Let's normalize the data first (remember scale affects KNN distance)
norm.values <- preProcess(train.df[, 1:2], method=c("center", "scale"))
train.norm.df[,1:2] <- predict(norm.values, train.df[, 1:2])
valid.norm.df[,1:2] <- predict(norm.values, valid.df[, 1:2])

# use knn() to compute knn. 
# knn() is available in library FNN (provides a list of the nearest neighbors)
# and library class (allows a numerical output variable).
# Let's plot again as a double-check
plot(Lot_Size ~ Income, data=train.norm.df, pch=ifelse(train.norm.df$Ownership=="1", 1, 3))
text(train.norm.df$Income, train.norm.df$Lot_Size, rownames(train.norm.df), pos=4)
round(dist(train.norm.df[,1:2]),2)
# Now we train the KNN model on the normalized training data
nn <- knn(train = train.norm.df[, 1:2], 
          test = valid.norm.df[, 1:2], 
          cl = train.norm.df[, 3], 
          k = 3)
points(valid.norm.df[1,1:2], pch = 4)

row.names(train.df)[attr(nn, "nn.index")[1,]]
table(valid.norm.df$Ownership, as.numeric(as.vector(nn)))

# but we also need to choose a parameter k for the knn model
# initialize a data frame with two columns: k, and accuracy.
accuracy.df <- data.frame(k = seq(1, 14, 1), accuracy = rep(0, 14))

# compute knn for different k on validation.
for(i in 1:14) {
  knn.pred <- knn(train.norm.df[, 1:2], 
                  valid.norm.df[, 1:2], 
                  cl = train.norm.df[, 3], 
                  k = i)
  accuracy.df[i, 2] <- confusionMatrix(factor(knn.pred , levels = c(0,1)), 
                                       factor(valid.norm.df[, 3], 
                                              levels = c(0,1)))$overall[1] 
}

# now lets visualize the results
plot(accuracy.df[,1], accuracy.df[,2], type = "l")
# I think a k of approx. 4 is good. 

# Let us build the final model
mower.norm.df <- mower.df
mower.norm.df[, 1:2] <- predict(preProcess(mower.df[, 1:2],  method=c("center", "scale")), mower.df[,1:2])
new.norm.df <- predict(preProcess(mower.df[, 1:2],  method=c("center", "scale")), new.df)
knn.pred.new <- knn(mower.norm.df[, 1:2], new.norm.df, 
                    cl = mower.norm.df[, 3], k = 4)
four_nearest_neighbours <- attr(knn.pred.new, "nn.index")

plot(Lot_Size ~ Income, data=mower.norm.df, pch=ifelse(mower.norm.df$Ownership==1, 1, 3), xlim = c(-2,2), ylim = c(-2,2))
text(mower.norm.df$Income, mower.norm.df$Lot_Size, rownames(mower.norm.df), pos=4)
text(new.norm.df$Income, new.norm.df$Lot_Size, "X")
legend("topleft", c("owner", "non-owner", "newhousehold"), pch = c(1, 3, 4))

# Conduct classification (probability)
mean(as.numeric(mower.norm.df$Ownership[four_nearest_neighbours]))
# Conduct classification (binary)
median(as.numeric(mower.norm.df$Ownership[four_nearest_neighbours]))

# And so we predict 1 for this new case

# Instead of classification, we might wish to generate a "prediction"
# Conduct a prediction of income for new datapoint:
std.pred <- mean(mower.norm.df$Income[four_nearest_neighbours])
scale <- preProcess(mower.df[, 1:2],  method=c("center", "scale"))
unstd.pred <- (std.pred[1] * scale$std[1]) + scale$mean[1]

# So for this case, we would predict income of 60.825 (average of neighbours)

# Logit ----

bank.df <- read.csv("UniversalBank.csv")
bank.df <- bank.df[ , -c(1, 5)]  # Drop ID and zip code columns.
# treat Education as categorical (R will create dummy variables)
bank.df$Education <- factor(bank.df$Education, levels = c(1, 2, 3), 
                            labels = c("Undergrad", "Graduate", "Advanced/Professional"))

# partition data
set.seed(32)
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)  
train.df <- bank.df[train.index, ]
valid.df <- bank.df[-train.index, ]

# run logistic regression
# use glm() (general linear model) with family = "binomial" to fit a logistic 
# regression.
logit.reg <- glm(Personal.Loan ~ ., data = train.df, family = "binomial") 
options(scipen=999)
summary(logit.reg)

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

plot(1:11, accuracy.df[,2], col = "black", type = "l")
lines(1:11, accuracy.df[,3], col = "blue", type = "l")

# So a classification cut-off of 0.5 makes sense.
