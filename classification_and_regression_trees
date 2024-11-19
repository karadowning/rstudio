library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(gbm)
library(neuralnet)
library(nnet)
library(e1071)

# Classification and Regression Trees ----

# load data
mower.df <- read.csv("RidingMowers.csv")

# Let us plot this data in 2d space
plot(mower.df$Income, mower.df$Lot_Size, pch = mower.df$Ownership)
abline(v = 60, lty = 2)
abline(h = 21, lty = 2)

# Measuring impurity
# Lets write a quick function for gini
gini <- function(vals)
{
  if (is(vals, "numeric"))
    counts <- vals
  else counts <- table(vals)
  total <- sum(counts)
  return(sum((counts/total) * (1 - counts/total)))
}

# Lets write a quick function for entropy
entropy <- function(vals) {
  freqs <- table(vals)/length(vals)
  return(-sum(freqs * log2(freqs)))
}

# Now lets apply the func
gini(mower.df$Ownership)
entropy(mower.df$Ownership)
# so the dataset is currently balanced 50/50

# Now lets calculate the impurity of the subsets
right <- mower.df[mower.df$Income >= 60, "Ownership"]
gini(right)
entropy(right)

left <- mower.df[mower.df$Income < 60, "Ownership"]
gini(left)
entropy(left)

# So the subsets have reduced complexity (or increased simplicity) over the
# whole dataset

# Now we apply a CART tree from the library

# First, lets load a new dataset:
bank.df <- read.csv("UniversalBank.csv")

# partition
set.seed(1)

# classification tree fully grown
default.ct <- rpart(Income ~ .,
                    data = bank.df,
                    cp = 0.008,
                    method = "anova")
# plot tree
prp(default.ct,
    type = 1,
    extra = 1,
    under = TRUE,
    split.font = 1,
    varlen = -10)

round(default.ct$cptable,3)

# Now lets apply our prediction process
# Slice data in train and valid
set.seed(1234)
train_index <- sample(5000,0.6*5000)
train.df <- bank.df[train_index,]
valid.df <- bank.df[-train_index, ]

# Now we train our model on the training data
train.ct <- rpart(Personal.Loan ~ .,
                  data = train.df,
                  method = "class",
                  cp = 0.007,
                  minsplit = 1)
prp(train.ct,
    type = 1,
    extra = 1,
    under = TRUE,
    split.font = 1,
    varlen = -10)

train.ct$cptable

# Lets inspect the cp table
train.ct$cptable
# classify records in the validation data using simpler model.
# set argument type = "class" in predict() to generate predicted class membership.
train.ct.pred <- predict(train.ct,train.df,type = "class")

# generate confusion matrix for training data
confusionMatrix(train.ct.pred, as.factor(train.df$Personal.Loan), positive = "1")
### wow! 100% accuracy (is this overfit?...)

# Now lets generate real out-of-sample predictions
valid.ct.pred <- predict(train.ct,valid.df,type = "class")

# generate confusion matrix for training data
confusionMatrix(valid.ct.pred, as.factor(valid.df$Personal.Loan), positive = "1")

# Now lets use our cp rule to better balance overfit
# So let us select the cp as larger (more complex)
train.ct <- rpart(Personal.Loan ~ .,
                  data = train.df,
                  method = "class",
                  cp = 0.007067138,
                  minsplit = 1)

# plot simpler "pruned" tree
prp(train.ct,
    type = 1,
    extra = 1,
    under = TRUE,
    split.font = 1,
    varlen = -10)

# Lets inspect the cp table
train.ct$cptable

# classify records in the validation data using simpler model.
# set argument type = "class" in predict() to generate predicted class membership.
train.ct.pred <- predict(train.ct,train.df,type = "class")

# generate confusion matrix for training data
confusionMatrix(train.ct.pred, as.factor(train.df$Personal.Loan), positive = "1")
# wow! now our fit is slightly worse

# Now lets generate real out-of-sample predictions
valid.ct.pred <- predict(train.ct,valid.df,type = "class")

# generate confusion matrix for training data
confusionMatrix(valid.ct.pred, as.factor(valid.df$Personal.Loan), positive = "1")

# the overall accuracy has improved in the out-of-sample
# Now lets repeat and really UNDERFIT the model
# let set cp = 0.10
train.ct <- rpart(Personal.Loan ~ .,
                  data = train.df,
                  method = "class",
                  cp = 0.1,
                  minsplit = 1)

train.ct.pred <- predict(train.ct,train.df,type = "class")

# generate confusion matrix for training data
confusionMatrix(train.ct.pred, as.factor(train.df$Personal.Loan), positive = "1")
# wow! now our fit is even worse

valid.ct.pred <- predict(train.ct,valid.df,type = "class")
confusionMatrix(valid.ct.pred, as.factor(valid.df$Personal.Loan), positive = "1")
# Overall accuracy has increased, but at the cost of sensitivity.

# Cross-validation in RPART using xval

# argument xval refers to the number of folds to use in rpart's built-in
# cross-validation procedure
# argument cp sets the smallest value for the complexity parameter.
cv.ct <- rpart(Personal.Loan ~ ., data = bank.df, method = "class",
               cp = 0.00001, minsplit = 5, xval = 10)

# use printcp() to print the table.
printcp(cv.ct)

# Random Forest ----
train.rf <- randomForest(as.factor(Personal.Loan) ~ .,
                         data = train.df,
                         ntree = 5,
                         importance = TRUE)

## variable importance plot
varImpPlot(train.rf, type = 1)

## confusion matrix
rf.pred <- predict(train.rf, valid.df)
confusionMatrix(rf.pred, as.factor(valid.df$Personal.Loan), positive = "1")

# Neural Nets ----
# Load data
df <- read.csv("TinyData.csv")

# Now lets make a new set of binary outcomes "Like" and Dislike"
df$Like <- (df$Acceptance=="like")*1
df$Dislike <- (df$Acceptance=="dislike")*1

set.seed(1)
nn <- neuralnet(Like + Dislike ~ Salt + Fat,
                data = df,
                linear.output = F,
                hidden = c(3))

# display weights
nn$weights

# display predictions
prediction(nn)

# plot network
plot(nn, rep="best")

# Generate predictions from data
predict <- compute(nn, data.frame(df$Salt, df$Fat))
predicted.class=apply(predict$net.result,1,which.max)-1

# Evaluate results
confusionMatrix(factor(ifelse(predicted.class=="1", "dislike", "like"), levels = c("dislike", "like")),
                factor(df$Acceptance, levels = c("dislike", "like")))


# Another example

# Load data
accidents.df <- read.csv("accidentsnn.csv")
# selected variables
vars <- c("ALCHL_I", "PROFIL_I_R", "VEH_INVL")

# partition the data
set.seed(2)
training=sample(row.names(accidents.df), dim(accidents.df)[1]*0.6)
validation=setdiff(row.names(accidents.df), training)

# when y has multiple classes - need to dummify
trainData <- cbind(accidents.df[training,c(vars)],
                   class.ind(accidents.df[training,]$SUR_COND),
                   class.ind(accidents.df[training,]$MAX_SEV_IR))
names(trainData) <- c(vars,
                      paste("SUR_COND_", c(1, 2, 3, 4, 9), sep=""), paste("MAX_SEV_IR_", c(0, 1, 2), sep=""))

vec <- rep(0,400)
validData <- cbind(accidents.df[validation,c(vars)],
                   class.ind(accidents.df[validation,]$SUR_COND),
                   vec,
                   class.ind(accidents.df[validation,]$MAX_SEV_IR))

names(validData) <- c(vars,
                      paste("SUR_COND_", c(1, 2, 3, 4, 9), sep=""), paste("MAX_SEV_IR_", c(0, 1, 2), sep=""))

# run nn with 2 hidden nodes
# use hidden= with a vector of integers specifying number of hidden nodes in each layer
nn <- neuralnet(MAX_SEV_IR_0 + MAX_SEV_IR_1 + MAX_SEV_IR_2 ~
                  ALCHL_I + PROFIL_I_R + VEH_INVL + SUR_COND_1 + SUR_COND_2
                + SUR_COND_3 + SUR_COND_4, data = trainData, hidden = 2)

plot(nn, rep="best")
training.prediction <- compute(nn, trainData[,-c(8:11)])
training.class <- apply(training.prediction$net.result,1,which.max)-1
table(training.class, accidents.df[training,]$MAX_SEV_IR)

validation.prediction <- compute(nn, validData[,-c(8:11)])
validation.class <-apply(validation.prediction$net.result,1,which.max)-1
table(validation.class, accidents.df[validation,]$MAX_SEV_IR)
