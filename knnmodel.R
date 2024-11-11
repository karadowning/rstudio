# Load necessary libraries
library(caret)  # For data splitting and pre-processing
library(mlbench) # For the BostonHousing dataset
library(class)   # For k-NN (we'll use class::knn())

# Load the data
data(BostonHousing)  
df <- BostonHousing

# Remove the 'b' variable 
df <- df[, -which(names(df) == "b")]
                   
# Separate predictors and target 
X <- df[, -13]  # All columns except 'medv'
y <- df$medv   # Target variable


# Normalize the data
preProcValues <- preProcess(X, method = c("center", "scale"))
X <- predict(preProcValues, X)

# Split into training and testing sets
set.seed(42)  # For reproducibility
trainIndex <- createDataPartition(y, p = 0.6, list = FALSE)
X_train <- X[trainIndex, ]
X_test <- X[-trainIndex, ]
y_train <- y[trainIndex]
y_test <- y[-trainIndex]
# --- Modified k-NN with specific k value ---
k <- 4  # Set your desired k value here

# Apply k-NN 
predictions <- class::knn(train = X_train, test = X_test, cl = y_train, k = k)

# Calculate RMSE
rmse <- sqrt(mean((y_test - as.numeric(as.character(predictions)))^2, na.rm = TRUE))

print(paste("RMSE for k =", k, ":", rmse))

# --- Plot the RMSE values ---
plot(k_values, rmse_values, type = "b", 
     xlab = "k (Number of Neighbors)", ylab = "RMSE",
     main = "k-NN Model Performance") 


# --- Predict MEDV for a new tract ---

# Create a data frame for the new tract (with the lstat column added)
new_tract <- data.frame(
  crim = 0.2, 
  zn = 0, 
  indus = 7, 
  chas = 0, 
  nox = 0.538, 
  rm = 6, 
  age = 62, 
  dis = 4.7, 
  rad = 4, 
  tax = 307, 
  ptratio = 21, 
  lstat = 10  # Add the lstat column with the appropriate value
)

# Normalize the new tract's data
new_tract_norm <- predict(preProcValues, new_tract)

# Predict MEDV using the best k
predicted_medv <- class::knn(train = X_train, test = new_tract_norm, cl = y_train, k = best_k)

# Print the predicted MEDV
print(paste("Predicted MEDV:", predicted_medv))
