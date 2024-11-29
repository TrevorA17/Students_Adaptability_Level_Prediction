# Load required libraries
library(caret)  # For cross-validation and data splitting
library(boot)   # For bootstrapping

# Load dataset
set.seed(123)  # Set seed for reproducibility

# 1. Data Splitting
# Split data into training (70%) and testing (30%) sets
train_index <- createDataPartition(EducationData$Adaptivity_Level, p = 0.7, list = FALSE)
train_data <- EducationData[train_index, ]
test_data <- EducationData[-train_index, ]

print("Training and Testing Data Summary:")
print(paste("Training Data Rows:", nrow(train_data)))
print(paste("Testing Data Rows:", nrow(test_data)))

# 2. Bootstrapping
# Define a function to calculate a statistic (e.g., mean of Adaptivity_Level)
bootstrap_function <- function(data, indices) {
  d <- data[indices, ]  # Resample with replacement
  return(mean(d$Adaptivity_Level, na.rm = TRUE))
}

# Apply bootstrapping with 1000 resamples
bootstrap_results <- boot(data = train_data, statistic = bootstrap_function, R = 1000)

print("Bootstrapping Results:")
print(bootstrap_results)

# 3. Cross-validation
# Basic k-fold cross-validation (k = 10)
cv_control <- trainControl(method = "cv", number = 10)
cv_model <- train(Adaptivity_Level ~ ., data = train_data, method = "lm", trControl = cv_control)

print("Basic Cross-validation Results:")
print(cv_model)

# Repeated k-fold cross-validation (k = 10, repeated 5 times)
repeated_cv_control <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
repeated_cv_model <- train(Adaptivity_Level ~ ., data = train_data, method = "lm", trControl = repeated_cv_control)

print("Repeated Cross-validation Results:")
print(repeated_cv_model)

# Leave-One-Out Cross-Validation (LOOCV)
loocv_control <- trainControl(method = "LOOCV")
loocv_model <- train(Adaptivity_Level ~ ., data = train_data, method = "lm", trControl = loocv_control)

print("LOOCV Results:")
print(loocv_model)

# Load required libraries
library(caret)       # For data splitting and model training
library(rpart)       # For Decision Tree
library(randomForest) # For Random Forest

# Set seed for reproducibility
set.seed(123)

# Split data into training and testing sets (70-30 split)
train_index <- createDataPartition(EducationData$Adaptivity_Level, p = 0.7, list = FALSE)
train_data <- EducationData[train_index, ]
test_data <- EducationData[-train_index, ]

# 1. Linear Regression
linear_model <- train(
  Adaptivity_Level ~ ., 
  data = train_data, 
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)  # 10-fold CV
)

print("Linear Regression Model:")
print(linear_model)

# 2. Decision Tree Regression
tree_model <- train(
  Adaptivity_Level ~ ., 
  data = train_data, 
  method = "rpart",
  trControl = trainControl(method = "cv", number = 10)  # 10-fold CV
)

print("Decision Tree Regression Model:")
print(tree_model)

# 3. Random Forest Regression
rf_model <- train(
  Adaptivity_Level ~ ., 
  data = train_data, 
  method = "rf",
  trControl = trainControl(method = "cv", number = 10),  # 10-fold CV
  tuneLength = 5  # Test 5 different values of mtry
)

print("Random Forest Regression Model:")
print(rf_model)

# Evaluate all models on the test data
linear_predictions <- predict(linear_model, newdata = test_data)
tree_predictions <- predict(tree_model, newdata = test_data)
rf_predictions <- predict(rf_model, newdata = test_data)

# Calculate performance metrics for all models
linear_rmse <- RMSE(linear_predictions, test_data$Adaptivity_Level)
tree_rmse <- RMSE(tree_predictions, test_data$Adaptivity_Level)
rf_rmse <- RMSE(rf_predictions, test_data$Adaptivity_Level)

print(paste("Linear Regression RMSE:", linear_rmse))
print(paste("Decision Tree RMSE:", tree_rmse))
print(paste("Random Forest RMSE:", rf_rmse))

# Compare model performance using resamples
models <- list(
  Linear_Regression = linear_model,
  Decision_Tree = tree_model,
  Random_Forest = rf_model
)

# Resample models to compare performance
resample_results <- resamples(models)

# Summary of resampling results
summary(resample_results)

# Boxplot for performance metrics
bwplot(resample_results, layout = c(3, 1))

# Dot plot for performance metrics
dotplot(resample_results, layout = c(3, 1))

# Pairwise comparison of model performance (optional)
model_diff <- diff(resample_results)
summary(model_diff)

