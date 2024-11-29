# Saving the model
saveRDS(rf_model, "./models/saved_rf_model.rds")

# Load the saved model
loaded_rf_model <- readRDS("./models/saved_rf_model.rds")

# Example data for predictions
new_data <- data.frame(
  Gender = "Boy", 
  Education_Level = "University", 
  Institution_Type = "Non Government", 
  IT_Student = "No", 
  Location = "Yes", 
  Load_shedding = "Low", 
  Financial_Condition = "Mid", 
  Internet_Type = "Wifi", 
  Network_Type = "4G", 
  Self_Lms = "No", 
  Device = "Tab", 
  Adaptivity_Level = "Moderate"
)

# Use the loaded model to make predictions
predictions_loaded_model <- predict(loaded_rf_model, newdata = new_data)

# Print predictions
print(predictions_loaded_model)

prediction_numeric <- predict(loaded_rf_model, newdata = new_data)

# Map numeric prediction to factor levels
prediction_factor <- ifelse(prediction_numeric <= 1.5, "Low", 
                            ifelse(prediction_numeric <= 2.5, "Moderate", "High"))

# Print the prediction as a factor
print(prediction_factor)
