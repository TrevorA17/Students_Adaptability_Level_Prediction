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
