# Plumber API 
# Load the saved RandomForest model
loaded_rf_model <- readRDS("./models/saved_rf_model.rds")

#* @apiTitle Adaptivity Level Prediction Model API

#* @apiDescription Used to predict the adaptivity level based on student information.

#* @param Gender Gender of the student (e.g., "Boy", "Girl")
#* @param Education_Level Education level (e.g., "University", "College")
#* @param Institution_Type Type of institution (e.g., "Non Government", "Government")
#* @param IT_Student Whether the student is an IT student (e.g., "Yes", "No")
#* @param Location Location availability (e.g., "Yes", "No")
#* @param Load_shedding Load shedding condition (e.g., "Low", "High")
#* @param Financial_Condition Financial condition (e.g., "Mid", "Poor", "High")
#* @param Internet_Type Type of internet (e.g., "Wifi", "Mobile Data")
#* @param Network_Type Network type (e.g., "4G", "3G")
#* @param Self_Lms Self-learning management system usage (e.g., "Yes", "No")
#* @param Device Device used (e.g., "Tab", "Mobile")
#* @param Adaptivity_Level Actual adaptivity level (e.g., "Low", "Moderate", "High")

#* @get /predict_adaptivity

predict_adaptivity <- 
  function(Gender, Education_Level, Institution_Type, IT_Student, Location, Load_shedding, Financial_Condition,
           Internet_Type, Network_Type, Self_Lms, Device, Adaptivity_Level) {
    
    # Create a data frame using the arguments
    to_be_predicted <- data.frame(
      Gender = as.character(Gender),
      Education_Level = as.character(Education_Level),
      Institution_Type = as.character(Institution_Type),
      IT_Student = as.character(IT_Student),
      Location = as.character(Location),
      Load_shedding = as.character(Load_shedding),
      Financial_Condition = as.character(Financial_Condition),
      Internet_Type = as.character(Internet_Type),
      Network_Type = as.character(Network_Type),
      Self_Lms = as.character(Self_Lms),
      Device = as.character(Device),
      Adaptivity_Level = as.character(Adaptivity_Level)
    )
    
    # Use the loaded model to make predictions
    prediction <- predict(loaded_rf_model, newdata = to_be_predicted)
    
    # Return the prediction
    return(prediction)
  }
