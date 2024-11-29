# Load the dataset
EducationData <- read.csv("data/students_adaptability_level_online_education.csv", colClasses = c(
  Gender = "factor",
  Education_Level = "factor",
  Institution_Type = "factor",
  IT_Student = "factor",
  Location = "factor",
  Load_shedding = "factor",
  Financial_Condition = "factor",
  Internet_Type = "factor",
  Network_Type = "factor",
  Self_Lms = "factor",
  Device = "factor",
  Adaptivity_Level = "factor"
))

# Display structure to verify data types
str(EducationData)

# Display first few rows to ensure data is loaded correctly
head(EducationData)

# View the dataset in a spreadsheet-like interface (optional)
View(EducationData)

# Load required libraries
library(DescTools)  # For Mode function

# 1. Measures of Frequency
gender_frequency <- table(EducationData$Gender)
education_level_frequency <- table(EducationData$Education_Level)

print("Frequency Tables:")
print(gender_frequency)
print(education_level_frequency)

# Relative frequency
gender_relative_freq <- prop.table(gender_frequency)
education_level_relative_freq <- prop.table(education_level_frequency)

print("Relative Frequencies:")
print(gender_relative_freq)
print(education_level_relative_freq)

# 2. Measures of Central Tendency
# Recode Adaptivity_Level to numeric for numerical operations
EducationData$Adaptivity_Level <- as.numeric(factor(EducationData$Adaptivity_Level, 
                                                    levels = c("Low", "Moderate", "High")))

mean_adaptivity <- mean(EducationData$Adaptivity_Level, na.rm = TRUE)
median_adaptivity <- median(EducationData$Adaptivity_Level, na.rm = TRUE)
mode_adaptivity <- Mode(EducationData$Adaptivity_Level, na.rm = TRUE)

print(paste("Mean Adaptivity Level:", mean_adaptivity))
print(paste("Median Adaptivity Level:", median_adaptivity))
print(paste("Mode Adaptivity Level:", mode_adaptivity))

# 3. Measures of Distribution
range_adaptivity <- range(EducationData$Adaptivity_Level, na.rm = TRUE)
variance_adaptivity <- var(EducationData$Adaptivity_Level, na.rm = TRUE)
sd_adaptivity <- sd(EducationData$Adaptivity_Level, na.rm = TRUE)
quantiles_adaptivity <- quantile(EducationData$Adaptivity_Level, na.rm = TRUE)

print(paste("Range:", range_adaptivity[1], "to", range_adaptivity[2]))
print(paste("Variance:", variance_adaptivity))
print(paste("Standard Deviation:", sd_adaptivity))
print("Quantiles:")
print(quantiles_adaptivity)

# 4. Measures of Relationship
# Correlation (numerical relationship)
correlation <- cor(as.numeric(EducationData$Financial_Condition), 
                   EducationData$Adaptivity_Level, 
                   use = "complete.obs")

print(paste("Correlation between Financial Condition and Adaptivity Level:", correlation))

# Contingency table (categorical relationship)
contingency_table <- table(EducationData$Gender, EducationData$Device)
print("Contingency Table:")
print(contingency_table)

# Chi-squared test for independence
chi_sq_test <- chisq.test(contingency_table)
print("Chi-squared Test Result:")
print(chi_sq_test)

