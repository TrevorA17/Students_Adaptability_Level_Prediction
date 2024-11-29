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

# Load required libraries
library(ggplot2)

# 1. Univariate Plots
# Histogram for a numerical variable (Adaptivity_Level)
ggplot(EducationData, aes(x = Adaptivity_Level)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
  labs(title = "Histogram of Adaptivity Level", x = "Adaptivity Level", y = "Count") +
  theme_minimal()

# Bar plot for a categorical variable (Gender)
ggplot(EducationData, aes(x = Gender)) +
  geom_bar(fill = "green", color = "black") +
  labs(title = "Bar Plot of Gender", x = "Gender", y = "Count") +
  theme_minimal()

# Boxplot for a numerical variable (Adaptivity_Level by Gender)
ggplot(EducationData, aes(x = Gender, y = Adaptivity_Level, fill = Gender)) +
  geom_boxplot() +
  labs(title = "Boxplot of Adaptivity Level by Gender", x = "Gender", y = "Adaptivity Level") +
  theme_minimal()

# 2. Multivariate Plots
# Scatter plot (Financial Condition vs. Adaptivity Level)
ggplot(EducationData, aes(x = as.numeric(Financial_Condition), y = Adaptivity_Level)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Scatter Plot of Financial Condition vs. Adaptivity Level", 
       x = "Financial Condition", y = "Adaptivity Level") +
  theme_minimal()

# Stacked bar plot (Gender and Device)
ggplot(EducationData, aes(x = Gender, fill = Device)) +
  geom_bar(position = "stack") +
  labs(title = "Stacked Bar Plot of Gender and Device", x = "Gender", y = "Count") +
  theme_minimal()

# Faceted plot (Adaptivity Level by Internet Type)
ggplot(EducationData, aes(x = Internet_Type, y = Adaptivity_Level, fill = Internet_Type)) +
  geom_boxplot() +
  facet_wrap(~ Gender) +
  labs(title = "Boxplot of Adaptivity Level by Internet Type (Faceted by Gender)",
       x = "Internet Type", y = "Adaptivity Level") +
  theme_minimal()

