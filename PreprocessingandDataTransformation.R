# Check for missing values in the dataset
missing_values_summary <- colSums(is.na(EducationData))

# Display the summary of missing values per column
print("Summary of Missing Values:")
print(missing_values_summary)

# Check if the dataset contains any missing values
any_missing <- any(is.na(EducationData))

if (any_missing) {
  print("The dataset contains missing values.")
} else {
  print("The dataset has no missing values.")
}

# Visualize missing values using a heatmap (optional)
if (requireNamespace("VIM", quietly = TRUE)) {
  library(VIM)
  aggr(EducationData, col = c("navyblue", "red"), 
       numbers = TRUE, sortVars = TRUE, 
       labels = names(EducationData), cex.axis = 0.7, 
       gap = 3, ylab = c("Missing Data Pattern"))
} else {
  print("Install the 'VIM' package to visualize missing values.")
}
