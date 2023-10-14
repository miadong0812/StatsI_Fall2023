
#Question 1: Political Science
#(a)calculate the Chi-Square 
# Step1.Create the observed data matrix and display the matrix with totals
observed_data <- matrix(c(14, 6, 7, 7, 7, 1), nrow = 2, byrow = TRUE)
rownames(observed_data) <- c("Upper class", "Lower class")
colnames(observed_data) <- c("Not Stopped", "Bribe requested", "Stopped/Given Warning")
observed_data_with_totals <- addmargins(observed_data)
print(observed_data_with_totals)

# Step2.Calculate expected frequencies
total <- sum(observed_data)
rows <- nrow(observed_data)
columns <- ncol(observed_data)
expected_data <- outer(rowSums(observed_data), colSums(observed_data)) / total
print(expected_data)

# Step3.Calculate chi-squared test statistic
chi_squared <- sum((observed_data - expected_data)^2 / expected_data)
print(chi_squared)

# (b) Calculate the p-value
df <- (rows - 1) * (columns - 1)
p_value <- pchisq(chi_squared, df = df, lower.tail = FALSE)
print(p_value)



# (C)Calculate the standardized residuals
# Step 1: Initialize the matrix for storing standardized residuals
standardized_residuals <- matrix(0, nrow = nrow(observed_data), ncol = ncol(observed_data))

# Step 2: Calculate row totals, column totals, and grand total
row_totals <- rowSums(observed_data)
col_totals <- colSums(observed_data)
grand_total <- sum(observed_data)

# Step 3: Calculate standardized residuals
for (i in 1:nrow(observed_data)) {
  for (j in 1:ncol(observed_data)) {
    fo <- observed_data[i, j]
    fe <- expected_data[i, j]
    Nr <- row_totals[i]
    Nc <- col_totals[j]
    N <- grand_total
    standardized_residuals[i, j] <- (fo - fe) / sqrt(fe * (1 - Nr / N) * (1 - Nc / N))
  }
}

# Step 4: Assign row and column names to the standardized residuals matrix
rownames(standardized_residuals) <- c("Upper class", "Lower class")
colnames(standardized_residuals) <- c("Not Stopped", "Bribe requested", "Stopped/Given Warning")

# Step 5: Display the standardized residuals matrix
print(standardized_residuals)

#Question 2: Economics
#run a bivariate regression 
data <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")
names(data)
model <- lm(water ~ reserved, data=data)
summary(model)

#calculated the CI
confint(model)



