#Question 1:Education
# Step 1: Calculate the sample mean.
y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
y_bar <- mean(y)
y_bar

# Step 2: Calculate the sample standard deviation (S)
S <- sd(y)
S
# Step 3: Calculate the Z-score
confidence_level <- 0.9
alpha <- 1 - confidence_level
z_score <- qnorm(1 - alpha / 2)
z_score
# Step 4: Calculate the margin of error (ME)
ME <- z_score * (S / sqrt(n))
ME
#Step 5:Calculate the confidence interval 
CI_lower <- y_bar - ME
CI_upper <- y_bar + ME
cat("90% Confidence Interval for Average Student IQ: [", CI_lower, ",", CI_upper, "]\n")

#2.Using the same sample, conduct the appropriate hypothesis test with alpha=0:05.
# Step 1: Given Data (already defined)
mu <- 100 # national average IQ
alpha <- 0.05

# Step 2: Calculate Z statistics
Z <- (y_bar - mu) / (S / sqrt(n))
Z

# Step 3: Calculate Critical value for one-tailed test
Z_alpha <- qnorm(1 - alpha)
Z_alpha

# Step 4: Calculate P-value
p_value <- 1 - pnorm(Z)
p_value

# Step 5: Make a decision
if (Z > Z_alpha) {
  decision <- "Reject Null Hypothesis"
} else {
  decision <- "Fail to Reject Null Hypothesis"
}
decision

#Question 2
save_lm_summary <- function(lm_model, output_file) {
  summary_text <- capture.output(summary(lm_model))
  write(summary_text, file = output_file, append = TRUE)
}


if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

library(ggplot2)


expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/expenditure.txt", header = TRUE)

# Dataset Summary
summary_stats <- summary(expenditure[, c("Y", "X1", "X2", "X3")])
cat("Dataset Summary:\n")
print(summary_stats)

# Calculate and print the correlation matrix
cor_matrix <- cor(expenditure[, c("Y", "X1", "X2", "X3")])
cat("Correlation Matrix:\n")
print(cor_matrix)

#Part 1 plot the relationships among Y, X1, X2, and X3
plot1 <- ggplot(expenditure, aes(x = X1, y = Y)) + geom_point() + geom_smooth(method = 'lm') + ggtitle("Y vs X1")
plot2 <- ggplot(expenditure, aes(x = X2, y = Y)) + geom_point() + geom_smooth(method = 'lm') + ggtitle("Y vs X2")
plot3 <- ggplot(expenditure, aes(x = X3, y = Y)) + geom_point() + geom_smooth(method = 'lm') + ggtitle("Y vs X3")
plot4 <- ggplot(expenditure, aes(x = X1, y = X2)) + geom_point() + geom_smooth(method = 'lm') + ggtitle("X1 vs X2")
plot5 <- ggplot(expenditure, aes(x = X1, y = X3)) + geom_point() + geom_smooth(method = 'lm') + ggtitle("X1 vs X3")
plot6 <- ggplot(expenditure, aes(x = X2, y = X3)) + geom_point() + geom_smooth(method = 'lm') + ggtitle("X2 vs X3")
print(plot1)
print(plot2)
print(plot3)
print(plot4)
print(plot5)
print(plot6)

#part 2
# Calculate the mean expenditure for each region
mean_expenditure_by_region <- aggregate(Y ~ Region, data = expenditure, FUN = mean)
print(mean_expenditure_by_region)
# Identify the region with the highest mean expenditure
highest_region <- mean_expenditure_by_region[which.max(mean_expenditure_by_region$Y), ]
cat("The region with the highest per capita expenditure on housing assistance is:", highest_region$Region)

plot7<- ggplot(expenditure, aes(x = as.factor(Region), y = Y, fill = as.factor(Region))) + 
  geom_boxplot() + 
  ggtitle("Y vs Region") + 
  scale_fill_manual(values = c("red", "blue", "green", "purple"))
print(plot7)

#Part 3
plot8 <- ggplot(expenditure, aes(x = X1, y = Y, color = as.factor(Region), shape = as.factor(Region))) + 
  geom_point() + 
  geom_smooth(method = 'lm', se = FALSE, aes(group = 1)) + 
  ggtitle("Y vs X1 by Region") +
  xlab("X1") +
  ylab("Y") +
  labs(color = "Region") +
  theme_minimal()

print(plot8)

save_lm_summary <- function(model, file_name) {
  summary_text <- capture.output(summary(model))
  writeLines(summary_text, con = file_name)
}
# Run the linear regression model between Y and X1
linear_regression_Y_X1 <- lm(Y ~ X1, data = expenditure)

linear_regression_summary <- summary(linear_regression_Y_X1)

linear_regression_summary_text <- capture.output(linear_regression_summary)

# Save the summary to a .tex file 
writeLines(linear_regression_summary_text, "/Users/miadong/Desktop/linear_regression_Y_X1_summary.tex")

library(stargazer)

# Run the regression
regression1 <- lm(Y ~ X1, data = expenditure)

# Define the output_stargazer function
output_stargazer <- function(outputFile, ...) {
  output <- capture.output(stargazer(..., type = "latex"))
  cat(paste(output, collapse = "\n"), "\n", file = outputFile, append = TRUE)
}
# Use the function to save the LaTeX-formatted table to a .tex file
output_stargazer("regression_output11.tex", regression1)
getwd()

