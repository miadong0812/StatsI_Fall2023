#Question 1
# Load the dataset from the provided URL and store it in a data frame 'inc.sub'.
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/incumbents_subset.csv")

# Rename the 'voteshare' column to 'Y' and 'difflog' to 'X1' for easier reference.
names(inc.sub)[names(inc.sub) == "voteshare"] <- "Y"
names(inc.sub)[names(inc.sub) == "difflog"] <- "X1"

# Fit a linear regression model with 'Y' as the dependent variable and 'X1' as the independent variable.
# This will model the relationship between the incumbent's vote share and the difference in campaign spending log.
model <- lm(Y ~ X1, data=inc.sub)

# Output a summary of the linear regression model to get detailed statistics,
summary(model)

# Create a scatterplot of 'X1' versus 'Y' using ggplot2 and add a linear regression line.
scatterplot <- ggplot(inc.sub, aes(x=X1, y=Y)) +
  geom_point(color = "#3498db", shape = 19, size = 2) + # A bright but soft blue color for points
  geom_smooth(method = "lm", color = "#e74c3c", se = FALSE, linetype = "solid", size = 1) + # A soft red for the regression line
  labs(title = "Vote Share vs. Campaign Spending Difference",
       x = "Logarithm of Campaign Spending Difference (X1)",
       y = "Incumbent's Vote Share (Y)") +
  theme_light() + # Use a light theme for a brighter background
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), # Center and bold the plot title
    axis.title.x = element_text(size = 14, face = "bold"), # Bold X axis title
    axis.title.y = element_text(size = 14, face = "bold"), # Bold Y axis title
    axis.text = element_text(size = 12, color = "#2c3e50"), # Dark text for better contrast
    axis.line = element_line(color = "#2c3e50"), # Dark axis lines for contrast
    panel.grid.major = element_line(color = "#bdc3c7", linetype = "dotted"), # Light gray for major grid lines
    panel.grid.minor = element_blank(), # Remove minor grid lines
    legend.position = "none" # Remove legend
  )

# Save the updated scatterplot as a PNG file
ggsave("scatterplot_Y_X1_regression_line.png", plot = scatterplot, width = 10, height = 8, dpi = 300)


# Extract the residuals from the fitted model and store them in an object called 'residuals'.
# Residuals are the differences between observed values and values predicted by the model.
residuals <- resid(model)
# View the first few residuals using the head function to get an immediate sense of their distribution.
head(residuals)
# Get a summary of the residuals to understand their central tendency and spread.
summary(residuals)

# Retrieve the intercept and slope coefficients from the model and store them in variables.
intercept <- coef(model)[1]
slope <- coef(model)[2]

# Construct the prediction equation in a human-readable form using the coefficients.
# This equation can be used to predict 'Y' given new values of 'X1'.
prediction_equation_Y_X1 <- paste("Y =", format(intercept, digits = 4), "+", format(slope, digits = 4), "* X1")
print(prediction_equation_Y_X1)

#question 2

# Rename the columns as specified for easier reference in the analysis
names(inc.sub)[names(inc.sub) == "presvote"] <- "X2"
names(inc.sub)[names(inc.sub) == "difflog"] <- "X1"

# Run the regression with 'X2' as the outcome variable and 'X1' as the explanatory variable
model_X2_X1 <- lm(X2 ~ X1, data=inc.sub)
# Output the summary of the model to get the coefficients and other statistical measures
summary_X2_X1 <- summary(model_X2_X1)
print(summary_X2_X1)

# Create a scatterplot of 'X1' versus 'X2' with a regression line
scatterplot_X2_X1 <- ggplot(inc.sub, aes(x=X1, y=X2)) +
  geom_point(color = "#3498db", shape = 19, size = 2) +
  geom_smooth(method = "lm", color = "#e74c3c", se = FALSE, linetype = "solid", size = 1) +
  labs(title = "Presidential Vote Share (X2) vs. Campaign Spending Difference (X1)",
       x = "Campaign Spending Difference (X1)",
       y = "Presidential Vote Share (X2)") +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12, color = "#2c3e50"),
    axis.line = element_line(color = "#2c3e50"),
    panel.grid.major = element_line(color = "#bdc3c7", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )
ggsave("scatterplot_X2_X1_regression_line.png", plot = scatterplot_X2_X1, width = 10, height = 8, dpi = 300)

# Save the residuals of the model in a separate object
residuals_X2_X1 <- resid(model_X2_X1)
# To view the first six residuals
head(residuals_X2_X1)
# To get a summary of the residuals
summary(residuals_X2_X1)


# Write the prediction equation based on the model's coefficients
intercept_X2_X1 <- coef(model_X2_X1)[1]
slope_X2_X1 <- coef(model_X2_X1)[2]
prediction_equation_X2_X1 <- paste("X2 =", format(intercept_X2_X1, digits = 4), "+", format(slope_X2_X1, digits = 4), "* X1")
# Print out the prediction equation
print(prediction_equation_X2_X1)

#Question 3

# Step 1:Assuming 'voteshare' is already named 'Y' and 'presvote' is named 'X2' from previous steps.
# If not, rename them as follows:
names(inc.sub)[names(inc.sub) == "voteshare"] <- "Y"
names(inc.sub)[names(inc.sub) == "presvote"] <- "X2"

# Step 2: Run the regression with 'Y' as the outcome variable and 'X2' as the explanatory variable
# This models the relationship between the incumbent's vote share and the presidential vote share.
model_Y_X2 <- lm(Y ~ X2, data=inc.sub)

# Step 3: Summarize the model to obtain the regression coefficients and other statistics.
# This summary provides details on the significance and strength of the relationship.
summary_Y_X2 <- summary(model_Y_X2)
print(summary_Y_X2)

# Step 4: Create a scatterplot with 'X2' on the x-axis and 'Y' on the y-axis.
# The scatterplot will also include the regression line to visualize the relationship.
scatterplot_Y_X2 <- ggplot(inc.sub, aes(x=X2, y=Y)) +
  geom_point(color = "#3498db", shape = 19, size = 2) + # Blue points
  geom_smooth(method = "lm", color = "#e74c3c", se = FALSE, linetype = "solid", size = 1) + # Red regression line
  labs(title = "Incumbent's Vote Share (Y) vs. Presidential Vote Share (X2)",
       x = "Presidential Vote Share (X2)",
       y = "Incumbent's Vote Share (Y)") +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12, color = "#2c3e50"),
    axis.line = element_line(color = "#2c3e50"),
    panel.grid.major = element_line(color = "#bdc3c7", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

# Save the scatterplot as a PNG file.
ggsave("scatterplot_Y_X2_regression_line.png", plot = scatterplot_Y_X2, width = 10, height = 8, dpi = 300)

# Step 5: Write the prediction equation based on the model's coefficients.
# This equation will allow us to predict 'Y' given a new value of 'X2'.
intercept_Y_X2 <- coef(model_Y_X2)[1]
slope_Y_X2 <- coef(model_Y_X2)[2]
prediction_equation_Y_X2 <- paste("Y =", format(intercept_Y_X2, digits = 4), "+", format(slope_Y_X2, digits = 4), "* X2")
print(prediction_equation_Y_X2)


#Question 4
# Use the residuals from Question 1
residuals_Q1 <- resid(model)

# Use the residuals from Question 2
residuals_Q2 <- resid(model_X2_X1)

# Step 1: Run the regression with residuals from Question 1 as the outcome variable
# and residuals from Question 2 as the explanatory variable.
model_residuals <- lm(residuals_Q1 ~ residuals_Q2)

# Step 2: Summarize the model to obtain the regression coefficients and other statistics.
summary_residuals <- summary(model_residuals)
print(summary_residuals)

# Step 3: Create a scatterplot with residuals from Question 2 on the x-axis and residuals from Question 1 on the y-axis.
# The scatterplot will also include the regression line to visualize the relationship.
scatterplot_residuals <- ggplot(data.frame(residuals_Q1, residuals_Q2), aes(x=residuals_Q2, y=residuals_Q1)) +
  geom_point(color = "#3498db", shape = 19, size = 2) + # Blue points
  geom_smooth(method = "lm", color = "#e74c3c", se = FALSE, linetype = "solid", size = 1) + # Red regression line
  labs(title = "Residuals of Y vs. Residuals of X2",
       x = "Residuals of Presidential Vote Share (X2)",
       y = "Residuals of Incumbent's Vote Share (Y)") +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12, color = "#2c3e50"),
    axis.line = element_line(color = "#2c3e50"),
    panel.grid.major = element_line(color = "#bdc3c7", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

# Save the scatterplot as a PNG file.
ggsave("scatterplot_residuals_regression_line.png", plot = scatterplot_residuals, width = 10, height = 8, dpi = 300)

# Step 4: Write the prediction equation based on the model's coefficients.
intercept_residuals <- coef(model_residuals)[1]
slope_residuals <- coef(model_residuals)[2]
prediction_equation_residuals <- paste("Residuals_Y =", format(intercept_residuals, digits = 4), "+", format(slope_residuals, digits = 4), "* Residuals_X2")
print(prediction_equation_residuals)

#Question 5

# Rename the columns to match the variables mentioned in the question
names(inc.sub)[names(inc.sub) == "voteshare"] <- "Y"
names(inc.sub)[names(inc.sub) == "difflog"] <- "X1"
names(inc.sub)[names(inc.sub) == "presvote"] <- "X2"

# Step 1: Run the regression with Y as the outcome variable and both X1 and X2 as explanatory variables
model_Y_X1_X2 <- lm(Y ~ X1 + X2, data=inc.sub)
#Step 2:Summarize the model to obtain the regression coefficients and other statistics.
summary_Y_X1_X2 <- summary(model_Y_X1_X2)
print(summary_Y_X1_X2)

# Step 3: Write the prediction equation based on the model's coefficients
coefficients <- coef(model_Y_X1_X2)
prediction_equation_Y_X1_X2 <- paste("Y =", format(coefficients[1], digits = 4), "+",
                                     format(coefficients[2], digits = 4), "* X1 +",
                                     format(coefficients[3], digits = 4), "* X2")
print(prediction_equation_Y_X1_X2)

