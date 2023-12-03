#Question 1
# Installing and loading the 'car' package
install.packages("car")
library(car)

# Loading the Prestige dataset
data(Prestige)
help(Prestige)

# a) Creating a new variable 'professional'
Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0)

# b) Running a linear model with prestige as an outcome

model <- lm(prestige ~ income + professional + income:professional, data = Prestige)
summary(model)


# Calculating the effect for (f)
income_effect_professional <- (coefficients["income"] + coefficients["income:professional"]) * 1000
cat("Effect of $1,000 increase in income for professionals: ", income_effect_professional, "\n")

# Calculating the effect for (g)
change_effect <- coefficients["professional"] + coefficients["income:professional"] * 6000
cat("Effect of changing to professional with $6,000 income: ", change_effect, "\n")

#Question 2
# Coefficients and Standard Errors
coeff_lawn_signs <- 0.042
se_lawn_signs <- 0.016
coeff_adjacent <- 0.042
se_adjacent <- 0.013

# Calculating t-values for (a)
t_lawn_signs <- coeff_lawn_signs / se_lawn_signs
cat("t-value for precincts with lawn signs: ", t_lawn_signs, "\n")
# Calculating t-values for (b)
t_adjacent <- coeff_adjacent / se_adjacent
cat("t-value for precincts adjacent to lawn signs: ", t_adjacent, "\n")


