# Load necessary libraries
#install.packages("ISLR")
library(ISLR)
library(dplyr)
library(caret)

# Load the Credit dataset
data(Credit)

# Drop the ID column
credit_data <- Credit %>% select(-ID)

# One-hot encode categorical variables
credit_data <- dummyVars(" ~ .", data = credit_data)
credit_data <- data.frame(predict(credit_data, newdata = Credit))

# Define the dependent and independent variables
balance <- credit_data$Balance
independent_vars <- credit_data %>% select(-Balance)

# Build the linear regression model
model <- lm(balance ~ ., data = independent_vars)

# Print the summary of the model
summary(model)


# Load necessary libraries
#install.packages("ISLR")
#install.packages("car")
library(ISLR)
library(car)
library(dplyr)
library(caret)

# Load the Credit dataset
data(Credit)

# Drop the ID column
credit_data <- Credit %>% select(-ID)

# One-hot encode categorical variables and exclude one level from each
credit_data <- dummyVars(" ~ .", data = credit_data, fullRank = TRUE)
credit_data <- data.frame(predict(credit_data, newdata = Credit))

# Define the dependent and independent variables
balance <- credit_data$Balance
independent_vars <- credit_data %>% select(-Balance)

# Build the linear regression model
model <- lm(balance ~ ., data = independent_vars)

# Calculate VIF for each predictor
vif_values <- vif(model)

# Print the VIF values
print(vif_values)

# Identify predictors with VIF over 5
high_vif_features <- names(vif_values[vif_values > 5])
print(high_vif_features)

#Calculate Cook's distance
cooksd <- cooks.distance(model)

# Print Cook's distance values
print(cooksd)

# Identify the number of influential points (Cook's distance > 1)
num_influential_points <- sum(cooksd > 1)
print(num_influential_points)


# Generate residual vs. fitted plot
plot(model, which = 1)



# Add 1 to Balance to handle log transformation of 0 values
Credit$Balance_log <- log(Credit$Balance + 1)

# Add 1 to independent variables for log transformation
Credit$Income_log <- log(Credit$Income + 1)
Credit$Rating_log <- log(Credit$Rating + 1)
Credit$Age_log <- log(Credit$Age + 1)

# Model 1: Linear-Linear Model
model1 <- lm(Balance ~ Income + Rating + Age, data = Credit)
summary(model1)$r.squared

# Model 2: Log-Linear Model
model2 <- lm(Balance_log ~ Income + Rating + Age, data = Credit)
summary(model2)$r.squared

# Model 3: Linear-Log Model
model3 <- lm(Balance ~ Income_log + Rating_log + Age_log, data = Credit)
summary(model3)$r.squared

# Model 4: Log-Log Model
model4 <- lm(Balance_log ~ Income_log + Rating_log + Age_log, data = Credit)
summary(model4)$r.squared



# Fit a linear regression model
model <- lm(Balance ~ Income + Student + Income:Student, data = Credit)

# Summary of the model
summary(model)

# Predict the average credit card balance for Amy Henderson
Amy_Income <- 30000
Amy_Student <- "Yes"

# Create a new data frame for prediction
Amy_data <- data.frame(Income = Amy_Income, Student = Amy_Student)

# Predicting Amy's average credit card balance
prediction <- predict(model, newdata = Amy_data)

# Round the prediction to the nearest dollar
prediction_rounded <- round(prediction)

# Print the rounded prediction
print(prediction_rounded)




# Amy Henderson's details
income_amy <- 30000
student_amy <- "Yes"  # Since Amy is a student

# Convert the student status to numeric as required for prediction
student_amy_numeric <- ifelse(student_amy == "Yes", 1, 0)

# Calculate the interaction term for Amy
income_student_amy <- income_amy * student_amy_numeric

# Create a data frame with Amy's details for prediction
new_customer <- data.frame(
  Income = income_amy,
  Student = student_amy_numeric,
  Income_Student = income_student_amy
)

# Predict the average balance for Amy
predicted_balance <- predict(model, new_customer)

# Round the prediction to the nearest dollar
predicted_balance_rounded <- round(predicted_balance)

# Print the predicted balance
predicted_balance_rounded




# Load necessary libraries
library(ISLR)
library(dplyr)

# Load the Credit dataset
data(Credit)

# Create a new interaction term between Income and Student
Credit <- Credit %>%
  mutate(Income_Student = Income * ifelse(Student == "Yes", 1, 0))

# Fit the linear model
model <- lm(Balance ~ Income + Student + Income_Student, data = Credit)

# Summary of the model to check the coefficients
summary(model)

# Amy Henderson's details
income_amy <- 30000
student_amy <- "Yes"  # Amy is a student

# Convert student status to numeric for prediction
student_amy_numeric <- ifelse(student_amy == "Yes", 1, 0)

# Calculate the interaction term for Amy
income_student_amy <- income_amy * student_amy_numeric

# Create a data frame with Amy's details for prediction
new_customer <- data.frame(
  Income = income_amy,
  Student = factor(student_amy, levels = c("No", "Yes")),
  Income_Student = income_student_amy
)

# Predict the average balance for Amy
predicted_balance <- predict(model, new_customer)

# Round the prediction to the nearest dollar
predicted_balance_rounded <- round(predicted_balance)

# Print the predicted balance
predicted_balance_rounded




# Load necessary libraries
library(ISLR)
library(dplyr)

# Load the Credit dataset
data(Credit)

# Create a new interaction term between Income and Student
Credit <- Credit %>%
  mutate(Income_Student = Income * ifelse(Student == "Yes", 1, 0))

# Fit the linear model
model <- lm(Balance ~ Income + Student + Income_Student, data = Credit)

# Summary of the model to check the coefficients
summary(model)

# Amy Henderson's details
income_amy <- 30000
student_amy <- "Yes"  # Amy is a student

# Convert student status to numeric for prediction
student_amy_numeric <- ifelse(student_amy == "Yes", 1, 0)

# Calculate the interaction term for Amy
income_student_amy <- income_amy * student_amy_numeric

# Create a data frame with Amy's details for prediction
new_customer <- data.frame(
  Income = income_amy,
  Student = factor(student_amy, levels = c("No", "Yes")),
  Income_Student = income_student_amy
)

# Predict the average balance for Amy
predicted_balance <- predict(model, new_customer)

# Round the prediction to the nearest dollar
predicted_balance_rounded <- round(predicted_balance)

# Print the predicted balance
predicted_balance_rounded



# Load necessary libraries
library(ISLR)
library(dplyr)

# Load the Credit dataset
data(Credit)

# Create a new interaction term between Income and Student
Credit <- Credit %>%
  mutate(Income_Student = Income * ifelse(Student == "Yes", 1, 0))

# Fit the linear model
model <- lm(Balance ~ Income + Student + Income_Student, data = Credit)

# Amy Henderson's details
income_amy <- 30
student_amy <- "Yes"  # Amy is a student

# Convert student status to numeric for prediction
student_amy_numeric <- ifelse(student_amy == "Yes", 1, 0)

# Calculate the interaction term for Amy
income_student_amy <- income_amy * student_amy_numeric

# Create a data frame with Amy's details for prediction
new_customer <- data.frame(
  Income = income_amy,
  Student = factor(student_amy, levels = c("No", "Yes")),
  Income_Student = income_student_amy
)

# Predict the average balance for Amy
predicted_balance <- predict(model, new_customer)

# Round the prediction to the nearest dollar
predicted_balance_rounded <- round(predicted_balance)

# Print the predicted balance
predicted_balance_rounded
