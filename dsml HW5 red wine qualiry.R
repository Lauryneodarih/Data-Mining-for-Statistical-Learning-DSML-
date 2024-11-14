#install.packages("rpart")   #install rpart package
library(rpart)              #load the rpart library
#install.packages("rpart.plot")  #install rpart.plot package
library(rpart.plot)             #load the rpart.plot library
#install.packages("caret")   #install the caret package
library(caret)              #load the caret library
#install.packages("randomForest") #install the random forest package
library(randomForest) #load the random forest library.
library(class) #load class library
library(caret)#load caret library
library(e1071)#load e1071 library
#install.packages("tidyverse") #install tidyverse package
library(tidyverse)#load tidyverse library
#install.packages("corrplot") #install the corrplot package
library(corrplot)       #load the corrplot library
library(dplyr)
library(car)  # Load the 'car' package for the scatterplotMatrix function

#Data Processing and Decision Tree Model
wine <- read.table(file="winequality-red.csv",header= TRUE, sep = ",")
head(wine)                  #first six rows of the data
summary(wine)               #summary statistics of the data.

# Create a scatterplot matrix for selected variables
scatterplotMatrix(~fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide, data = wine)

hist(wine$quality)          #histogram of quality of wine
count(wine)                 #counts the number of rows in the data

0.7*count(wine)             #classifies data into training and testing sets
s <- sample(1599, 1119)     #classifies data into training and testing sets
wine_training <- wine[s,]   #assigns data for training and testing sets.
wine_test=wine[- s,]        #assigns data for training and testing sets.
dim(wine_training)          #gets dimension of training data set.
dim(wine_test)              #gets dimension of testing data set.

# Linear Regression Model
# Assuming you want to predict 'quality' based on some variables

# Create a linear regression model using the training data
lm_model <- lm(quality ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide, data = wine_training)

# Print a summary of the linear regression model
summary(lm_model)

# Make predictions on the test data
lm_predictions <- predict(lm_model, newdata = wine_test)

# Evaluate the regression model (e.g., using Mean Squared Error)
mse <- mean((lm_predictions - wine_test$quality)^2)
cat("Mean Squared Error:", mse, "\n")

# You can also visualize the regression results if needed
plot(wine_test$quality, lm_predictions, main = "Linear Regression Model", xlab = "Actual Quality", ylab = "Predicted Quality")
abline(0, 1, col = "red")  # Add a 45-degree reference line



# Linear Regression Model with Variable Selection
# Assuming you want to predict 'quality' based on variable selection

# Perform stepwise variable selection using the training data
stepwise_model <- step(lm(quality ~ ., data = wine_training))

# Print a summary of the stepwise selected linear regression model
summary(stepwise_model)

# Make predictions on the test data
stepwise_predictions <- predict(stepwise_model, newdata = wine_test)

# Evaluate the regression model (e.g., using Mean Squared Error)
mse_stepwise <- mean((stepwise_predictions - wine_test$quality)^2)
cat("Mean Squared Error (Stepwise Selection):", mse_stepwise, "\n")

# You can also visualize the regression results if needed
plot(wine_test$quality, stepwise_predictions, main = "Linear Regression with Stepwise Selection", xlab = "Actual Quality", ylab = "Predicted Quality")
abline(0, 1, col = "red")  # Add a 45-degree reference line


# Linear Discriminant Analysis (LDA) for Classification

# Load the MASS package for LDA
library(MASS)

# Create a 'taste' variable based on 'quality' in the training dataset
wine_training$taste <- ifelse(wine_training$quality < 5, "bad", "good")
wine_training$taste[wine_training$quality == 5] <- "Average"
wine_training$taste[wine_training$quality == 6] <- "Average"
wine_training$taste <- as.factor(wine_training$taste)

# Create a 'taste' variable in the test dataset as well
wine_test$taste <- ifelse(wine_test$quality < 5, "bad", "good")
wine_test$taste[wine_test$quality == 5] <- "Average"
wine_test$taste[wine_test$quality == 6] <- "Average"
wine_test$taste <- as.factor(wine_test$taste)

# Perform LDA classification using the training data
lda_model <- lda(taste ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide, data = wine_training)

# Make predictions on the test data
lda_predictions <- predict(lda_model, wine_test)

# Extract predicted class labels
predicted_classes <- lda_predictions$class

# Evaluate the LDA model using a confusion matrix
confusion <- table(predicted_classes, wine_test$taste)
cat("Confusion Matrix:\n", confusion, "\n")

# Calculate classification accuracy
accuracy <- sum(diag(confusion)) / sum(confusion)
cat("Classification Accuracy:", accuracy, "\n")

# Adjust figure margins before plotting
par(mar = rep(2, 4))  # Adjust margins
# You can also visualize the results if needed
plot(lda_model, dimen = 1)  # Plot LDA results
# Reset figure margins
par(mar = c(5, 4, 4, 2) + 0.1)


# Quadratic Discriminant Analysis (QDA) for Classification
# Assuming you want to classify wine quality into categories based on selected variables

# Load the MASS package for QDA
library(MASS)

# Create a 'taste' variable based on 'quality' in the training dataset
wine_training$taste <- ifelse(wine_training$quality < 5, "bad", "good")
wine_training$taste[wine_training$quality == 5] <- "Average"
wine_training$taste[wine_training$quality == 6] <- "Average"
wine_training$taste <- as.factor(wine_training$taste)

# Create a 'taste' variable in the test dataset as well
wine_test$taste <- ifelse(wine_test$quality < 5, "bad", "good")
wine_test$taste[wine_test$quality == 5] <- "Average"
wine_test$taste[wine_test$quality == 6] <- "Average"
wine_test$taste <- as.factor(wine_test$taste)

# Perform QDA classification using the training data
qda_model <- qda(taste ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide, data = wine_training)

# Make predictions on the test data
qda_predictions <- predict(qda_model, wine_test)

# Extract predicted class labels
predicted_classes <- qda_predictions$class

# Evaluate the QDA model using a confusion matrix
confusion <- table(predicted_classes, wine_test$taste)
cat("Confusion Matrix:\n", confusion, "\n")

# Calculate classification accuracy
accuracy <- sum(diag(confusion)) / sum(confusion)
cat("Classification Accuracy:", accuracy, "\n")
# Visualization: Create a scatterplot of predicted class memberships
plot(wine_test[, c("fixed.acidity", "volatile.acidity")], col = predicted_classes, pch = 20, 
     main = "QDA Classification Results", xlab = "Fixed Acidity", ylab = "Volatile Acidity")

# Naive Bayes Classification
# Assuming you want to classify wine quality into categories based on selected variables

# Load the e1071 package for Naive Bayes
library(e1071)

# Create a 'taste' variable based on 'quality' in the training dataset
wine_training$taste <- ifelse(wine_training$quality < 5, "bad", "good")
wine_training$taste[wine_training$quality == 5] <- "Average"
wine_training$taste[wine_training$quality == 6] <- "Average"
wine_training$taste <- as.factor(wine_training$taste)

# Create a 'taste' variable in the test dataset as well
wine_test$taste <- ifelse(wine_test$quality < 5, "bad", "good")
wine_test$taste[wine_test$quality == 5] <- "Average"
wine_test$taste[wine_test$quality == 6] <- "Average"
wine_test$taste <- as.factor(wine_test$taste)

# Perform Naive Bayes classification using the training data
nb_model <- naiveBayes(taste ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide, data = wine_training)

# Make predictions on the test data
nb_predictions <- predict(nb_model, wine_test)

# Evaluate the Naive Bayes model using a confusion matrix
confusion <- table(nb_predictions, wine_test$taste)
cat("Confusion Matrix:\n", confusion, "\n")

# Calculate classification accuracy
accuracy <- sum(diag(confusion)) / sum(confusion)
cat("Classification Accuracy:", accuracy, "\n")


tm <- rpart(quality~., wine_training, method = "class") #assigns decision tree values

rpart.plot(tm, tweak = 1.8)     #generate the decision tree
rpart.plot(tm, type = 4, extra = 101, tweak = 1.8) #

#generate decision tree with more descriptions
pred <- predict(tm, wine_test, type = "class") #test the model on the test data set
table(wine_test$quality, pred)

confusionMatrix(table(pred, wine_test$quality)) #generate confusion matrix

#Random Forests
wine$taste <- ifelse(wine$quality < 5, "bad", "good") #classification of wine quality as bad and good. 
wine$taste[wine$quality == 5] <- "Average"            #Assignment of a score of 5 as average quality
wine$taste[wine$quality == 6] <- "Average"            #Assignment of a score of 6 as average quality
wine$taste <- as.factor(wine$taste)                   #assigns quality scores to respective quality (good, bad, average)
str(wine$taste)
barplot(table(wine$taste))                            #Plotting quality of wine
training <- sample(1599, 1119)  #separation of data set into training and testing sets.
wine_train <- wine[training, ]  #Assign training data. 
wine_test <- wine[-training, ]  #Assign testing data.
dim(wine_train) #dimension of training data.
dim(wine_test)  #dimension of test data. 

model <- randomForest(taste ~ . - quality, data = wine_train) #train the model using using training data.
model
prediction <- predict(model, newdata = wine_test) #test the model. 
table(prediction, wine_test$taste)#tabulate model statistics. 

#KNN Classification 

wine$taste <- ifelse(wine$quality < 5, "bad", "good") #classification of wine quality into good and bad.
wine$taste <- as.factor(wine$taste)#assign quality of wine into the groups
str(wine$taste)
barplot(table(wine$taste)) #plot wine quality

wine$qualitynum=wine$quality
wine$quality[which(wine$quality%in%c(3,4))]='bad'     #classify quality score of 3 and 4 into bad
wine$quality[which(wine$quality%in%c(5,6))]='average'#classify quality score of 5 and 6 into average
wine$quality[which(wine$quality%in%c(7,8,9))]='good'#classify quality score of 7,8,9 into good
features=colnames(wine)[1:11] #exclude the quality column
train.set=createDataPartition(wine$quality, p=0.7, list = FALSE) #separate data into training and testing. 
exclude=which(names(wine)%in%c("color", "qualitynum"))
train=wine[train.set, -exclude]#assign training data set
test=wine[-train.set, -exclude]#assign testing data set
ctrl=trainControl(method = "repeatedcv", repeats = 5, classProbs =TRUE)
knn.mod=train(quality~.,data=train, #knn model
              method='knn',
              preProc=c("center", "scale"),
              metric="Accuracy",
              trControl=ctrl, tuneLength=75)
knn.mod               #KNN model
plot(knn.mod)       #plot of KNN Model`

# Install and load the Adabag package for multi-class AdaBoost
#install.packages("adabag")
library(adabag)

# Perform multi-class AdaBoost classification using the training data
ada_model <- boosting(taste ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide, data = wine_training)

# Make predictions on the test data
ada_predictions <- predict.boosting(ada_model, wine_test)

# Evaluate the AdaBoost model using a confusion matrix
confusion <- table(ada_predictions$class, wine_test$taste)
cat("Confusion Matrix:\n", confusion, "\n")

# Calculate classification accuracy
accuracy <- sum(diag(confusion)) / sum(confusion)
cat("Classification Accuracy:", accuracy, "\n")


#Correlation Coefficients 
data= read.table(file="winequality-red.csv",header= TRUE, sep = ",")
round(cor(data), #correlation of variables, rounded off to 2 decimal places
      digits = 2)

corrplot(cor(data), method="pie") #correlation plot.

### Training and testing errors for the models

# Calculate training error for Linear Regression Model
lm_train_predictions <- predict(lm_model, newdata = wine_training)
lm_train_mse <- mean((lm_train_predictions - wine_training$quality)^2)

# Calculate testing error for Linear Regression Model
lm_test_predictions <- predict(lm_model, newdata = wine_test)
lm_test_mse <- mean((lm_test_predictions - wine_test$quality)^2)

cat("Linear Regression Training Error (MSE):", lm_train_mse, "\n")
cat("Linear Regression Testing Error (MSE):", lm_test_mse, "\n")

# Calculate training error for Stepwise Linear Regression Model
stepwise_train_predictions <- predict(stepwise_model, newdata = wine_training)
stepwise_train_mse <- mean((stepwise_train_predictions - wine_training$quality)^2)

# Calculate testing error for Stepwise Linear Regression Model
stepwise_test_predictions <- predict(stepwise_model, newdata = wine_test)
stepwise_test_mse <- mean((stepwise_test_predictions - wine_test$quality)^2)

cat("Stepwise Linear Regression Training Error (MSE):", stepwise_train_mse, "\n")
cat("Stepwise Linear Regression Testing Error (MSE):", stepwise_test_mse, "\n")


# Calculate training error for LDA
lda_train_predictions <- predict(lda_model, wine_training)
lda_train_error <- mean(lda_train_predictions$class != wine_training$taste)

# Calculate testing error for LDA
lda_test_predictions <- predict(lda_model, wine_test)
lda_test_error <- mean(lda_test_predictions$class != wine_test$taste)

cat("LDA Training Error (Misclassification Rate):", lda_train_error, "\n")
cat("LDA Testing Error (Misclassification Rate):", lda_test_error, "\n")


# Calculate training error for QDA
qda_train_predictions <- predict(qda_model, wine_training)
qda_train_error <- mean(qda_train_predictions$class != wine_training$taste)

# Calculate testing error for QDA
qda_test_predictions <- predict(qda_model, wine_test)
qda_test_error <- mean(qda_test_predictions$class != wine_test$taste)

cat("QDA Training Error (Misclassification Rate):", qda_train_error, "\n")
cat("QDA Testing Error (Misclassification Rate):", qda_test_error, "\n")


# Calculate training error for Naive Bayes
nb_train_predictions <- predict(nb_model, newdata = train)
nb_train_error <- mean(nb_train_predictions != train$quality)

# Calculate training error for Naive Bayes
nb_train_predictions <- predict(nb_model, newdata = train)
nb_train_error <- mean(nb_train_predictions != train$quality)

# Calculate testing error for Naive Bayes
nb_test_predictions <- predict(nb_model, newdata = test)
nb_test_error <- mean(nb_test_predictions != test$quality)

cat("Naive Bayes Training Error (Misclassification Rate):", nb_train_error, "\n")
cat("Naive Bayes Testing Error (Misclassification Rate):", nb_test_error, "\n")


# Calculate training error for Decision Trees
tm_train_predictions <- predict(tm, wine_training, type = "class")
tm_train_error <- mean(tm_train_predictions != wine_training$quality)

# Calculate testing error for Decision Trees
tm_test_predictions <- predict(tm, wine_test, type = "class")
tm_test_error <- mean(tm_test_predictions != wine_test$quality)

cat("Decision Trees Training Error (Misclassification Rate):", tm_train_error, "\n")
cat("Decision Trees Testing Error (Misclassification Rate):", tm_test_error, "\n")


# Calculate training error for Random Forest
rf_train_predictions <- predict(model, newdata = wine_train)
rf_train_error <- mean(rf_train_predictions != wine_train$taste)

# Calculate testing error for Random Forest
rf_test_predictions <- predict(model, newdata = wine_test)
rf_test_error <- mean(rf_test_predictions != wine_test$taste)

cat("Random Forest Training Error (Misclassification Rate):", rf_train_error, "\n")
cat("Random Forest Testing Error (Misclassification Rate):", rf_test_error, "\n")


# K-Nearest Neighbors (KNN) Classification
knn_mod <- train(quality ~ ., data = train, method = "knn", preProcess = c("center", "scale"))
knn_train_predictions <- predict(knn_mod, newdata = train)
knn_test_predictions <- predict(knn_mod, newdata = test)

# Map the predictions back to their original classes
knn_train_predictions <- as.factor(knn_train_predictions)
knn_test_predictions <- as.factor(knn_test_predictions)

# Calculate training error for KNN
knn_train_error <- 1 - sum(knn_train_predictions == train$quality) / length(train$quality)

# Calculate testing error for KNN
knn_test_error <- 1 - sum(knn_test_predictions == test$quality) / length(test$quality)

cat("KNN Training Error (Misclassification Rate):", knn_train_error, "\n")
cat("KNN Testing Error (Misclassification Rate):", knn_test_error, "\n")




# Calculate training error for AdaBoost
ada_train_predictions <- predict.boosting(ada_model, wine_training)
ada_train_error <- mean(ada_train_predictions$class != wine_training$taste)

# Calculate testing error for AdaBoost
ada_test_predictions <- predict.boosting(ada_model, wine_test)
ada_test_error <- mean(ada_test_predictions$class != wine_test$taste)

cat("AdaBoost Training Error (Misclassification Rate):", ada_train_error, "\n")
cat("AdaBoost Testing Error (Misclassification Rate):", ada_test_error, "\n")




# Set the number of Monte Carlo iterations
num_iterations <- 10  # You can adjust this number as needed

# Initialize vectors to store performance metrics for each iteration
lm_train_errors <- numeric(num_iterations)
lm_test_errors <- numeric(num_iterations)
stepwise_train_errors <- numeric(num_iterations)
stepwise_test_errors <- numeric(num_iterations)
lda_train_errors <- numeric(num_iterations)
lda_test_errors <- numeric(num_iterations)
qda_train_errors <- numeric(num_iterations)
qda_test_errors <- numeric(num_iterations)
nb_train_errors <- numeric(num_iterations)
nb_test_errors <- numeric(num_iterations)
tm_train_errors <- numeric(num_iterations)
tm_test_errors <- numeric(num_iterations)
rf_train_errors <- numeric(num_iterations)
rf_test_errors <- numeric(num_iterations)
knn_train_errors <- numeric(num_iterations)
knn_test_errors <- numeric(num_iterations)
ada_train_errors <- numeric(num_iterations)
ada_test_errors <- numeric(num_iterations)

for (iteration in 1:num_iterations) {
  # Randomly split the data into training and testing sets
  training <- sample(1599, 1119)
  wine_training <- wine[training, ]
  wine_test <- wine[-training, ]
  
  # Linear Regression Model
  # ... (Rest of the Linear Regression code)
  
  # Linear Regression Model with Variable Selection
  # ... (Rest of the Stepwise Linear Regression code)
  
  # Linear Discriminant Analysis (LDA) for Classification
  # ... (Rest of the LDA code)
  
  # Quadratic Discriminant Analysis (QDA) for Classification
  # ... (Rest of the QDA code)
  
  # Naive Bayes Classification
  # ... (Rest of the Naive Bayes code)
  
  # Decision Trees
  # ... (Rest of the Decision Trees code)
  
  # Random Forests
  # ... (Rest of the Random Forest code)
  
  # K-Nearest Neighbors (KNN) Classification
  # ... (Rest of the KNN code)
  
  # AdaBoost Classification
  # ... (Rest of the AdaBoost code)
  
  # Calculate and store training and testing errors for each model
  lm_train_errors[iteration] <- lm_train_mse
  lm_test_errors[iteration] <- lm_test_mse
  stepwise_train_errors[iteration] <- stepwise_train_mse
  stepwise_test_errors[iteration] <- stepwise_test_mse
  lda_train_errors[iteration] <- lda_train_error
  lda_test_errors[iteration] <- lda_test_error
  qda_train_errors[iteration] <- qda_train_error
  qda_test_errors[iteration] <- qda_test_error
  nb_train_errors[iteration] <- nb_train_error
  nb_test_errors[iteration] <- nb_test_error
  tm_train_errors[iteration] <- tm_train_error
  tm_test_errors[iteration] <- tm_test_error
  rf_train_errors[iteration] <- rf_train_error
  rf_test_errors[iteration] <- rf_test_error
  knn_train_errors[iteration] <- knn_train_error
  knn_test_errors[iteration] <- knn_test_error
  ada_train_errors[iteration] <- ada_train_error
  ada_test_errors[iteration] <- ada_test_error
}

# Calculate average training and testing errors over all iterations
avg_lm_train_error <- mean(lm_train_errors)
avg_lm_test_error <- mean(lm_test_errors)
avg_stepwise_train_error <- mean(stepwise_train_errors)
avg_stepwise_test_error <- mean(stepwise_test_errors)
avg_lda_train_error <- mean(lda_train_errors)
avg_lda_test_error <- mean(lda_test_errors)
avg_qda_train_error <- mean(qda_train_errors)
avg_qda_test_error <- mean(qda_test_errors)
avg_nb_train_error <- mean(nb_train_errors)
avg_nb_test_error <- mean(nb_test_errors)
avg_tm_train_error <- mean(tm_train_errors)
avg_tm_test_error <- mean(tm_test_errors)
avg_rf_train_error <- mean(rf_train_errors)
avg_rf_test_error <- mean(rf_test_errors)
avg_knn_train_error <- mean(knn_train_errors)
avg_knn_test_error <- mean(knn_test_errors)
avg_ada_train_error <- mean(ada_train_errors)
avg_ada_test_error <- mean(ada_test_errors)

# Print or visualize the average training and testing errors for each model
cat("Linear Regression Training Error (MSE):", avg_lm_train_error, "\n")
cat("Linear Regression Testing Error (MSE):", avg_lm_test_error, "\n")
cat("Stepwise Linear Regression Training Error (MSE):", avg_stepwise_train_error, "\n")
cat("Stepwise Linear Regression Testing Error (MSE):", avg_stepwise_test_error, "\n")
cat("LDA Training Error (Misclassification Rate):", avg_lda_train_error, "\n")
cat("LDA Testing Error (Misclassification Rate):", avg_lda_test_error, "\n")
cat("QDA Training Error (Misclassification Rate):", avg_qda_train_error, "\n")
cat("QDA Testing Error (Misclassification Rate):", avg_qda_test_error, "\n")
cat("Naive Bayes Training Error (Misclassification Rate):", avg_nb_train_error, "\n")
cat("Naive Bayes Testing Error (Misclassification Rate):", avg_nb_test_error, "\n")
cat("Decision Trees Training Error (Misclassification Rate):", avg_tm_train_error, "\n")
cat("Decision Trees Testing Error (Misclassification Rate):", avg_tm_test_error, "\n")
cat("Random Forest Training Error (Misclassification Rate):", avg_rf_train_error, "\n")
cat("Random Forest Testing Error (Misclassification Rate):", avg_rf_test_error, "\n")
cat("KNN Training Error (Misclassification Rate):", avg_knn_train_error, "\n")
cat("KNN Testing Error (Misclassification Rate):", avg_knn_test_error, "\n")
cat("AdaBoost Training Error (Misclassification Rate):", avg_ada_train_error, "\n")
cat("AdaBoost Testing Error (Misclassification Rate):", avg_ada_test_error, "\n")

