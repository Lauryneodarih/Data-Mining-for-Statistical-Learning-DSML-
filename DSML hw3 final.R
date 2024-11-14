# Load necessary libraries
library(ggplot2)
library(gridExtra)
library(MASS)
library(e1071)
library(nnet)
library(class)
### Read the data
Auto1 <- read.table(file = "Auto.csv", sep = ",", header = TRUE)
head(Auto1)

### Do the classification
mpg01 <- as.factor(I(Auto1$mpg >= median(Auto1$mpg)))
Auto <- data.frame(mpg01, Auto1[, -1])  ## replace column "mpg" by "mpg01".

### Graphical analysis
## scatter plot
scat_cylinders <- ggplot(Auto1, aes(y = mpg, x = cylinders)) + geom_point()
scat_displacement <- ggplot(Auto1, aes(y = mpg, x = displacement)) + geom_point()
scat_horsepower <- ggplot(Auto1, aes(y = mpg, x = horsepower)) + geom_point()
scat_weight <- ggplot(Auto1, aes(y = mpg, x = weight)) + geom_point()
scat_acceleration <- ggplot(Auto1, aes(y = mpg, x = acceleration)) + geom_point()
scat_year <- ggplot(Auto1, aes(y = mpg, x = year)) + geom_point()
scat_origin <- ggplot(Auto1, aes(y = mpg, x = origin)) + geom_point()



grid.arrange(scat_cylinders, scat_displacement, scat_horsepower, scat_weight,
             scat_acceleration, scat_year, scat_origin, ncol = 2)

# box plot
box_cylinders <- ggplot(Auto1, aes(y = cylinders, x = mpg01)) + geom_boxplot()
box_displacement <- ggplot(Auto1, aes(y = displacement, x = mpg01)) + geom_boxplot()
box_horsepower <- ggplot(Auto1, aes(y = horsepower, x = mpg01)) + geom_boxplot()
box_weight <- ggplot(Auto1, aes(y = weight, x = mpg01)) + geom_boxplot()
box_acceleration <- ggplot(Auto1, aes(y = acceleration, x = mpg01)) + geom_boxplot()
box_year <- ggplot(Auto1, aes(y = year, x = mpg01)) + geom_boxplot()
box_origin <- ggplot(Auto1, aes(y = origin, x = mpg01)) + geom_boxplot()
grid.arrange(box_cylinders, box_displacement, box_horsepower, box_weight,
             box_acceleration, box_year, box_origin, ncol = 2)

# The correlation table
round(cor(Auto[, -1]), 2)

### scatter plot matrix
pairs(Auto, pch = 10)
# Another scatter plot matrix
pairs.panels(Auto[, -1],
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE, # show density plots
             ellipses = TRUE # show correlation ellipses
)
### cylinders, displacement, horsepower, weight, and origin are the most important variables
Auto2 <- Auto[, c(1:5, 8)]

### Split the data into train and test
n <- dim(Auto2)[1]  ### total number of observations
n1 <- round(n / 10)  ### number of observations randomly selected for testing data
set.seed(19930419)  ### set the random seed
flag <- sort(sample(1:n, n1))
Auto2train <- Auto2[-flag, ]
Auto2test <- Auto2[flag, ]
Auto2train$mpg01 <- as.factor(Auto2train$mpg01)

### (i) LDA
fit1 <- lda(mpg01 ~ ., data = Auto2train)
##
pred1 <- predict(fit1, Auto2train)$class
train_err1 <- mean(pred1 != Auto2train$mpg01)
test_err1 <- mean(predict(fit1, Auto2test)$class != Auto2test$mpg01)

# (ii) QDA
fit2 <- qda(mpg01 ~ ., data = Auto2train)
##
pred2 <- predict(fit2, Auto2train)$class
train_err2 <- mean(pred2 != Auto2train$mpg01)
test_err2 <- mean(predict(fit2, Auto2test)$class != Auto2test$mpg01)

# (iii) Naive Bayes
fit3 <- naiveBayes(mpg01 ~ ., data = Auto2train)
pred3 <- predict(fit3, Auto2train)
train_err3 <- mean(pred3 != Auto2train$mpg01)
test_err3 <- mean(predict(fit3, Auto2test) != Auto2test$mpg01)

# (iv) Logistic Regression
fit4 <- multinom(mpg01 ~ cylinders + displacement + horsepower + weight + origin,
                 data = Auto2train)
summary(fit4)
pred4 <- predict(fit4, Auto2train)
train_err4 <- mean(pred4 != Auto2train$mpg01)
test_err4 <- mean(predict(fit4, Auto2test) != Auto2test$mpg01)

# (v) KNN
k_values <- 1:10
train_errors_knn <- numeric(length(k_values))
test_errors_knn <- numeric(length(k_values))
for (i in 1:length(k_values)) {
  pred5 <- knn(train = Auto2train[, -1], test = Auto2train[, -1], cl = Auto2train$mpg01, k = k_values[i])
  train_errors_knn[i] <- mean(pred5 != Auto2train$mpg01)
  
  pred5_test <- knn(train = Auto2train[, -1], test = Auto2test[, -1], cl = Auto2train$mpg01, k = k_values[i])
  test_errors_knn[i] <- mean(pred5_test != Auto2test$mpg01)
}

# Print training and testing errors for KNN
train_errors_knn
test_errors_knn

# Average performance
B <- 100  # number of loops
TrainErrALL <- NULL
TestErrALL <- NULL  # Final TE values
n <- dim(Auto2)[1]  # total number of observations
n1 <- round(n / 10)  # number of observations randomly selected for testing data
for (b in 1:B) {
  flag <- sort(sample(1:n, n1))
  Auto2train <- Auto[-flag, ]
  Auto2test <- Auto[flag, ]
  Auto2train$mpg01 <- as.factor(Auto2train$mpg01)
  
  # (i) LDA
  fit1 <- lda(mpg01 ~ ., data = Auto2train)
  pred1 <- predict(fit1, Auto2train)$class
  train_err1 <- mean(pred1 != Auto2train$mpg01)
  test_err1 <- mean(predict(fit1, Auto2test)$class != Auto2test$mpg01)
  
  # (ii) QDA
  fit2 <- qda(mpg01 ~ ., data = Auto2train)
  pred2 <- predict(fit2, Auto2train)$class
  train_err2 <- mean(pred2 != Auto2train$mpg01)
  test_err2 <- mean(predict(fit2, Auto2test)$class != Auto2test$mpg01)
  
  # (iii) Naive Bayes
  fit3 <- naiveBayes(mpg01 ~ ., data = Auto2train)
  pred3 <- predict(fit3, Auto2train)
  train_err3 <- mean(pred3 != Auto2train$mpg01)
  test_err3 <- mean(predict(fit3, Auto2test) != Auto2test$mpg01)
  
  # (iv) Logistic Regression
  fit4 <- multinom(mpg01 ~ cylinders + displacement + horsepower + weight + origin,
                   data = Auto2train)
  pred4 <- predict(fit4, Auto2train)
  train_err4 <- mean(pred4 != Auto2train$mpg01)
  test_err4 <- mean(predict(fit4, Auto2test) != Auto2test$mpg01)
  
  # (v) KNN
  pred5 <- knn(train = Auto2train[, -1], test = Auto2train[, -1],
               cl = Auto2train$mpg01, k = 3)
  train_err5 <- mean(pred5 != Auto2train$mpg01)
  test_err5 <- mean(knn(train = Auto2train[, -1], test = Auto2test[, -1],
                        cl = Auto2train$mpg01, k = 3) != Auto2test$mpg01)
  
  TrainErrALL <- rbind(TrainErrALL, c(train_err1, train_err2, train_err3, train_err4, train_err5))
  TestErrALL <- rbind(TestErrALL, c(test_err1, test_err2, test_err3, test_err4, test_err5))
}

# Report the sample mean and sample variances for the five models
round(apply(TrainErrALL, 2, mean), 4)
round(sqrt(apply(TrainErrALL, 2, var)), 4)
round(apply(TestErrALL, 2, mean), 4)
round(sqrt(apply(TestErrALL, 2, var)), 4)

# Compare QDA with others
t.test(TestErrALL[, 1], TestErrALL[, 2], paired = TRUE)
t.test(TestErrALL[, 3], TestErrALL[, 2], paired = TRUE)
t.test(TestErrALL[, 4], TestErrALL[, 2], paired = TRUE)
t.test(TestErrALL[, 5], TestErrALL[, 2], paired = TRUE)

