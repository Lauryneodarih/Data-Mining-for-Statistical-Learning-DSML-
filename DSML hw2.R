### Read the data
fat <- read.table(file = "C:/Users/laury/OneDrive/Documents/fat.csv", sep=",", header=TRUE);
### Split the data as in Part (a)
n = dim(fat)[1]; ### total number of observations
n1 = round(n/10); ### number of observations randomly selected for testing data
## To fix our ideas, let the following 25 rows of data as the testing subset:
flag = c(1, 21, 22, 57, 70, 88, 91, 94, 121, 127, 149, 151, 159, 162,
         164, 177, 179, 194, 206, 214, 215, 221, 240, 241, 243);
fat1train = fat[-flag,];
fat1test = fat[flag,];

summary(fat1train$brozek);
summary(fat1test$brozek);
round(cor(fat1train),2);
hist(fat1train$brozek);
hist(fat1test$brozek);
qqnorm(fat1train$brozek)
qqnorm(fat1test$brozek)
summary(fat1test$weight);

#exploratory data analysis
boxplot(fat1test[,-1]) 
## The true Y response for the testing subset
ytrue <- fat1test$brozek;
length(ytrue)


#building models based on the training data
fat1train

### For each of these 7 models or methods, we fit to the training subset, and then compute its training and testing errors.
## Let us prepare to save all training and testing errors
MSEtrain <- NULL;
MSEtest <- NULL;

### (i) Linear regression with all predictors

model1 <- lm( fat1train$brozek ~ ., data = fat1train);
summary(model1)
## Model1 training error
MSEmod1train <- mean((resid(model1))^2);
MSEtrain <- c(MSEtrain, MSEmod1train);
# Model 1: testing error
pred1a <- predict(model1, fat1test);
MSEmod1test <- mean((pred1a - ytrue)^2);
MSEmod1test;
MSEtest <- c(MSEtest, MSEmod1test);

### (ii) Linear regression with the best subset of k = 5 predictors variables;
library(leaps);
fat.leaps <- regsubsets(fat1train$brozek ~ ., data = fat1train, nbest= 100, really.big=
                          TRUE);

#Record useful information from the output
fat.models <- summary(fat.leaps)$which;
fat.models.size <- as.numeric(attr(fat.models, "dimnames")[[1]]);
fat.models.rss <- summary(fat.leaps)$rss;

## 2a: Plots of all subset models
## and the best subset model for each subset size k
plot(fat.models.size, fat.models.rss);
## find the smallest RSS values for each subset size
fat.models.best.rss <- tapply(fat.models.rss, fat.models.size, min);
## Also add the results for the only intercept model
fat.model0 <- lm( fat1train$brozek ~ 1, data = fat1train);
fat.models.best.rss <- c( sum(resid(fat.model0)^2), fat.models.best.rss);
## plot all RSS for all subset models and highlight the smallest values
plot( 0:8, fat.models.best.rss, type = "b", col= "red", xlab="Subset Size k",
      ylab="Residual Sum-of-Square")
points(fat.models.size, fat.models.rss)

# What is the best subset with k=5
op2 <- which(fat.models.size == 5);
flag2 <- op2[which.min(fat.models.rss[op2])];
flag2
## There are two ways to fit this best subset model with k=5.
## we can auto-find the best subset with k=5
## this is useful when doing cross-validation
mod2selectedmodel <- fat.models[flag2,];
mod2Xname <- paste(names(mod2selectedmodel)[mod2selectedmodel][-1], collapse="+");
mod2form <- paste ("fat1train$brozek ~", mod2Xname);
## To auto-fit the best subset model with k=5 to the data
model2 <- lm( as.formula(mod2form), data= fat1train);
model2

# Model2 training error
MSEmod2train <- mean(resid(model2)^2);
MSEmod2train
MSEtrain <- c(MSEtrain, MSEmod2train);
MSEtrain;
## Model2 testing error
pred2 <- predict(model2, fat1test);
MSEmod2test <- mean((pred2 - ytrue)^2);
#[1] 0.00278621807251166
MSEtest <- c(MSEtest, MSEmod2test);
MSEtest;

## As compared to the full model, the best subset model with K=5
## has a larger training error (0.03146801 vs 0.02930823),
## but has a smaller testing error (0.002786218 vs 0.008755981).

### (iii)Linear regression with variables (stepwise) selected using AIC;

model1 <- lm( fat1train$brozek ~ ., data = fat1train);
model3 <- step(model1);
model3
round(coef(model3),3)
summary(model3)

## Model3 training and testing errors
MSEmod3train <- mean(resid(model3)^2);
MSEmod3train
pred3 <- predict(model3, fat1test);
MSEmod3test <- mean((pred3 - ytrue)^2);
MSEmod3test
MSEtrain <- c(MSEtrain, MSEmod3train);
MSEtrain;
MSEtest <- c(MSEtest, MSEmod3test);
MSEtest;

### (iv) Ridge regression
library(MASS);

## ridge regression for all penality function lamdba
fat.ridge <- lm.ridge( fat1train$brozek ~ ., data = fat1train, lambda=
                         seq(0,100,0.001));

## Ridge Regression plot how the \beta coefficients change with \lambda values
plot(fat.ridge)
### You can use "matplot" to plot the columns of one matrix against the columns of another
matplot(fat.ridge$lambda, t(fat.ridge$coef), type="l", lty=1,
        xlab=expression(lambda), ylab=expression(hat(beta)))

## manually find the optimal lambda value
select(fat.ridge)

# The output suggests that GCV lambda = 0.003

abline(v=0.003, col="red")

# We have to compare the ceofficients of ridge regression with lambda= 0.003
## versus the full linear regression model #1 (i.e., with lambda = 0)
fat.ridge$coef[, which(fat.ridge$lambda == 0.003)]
fat.ridge$coef[, which(fat.ridge$lambda == 0)]

## Auto-find the "index" for the optimal lambda value for Ridge regression
## and auto-compute the corresponding testing and testing error
indexopt <- which.min(fat.ridge$GCV); 
indexopt
## For the estimated \beta, we need to sparate \beta_0 (intercept) with other \beta's
ridge.coeffs = fat.ridge$coef[,indexopt]/ fat.ridge$scales;
intercept = -sum(ridge.coeffs*colMeans(fat1train[,-1]))+ mean(fat1train$brozek);
##the coefficients estimated from the Ridge Regression
## on the original data scale
c(intercept, ridge.coeffs);

## Model4 (Ridge)training errors
yhat4train <- as.matrix( fat1train[,-1]) %*% as.vector(ridge.coeffs) + intercept;
MSEmod4train <- mean((yhat4train - fat1train$brozek)^2);
MSEmod4train
MSEtrain <- c(MSEtrain, MSEmod4train);
MSEtrain
## Model4(Ridge) testing errors in the subset "test"
pred4test <- as.matrix( fat1test[,-1]) %*% as.vector(ridge.coeffs) + intercept;
MSEmod4test <- mean((pred4test - ytrue)^2);
MSEmod4test
MSEtest <- c(MSEtest, MSEmod4test);
MSEtest;

## (v) LASSO
#install.packages("lars")
library(lars)
fat.lars <- lars( as.matrix(fat1train[,-1]), fat1train$brozek, type= "lasso", trace=
                    TRUE);
## plots for LASSO for all penalty parameters \lambda
plot(fat.lars)

## choosing the optimal \lambda value that minimizes Mellon's Cp criterion
Cp1 <- summary(fat.lars)$Cp;
index1 <- which.min(Cp1);

## beta coefficient values (except the intercepts),there are three equivalent ways
## the first two are directly from the lars algorithm
coef(fat.lars)[index1,]
fat.lars$beta[index1,]
## the third way is to get the coefficients via prediction function
lasso.lambda <- fat.lars$lambda[index1]
coef.lars1 <- predict(fat.lars, s=lasso.lambda, type="coef", mode="lambda")

## Get the intercept value for all linear models including LASSO
LASSOintercept = mean(fat1train$brozek) -sum( coef.lars1$coef *
                                                colMeans(fat1train[,-1] ));
c(LASSOintercept, coef.lars1$coef)

## training error for lasso

pred5train <- predict(fat.lars, as.matrix(fat1train[,-1]), s=lasso.lambda, type="fit",
                      mode="lambda");
yhat5train <- pred5train$fit;
MSEmod5train <- mean((yhat5train - fat1train$brozek)^2);
MSEtrain <- c(MSEtrain, MSEmod5train);
MSEmod5train
MSEtrain

## testing error for lasso 
pred5test <- predict(fat.lars, as.matrix(fat1test[,-1]), s=lasso.lambda, type="fit",
                     mode="lambda");
yhat5test <- pred5test$fit;
MSEmod5test <- mean( (yhat5test - fat1test$brozek)^2);
MSEmod5test
MSEtest <- c(MSEtest, MSEmod5test);
MSEtest;


#### (vi) Principal Component Regression (PCR)
##  Manual PCR:
## plots for PCA of training data
trainpca <- prcomp(fat1train[,-1]);

## Examine the square root of eigenvalues
trainpca$sdev
round(trainpca$sdev,2)

### Eigenvectors are in oj$rotation
matplot(1:17, trainpca$rot[,1:3], type ="l", xlab="", ylab="")
matplot(1:17, trainpca$rot[,1:5], type ="l", xlab="", ylab="")

## Choose a number beyond which all e. values are relatively small
plot(trainpca$sdev,type="l", ylab="SD of PC", xlab="PC number")

## An example for doing Regression on the first 4 PCs (DELETE)
modelpca <- lm(fat1train$brozek ~ trainpca$x[,1:4], data = fat1train)

## Note that this is on the PC space (denote by Z), with model Y= Z\gamma +epsilon(DELETE)
## Since the PCs Z= X U for the original data, this yields to
## Y= X (U\gamma) + epsilon,
## which is the form Y=X\beta + epsilon in the original data space
## with \beta = U \gamma.
beta.pca <- trainpca$rot[,1:4] %*% modelpca$coef[-1];

## comparion of \beta for PCA, OLS, Ridge and LASSO (DELETE)
cbind(beta.pca, coef(model1)[-1], ridge.coeffs, coef.lars1$coef)

### Prediciton for PCA
### To do so, we need to first standardize the training or testing data,
xmean <- apply(fat1train[,-1], 2, mean);
xtesttransform <- as.matrix(sweep(fat1test[,-1], 2, xmean));

## New testing data X on the four PCs
xtestPC <- xtesttransform %*% trainpca$rot[,1:4];

## the Predicted Y
ypred6 <- cbind(1, xtestPC) %*% modelpca$coef; 


## auto-run PCR
library(pls)
fat.pca <- pcr(fat1train$brozek~., data=fat1train, validation="CV"); 
## plots to see the effects on the number of PCs
validationplot(fat.pca);
summary(fat.pca);
## The minimum occurs at 17 components
## so for this dataset, we should use full data
##auto-select # of components
## automatically optimazation by PCR based on the cross-validation
ncompopt <- which.min(fat.pca$validation$adj);

## Training Error with the optimal choice of PCs
ypred6train <- predict(fat.pca, ncomp = ncompopt, newdata = fat1train[,-1]);
MSEmod6train <- mean( (ypred6train - fat1train$brozek)^2);
MSEmod6train
MSEtrain <- c(MSEtrain, MSEmod6train);
MSEtrain;
## For this specific example, the optimal # of PC
## ncompopt = 17, which is the full dimension of the original data
## and thus the PCR reduces to the full model!!!

### (vii) Partial Least Squares (PLS) Regression
library(pls)
fat.pls <- plsr(fat1train$brozek ~ ., data = fat1train, validation="CV");
### auto-select the optimal # of components of PLS
mod7ncompopt <- which.min(fat.pls$validation$adj);
## The optomal # of components is 11 for this dataset,
## PLS also reduces to the full model

# Training Error with the optimal, the prediction is from "fat.pls" with "mod7ncompopt"
ypred7train <- predict(fat.pls, ncomp = mod7ncompopt, newdata = fat1train);
MSEmod7train <- mean( (ypred7train - fat1train$brozek)^2);
MSEmod7train
MSEtrain <- c(MSEtrain, MSEmod7train);
MSEtrain;

## Testing Error with the optimal choice
ypred7test <- predict(fat.pls, ncomp = mod7ncompopt, newdata = fat1test[,-1]);
MSEmod7test <- mean( (ypred7test - fat1test$brozek)^2);
MSEmod7test
MSEtest <- c(MSEtest, MSEmod7test);
MSEtest;

MSEtrain
MSEtest
## Testing errors of these 7 models/methods
## For this dataset, PCR and PLS reduce to the full model

### Part (e)
B = 100; ### number of loops
TEALL = NULL; ### Final TE values
set.seed(1234); ### You might want to set the seed for randomization
for (b in 1:B){
  ### randomly select 25 observations as testing data in each loop
  flag <- sort(sample(1:n, n1));
  fattrain <- fat[-flag,];
  fattest <- fat[flag,];
  ### you can write your own R code here to first fit each model to "fattrain"
  ## model 1
  model1 <- lm(fattrain$brozek ~ ., data = fattrain);
  pred1a <- predict(model1, fattest[,-1]);
  te1 <- mean((pred1a - fattest$brozek)^2);
  ## model 2
  library(leaps);
  fat.leaps <- regsubsets(fattrain$brozek ~ ., data = fattrain, nbest= 100, really.big=
                            TRUE);
  ## Record useful information from the output
  fat.models <- summary(fat.leaps)$which;
  fat.models.size <- as.numeric(attr(fat.models, "dimnames")[[1]]);
  fat.models.rss <- summary(fat.leaps)$rss;
  # What is the best subset with k=5
  op2 <- which(fat.models.size == 5);
  flag2 <- op2[which.min(fat.models.rss[op2])];
  mod2selectedmodel <- fat.models[flag2,];
  mod2Xname <- paste(names(mod2selectedmodel)[mod2selectedmodel][-1], collapse="+");
  mod2form <- paste ("fattrain$brozek ~", mod2Xname);
  model2 <- lm( as.formula(mod2form), data= fattrain);
  ## Model2 testing error
  pred2 <- predict(model2, fattest);
  te2 <- mean((pred2 - fattest$brozek)^2);
  ## model3
  model1 <- lm( fattrain$brozek ~ ., data = fattrain);
  model3 <- step(model1);
  pred3 <- predict(model3, fattest);
  te3 <- mean((pred3 - fattest$brozek)^2);
  ## model4
  library(MASS);
  fat.ridge <- lm.ridge( fattrain$brozek ~ ., data = fattrain, lambda=
                           seq(0,100,0.001));
  select(fat.ridge)
  indexopt <- which.min(fat.ridge$GCV); 
  fat.ridge$coef[,indexopt]
  ridge.coeffs = fat.ridge$coef[,indexopt]/ fat.ridge$scales;
  intercept = -sum(ridge.coeffs*colMeans(fattrain[,-1]))+ mean(fattrain$brozek);
  pred4test <- as.matrix( fattest[,-1]) %*% as.vector(ridge.coeffs) + intercept;
  te4 <- mean((pred4test - fattest$brozek)^2);
  ## model5
  library(lars)
  fat.lars <- lars( as.matrix(fattrain[,-1]), fattrain$brozek, type= "lasso", trace=
                      TRUE);
  Cp1 <- summary(fat.lars)$Cp;
  index1 <- which.min(Cp1);
  coef(fat.lars)[index1,]
  fat.lars$beta[index1,]
  lasso.lambda <- fat.lars$lambda[index1]
  ## Model 5: test error for lasso 
  pred5test <- predict(fat.lars, as.matrix(fattest[,-1]), s=lasso.lambda, type="fit",
                       mode="lambda");
  yhat5test <- pred5test$fit;
  te5 <- mean( (yhat5test - fattest$brozek)^2);
  ## Model 6
  library(pls)
  fat.pca <- pcr(fattrain$brozek~., data=fattrain, validation="CV"); 
  ncompopt <- which.min(fat.pca$validation$adj);
  ypred6test <- predict(fat.pca, ncomp = ncompopt, newdata = fattest[,-1]);
  te6 <- mean( (ypred6test - fattest$brozek)^2);
  ## Model7
  fat.pls <- plsr(fattrain$brozek ~ ., data = fattrain, validation="CV");
  mod7ncompopt <- which.min(fat.pls$validation$adj);
  ypred7test <- predict(fat.pls, ncomp = mod7ncompopt, newdata = fattest[,-1]);
  te7 <- mean( (ypred7test - fattest$brozek)^2);
  ### Suppose that you save the TE values for these 7 models as
  ### te1, te2, te3, te4, te5, te6, te7, respectively, within this loop
  ### Then you can save these 7 Testing Error values by using the R code
  ###
  TEALL = rbind( TEALL, cbind(te1, te2, te3, te4, te5, te6, te7) );
}
dim(TEALL); ### This should be a Bx7 matrices
### if you want, you can change the column name of TEALL
colnames(TEALL) <- c("mod1", "mod2", "mod3", "mod4", "mod5", "mod6", "mod7");
## You can report the sample mean and sample variances for the seven models
apply(TEALL, 2, mean);
apply(TEALL, 2, var);

