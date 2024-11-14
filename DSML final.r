```{r} 
library(vtable) 
library(Amelia) 
library(caret) 
library(reshape2) 
library(broom) 
library(randomForest) 
library(gbm) 
library(class) #KDD 
library(MASS) #lda 
library(e1071) #Naive Bayes, SVM 
library(rpart) #single tree 
library(gam) 
library(splitTools) 
library(tidyverse) 
set.seed(123) 
``` 
```{r} 
dat.raw <- read.csv("heart.csv") 
#dat_o2 <- read.csv("o2Saturation.csv") 
``` 
```{r} 
#look for duplicates --> remove 1 
dat <-distinct(dat.raw) 
``` 
```{r} 
### We will now look for missing values 
missmap(dat, main = "Missing values vs observed") 
``` 
```{r} 
summary(dat) 
st(dat) 
``` 
Distribution and Outliers 
```{r} 
#and an onbservation number to data set  
dat <- tibble::rowid_to_column(dat, "obs") 
dat_long <- gather(dat, variable, measurement, age:output, factor_key = TRUE) 
p1 <- dat_long %>% ggplot(aes(variable, measurement)) + 
geom_boxplot() + 
  facet_wrap(~variable, scales="free") + 
  labs(title = "Distribution and Outliers in Heart Attack Dataset", 
       subtitle = "Outliers shown as points outside of the wiskers", 
       xlab = "Variable", 
       ylab = "Value") 
p1 
``` 
 
```{r, fig.height=10, fig.width=8} 
is_outlier <- function(x) { 
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x)) 
} 
 
dat.out <- dat_long %>% 
  group_by(variable) %>% 
  mutate(outlier = ifelse(is_outlier(measurement), measurement, as.numeric(NA))) 
 
p2 <- dat.out %>% ggplot(aes(variable, measurement)) + 
  geom_boxplot() + 
  facet_wrap(~variable, scales="free") + 
  labs(title = "Distribution and Outliers in Heart Attack Dataset", 
       subtitle = "Outliers shown as points outside of the wiskers", 
       xlab = "Variable", 
       ylab = "Value") + 
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3 , size = 3)  
 
p2 
``` 
 
```{r} 
dat.drop <- dat_long %>% 
  group_by(variable) %>% 
  mutate(outlier = ifelse(is_outlier(measurement), measurement, as.numeric(NA))) %>% 
  filter(!is.na(outlier)) #%>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) 
 
dat.drop 
``` 
 
 
```{r} 
#remove  outliers (92) 
dat.keep <- dat_long %>% 
  group_by(variable) %>% 
  mutate(outlier = ifelse(is_outlier(measurement), measurement, as.numeric(NA))) %>% 
  filter(is.na(outlier)) %>% 
  dplyr::select(obs, variable, measurement) 
 
datA <- dat.keep %>% 
  spread(key = variable, value = measurement) %>% 
  drop_na() 
``` 
 
Correlation 
```{r} 
# creating correlation matrix 
corr_mat <- round(cor(dat),2) 
melted_corr_mat <- melt(corr_mat) 
head(melted_corr_mat) 
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2, fill=value)) +  
geom_tile() + 
geom_text(aes(Var2, Var1, label = value),  
color = "black", size = 3) + 
labs(title = "Correlation Between Variable in the Heart Attack Dataset", 
xlab = "Variable 1", 
ylab = "Variable 2") 
``` 
```{r} 
set.seed(123) 
#create ID column 
dat$id <- 1:nrow(dat) 
#use 70% of dataset as training set and 15% as test set and 15% as validation 
# Split data into partitions 
inds <- partition(dat$id, p = c(train = 0.70, valid = 0.15, test = 0.15)) 
train <- dat[inds$train, ] 
valid <- dat[inds$valid, ] 
test <- dat[inds$test, ] 
## Extra the true response value for training and testing data 
y1  <- train$output 
y2  <- valid$output 
y3  <- test$output 
#remove id 
train <- train %>% select(-obs, -id) 
valid <- valid %>% select(-obs, -id) 
test <- test %>% select(-obs, -id) 
dat <- dat %>% select(-obs, -id) 
``` 
############################## 
Variable Selection 
############################## 
```{r} 
#reproducible data to simulate your case 
dat$output <- as.factor(dat$output) 
# capture the columns you want to t.test 
cols_not_out <- names(dat)[-14] 
# turn those column names into formulas 
formulas <- paste(cols_not_out, "~ output") %>% 
map(as.formula) %>% # needs to be class formula 
set_names(cols_not_out) # useful for map_df() 
# do the tests, then stack them all together 
dat.t.test <- map_df(formulas, ~ tidy(t.test(formula = ., data = dat)), 
.id = "column_id") %>% 
select("column_id", "p.value") %>% 
arrange(p.value) 
dat.t.test 
``` 
Which to drop by t-test p-value 
```{r} 
dat.t.test.drop <- dat.t.test %>% 
f
 ilter(p.value >= 0.05) 
dat.t.test.drop 
``` 
Which to keep by t-test p-value 
```{r} 
dat.t.test.keep <- dat.t.test %>% 
f
 ilter(p.value < 0.05) 
dat.t.test.keep 
``` 
############################## 
Models - All Variables 
############################## 
Compare Models 
```{r} 
#Cross Validation 
set.seed(123) 
full <- rbind(train, valid) #rename 
full$age <- as.numeric(full$age) 
n = 253 ## the total sample size 
n1 = 203 # training set sample size 
n2= 48# validation set sample size 
### Initialize the TE values for all models in all $B=100$ loops 
B= 100; ### number of loops 
logreg.error <- list() 
lda.error <- list() 
nb.error <- list() 
gam.error <- list() 
tree.error <- list() 
rf.error <- list() 
boost.error <- list() 
knn.error <- list() 
svm.error <- list() 
################################## 
#A. Logistic regression:  
for (b in 1:B){ 
### randomly select n1 observations as a new training subset in each loop 
f
 lag <- sort(sample(1:n, n1)); 
train.temp <- full[flag,]; ## temp training set for CV 
test.temp <- full[-flag,]; ## temp testing set for CV 
mod0.cv <- step(glm(output ~ ., data = train.temp)) 
pred4.temp <- ifelse(predict(mod0.cv, test[,-14], type="response" ) < 0.5, 0, 1) 
#what percentage of the time do the precited and actual values NOT match 
error.temp <- mean(pred4.temp != test.temp$output) 
logreg.error[b] <- error.temp 
} 
logreg.cv.error <- mean(unlist(logreg.error)) 
logreg.cv.var <- var(unlist(logreg.error)) 
################################## 
#B.Linear Discriminant Analysis :  
#lda 
for (b in 1:B){ 
### randomly select n1 observations as a new training subset in each loop 
f
 lag <- sort(sample(1:n, n1)); 
train.temp <- full[flag,]; ## temp training set for CV 
test.temp <- full[-flag,]; ## temp testing set for CV 
mod1.cv <- lda(train.temp[,-14], train.temp[,14]) 
pred1.temp <- predict(mod1.cv, test.temp[,-14])$class 
#what percentage of the time do the precited and actual values NOT match 
error.temp <- mean(pred1.temp != test.temp$output) 
lda.error[b] <- error.temp 
} 
lda.cv.error <- mean(unlist(lda.error)) 
lda.cv.var <- var(unlist(lda.error)) 
################################## 
## C. Generalized additive model (GAM) with splines:  
for (b in 1:B){ 
### randomly select n1 observations as a new training subset in each loop 
 flag <- sort(sample(1:n, n1)); 
 train.temp <- full[flag,]; ## temp training set for CV 
 test.temp <- full[-flag,]; ## temp testing set for CV 
 
 mod2.cv <-gam(output ~ . + s(age) + s(trtbps) + s(chol) 
            + s(oldpeak) +s(thalachh),  
             family = binomial, data= train.temp, trace=TRUE) 
 pred2.temp <- predict(mod2.cv, test.temp[,-14],type="response") 
 #what percentage of the time do the precited and actual values NOT match 
 error.temp <-  ifelse(pred2.temp <0.5,0,1) 
 gam.error[b] <- sum(error.temp != test.temp$output)/length(test.temp$output) 
} 
 
gam.cv.error <- mean(unlist(gam.error)) 
gam.cv.var <- var(unlist(gam.error)) 
 
 
################################## 
## D. Naive Bayes (with full X).  
#naive bayes 
for (b in 1:B){ 
### randomly select n1 observations as a new training subset in each loop 
 flag <- sort(sample(1:n, n1)); 
 train.temp <- full[flag,]; ## temp training set for CV 
 test.temp <- full[-flag,]; ## temp testing set for CV 
 
 mod3.cv <- naiveBayes(train.temp[,-14], train.temp[,14]) 
 pred3.temp <- predict(mod3.cv, test.temp[,-14]) 
 #what percentage of the time do the precited and actual values NOT match 
 error.temp <- mean(pred3.temp != test.temp$output) 
 nb.error[b] <- error.temp 
} 
 
nb.cv.error <- mean(unlist(nb.error)) 
nb.cv.var <- var(unlist(nb.error)) 
 
################################## 
#E: a single Tree:  
for (b in 1:B){ 
### randomly select n1 observations as a new training subset in each loop 
  flag <- sort(sample(1:n, n1)); 
  train.temp <- full[flag,]; ## temp training set for CV 
  test.temp <- full[-flag,]; ## temp testing set for CV 
 
  mod4.cv <-  rpart(output ~ .,data=train.temp, method="class",  
                     parms=list(split="gini")) 
  opt2 <- which.min(mod4.cv$cptable[, "xerror"])  
  cp1 <- mod4.cv$cptable[opt2, "CP"] 
  modE <- prune(mod4.cv,cp=cp1) 
 
  pred4.temp <- predict(modE, test.temp[,-14],type="class") 
 #what percentage of the time do the precited and actual values NOT match 
  error.temp <- mean(pred4.temp != test.temp$output) 
  tree.error[b] <- error.temp 
} 
 
tree.cv.error <- mean(unlist(tree.error)) 
tree.cv.var <- var(unlist(tree.error)) 
 
 
################################## 
#F Random Forest 
for (b in 1:B){ 
### randomly select n1 observations as a new training subset in each loop 
  flag <- sort(sample(1:n, n1)); 
  train.temp <- full[flag,]; ## temp training set for CV 
  test.temp <- full[-flag,]; ## temp testing set for CV 
 
 
  mod5.cv <-  randomForest(as.factor(output) ~., data=train.temp, ntree= 2000,  
                    mtry=2, nodesize =2, importance=TRUE) 
   
 
  pred5.temp <- predict(mod5.cv, test.temp,type="class") 
 #what percentage of the time do the precited and actual values NOT match 
  error.temp <- mean(pred5.temp != test.temp$output) 
  rf.error[b] <- error.temp 
} 
 
rf.cv.error <- mean(unlist(rf.error)) 
rf.cv.var <- var(unlist(rf.error)) 
 
 
################################## 
#G Boosting 
for (b in 1:B){ 
### randomly select n1 observations as a new training subset in each loop 
  flag <- sort(sample(1:n, n1)); 
  train.temp <- full[flag,]; ## temp training set for CV 
  test.temp <- full[-flag,]; ## temp testing set for CV 
   
  mod6 <- gbm(output ~ .,data=train, 
                 distribution = 'bernoulli', 
                 n.trees = 6000,  
                 shrinkage = 0.01,  
                 interaction.depth = 3, 
                 n.minobsinnode = 15,  
                 cv.folds = 10) 
 
 
  perf_gbm1 <- gbm.perf(mod6, method="cv")  
   
  pred6.temp <- ifelse(predict(mod6, newdata = test.temp[,-14], n.trees=perf_gbm1, type="response") < 0.5, 0, 1) 
   
  error.temp <- mean(pred6.temp != test.temp$output) 
  boost.error[b] <- error.temp 
} 
 
boost.cv.error <- mean(unlist(boost.error)) 
boost.cv.var <- var(unlist(boost.error)) 
################################## 
#H KNN 
for (b in 1:B){ 
### randomly select n1 observations as a new training subset in each loop 
f
 lag <- sort(sample(1:n, n1)); 
train.temp <- full[flag,]; ## temp training set for CV 
test.temp <- full[-flag,]; ## temp testing set for CV 
train_scaled.temp = scale(train.temp[-14]) 
test_scaled.temp = scale(test.temp[-14]) 
mod7.temp <- knn(train_scaled.temp, test_scaled.temp, cl=train.temp$output, k=2) 
error.temp <- mean(mod7.temp != test.temp$output) 
knn.error[b] <- error.temp 
} 
knn.cv.error <- mean(unlist(knn.error)) 
knn.cv.var <- var(unlist(knn.error)) 
################################## 
#I SVM 
#https://uc-r.github.io/svm 
for (b in 1:B){ 
### randomly select n1 observations as a new training subset in each loop 
f
 lag <- sort(sample(1:n, n1)); 
train.temp <- full[flag,]; ## temp training set for CV 
test.temp <- full[-flag,]; ## temp testing set for CV 
train.temp$output <- as.factor(train.temp$output) 
test.temp$output <- as.factor(test.temp$output) 
#Per default, data are scaled internally (both x and y variables) to zero mean and unit variance. 
mod8.temp.fit <- svm(output ~ ., data= train.temp) 
pred8.temp <- predict(mod8.temp.fit, test.temp) 
error.temp <- mean(pred8.temp != test.temp$output) 
svm.error[b] <- error.temp 
} 
svm.cv.error <- mean(unlist(svm.error)) 
svm.cv.var <- var(unlist(svm.error)) 
################################## 
#put results in dataframe 
all.cv.error <- bind_cols(rf.cv.error, boost.cv.error, logreg.cv.error, lda.cv.error, nb.cv.error, gam.cv.error, tree.cv.error, 
knn.cv.error, svm.cv.error) 
all.cv.var <- bind_cols(rf.cv.var, boost.cv.var, logreg.cv.var, lda.cv.var, nb.cv.var, gam.cv.var, tree.cv.var, knn.cv.var, 
svm.cv.var) 
CV_Error_Mean <- t(all.cv.error) 
CV_Error_Var <- t(all.cv.var) 
Model <- c("Random Forest", "Boosting", "Logistic Regression", "LDA", "Naive Bayes", "GAM","Single Tree", "KNN; K=2", 
"SVM") 
df_all <- data.frame(Model, CV_Error_Mean, CV_Error_Var) %>% 
arrange(CV_Error_Mean) 
df_all 
``` 
```{r, fig.width=8} 
df_all %>% ggplot(aes(x=reorder(Model, CV_Error_Mean), CV_Error_Mean, group = Model, fill = Model)) + 
geom_col(aes(fill = Model))+ 
geom_errorbar(data = df_all, aes(ymin = CV_Error_Mean-CV_Error_Var, 
ymax = CV_Error_Mean + CV_Error_Var)) + 
labs(title = "Cross Validation Erorr Rate for Classification Models For Heart Attack Dataset", 
subtitle = "All Variables Included", 
x = "Model", 
y = "Mean Cross Validation Error") + 
theme(axis.text.x=element_text(angle=45, hjust=1)) + 
scale_fill_discrete(limits = df_all$Model) + 
#theme_dark() + 
scale_fill_brewer(palette="Paired") 
``` 
############################## 
Models - Reduced Variables 
############################## 
chol   
f
 bs 
```{r} 
#remove non-significant t-test variables 
train.r <- train %>% 
select(-chol, -fbs) 
valid.r <- valid %>% 
select(-chol, -fbs) 
``` 
A comparison with other methods 
```{r} 
#Cross Validation 
set.seed(123) ### set the seed for randomization 
full.r <- rbind(train.r, valid.r) #rename 
full.r$age <- as.numeric(full.r$age) 
n = 253 ## the total sample size 
n1 = 203 # training set sample size 
n2= 48# testing set sample size 
### Initialize the TE values for all models in all $B=100$ loops 
B= 100; ### number of loops 
logreg.error.r <- list() 
lda.error.r <- list() 
nb.error.r <- list() 
gam.error.r <- list() 
tree.error.r <- list() 
rf.error.r <- list() 
boost.error.r <- list() 
knn.error.r <- list() 
svm.error.r <- list() 
################################## 
#A. Logistic regression:  
for (b in 1:B){ 
### randomly select n1 observations as a new training subset in each loop 
f
 lag <- sort(sample(1:n, n1)); 
train.temp <- full.r[flag,]; ## temp training set for CV 
test.temp <- full.r[-flag,]; ## temp testing set for CV 
mod0.cv <- step(glm(output ~ ., data = train.temp)) 
pred4.temp <- ifelse(predict(mod0.cv, test[,-14], type="response" ) < 0.5, 0, 1) 
#what percentage of the time do the precited and actual values NOT match 
error.temp <- mean(pred4.temp != test.temp$output) 
logreg.error.r[b] <- error.temp 
} 
logreg.cv.error.r <- mean(unlist(logreg.error.r)) 
logreg.cv.var.r <- var(unlist(logreg.error.r)) 
################################## 
#B.Linear Discriminant Analysis :  
#lda 
for (b in 1:B){ 
### randomly select n1 observations as a new training subset in each loop 
f
 lag <- sort(sample(1:n, n1)); 
train.temp <- full.r[flag,]; ## temp training set for CV 
test.temp <- full.r[-flag,]; ## temp testing set for CV 
mod1.cv <- lda(train.temp[,-12], train.temp[,12]) 
pred1.temp <- predict(mod1.cv, test.temp[,-12])$class 
#what percentage of the time do the precited and actual values NOT match 
error.temp <- mean(pred1.temp != test.temp$output) 
lda.error.r[b] <- error.temp 
} 
lda.cv.error.r <- mean(unlist(lda.error.r)) 
lda.cv.var.r <- var(unlist(lda.error.r)) 
################################## 
## C. Generalized additive model (GAM) with splines:  
for (b in 1:B){ 
### randomly select n1 observations as a new training subset in each loop 
f
 lag <- sort(sample(1:n, n1)); 
train.temp <- full.r[flag,]; ## temp training set for CV 
test.temp <- full.r[-flag,]; ## temp testing set for CV 
mod2.cv <-gam(output ~ . + s(age) + s(trtbps) 
+ s(oldpeak) +s(thalachh),  
family = binomial, data= train.temp, trace=TRUE) 
pred2.temp <- predict(mod2.cv, test.temp[,-12],type="response") 
#what percentage of the time do the precited and actual values NOT match 
error.temp <-  ifelse(pred2.temp <0.5,0,1) 
gam.error.r[b] <- sum(error.temp != test.temp$output)/length(test.temp$output) 
} 
gam.cv.error.r <- mean(unlist(gam.error.r)) 
gam.cv.var.r <- var(unlist(gam.error.r)) 
################################## 
## D. Naive Bayes (with full.r X).  
#naive bayes 
for (b in 1:B){ 
### randomly select n1 observations as a new training subset in each loop 
f
 lag <- sort(sample(1:n, n1)); 
train.temp <- full.r[flag,]; ## temp training set for CV 
test.temp <- full.r[-flag,]; ## temp testing set for CV 
mod3.cv <- naiveBayes(train.temp[,-12], train.temp[,12]) 
pred3.temp <- predict(mod3.cv, test.temp[,-12]) 
#what percentage of the time do the precited and actual values NOT match 
error.temp <- mean(pred3.temp != test.temp$output) 
nb.error.r[b] <- error.temp 
} 
nb.cv.error.r <- mean(unlist(nb.error.r)) 
nb.cv.var.r <- var(unlist(nb.error.r)) 
################################## 
#E: a single Tree:  
for (b in 1:B){ 
### randomly select n1 observations as a new training subset in each loop 
f
 lag <- sort(sample(1:n, n1)); 
train.temp <- full.r[flag,]; ## temp training set for CV 
test.temp <- full.r[-flag,]; ## temp testing set for CV 
mod4.cv <-  rpart(output ~ .,data=train.temp, method="class",  
                     parms=list(split="gini")) 
  opt2 <- which.min(mod4.cv$cptable[, "xerror"])  
  cp1 <- mod4.cv$cptable[opt2, "CP"] 
  modE <- prune(mod4.cv,cp=cp1) 
 
  pred4.temp <- predict(modE, test.temp[,-14],type="class") 
 #what percentage of the time do the precited and actual values NOT match 
  error.temp <- mean(pred4.temp != test.temp$output) 
  tree.error.r[b] <- error.temp 
} 
 
tree.cv.error.r <- mean(unlist(tree.error.r)) 
tree.cv.var.r <- var(unlist(tree.error.r)) 
 
 
################################## 
#F Random Forest 
for (b in 1:B){ 
### randomly select n1 observations as a new training subset in each loop 
  flag <- sort(sample(1:n, n1)); 
  train.temp <- full.r[flag,]; ## temp training set for CV 
  test.temp <- full.r[-flag,]; ## temp testing set for CV 
 
 
  mod5.cv <-  randomForest(as.factor(output) ~., data=train.temp, ntree= 2000,  
                    mtry=2, nodesize =2, importance=TRUE) 
   
 
  pred5.temp <- predict(mod5.cv, test.temp,type="class") 
 #what percentage of the time do the precited and actual values NOT match 
  error.temp <- mean(pred5.temp != test.temp$output) 
  rf.error.r[b] <- error.temp 
} 
 
rf.cv.error.r <- mean(unlist(rf.error.r)) 
rf.cv.var.r <- var(unlist(rf.error.r)) 
 
 
################################## 
#G Boosting 
for (b in 1:B){ 
### randomly select n1 observations as a new training subset in each loop 
  flag <- sort(sample(1:n, n1)); 
  train.temp <- full.r[flag,]; ## temp training set for CV 
  test.temp <- full.r[-flag,]; ## temp testing set for CV 
   
  mod6 <- gbm(output ~ .,data=train.temp, 
                 distribution = 'bernoulli', 
                 n.trees = 6000,  
                 shrinkage = 0.01,  
                 interaction.depth = 3, 
                 n.minobsinnode = 15,  
                 cv.folds = 10) 
 
perf_gbm1 <- gbm.perf(mod6, method="cv")  
pred6.temp <- ifelse(predict(mod6, newdata = test.temp[,-12], n.trees=perf_gbm1, type="response") < 0.5, 0, 1) 
error.temp <- mean(pred6.temp != test.temp$output) 
boost.error.r[b] <- error.temp 
} 
boost.cv.error.r <- mean(unlist(boost.error.r)) 
boost.cv.var.r <- var(unlist(boost.error.r)) 
################################## 
#H KNN 
for (b in 1:B){ 
### randomly select n1 observations as a new training subset in each loop 
f
 lag <- sort(sample(1:n, n1)); 
train.temp <- full.r[flag,]; ## temp training set for CV 
test.temp <- full.r[-flag,]; ## temp testing set for CV 
train_scaled.temp = scale(train.temp[-12]) 
test_scaled.temp = scale(test.temp[-12]) 
mod7.temp <- knn(train_scaled.temp, test_scaled.temp, cl=train.temp$output, k=2) 
error.temp <- mean(mod7.temp != test.temp$output) 
knn.error.r[b] <- error.temp 
} 
knn.cv.error.r <- mean(unlist(knn.error.r)) 
knn.cv.var.r <- var(unlist(knn.error.r)) 
################################## 
#I SVM 
#https://uc-r.github.io/svm 
for (b in 1:B){ 
### randomly select n1 observations as a new training subset in each loop 
f
 lag <- sort(sample(1:n, n1)); 
train.temp <- full.r[flag,]; ## temp training set for CV 
test.temp <- full.r[-flag,]; ## temp testing set for CV 
train.temp$output <- as.factor(train.temp$output) 
test.temp$output <- as.factor(test.temp$output) 
#Per default, data are scaled internally (both x and y variables) to zero mean and unit variance. 
mod8.temp.fit <- svm(output ~ ., data= train.temp) 
pred8.temp <- predict(mod8.temp.fit, test.temp) 
error.temp <- mean(pred8.temp != test.temp$output) 
svm.error.r[b] <- error.temp 
} 
svm.cv.error.r <- mean(unlist(svm.error.r)) 
svm.cv.var.r <- var(unlist(svm.error.r)) 
################################## 
#put results in dataframe 
all.cv.error.r <- bind_cols(rf.cv.error.r, boost.cv.error.r, logreg.cv.error.r, lda.cv.error.r, nb.cv.error.r, gam.cv.error.r, 
tree.cv.error.r, knn.cv.error.r, svm.cv.error.r) 
all.cv.var.r <- bind_cols(rf.cv.var.r, boost.cv.var.r, logreg.cv.var.r, lda.cv.var.r, nb.cv.var.r, gam.cv.var.r, tree.cv.var.r, knn.cv.var.r, 
svm.cv.var.r) 
CV_Error_Mean.r <- t(all.cv.error.r) 
CV_Error_Var.r <- t(all.cv.var.r) 
Model <- c("Random Forest", "Boosting", "Logistic Regression", "LDA", "Naive Bayes", "GAM","Single Tree", "KNN; K=2", 
"SVM") 
df_all.r <- data.frame(Model, CV_Error_Mean.r, CV_Error_Var.r) %>% 
arrange(CV_Error_Mean.r) 
df_all.r 
``` 
```{r, fig.width=8} 
df_all.r %>% ggplot(aes(x=reorder(Model, CV_Error_Mean.r), CV_Error_Mean.r, group = Model, fill = Model)) + 
geom_col(aes(fill = Model))+ 
geom_errorbar(data = df_all.r, aes(ymin = CV_Error_Mean.r-CV_Error_Var.r, 
ymax = CV_Error_Mean.r + CV_Error_Var.r)) + 
labs(title = "Cross Validation Erorr Rate for Classification Models For Heart Attack Dataset", 
subtitle = "Reduced Variable Set", 
x = "Model", 
y = "Mean Cross Validation Error") + 
theme(axis.text.x=element_text(angle=45, hjust=1)) + 
scale_fill_discrete(limits = df_all.r$Model) + 
scale_fill_brewer(palette="Paired") 
``` 
############################## 
PARAMETER OPTIMIZATION 
############################## 
Boosting Grid Search 
```{r} 
# refined hyperparameter grid 
# search grid 
hyper_grid <- expand.grid( 
  n.trees = 6000, 
  shrinkage = 0.01, 
  interaction.depth = c(3, 5, 7), 
  n.minobsinnode = c(5, 10, 15) 
) 
 
# create model fit function 
model_fit <- function(n.trees, shrinkage, interaction.depth, n.minobsinnode) { 
  set.seed(123) 
   
 m <- gbm(output ~ ., 
          data=train, 
          distribution = 'bernoulli', 
          n.trees = n.trees, 
          shrinkage = shrinkage, 
          interaction.depth = interaction.depth, 
          n.minobsinnode = n.minobsinnode, 
          cv.folds = 10 
  ) 
  # compute RMSE 
  sqrt(min(m$cv.error)) 
} 
 
# perform search grid with functional programming 
hyper_grid$rmse <- purrr::pmap_dbl( 
  hyper_grid, 
  ~ model_fit( 
    n.trees = ..1, 
    shrinkage = ..2, 
    interaction.depth = ..3, 
    n.minobsinnode = ..4 
    ) 
) 
 
# results 
boost.grid.results <- arrange(hyper_grid, rmse) 
write.csv(boost.grid.results, "boost_grid_results.csv") 
 
boost.grid.results 
 
 
``` 
 
```{r} 
# Make Prediction 
## use "predict" to find the training or testing error 
 
gbm.2 <- gbm(output ~ .,data=train, 
                 distribution = 'bernoulli', 
                 n.trees = boost.grid.results[1,]$n.trees,  
                 shrinkage = boost.grid.results[1,]$shrinkage,  
                 interaction.depth = boost.grid.results[1,]$interaction.depth, 
                 n.minobsinnode = boost.grid.results[1,]$n.minobsinnode,  
                 cv.folds = 10) 
## Training error 
pred2gbm <- predict(gbm.2, newdata = train, type="response") 
#pred1gbm[1:10] 
y1hatB <- ifelse(pred2gbm < 0.5, 0, 1) 
#y1hat[1:10] 
boost.tr.error <- sum(y1hatB != y1)/length(y1)  ##Training error = 
boost.tr.error 
``` 
```{r} 
## Testing Error 
y3hat <- ifelse(predict(gbm.2, newdata = test[,-14], type="response") < 0.5, 0, 1) 
boost.tst.error <- mean(y3hat != y3) ## Testing error   
boost.tst.error 
``` 
```{r} 
#Boosting confusion matrix 
table(y3hat, y3) 
``` 
```{r, fig.height = 11} 
## summary model 
## Which variances are important 
summary(gbm.2) 
```