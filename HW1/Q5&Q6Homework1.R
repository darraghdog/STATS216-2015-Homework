library(MASS)
attach(Boston)
library('fBasics')
Minmedv <- Boston[medv == min(medv),]
rownames(Minmedv) <- paste("Min Medv row", rownames(Minmedv))
rbind(Minmedv, as.data.frame(basicStats(Boston))[3:8,])


#### 5. In this exercise, we will predict per capita crime rate by town using 
#### the other variables in the Boston data set.
##### (a) Split the data set into a training set and a test set of approximately 
##### equal size. Include your R code.

library(caret) 
set.seed(32343) 
folds <- createFolds(y=Boston$crim[], k=2, list=TRUE, returnTrain=TRUE)
train <- Boston[folds[[1]],]
test <- Boston[folds[[2]],]
c(nrow(test), nrow(train))
head(train) 

##### (b) Fit a linear model using least squares on the training set, and report
##### the mean training and mean test error obtained.
# Fit a linear model as seen below. 
fit <- lm(crim ~., data=train)

# Calculate out the mean training squared error : 
mean((train$crim - (predict(fit,train)))^2)

# Calculate out the mean test squared error : 
mean((test$crim - (predict(fit,test)))^2)

##### (c) Comment on the results obtained. How accurately can we predict per 
##### capita crime rate by town? What are the most important predictors?

# Given the range of the crime rate, as seen below, in both the training and the 
# test data set, the crime rate can be predicted reasonably accurately. The training 
# set obviously is predicted better than the test set. 

range(test$crim)
range(train$crim)
summary(fit)

#### (6) Using the same setup as in the previous question, form a new outcome 
#### variable Y which equals one if per capita crime rate by town (crim) is greater than 
#### or equal to the overall median and zero otherwise. Fit a logistic regression model to Y
#### , and report the training and test misclassifcation rates and the most important 
#### predictors. Compare the results of this analysis to that of the linear regression 
#### approach in the previous question.

# Create variable Y as decribed in the question 
Y <- ifelse(Boston$crim>=median(Boston$crim), 1, 0)

# Fit the model using only the first fold. Assuming we do not use crim as a predicor. 
glm.fit <- glm(Y[folds[[1]]] ~ . -crim, data=Boston[folds[[1]],])

# Make a prediction, using logistic regression for the whole dataset
pred <- ifelse(predict(glm.fit,Boston)>=median(Boston$crim), 1, 0)

# Produce a confusion matrix of the training misclassifcation rates : 
table(Y[folds[[1]]],pred[folds[[1]]])
# Misclassification error rate on training set
sum(Y[folds[[1]]]!=pred[folds[[1]]])/length(folds[[1]])

# Produce a confusion matrix of the test misclassifcation rates : 
table(Y[folds[[2]]],pred[folds[[2]]])
# Misclassification error rate on training set
sum(Y[folds[[1]]]!=pred[folds[[1]]])/length(folds[[1]])


summary(glm.fit)

# Compare in how many cases the modified output from the linear regression agrees with the output from the logistic regression.
# Make a prediction, using logistic regression for the whole dataset
pred_lm <- ifelse(predict(fit,Boston)>=median(Boston$crim), 1, 0)
# Produce a confusion matrix of the linear model with the training misclassifcation rates : 
table(Y[folds[[1]]],pred_lm[folds[[1]]])
# Misclassification error rate of the linar model with the on training set
sum(Y[folds[[1]]]!=pred_lm[folds[[1]]])/length(folds[[1]])
