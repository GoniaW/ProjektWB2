library(dplyr)
library(readr)
library(caret)
# library(mlr3)
library(MASS)
data <- readr::read_csv("phpTJRsqa.csv")
#install.packages("tableone")
#install.packages("dlookr")
library(glmnet)
library(ranger)

"""
Attribute Information:

For more information, read [Cortez et al., 2009].
Input variables (based on physicochemical tests):
1 - fixed acidity
2 - volatile acidity  
3 - citric acid
4 - residual sugar
5 - chlorides
6 - free sulfur dioxide
7 - total sulfur dioxide
8 - density
9 - pH
10 - sulphates
11 - alcohol
Output variable (based on sensory data):
12 - quality (score between 0 and 10)
"""



data <- data[data$Class!=7 & data$Class!=1,]
data$Class <- as.factor(data$Class)
data$Class <- ordered(data$Class)
features <- data[, -12]
set.seed(999)
train_index <- createDataPartition(data$Class, p=0.8, list=FALSE, times = 1)
levels(data$Class)
train_ds <- data[train_index,]
test_ds <- data[-train_index,]

mse <- function(x,y){
  mean((as.integer(x)-as.integer(y))^2)
}

acc <- function(x,y){
  mean(as.numeric(x) == as.numeric(y))
}

acc1 <- function(x,y){
  mean(abs((as.numeric(x)-as.numeric(y)) <= 1))
}

m1 <- polr(Class ~ ., data = train_ds, Hess=TRUE)
m1_pred <- predict(m1, test_ds)

library(nnet)
m2 <- nnet::multinom(Class ~ ., data = train_ds)
#m2 <- glmnet::glmnet(x = as.matrix(train_ds[, -12]), y = as.matrix(train_ds[,12]), family = "multinomial", alpha=0.5)
m2_pred <- predict(m2, test_ds)

mse(test_ds$Class, m1_pred)
mse(test_ds$Class, m2_pred)

acc(test_ds$Class, m1_pred)
acc(test_ds$Class, m2_pred)

#----------------------------------------------------------------------------------------

convert <- function(x){
  tresholds <- as.numeric(levels(train_ds$Class))
  tresholds <- tresholds[-length(tresholds)]
  lables <- matrix(ncol = 4, nrow = nrow(x))
  for(i in 1:nrow(x)){
    lables[i,] <- as.logical(as.numeric(x$Class[i]) > tresholds-1)
  }
  lables
  list(cbind(train_ds[,-12], a=lables[,1]), cbind(train_ds[,-12], b=lables[,2]), cbind(train_ds[,-12], c=lables[,3]), cbind(train_ds[,-12], d=lables[,4]))
}

#-------------------------------------------------------------------------------------------
calc_prob <- function(responses){
  a <- responses[[1]]
  b <- responses[[2]]
  c <- responses[[3]]
  d <- responses[[4]]
  # a,b,c,d - probabilities tresholds (vectors) from model for classes
  n <- length(a)
  predicts <- numeric(n)
  for(i in 1:n){
    probs <- numeric(5)
    probs[1] <- 1 - a[i]
    probs[2] <- a[i] - b[i]
    probs[3] <- b[i] - c[i]
    probs[4] <- c[i] - d[i]
    probs[5] <- d[i]
    
    predicts[i] <- which.max(probs) + 1
  }
  predicts
}

stack <- convert(train_ds)
responses <- list()

for(i in 1:4){
  print(i)
  df <- stack[[i]]
  target_name <- colnames(df)[ncol(df)]
  m <- ranger(paste(c(target_name, " ~ ."), collapse = ""), data = df)
  #glm(paste(c(target_name, " ~ ."), collapse = ""), data = df, family = "binomial")
  #m <- glmnet::glmnet(x = as.matrix(stack[[i]][, -12]), y = as.matrix(stack[[i]][,12]), family = "binomial", alpha=0.5)
  m_pred <- predict(m, test_ds[,-12], type="response")
  responses[[i]] <-  m_pred$predictions
}
m3_pred <- calc_prob(responses)

#----------------------------------------------------------
m4 <- ranger(Class ~ ., train_ds)
m4_pred <- predict(m4, test_ds, type = "response")
m4_pred <- m4_pred$predictions

# ----------------------------------------------------------------

responses_logit <- list()

for(i in 1:4){
  print(i)
  df <- stack[[i]]
  target_name <- colnames(df)[ncol(df)]
  m <- multinom(paste(c(target_name, " ~ ."), collapse = ""), data = df)
  #glm(paste(c(target_name, " ~ ."), collapse = ""), data = df, family = "binomial")
  #m <- glmnet::glmnet(x = as.matrix(stack[[i]][, -12]), y = as.matrix(stack[[i]][,12]), family = "binomial", alpha=0.5)
  m_pred <- predict(m, test_ds[,-12], type="probs")
  responses[[i]] <-  m_pred
}
m5_pred <- calc_prob(responses)

# ----------------------------------------------------------------

mse(test_ds$Class, m1_pred)
mse(test_ds$Class, m2_pred)
mse(test_ds$Class, m3_pred)
mse(test_ds$Class, m4_pred)
mse(test_ds$Class, m5_pred)

acc(test_ds$Class, m1_pred)
acc(test_ds$Class, m2_pred)
acc(test_ds$Class, m3_pred)
acc(test_ds$Class, m4_pred)
acc(test_ds$Class, m5_pred)


acc1(test_ds$Class, m1_pred)
acc1(test_ds$Class, m2_pred)
acc1(test_ds$Class, m3_pred)

