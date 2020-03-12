library(dplyr)
library(readr)
library(caret)
# library(mlr3)
library(MASS)
data <- readr::read_csv("phpTJRsqa.csv")
install.packages("tableone")
install.packages("dlookr")
library(glmnet)

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
features <- data[, -12]
set.seed(999)
train_index <- createDataPartition(data$Class, p=0.8, list=FALSE, times = 1)
levels(data$Class)
train_ds <- data[train_index,]
test_ds <- data[-train_index,]

m1 <- polr(Class ~ ., data = train_ds, Hess=TRUE)
m1_pred <- predict(m1, test_ds)

mse <- function(x,y){
  mean((as.integer(x)-as.integer(y))^2)
}

mse(test_ds$Class, m1_pred)

m2 <- glmnet::glmnet(x = as.matrix(train_ds[, -12]), y = as.matrix(train_ds[,12]), family = "multinomial")

m2_pred <- predict(m2, as.matrix(test_ds[,-12]), type = "class" )

mse(test_ds$Class, m2_pred)
