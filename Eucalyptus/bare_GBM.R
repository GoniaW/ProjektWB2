#------------------------------------------------------------------
# GOAL: check AUC for xgboost model without preprocessing
# Result: It's better without preprocessing

#------------------------------------------------------------------
# Libraries
library(mlr)
library(tidyverse)
library(knitr)  
library(OpenML)
library(farff)
library(caret)
library(pROC)
#------------------------------------------------------------------
# Importing data
dataset <- getOMLDataSet(data.name = "BNG(eucalyptus)")
eucalyptus <- dataset$data
head(eucalyptus)
# info about the data can be found under this link: https://www.openml.org/d/188

#------------------------------------------------------------------
# basic preprocessing to be able to compare results
period1 <- eucalyptus[eucalyptus$Year<1984,]
nrow(period1)
eucalyptus <- period1
eucalyptus <- eucalyptus[eucalyptus$Utility!="none",]

eucalyptus <- createDummyFeatures(
  eucalyptus, target = "Utility",
  cols = c("Sp", "Latitude", "Map_Ref", "Locality", "Abbrev")
)
#-----------------------------
# Splitting into train and test data
eucalyptus$Utility <- as.factor(as.character(eucalyptus$Utility))
set.seed(3456)
trainIndex <- createDataPartition(eucalyptus$Utility, p = .7, 
                                  list = FALSE, 
                                  times = 1)
head(trainIndex)
train_euc <- eucalyptus[ trainIndex,]
test_euc <-  eucalyptus[-trainIndex,]
#-----------------------------      

#------------------------------------------------------------------
# Black box model xgboost

trainTask <- makeClassifTask(data = train_euc, target = "Utility")
testTask <- makeClassifTask(data = test_euc, target = "Utility")

set.seed(1)
xgb_learner <- makeLearner(
  "classif.xgboost",
  predict.type = "response",
  par.vals = list(
    objective = "multi:softmax",
    nrounds = 200
  )
)
xgb_model <- mlr::train(xgb_learner, task = trainTask)
result <- predict(xgb_model, testTask)
head(result)

head(result$data) %>%
  kable()

result$data$truth <- as.character(result$data$truth)
result$data$truth <- ifelse(result$data$truth=="low","0",result$data$truth)
result$data$truth <- ifelse(result$data$truth=="average","1",result$data$truth)
result$data$truth <- ifelse(result$data$truth=="good","2",result$data$truth)
result$data$truth <- ifelse(result$data$truth=="best","3",result$data$truth)
result$data$truth <- as.numeric(result$data$truth)

result$data$response <- as.character(result$data$response)
result$data$response <- ifelse(result$data$response=="low","0",result$data$response)
result$data$response <- ifelse(result$data$response=="average","1",result$data$response)
result$data$response <- ifelse(result$data$response=="good","2",result$data$response)
result$data$response <- ifelse(result$data$response=="best","3",result$data$response)
result$data$response <- as.numeric(result$data$response)

multiclass.roc(result$data$truth,result$data$response) # 0.8897 - 0.868 is max on openml
ACC <- mean(ifelse(result$data$truth==result$data$response,1,0))
ACC # 0.7777381
