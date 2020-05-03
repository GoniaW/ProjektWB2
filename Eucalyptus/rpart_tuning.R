#------------------------------------------------------------------
# Data: original (small) dataset

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
dataset <- getOMLDataSet(data.id = 188)
eucalyptus <- dataset$data
head(eucalyptus)
# info about the data can be found under this link: https://www.openml.org/d/188

#------------------------------------------------------------------
# basic preprocessing to be able to compare results
eucalyptus <- eucalyptus[eucalyptus$Utility!="none",]

eucalyptus <- createDummyFeatures(
  eucalyptus, target = "Utility",
  cols = c("Sp", "Latitude", "Map_Ref", "Locality", "Abbrev")
)
#-----------------------------
# Splitting into train and test data
eucalyptus$Utility <- as.factor(as.character(eucalyptus$Utility))
set.seed(123)
trainIndex <- createDataPartition(eucalyptus$Utility, p = .8, 
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

xgb_learner <- makeLearner(
  "classif.xgboost",
  predict.type = "response",
  par.vals = list(
    objective = "multi:softmax",
    nrounds = 100
  )
)
xgb_model <- mlr::train(xgb_learner, task = trainTask)
result <- predict(xgb_model, testTask)
result <- process_result(result)
ACC(result) # 0.6363636
ACC_within1(result) # 0.9818182
ROC(result) # 0.8576

#------------------------------------------------------------------
# Explainable model  - rpart

trainTask <- makeClassifTask(data = train_euc, target = "Utility")
testTask <- makeClassifTask(data = test_euc, target = "Utility")

rpart_learner <- makeLearner(
  "classif.rpart",
  predict.type = "response"
)
rpart_model <- mlr::train(rpart_learner, task = trainTask)
result <- predict(rpart_model, testTask)
result <- process_result(result)
ACC(result) # 0.6090909
ACC_within1(result) # 0.9818182
ROC(result) # 0.8396

#------------------------------------------------------------------
# Explainable model  - rpart
# Tuning hyperparameters

trainTask <- makeClassifTask(data = train_euc, target = "Utility")
testTask <- makeClassifTask(data = test_euc, target = "Utility")

rpart_learner <- makeLearner(
  "classif.rpart",
  predict.type = "response"
)
rpart_model <- mlr::train(rpart_learner, task = trainTask)

ps = makeParamSet(
  makeDiscreteParam("cp", values=c(0.01, 0.1, 1)),
  makeDiscreteParam("minsplit", values=c(5,10,20,30)),
  makeDiscreteParam("maxdepth", values=c(5,10,20,30)),
  makeDiscreteParam("xval", values=c(1,10,50,100))
)

ctrl = makeTuneControlRandom(maxit = 500L)

rdesc = makeResampleDesc("CV", iters = 3L)
res = tuneParams("classif.rpart", task = trainTask, resampling = rdesc,
                 par.set = ps, control = ctrl, measures=mlr::auc)

rpart_learner <- makeLearner(
  "classif.rpart",
  predict.type = "response",
  par.vals = list(
    maxdepth = 5,
    minsplit = 10,
    xval = 20,
    cp = 0.0162
  )
)
rpart_model <- mlr::train(rpart_learner, task = trainTask)
result <- predict(rpart_model, testTask)
result <- process_result(result)

ROC(result) # 0.8307
ACC(result) # 0.6
ACC_within1(result) # 1

rpart.plot(getLearnerModel(rpart_model), roundint=FALSE)

generateFeatureImportanceData(trainTask, learner=rpart_learner, measure=mlr::acc)
