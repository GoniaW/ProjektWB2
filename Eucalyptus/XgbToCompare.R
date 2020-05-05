library(dplyr)
library(readr)
library(caret)
library(MASS)
library(mlr)
library(tidyverse)
library(knitr)  
library(nnet)
library(pROC)
library(rpart)


#------------------------------------------------------------------
# Importing data
library(OpenML)
dataset <- getOMLDataSet(188)
eucalyptus <- dataset$data
head(eucalyptus)

#-----------------------------------------------------------
#-----------------------------------------------------------
#-----------------------------------------------------------
#------------ DATA PREPROCESSING --------------------------- 

#-----------------------------
# Changing type of latitude
class(eucalyptus$Latitude)
eucalyptus$Latitude <- as.character(eucalyptus$Latitude)
head(eucalyptus$Latitude)

eucalyptus$Latitude <- as.numeric(substr(eucalyptus$Latitude,1,2)) +
  (as.numeric(substr(eucalyptus$Latitude,5,6))/60)
class(eucalyptus$Latitude)
unique(eucalyptus$Latitude)
nrow(eucalyptus[eucalyptus$Latitude>80,c(3,5)]) # <1% of data
# Some destinations have latitude of 82 degrees
# but the real latitude of these destinations is around 40 degrees
# this is a good reason to delete these rows
eucalyptus <- eucalyptus[eucalyptus$Latitude<80,]

#-----------------------------
# Deleting the rows with the 'none' target class
nrow(eucalyptus)
eucalyptus$Utility <- as.character(eucalyptus$Utility)
eucalyptus <- eucalyptus[eucalyptus$Utility!="none",]
nrow(eucalyptus)

#-----------------------------
# Handling missing data
nrow(eucalyptus)
eucalyptus <- na.omit(eucalyptus)
nrow(eucalyptus)

#-----------------------------
# One hot encoding
eucalyptus <- createDummyFeatures(
  eucalyptus, target = "Utility",
  cols = c("Sp","Abbrev","Locality","Map_Ref")
)

# dummy features makes n columns for n factor values but it should be n-1, so the excess is deleted
colnames(eucalyptus)
eucalyptus <- subset(eucalyptus, select = -c(Sp.te, Abbrev.WSh, Locality.Central_Poverty_Bay, 
                                             Map_Ref.N151_922.226))

#-----------------------------
# Splitting into train and test data
eucalyptus$Utility <- as.factor(as.character(eucalyptus$Utility))
library(caret)
set.seed(3456)
trainIndex <- createDataPartition(eucalyptus$Utility, p = .7, 
                                  list = FALSE, 
                                  times = 1)
head(trainIndex)
train_euc <- eucalyptus[ trainIndex,]
test_euc <-  eucalyptus[-trainIndex,]


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
xgb_model <- train(xgb_learner, task = trainTask)
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

library(pROC)
multiclass.roc(result$data$truth,result$data$response) # 0.8376
ACC <- mean(ifelse(result$data$truth==result$data$response,1,0))
ACC # 0.5796178
