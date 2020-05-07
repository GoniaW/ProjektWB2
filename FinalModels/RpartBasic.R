#------------------------------------------------------------------
# Loading needed packages
library(funModeling)
library(OpenML)
library(caret)
library(mlr)
library(pROC)
library(rpart)

#------------------------------------------------------------------
# Importing data
dataset <- getOMLDataSet(data.id = 188L)
eucalyptus <- dataset$data
head(eucalyptus)

#------------------------------------------------------------------
# Data preprocessing

#-----------------------------
# Deleting the rows with the 'none' target class
nrow(eucalyptus)
eucalyptus <- eucalyptus[eucalyptus$Utility!="none",]
nrow(eucalyptus)

#-----------------------------
# One hot encoding on factor variables
eucalyptus <- createDummyFeatures(
  eucalyptus, target = "Utility",
  cols = c("Latitude","Sp","Abbrev","Locality","Map_Ref")
)

# dummy features makes n columns for n factor values but it should be n-1, so the excess is deleted
colnames(eucalyptus)
eucalyptus <- subset(eucalyptus, select = -c(Latitude.82__32, Sp.te, Abbrev.WSh, Locality.Central_Poverty_Bay, 
                                             Map_Ref.N151_922.226))

#-----------------------------
# Splitting into train and test data
eucalyptus$Utility <- as.factor(as.character(eucalyptus$Utility))
set.seed(3456)
trainIndex <- createDataPartition(eucalyptus$Utility, p = .7, 
                                  list = FALSE, 
                                  times = 1)
head(trainIndex)
trainIndex2 <- read.csv('trainIndex.csv')[,2]

train_euc <- eucalyptus[ trainIndex,]
test_euc <-  eucalyptus[-trainIndex,]

#train_euc <- eucalyptus[ trainIndex2,]
#test_euc <-  eucalyptus[-trainIndex2,]

#-----------------------------
# Handling missing data in test set
status(test_euc) # 10 NA in Surv variablet
nrow(test_euc)
test_euc <- na.omit(test_euc)
nrow(test_euc)

#-----------------------------
# Handling missing data in train set
status(train_euc) # 10 NA in Surv variablet
nrow(train_euc)
train_euc <- na.omit(train_euc)
nrow(train_euc)

#------------------------------------------------------------------
# Basic rpart model
trainTask <- makeClassifTask(data = train_euc, target = "Utility")
testTask <- makeClassifTask(data = test_euc, target = "Utility")

set.seed(1)
rpart_learner <- makeLearner(
  "classif.rpart",
  predict.type = "response"
)
rpart_learner

rpart_model <- train(rpart_learner, task = trainTask)
result <- predict(rpart_model, testTask)
head(result)

# Changing data type of result from factor to ordered factor
result$data$truth <- factor(result$data$truth, ordered = TRUE, 
                            levels = c("low", "average", "good", "best"))
result$data$response <- factor(result$data$response, ordered = TRUE, 
                               levels = c("low", "average", "good", "best"))

# Measuring the outcome
multiclass.roc(result$data$truth,result$data$response) # 0.8329
ACC <- mean(ifelse(result$data$truth==result$data$response,1,0))
ACC # 0.5641026
