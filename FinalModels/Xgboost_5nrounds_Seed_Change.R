test <- function(seed){
  #------------------------------------------------------------------
  # Loading needed packages
  library(funModeling)
  library(OpenML)
  library(caret)
  library(mlr)
  library(pROC)
  
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
  
  #-----------------------------
  # Splitting into train and test data
  eucalyptus$Utility <- as.factor(as.character(eucalyptus$Utility))
  set.seed(seed)
  trainIndex <- createDataPartition(eucalyptus$Utility, p = .7, 
                                    list = FALSE, 
                                    times = 1)
  head(trainIndex)
  trainIndex2 <- read.csv('trainIndex2.csv')[,2]
  
  train_euc <- eucalyptus[ trainIndex,]
  test_euc <-  eucalyptus[-trainIndex,]
  
  # train_euc <- eucalyptus[ trainIndex2,]
  # test_euc <-  eucalyptus[-trainIndex2,]
  
  #-----------------------------
  # Handling missing data in test set
  status(test_euc) # 10 NA in Surv variablet
  nrow(test_euc)
  test_euc <- na.omit(test_euc)
  nrow(test_euc)
  
  #------------------------------------------------------------------
  # Black box model xgboost
  trainTask <- makeClassifTask(data = train_euc, target = "Utility")
  testTask <- makeClassifTask(data = test_euc, target = "Utility")
  
  xgb_learner <- makeLearner(
    "classif.xgboost",
    predict.type = "response",
    par.vals = list(
      objective = "multi:softmax",
      nrounds = 5
    )
  )
  xgb_learner
  xgb_model <- train(xgb_learner, task = trainTask)
  result <- predict(xgb_model, testTask)
  head(result)
  
  # Changing data type of result from factor to ordered factor
  result$data$truth <- factor(result$data$truth, ordered = TRUE, 
                              levels = c("low", "average", "good", "best"))
  result$data$response <- factor(result$data$response, ordered = TRUE, 
                                 levels = c("low", "average", "good", "best"))
  
  # Measuring the outcome
  c(multiclass.roc(result$data$truth,result$data$response)$auc, mse(result$data$truth,result$data$response), acc1(result$data$truth,result$data$response), acc2(result$data$truth,result$data$response)) 
}
source("../metrics.R")
set.seed(0)
N <- 100
aucs <- numeric(N)
mses <- numeric(N)
accs <- numeric(N)
acc2s <- numeric(N)

seeds <- runif(N, 0, 500)
for(i in 1:N){
  rt <- test(seeds[i])
  aucs[i] <- rt[1]
  mses[i] <- rt[2]
  accs[i] <- rt[3]
  acc2s[i] <- rt[4]
}
c(mean(aucs),mean(mses),mean(accs),mean(acc2s))