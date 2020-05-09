test <- function(seed){
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
  eucalyptus <- subset(eucalyptus, select = -c(Abbrev, Rep, Locality, Map_Ref))
  nrow(eucalyptus)
  
  #-----------------------------
  # One hot encoding on factor variables
  eucalyptus <- createDummyFeatures(
    eucalyptus, target = "Utility",
    cols = c("Latitude","Sp")
  )
  
  # dummy features makes n columns for n factor values but it should be n-1, so the excess is deleted
  colnames(eucalyptus)
  eucalyptus <- subset(eucalyptus, select = -c(Latitude.82__32, Sp.te))
  eucalyptus$Utility <- as.character(eucalyptus$Utility)
  eucalyptus$Utility <- ifelse(eucalyptus$Utility=="low","1",eucalyptus$Utility)
  eucalyptus$Utility <- ifelse(eucalyptus$Utility=="average","2",eucalyptus$Utility)
  eucalyptus$Utility <- ifelse(eucalyptus$Utility=="good","3",eucalyptus$Utility)
  eucalyptus$Utility <- ifelse(eucalyptus$Utility=="best","4",eucalyptus$Utility)

  
  #-----------------------------
  # Splitting into train and test data
  #eucalyptus$Utility <- as.factor(eucalyptus$Utility)
  set.seed(seed)
  trainIndex2 <- createDataPartition(eucalyptus$Utility, p = .7, 
                                     list = FALSE, 
                                     times = 1)
  eucalyptus$Utility <- as.numeric(eucalyptus$Utility)
  train_euc <- eucalyptus[ trainIndex2,]
  test_euc <-  eucalyptus[-trainIndex2,]
  
  
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
  trainTask <- makeRegrTask(data = train_euc, target = "Utility")
  testTask <- makeRegrTask(data = test_euc, target = "Utility")
  
  set.seed(seed)
  rpart_learner <- makeLearner(
    "regr.rpart",
    predict.type = "response"
  )
  rpart_learner
  
  rpart_model <- train(rpart_learner, task = trainTask)
  result <- predict(rpart_model, testTask)
  head(result)
  
  # Measuring the outcome
  c(multiclass.roc(result$data$truth,result$data$response)$auc, mse(result$data$truth, round(result$data$response,0)), acc1(result$data$truth,round(result$data$response, 0)), acc2(result$data$truth,result$data$response)) 
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