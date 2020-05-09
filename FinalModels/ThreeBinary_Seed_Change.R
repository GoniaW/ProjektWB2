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
  # Changing Utility factor levels to numeric values
  eucalyptus$Utility <- as.character(eucalyptus$Utility)
  eucalyptus$Utility <- ifelse(eucalyptus$Utility=="low","1",eucalyptus$Utility)
  eucalyptus$Utility <- ifelse(eucalyptus$Utility=="average","2",eucalyptus$Utility)
  eucalyptus$Utility <- ifelse(eucalyptus$Utility=="good","3",eucalyptus$Utility)
  eucalyptus$Utility <- ifelse(eucalyptus$Utility=="best","4",eucalyptus$Utility)
  
  #-----------------------------
  # Splitting into train and test data
  eucalyptus$Utility <- as.factor(eucalyptus$Utility)
  set.seed(seed)
  trainIndex2 <- createDataPartition(eucalyptus$Utility, p = .7, 
                                     list = FALSE, 
                                     times = 1)
  head(trainIndex2)
  write.csv(trainIndex2,'trainIndex2.csv')
  
  train_euc <- eucalyptus[ trainIndex2,]
  test_euc <-  eucalyptus[-trainIndex2,]
  
  # trainIndex <- read.csv('trainIndex.csv')[,2]
  # train_euc <- eucalyptus[ trainIndex,]
  # test_euc <-  eucalyptus[-trainIndex,]
  
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
  # Three binary rpart models
  
  #-----------------------------
  # Self defined useful functions 
  
  convert <- function(dataset, target){
    # Conversion of a dataset to 3 binary-class datasets
    # Arguments:
    # dataset - a dataframe without target variable
    # target - a vector of target labels
    
    tresholds <- c(1,2,3,4)
    tresholds <- tresholds[-length(tresholds)]
    
    labels <- matrix(ncol = 3, nrow = nrow(dataset))
    for(i in 1:nrow(dataset)){
      labels[i,] <- as.logical(as.numeric(target[i]) > (tresholds))
    }
    labels
    list(cbind(dataset, a=labels[,1]), cbind(dataset, b=labels[,2]), cbind(dataset, c=labels[,3]))
  }
  
  calc_prob <- function(responses){
    # Returns predicted classes
    # Arguments:
    # responses - list of vectors of binary-class probabilities
    
    a <- responses[[1]]
    b <- responses[[2]]
    c <- responses[[3]]
    # a,b,c - probabilities tresholds (vectors) from model for classes
    n <- length(a)
    predicts <- numeric(n)
    for(i in 1:n){
      probs <- numeric(4)
      probs[1] <- 1 - a[i]
      probs[2] <- a[i] - b[i]
      probs[3] <- b[i] - c[i]
      probs[4] <- c[i]
      
      predicts[i] <- which.max(probs)
    }
    predicts
  }
  
  #-----------------------------
  # Training the model and predicting on training set 
  
  stack <- convert(train_euc[, -15], train_euc$Utility)
  responses_rpart <- list()
  for(i in 1:3){
    # creates 3 models
    print(i)
    df <- stack[[i]]
    target_name <- colnames(df)[ncol(df)]
    m <- rpart(paste(c(target_name, " ~ ."), collapse = ""), data = df)
    m_pred <- predict(m, test_euc[,-15])
    responses_rpart[[i]] <-  m_pred
  }
  m_pred <- calc_prob(responses_rpart)

  #-----------------------------------------------------------
  # Measuring the outcome
  c(multiclass.roc(m_pred, as.numeric(test_euc$Utility))$auc, mse(m_pred, test_euc$Utility), acc1(m_pred, test_euc$Utility), acc2(m_pred, test_euc$Utility)) 
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