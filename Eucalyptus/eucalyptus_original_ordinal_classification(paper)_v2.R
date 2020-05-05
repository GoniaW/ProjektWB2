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

eucalyptus$Utility <- ifelse(eucalyptus$Utility=="low",1,eucalyptus$Utility)
eucalyptus$Utility <- ifelse(eucalyptus$Utility=="average",2,eucalyptus$Utility)
eucalyptus$Utility <- ifelse(eucalyptus$Utility=="good",3,eucalyptus$Utility)
eucalyptus$Utility <- ifelse(eucalyptus$Utility=="best",4,eucalyptus$Utility)
eucalyptus$Utility <- as.numeric(eucalyptus$Utility)

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

#-----------------------------------------------------------
#-----------------------------------------------------------
#-----------------------------------------------------------
#------------ MODEL (FROM PAPER) FUNCTIONS -----------------

mse <- function(x,y){
  mean((as.integer(x)-as.integer(y))^2)
}

acc <- function(x,y){
  mean(as.numeric(x) == as.numeric(y))
}

acc1 <- function(x,y){
  mean(abs((as.numeric(x)-as.numeric(y)) <= 1))
}

#-----------------------------------------------------------

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

#-------------------------------------------------------------------------------------------
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

make_prediction <- function(stack, test_ds, target_test_ind){
  # Predictions for a Ranger model as a function
  # Arguments:
  # stack - output of convert()
  # test_ds - dataframe with test dataset
  
  responses <- list()
  for(i in 1:3){
    # creates 3 models
    print(i)
    df <- stack[[i]]
    target_name <- colnames(df)[ncol(df)]
    target_ind <- ncol(df)
    m <- ranger(paste(c(target_name, " ~ ."), collapse = ""), data = df)
    m_pred <- predict(m, test_ds, type="response")
    print(m_pred)
    responses[[i]] <-  m_pred$predictions
  }
  responses
}

#-----------------------------------------------------------
#-----------------------------------------------------------
#-----------------------------------------------------------
#------------ BUILDING MODELS ------------------------------

#-----------------------------------------------------------
# RPART

stack <- convert(train_euc[, -16], train_euc$Utility)
responses_rpart <- list()
for(i in 1:3){
  # creates 3 models
  print(i)
  df <- stack[[i]]
  target_name <- colnames(df)[ncol(df)]
  m <- rpart(paste(c(target_name, " ~ ."), collapse = ""), data = df)
  m_pred <- predict(m, test_euc[,-16])
  responses_rpart[[i]] <-  m_pred
}
m3_pred <- calc_prob(responses_rpart)

#-----------------------------------------------------------
# Results: AUC
multiclass.roc(test_euc$Utility, m3_pred) # AUC: 0.852
