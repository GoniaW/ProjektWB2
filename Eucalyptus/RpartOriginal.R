#------------------------------------------------------------------
# Importing data
library(OpenML)
library(farff)
dataset <- getOMLDataSet(data.id = 188L)
eucalyptus <- dataset$data
head(eucalyptus)


#------------------------------------------------------------------
# EDA
library(tidyverse)
library(funModeling)
glimpse(eucalyptus)
status(eucalyptus) 
  # latitude should be a numerical instead of factor

freq(eucalyptus)
  # unevenly distributed species, one taking from 1,5% to 9% of all species
  # utlity - target column is unevenly distributed and over 24% is equal to none
  # target column doesn't account for ordinal structure

plot_num(eucalyptus)
  # survival and rainfall are skewed - maybe a transformation?

# DBH - diameter at breast hight should be > 0
eucalyptus[eucalyptus$DBH>0,] # one observation is equal to NA for all variables 


#------------------------------------------------------------------
# Data preprocessing

  #-----------------------------
  # Taking away one observation equal to NA for all columns
eucalyptus <- eucalyptus[!is.na(eucalyptus$DBH),]

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
eucalyptus <- eucalyptus[eucalyptus$Utility!="none",]
nrow(eucalyptus)
  
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

  #-----------------------------
  # One hot encoding on species
library(mlr)
library(tidyverse)
library(knitr)  

train <- train_euc %>%
  mutate(dataset = "train")

test <- test_euc %>%
  mutate(dataset = "test")

combined <- bind_rows(train, test)
summarizeColumns(combined) %>%
  kable(digits = 2)

summarizeColumns(combined) %>%
  kable(digits = 2)

combined <- createDummyFeatures(
  combined, target = "Utility",
  cols = c("Sp","Abbrev","Locality","Map_Ref")
)
summarizeColumns(combined) %>%
  kable(digits = 2)

train <- combined %>%
  filter(dataset == "train") %>%
  select(-dataset)

test <- combined %>%
  filter(dataset == "test") %>%
  select(-dataset)


#------------------------------------------------------------------
# White model rpart

trainTask <- makeClassifTask(data = train, target = "Utility")
testTask <- makeClassifTask(data = test, target = "Utility")

set.seed(1)
rpart_learner <- makeLearner(
  "classif.rpart",
  predict.type = "response"
)
rpart_learner

rpart_model <- train(rpart_learner, task = trainTask)
result <- predict(rpart_model, testTask)
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
multiclass.roc(result$data$truth,result$data$response) # 0.836
ACC <- mean(ifelse(result$data$truth==result$data$response,1,0))
ACC # 0.6024096
