#------------------------------------------------------------------
# Importing data
library(OpenML)
library(farff)
dataset <- getOMLDataSet(data.name = "BNG(eucalyptus)")
eucalyptus <- dataset$data
head(eucalyptus)
 # info about the data can be found under this link: https://www.openml.org/d/188

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
  # frosts have only 2 values: -2degC and -3degC - not much of difference
  # DBH is best diameter base height- is this diameter at the base of eucalyptus?
    # if so it should not be equal to 100meters or to negative numbers and it does
  # data comes from 2 periods, method of data collection changed over time
    # maybe choose one of the periods?

# Frosts
dataset$colnames.new
deg_2 <- eucalyptus[eucalyptus$Frosts==-2,c(8,20)]
deg_3 <- eucalyptus[eucalyptus$Frosts==-3,c(8,20)]
deg_2 <- deg_2[deg_2$Utility!="none",]
deg_3 <- deg_3[deg_3$Utility!="none",]
freq(deg_2) # around 65% are >= good
freq(deg_3) # around 50% are >= good
  # Conclusion: Frosts influences target column so it makes sense to keep it

# DBH - diameter at breast hight
 # it should not be smaller than 0
nrow(eucalyptus[eucalyptus$DBH<0,])/nrow(eucalyptus) # 9% is smaller than 0 
  # that's because the dataset is artificially generated based on other smaller data

#------------------------------------------------------------------
# Data preprocessing

  #-----------------------------
  # Changing type of latitude
class(eucalyptus$Latitude)
eucalyptus$Latitude <- as.character(eucalyptus$Latitude)
head(eucalyptus$Latitude)
class(eucalyptus$Latitude)
#test <- as.numeric(substr(eucalyptus$Latitude,1,2)) + (as.numeric(substr(eucalyptus$Latitude,5,6))/60)
#unique(test)
#unique(eucalyptus$Latitude) # looks legit
eucalyptus$Latitude <- as.numeric(substr(eucalyptus$Latitude,1,2)) +
  (as.numeric(substr(eucalyptus$Latitude,5,6))/60)
class(eucalyptus$Latitude)
unique(eucalyptus$Latitude)
nrow(eucalyptus[eucalyptus$Latitude>80,c(3,5)]) # <4% of data
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
  # Deleting name of sites - the aim is to tell which eucaliptus will grow
    # the best in new places, not old ones
dataset$colnames.new
head(eucalyptus)
      # first 3 are related only to site names
eucalyptus <- eucalyptus[,4:20]
head(eucalyptus)
      # map location in the North Island is not longitude nor latitude and it will 
        # not help with futre identification of which eucaliptus is the best
eucalyptus <- eucalyptus[,2:17]
head(eucalyptus)

  #-----------------------------
  # Deleting data with diameter < 0
eucalyptus <- eucalyptus[eucalyptus$DBH>=0,]
  
  #-----------------------------
  # Splitting data by the periods of data collection
unique(eucalyptus$Year)
class(eucalyptus$Year)
period1 <- eucalyptus[eucalyptus$Year<1984,]
period2 <- eucalyptus[eucalyptus$Year>1984,]
nrow(period1) # more than half of the original dataset
nrow(period2)
    # choose period1 for dataset

eucalyptus <- period1

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
  # Normalizing features, one hot encoding on species
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

combined <- normalizeFeatures(combined, target = "Utility")

summarizeColumns(combined) %>%
  kable(digits = 2)

combined <- createDummyFeatures(
  combined, target = "Utility",
  cols = "Sp"
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
# Black box model xgboost

trainTask <- makeClassifTask(data = train, target = "Utility")
testTask <- makeClassifTask(data = test, target = "Utility")

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

multiclass.roc(result$data$truth,result$data$response) # 0.8755  - 0.868 is max on openml
ACC <- mean(ifelse(result$data$truth==result$data$response,1,0))
ACC # 0.7528746 


#------------------------------------------------------------------
# White model - linear regression on Utility = 0,1,2,3

 # Changing target classes to 0,1,2,3
regr_set <- combined
unique(regr_set$Utility)
regr_set$Utility <- as.character(regr_set$Utility)
regr_set$Utility <- ifelse(regr_set$Utility=="low","0",regr_set$Utility)
regr_set$Utility <- ifelse(regr_set$Utility=="average","1",regr_set$Utility)
regr_set$Utility <- ifelse(regr_set$Utility=="good","2",regr_set$Utility)
regr_set$Utility <- ifelse(regr_set$Utility=="best","3",regr_set$Utility)
regr_set$Utility <- as.numeric(regr_set$Utility)
unique(regr_set$Utility)

train <- regr_set %>%
  filter(dataset == "train") %>%
  select(-dataset)

test <- regr_set %>%
  filter(dataset == "test") %>%
  select(-dataset)

  # deleting Sp.te - this is the problem with dummy coefficient definition
  # this is completely dependent on other Sp values
train <- train[,-42]
test <- test[,-42]
  
  # training and fitting the model
m1 <- lm(Utility~.,train)
regr_out <- predict(m1,newdata = test[,-15])

  # making it so predictions are discrete values from range [0,3]
regr_out <- round(regr_out,0)
unique(regr_out)
regr_out <- ifelse(regr_out<0,0,regr_out)
unique(regr_out)
regr_out <- ifelse(regr_out>3,3,regr_out)
unique(regr_out)

  # measures of quality of prediction
ACC <- mean(ifelse(test$Utility==regr_out,1,0))
ACC # 0.5407321
ACC_within_1 <- mean(ifelse(abs(test$Utility-regr_out)==0|abs(test$Utility-regr_out)==1,1,0))
ACC_within_1 # 0.961355 - raczej wysokie

# AUC per class
multiclass.roc(test$Utility,regr_out) # 0.7896
plot(test$Utility,regr_out)
plot(regr_out,test$Utility-regr_out)
combined_2 <- test
combined_2$regr_out <- regr_out
combined_2$residual <- combined_2$Utility-combined_2$regr_out

library(ggplot2)
# residual vs fitted graph
ggplot(combined_2,aes(regr_out,residual)) +
  geom_count() # it's linear which indicates a linear dataset
