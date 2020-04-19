#------------------------------------------------------------------
# GOAL: check POLR model (ordinal classification)
# Result: It's better than linear regression

#------------------------------------------------------------------
# Libraries

library(tidyverse)
library(MASS)
#------------------------------------------------------------------
# Data load
eucalyptus <- readr::read_csv('combined_04_19_wersja_Gosi.csv')
eucalyptus <- eucalyptus[,-43]
eucalyptus$Utility <- factor(eucalyptus$Utility, levels = c('low', 'average', 'good', 'best'), ordered = TRUE)
  # factor must be ordered for POLR model
set.seed(3456)
trainIndex <- createDataPartition(eucalyptus$Utility, p = .7, 
                                  list = FALSE, 
                                  times = 1)

train_euc <- eucalyptus[trainIndex[,1],]
test_euc <-  eucalyptus[-trainIndex[,1],]
#------------------------------------------------------------------
# Model

polr_model <- polr(Utility ~ ., data = train_euc, Hess=TRUE)
result <- predict(polr_model, test_euc)

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

multiclass.roc(result, test_euc$Utility) # 0.8322 - 0.868 is max on openml
ACC <- mean(ifelse(as.character(test_euc$Utility)==as.character(result),1,0))
ACC #0.5634248

ACC_within_1 <- mean(ifelse(abs(as.numeric(test_euc$Utility)-as.numeric(result))==0|abs(as.numeric(test_euc$Utility)-as.numeric(result))==1,1,0))
ACC_within_1 # 0.9541202

