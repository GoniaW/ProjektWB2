process_result <- function(result) {
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
  
  result
}

ROC <- function(result) {
  multiclass.roc(result$data$truth,result$data$response) #
}

ACC <- function(result) {
  mean(result$data$truth==result$data$response)
}

ACC_within1 <- function(result) {
  mean(result$data$truth - result$data$response<=1)
}
