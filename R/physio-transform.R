#' transform.physio()
#' 
#' This function transform a list of class physio, doing the following: remove empty input colums; 
#' calculate time since tracking has started; normalize the sensor data using z-scores.
#' Output is again a list, where each subject is a  list item.
#' 
#' @param data a list of class physio
#' @return list

transform.physio <- function(data){
  data <- as.data.frame(unclass(data), stringsAsFactors = F)
  subjects <- unique(data$SUBJECT_ID_SUBJECT)
  data_list <- list()
  for (subject in subjects){
    data <- data[data$SUBJECT_ID_SUBJECT == subject,] #split df for subjects
    data <- removeEmptyColums(data)
    data <- normTime(data)
    data <- normData(data)
    data$SUBJECT_ID_SUBJECT <- NULL
    data <- list(data)
    names(data) <- paste0("SUBJECT_ID_SUBJECT", subject)
    data_list <- c(data_list, data)
  }
  return(data_list)
}

#' removeEmptyColums()
#' 
#' Removes every colum containing zeros, as it appears for inputs with no connected sensor in a experiment.
#' 
#' @param data a physio dataframe 
#' @return dataframe

removeEmptyColums <- function(data) {
  colums <- 1:length(data)
  del_col <- NULL
  for (colum in colums) {
    if (all(data[colum] == 0)) del_col <- c(del_col, colum)
  }
  if (!is.null(del_col)) data <- data[,-del_col]
  return(data)
}

#' normTime()
#' 
#' Caculate time since tracking has started.
#' 
#' @param data a physio dataframe 
#' @return dataframe

normTime <- function(data) {
  data$SampleTime <- data$ReceiveTime - data$ReceiveTime[1]
  colnames(data)[2] <- "Time"
  data <- data[,-1] #delet colum ReceiveTime
  return(data)
}
#' normData()
#' 
#' Normalize data colums using z score for colums with name contains "input".
#' 
#' @param data a physio dataframe 
#' @return dataframe
#'
normData <- function(data) {
  index <- grep("input", colnames(data), ignore.case = T)
  for (i in index) {
    data[i] <- scale(data[i])
  }
  return(data)
}