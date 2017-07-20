#' transformInputs(data, inputorder)
#' 
#' Function order the inputs as they are needed for further physio processings. Additional, all Inputs
#' which are not part of the parameter inputorder will be removed.
#' 
#' @param data a dataframe containing the tracked sensor data of one subject
#' @param inputorder a named character vector, names needs to be Input1 to Input8, values only "EKG, "BVP", or "EDA"
#' @return dataframe
#' @example 
#' inputorder <-c(EKG = "Input1", BVP = "Input2", EDA ="Input3")


transformInputs <- function(data, inputorder){
  if (!all(names(inputorder) %in% c("BVP", "EDA", "EKG"))){
    stop("Vectornames of parameter inputorder needs to be BVP, EDA or EKG, occured only one time and all three sensor types needs to be specified")
  } else if (inputorder %in% c("Input1", "Input2", "Input3", "Input4","Input5", "Input6", "Input7", "Input8")){
    stop("Values of parameter inputorder needs to be between Input1 and Input8")
  }
  else {
    EKG <- as.integer(substr(inputorder[names(inputorder) == "EKG"], 6, 6)) + 3
    BVP <- as.integer(substr(inputorder[names(inputorder) == "BVP"], 6, 6)) + 3
    EDA <- as.integer(substr(inputorder[names(inputorder) == "EDA"], 6, 6)) + 3
    colum_order <- c(1:3, EKG, BVP, EDA)
    data <- df[,colum_order]
    return(data)
  }
}

#' createMarkerFile.exp(data, subject)
#' 
#' Creates a "Marker File" for a specified subject. It's needed to calculate for e.g. heart reates for each experimental screen.
#' For unisens, this file needs to be in *wd*/data/physio/data.
#' 
#' @param data a list of class exp
#' @param subject, a integer identifies subject for which marker file will be created
#' @param minSampleTimeSubject, a integer - minimum sample time of subject is needed to caluclate durations of screens and sync sample to exp data
#' @return a dataframe 

createMarkerFile.exp <- function(data, subject, minSampleTimeSubject){
  data <- as.data.frame(unclass(data), stringsAsFactors = F)
  data <- data[data$SUBJECT_ID_SUBJECT == subject, c(2,3)] #filter by specified subject, keep only ClienTime, Screen
  data[, 3] <- 0
  data <- data[,c(1,3,2)] #reorder colums to get right format
  data$CLIENT_TIME <- as.numeric(data$CLIENT_TIME) - minSampleTimeSubject #calculate duration
  names(data) <- c("DURATION", "0", "EVENT")
  data <- data[order(data$DURATION),] #order by duration
  return(data)
}


#Function is not needed at the moment: 20.7.-----------
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



#Function is not needed at the moment: 20.7.-----------
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
    data$SUBJECT_ID_SUBJECT <- NULL
    data <- list(data)
    names(data) <- paste0("SUBJECT_ID_SUBJECT", subject)
    data_list <- c(data_list, data)
  }
  return(data_list)
}