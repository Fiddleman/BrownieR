#' transformInputs(data, inputorder)
#' 
#' Function order the inputs as they are needed for further physio processings. Additional, all Inputs
#' which are not part of the parameter inputorder will be removed.
#' 
#' @param data a dataframe containing the tracked sensor data of one subject
#' @param inputorder a named character vector, names needs to be Input1 to Input8, values only "EKG, "BVP", or "EDA"
#' @return dataframe
#' @examples  
#' inputorder <-c(EKG = "Input1", BVP = "Input2", EDA ="Input3")

transformInputs <- function(data, inputorder){
  if (!all(names(inputorder) %in% c("BVP", "EDA", "EKG"))){
    stop("Vectornames of parameter inputorder needs to be BVP, EDA or EKG, occured only one time and all three sensor types needs to be specified")
  } else if (!all(inputorder %in% c("Input1", "Input2", "Input3", "Input4","Input5", "Input6", "Input7", "Input8"))){
    stop("Values of parameter inputorder needs to be between Input1 and Input8")
  }
  else {
    # # part of new logic that doesn require all three datatypes to be tracked and specified, but their is a logic problem that it doesn sort the input yet...
    # colum_order <- c(1:3)
    # for (input in inputorder){
    #   temp <- as.integer(substr(inputorder[names(inputorder) == names(input)], 6, 6)) + 3
    #   colum_order <- c(colum_order,temp)
    # }
    EKG <- as.integer(substr(inputorder[names(inputorder) == "EKG"], 6, 6)) + 3
    BVP <- as.integer(substr(inputorder[names(inputorder) == "BVP"], 6, 6)) + 3
    EDA <- as.integer(substr(inputorder[names(inputorder) == "EDA"], 6, 6)) + 3
    colum_order <- c(1:3, EKG, BVP, EDA)
    data <- data[,colum_order]
    colnames(data)  <- c("ReceiveTime", "SampleTime", "PlugSeqNo", "Input1", "Input2", "Input3")
    return(data)
  }
}

#' createMarkerFile(data, subject)
#' 
#' Creates a "Marker File" for a specified subject. It's needed to calculate for e.g. heart reates for each experimental screen.
#' For unisens, this file needs to be in *wd*/data/physio/data .
#' 
#' @param data a list of class exp or web
#' @param subject, a integer identifies subject for which marker file will be created
#' @param minSampleTimeSubject, a integer - minimum sample time of subject is needed to caluclate durations of screens and sync sample to exp data
#' @return a dataframe 

createMarkerFile <- function(data, subject, minSampleTimeSubject){
  if (class(data) == "exp"){
    data <- as.data.frame(unclass(data), stringsAsFactors = F)
    data <- subset(data,SUBJECT_ID_SUBJECT == subject, select = c(CLIENT_TIME, SCREEN_NAME)) #filter by specified subject, keep only ClienTime, Screen    
  } else if (class(data) == "web"){
    data <- as.data.frame(unclass(data), stringsAsFactors = F)
    data  <- subset(data, (Event == "URL-Change") | (Event == "first URL"), select = c("Time", "URL", "SUBJECT_ID_SUBJECT"))
    data <-  subset(data, SUBJECT_ID_SUBJECT == subject, select = c("Time", "URL"))
  } else return(stop("Data of type web or exp required!"))
  
  data[, 3] <- 0
  data <- data[,c(1,3,2)] #reorder colums to get right format
  data[ ,1] <- as.numeric(data[ ,1]) - minSampleTimeSubject #calculate duration
  names(data) <- c("DURATION", "0", "EVENT")
  data <- data[order(data$DURATION),] #order by duration
  return(data)
}