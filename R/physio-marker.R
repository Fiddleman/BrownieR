#' marker(data, subject)
#' 
#' Creates a "Marker File" for a specified subject. It's needed to calculate for e.g. heart reates for each experimental screen.
#' For unisens, this file needs to be in *wd*/*data*/physio .
#' 
#' @param data a list of class exp or web
#' @param subject, a integer identifies subject for which marker file will be created
#' @export
#' @return a dataframe 

marker <- function(data, subject){
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
  data[ ,1] <- as.numeric(data[ ,1]) - as.numeric(min(data[ ,1], na.rm = T)) #calculate duration
  names(data) <- c("DURATION", "0", "EVENT")
  data <- data[order(data$DURATION),] #order by duration
  return(data)
}