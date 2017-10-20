#' compress_eda.physio() 
#'
#' Function compresses a physio dataframe by extracting only the EDA data ($Input2) and calculating the average eda values per second.
#' A start_time needs to be speficied to sync the data to a marker object. 
#'
#' @param data; a dataframe of class physio for a specific subject
#' @param start_time; a POSIXct value, time when experiment started. It's usually the minimal time for subject in exp or web data. 
#' @return dataframe
#' @export
#' @examples
#' start_time <- min(labSession_web$Time, na.rm = T)
#' compress_eda.physio(data = physio_data, start_time)
#' 

compress_eda.physio <- function(data, start_time){
  
  #Cut data ab min time von web oder exp
  synced_data <- subset(data, ReceiveTime >= start_time, select = c("ReceiveTime", "Input2"))
  dates <- unique(synced_data$ReceiveTime)
  
  #calculate average per second ---------
  edas <- NULL
  for (sec in as.list(dates)) {
    values_per_sec <- synced_data[synced_data$ReceiveTime == sec,]
    avg_eda_per_sec <- mean(values_per_sec$Input2, na.rm = T)
    edas <- c(edas, avg_eda_per_sec)
  }
  #calculate durations -----------------
  dates <- round(dates - start_time, 2)
  data.frame(Time = dates, AverageEda = edas)
}