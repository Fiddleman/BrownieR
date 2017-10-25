#' plot.physio()
#'
#' Visualize physio data for one subject. Currently only plot of EDA and ECG data is possible.
#' 
#' @param data; a list of class physio
#' @param type; a char, specify type of physio data you want to plot. Values are "eda" or "ecg". For type "ecg" it's important that
#' physio data is moved to folder *wd*/data/physio/ 
#' @param subject; a integer, specify subject for which summary should be shown
#' @param marker; a dataframe generate by marker() if type is "eda" or a path if type is "ecg"
#' @param start_time; a POSIXt, minimal time in exp or a web data for the subject which is specified 
#' @param physio_unisens_dir; a path, directory where unisense data is created and contains physio csv-files
#' in the subject parameter of this function
#' @return dataframe

plot.physio <- function(data, marker, type, subject, start_time, physio_unisens_dir){
  if (type == "eda"){
    #if (is.dir(marker)) stop("Parameter marker needs to be a dataframe for plotting eda data")
    summary <- summary.physio(data, type = "eda", subject, marker, start_time)
    summary <- data.frame(summary, Time2 = c(summary$Time[-1], tail(summary$Time, n = 1) + 1))
    data <- data[[subject]]
    data <- compress_eda.physio(data, start_time)
    ggplot() + 
      geom_line(data = data, mapping = aes(x = Time, y = AverageEda), size = 0.75) +
      geom_rect(data = summary, mapping = aes(xmin = Time, ymin = min(data$AverageEda), xmax = Time2, ymax = max(data$AverageEda), fill = Event), alpha = 0.15) +
      geom_point(data = summary, mapping = aes(x = Time, color = Event, y = AverageEda), size = 4 ) +
      geom_rect(data = summary, mapping=aes(xmin = Time, ymin = AverageEda - 0.5, xmax = Time2, ymax = AverageEda +0.5, fill = Event), alpha = 0.4) +
      geom_text(data = summary, mapping=aes(x = Time, y = AverageEda, label = AverageEda), angle=-20, hjust=1.2, vjust = 0.2, size=4.5)
  } else if (type == "ecg"){
    destFolderAnslab <- file.path(paste0(physio_unisens_dir, "02_unisens_trimmed"))
    initalize_unisens()
    createUnisensData(data = data[[subject]],
                      subjectname = as.character(subject),
                      destinationDirectory = physio_unisens_dir,
                      nameUnisensDataFolder = "01_unisens_data",
                      nameTrimmedUnisensDataFolder = "02_unisens_trimmed",
                      pathMarkerFile = marker)
    ecg_setTriggersDataOpenAnslab(destFolder = destFolderAnslab, subject = subject) #plotting function after all transformations
  }
  else stop("Plot is  only available for EDA and ECG data, BVG is tba.")
}