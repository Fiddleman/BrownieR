#' transform.web, helper function
#'
#' Function gets called by \code{link{plot_brownie}} to calculate data points to plot on screenshot.
#'
#' Extracts data for specified subjects, URL and event. If type euqals "motion" than only data for the
#' Event MouseMotion is used (for type equals "click" only Event MouseButtonPressed is used).
#' The movement data is computed by the formulars:
#' \itemize{
#'  \item{
#'  X = H.ScrollPosition + X.Position
#'  }
#'  \item{
#'  X = H.ScrollPosition + X.Position
#'  }
#' }
#'
#' @param data a list of class web
#' @param url a character
#' @param type a character, "motion" or "click"
#' @param subject a integer vector including the subjects which should be considered for calculation
#' @return dataframe

transform.web <- function(data, url, type = "motion", subject = c(1, 2)) {
  data <- as.data.frame(unclass(data), stringsAsFactors = F)
  if (type == "click") {
    type_of_plot <- "MouseButtonPressed"
  }  else type_of_plot <- "MouseMotion"
  data_pos <- subset(data, (URL == url) & (Event == type_of_plot) & (SUBJECT_ID_SUBJECT == subject), select = c("H.ScrollPosition", "X.Position", 
                                                                                                                "V.ScrollPosition", "Y.Position",
                                                                                                                "SUBJECT_ID_SUBJECT"))
  x <- data_pos$H.ScrollPosition + data_pos$X.Position
  y <- data_pos$V.ScrollPosition + data_pos$Y.Position
  data_points <- data.frame(X = x, Y = y, subject = data_pos$SUBJECT_ID_SUBJECT)
  return(data_points)
}