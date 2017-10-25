#' summary.physio()
#'
#' Implemented as additional method for the gerneic function summary() and objects of class "physio". 
#' Run this function to get average EDA values per Screen or visited web pages.
#' passed to the argument objectives.
#'
#' @param data; a list of class physio
#' @param type; a char, specify type of data you want to summarize. Currently only a summary of EDA is possible. BVG and ECG are planned.
#' @param subject; a integer, specify subject for which summary should be shown
#' @param marker; a dataframe generate by marker()
#' @param start_time; a POSIXt, minimal time in exp or a web data for the subject which is specified in the subject parameter of this function
#' @return dataframe
#' @export


summary.physio <- function(data, type = "eda", subject, marker, start_time){
  if (type == "eda"){
    synced_data <- compress_eda.physio(data[[subject]], start_time)
    eda_matched <- match_marker_eda.physio(synced_data, marker)
    summary_eda.physio(eda_matched)
  } else stop("Summary is  only available for EDA data. Summaries for BVG and ECG data are tba.")
}