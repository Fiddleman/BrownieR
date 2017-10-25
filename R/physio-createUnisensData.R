#' createUnisensData
#'
#' This function groups a couple of other functions which create together Unisens data out of a MarkerFile and physio sensor data of one subject
#'
#' @param data a dataframe, pyhsio sensordata for one subject
#' @param subjectname a char name of a subject, for e.g. the subject_id_subject number. It will be the name of the subfolder with Unisense data.
#' @param destinationDirectory a char directory where all the files and subfolder will be placed at.
#' @param nameUnisensDataFolder a char name of a subdirectory in destinationDirectory where all the preproccessed unisens files will be created.
#' @param nameTrimmedUnisensDataFolder a char name of a subdirectory in destinationDirectory where all the final unisens files will be placed at.
#' @param pathMarkerFile a char, path to the marker file as csv. The function createMarkerFile will create a dataframe in the proper format.
#' @return prints log files of processing and finally the path to all avaliable unisense data for each subject
#' @export
createUnisensData <- function(data = physio174,
                              subjectname = "174",
                              destinationDirectory = "tests/testthat/datafortestingphysio/",
                              nameUnisensDataFolder = "01_unisens_data",
                              nameTrimmedUnisensDataFolder = "02_unisens_trimmed",
                              pathMarkerFile = "tests/testthat/datafortestingphysio/marker174.csv"){
  #transform physio data, transform time colums to original format
  data[t_cols_physio] <- lapply(data[t_cols_physio], function(x){as.integer(x)*1000})
  data[,c(7:11)] <- 0
  
  #path configurations
  unisense_folder <- file.path(destinationDirectory, nameUnisensDataFolder)
  trimmed_unisens_folder <- file.path(destinationDirectory, nameTrimmedUnisensDataFolder)
  cut_folder <- file.path(destinationDirectory, "00_raw_data_cut")
  
  # load unisens java library
  initalize_unisens()
  # function calls
  cutData(data, subjectname, destinationDirectory)
  generateTimestampOffsetInput(data, subjectname, destinationDirectory)
  loadRawData(cut_folder, unisense_folder, subjectname)
  insertEventEntry(unisense_folder, subjectname, pathMarkerFile)
  insertTimestampOffsetEntry(unisense_folder, subjectname)
  insertOffsetCorrectedEventEntry(unisense_folder, subjectname)
  trimUnisensData(unisense_folder, trimmed_unisens_folder, subjectname)
  runFiltersEcgData(trimmed_unisens_folder, subjectname)
  
  #print of created directory for further analysis 
  return(print(list.files(trimmed_unisens_folder, include.dirs = T, full.names = T)))
}