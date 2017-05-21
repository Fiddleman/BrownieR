#' detectDataType(path)
#'
#' Detect the data type of a given file
#'
#' @param path, a character
#' @return constant web, physio, exp or none
#'
detectDataType <- function(file){
  #ToDo write for in loop for better code; Define exp, web, physio and unkown als global variabels
  if(as.logical(length(grep("VALUENAME", readLines(file, n=1), value = F)))) return ("exp")
  else if(as.logical(length(grep("H-ScrollPosition", readLines(file, n=1), value = F)))) return ("web")
  else if(as.logical(length(grep("PlugSeqNo", readLines(file, n=1), value = F)))) return ("physio")
  else return("unkown")
}

#' createFileIndex(path)
#' 
#' Creates an index of  files in a folder with corresponding data types.
#' 
#' @param path, a charater
#' @return array with file and corresponding datatype
createFileIndex <- function(path){
  files <-list.files(path, pattern = "*.csv|.expdata", full.names=T)
  types <- NULL
  for (file in files){
    types <- append(types, detectDataType(file))
  }
  files_types <- append(files, types)
  return(array(files_types, dim = c(length(files),2)))
}

#' cbindFiles(files)
#'
#' Binds files in one dataframe and separate them by adding a colum Session.
#'
#' @param files, vector of files
#' @return dataframe
#'


#' importData(path, prefix)
#' 
#' Imports brownie data (of heterogenous types) from folder and adds dataframes for each type to users environment (naming by prefix).
#' 
#' @param path, prefix
#' @return none (adds dataframes directly to environment)
#' 


