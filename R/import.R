#' import(path, prefix)
#' 
#' Imports brownie data (of heterogenous types) from folder and adds list containing dataframes and type to global environment (nameing by prefix).
#' 
#' @param path; a character
#' @param prefix; a character, nameing of lists
#' @param physio_mv_dir; a logical, indicates if physio data should be moved in subfolder "physio", necessary for create unisense data objects
#' @param create_physio_object; a logical, indicates if physio data should be added to enviornment, necessary for do eda summary and eda plot
#' @return none (adds list directly to global environment)
#' @export

import <- function(path, prefix, physio_mv_dir = T, create_physio_object = F){
  file_index <- indexFiles(path = path)
  datatypes <- unique(file_index[,2])
  for (dtype in datatypes) {
    if (dtype == "web" || dtype == "exp"){
      files <- file_index[file_index[,2] == dtype, 1]
      data <- combineFiles(files, dtype)
      data <- convertTime(data, dtype)
      data_object <- structure(data, class = dtype, files = files)
      assign(paste0(prefix, "_", dtype), data_object, envir = .GlobalEnv)
    } else {
      if (create_physio_object){
        files <- file_index[file_index[,2] == "physio", 1]
        count <- 1
        data <- list()
        for (file in files) {
          single_df <- read.csv(file = file, sep = ";", header = T, stringsAsFactors = F)
          single_df <- rm_empty_col(single_df)
          single_df <- convertTime(single_df, dtype)
          data[[count]] <- single_df
          count <- count + 1
        }
      data_object <- structure(data, class = dtype, files = files)
      assign(paste0(prefix, "_", dtype), data_object, envir = .GlobalEnv)
      if (physio_mv_dir){
        topath <- paste0(path, file.path("/physio/"))
        files <- file_index[file_index[,2] == dtype, 1]
        if (!dir.exists(topath)) dir.create(topath, recursive=TRUE)
        for (file in files) {
          tofilename = paste0(topath, basename(file))
          file.rename(from = file, to = tofilename)            
        }
      }
      }
    }
  }
}

#' detectDataType(path)
#'
#' Detect the data type of a given file
#'
#' @param path; a character
#' @return constant web, physio, exp or none
#'
detectDataType <- function(file){
  # ToDo: write for in loop for better code; Define exp, web, physio and unkown als global variabels with identfiers
  if(as.logical(length(grep("VALUENAME", readLines(file, n=1), value = F)))) return ("exp")
  else if(as.logical(length(grep("H-ScrollPosition", readLines(file, n=1), value = F)))) return ("web")
  else if(as.logical(length(grep("PlugSeqNo", readLines(file, n=1), value = F)))) return ("physio")
  else return("unkown")
}

#' indexFiles(path)
#' 
#' Creates an index of  files in a folder with corresponding data types.
#' 
#' @param path; a charater
#' @return array with file and corresponding datatype
indexFiles <- function(path){
  files <-list.files(path, pattern = "*.csv|.expdata", full.names=T)
  types <- NULL
  for (file in files){
    types <- append(types, detectDataType(file))
  }
  files_types <- data.frame(files, types)
  file_index <- files_types[files_types[,2] != "unkown",] #remove unknown file types from index
  return(file_index)
}

#' combineFiles(files, type)
#'
#' Binds files of the same type in one dataframe and separate them by adding a colum SUBJETCT_ID_SUBJECT
#'
#' @param files; a vector
#' @param type; a character constant
#' @return dataframe

combineFiles <- function(files, type){
  # types in globalVar.R (by def. sep, header, parse func., )
  full_data <- data.frame()
  session_count <- 1
  if (type == "web" || type == "physio") {
    for (file in files) {
      single_df <- read.csv(file = file, sep = ";", header = T, stringsAsFactors = F)
      single_df$SUBJECT_ID_SUBJECT <- session_count
      session_count <- session_count + 1
      full_data <- rbind(full_data, single_df)
    }
    return(full_data)
  } else if (type == "exp") {
    for (file in files) {
      exp_data <- readLines(file)
      exp_data <- gsub("\"", "", exp_data)
      single_df <- read.csv(text = exp_data, sep = ",", quote = "\"", header = T, stringsAsFactors = F)
      full_data <- rbind(full_data, single_df)
    }
    return(full_data)
  } else stop("Filetype not supported, only web, physio or exp is possible.")
}

#' convertTime(data, type)
#' 
#' @param data; a dataframe
#' @param type; a character constant
#' @return dataframe

convertTime <- function(data, type){
    if (type == "web"){
      data[t_cols_web] <- lapply(data[t_cols_web], function(x){as.POSIXct(x/1000, origin = "1970-01-01 00:00:00")})
    } else if (type =="physio") {
      data[t_cols_physio] <- lapply(data[t_cols_physio], function(x){as.POSIXct(x/1000, origin = "1970-01-01 00:00:00")})
    } else if (type == "exp") {
    data[t_cols_exp] <- lapply(data[t_cols_exp], function(x){as.POSIXct(x/1000, origin = "1970-01-01 00:00:00")})
    } else stop("Filetype not supported, only web, physio or exp is possible.")
  return(data)
}

#' rm_empty_col()
#' 
#' Removes every colum containing zeros, as it appears for inputs with no connected sensor in a experiment.
#' This reduces the size to a half.
#' 
#' @param data a physio dataframe 
#' @return dataframe

rm_empty_col <- function(data) {
  colums <- 1:length(data)
  del_col <- NULL
  for (colum in colums) {
    if (all(data[colum] == 0)) del_col <- c(del_col, colum)
  }
  if (!is.null(del_col)) data <- data[,-del_col]
  return(data)
}