#' detectDataType(path)
#'
#' Detect the data type of a given file
#'
#' @param path a character
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
#' @param path a charater
#' @return array with file and corresponding datatype
createFileIndex <- function(path){
  files <-list.files(path, pattern = "*.csv|.expdata", full.names=T)
  types <- NULL
  for (file in files){
    types <- append(types, detectDataType(file))
  }
  files_types <- append(files, types)
  a_files_types <- array(files_types, dim = c(length(files),2))
  file_index <- a_files_types[a_files_types[,2] != "unkown",] #remove unknown file types from index
  return(file_index)
}

#' cbindFiles(files, type)
#'
#' Binds files in one dataframe and separate them by adding a colum Session.
#'
#' @param files a vector, type a character constant
#' @return dataframe

readBindFiles <- function(files, type){
  full_data <- data.frame()
  session_count <- 1
  if (type == "web" || type == "physio") {
    for (file in files) {
      single_df <- read.csv(file = file, sep = ";", header = T, stringsAsFactors = F)
      single_df$Session <- session_count
      session_count <- session_count + 1
      full_data <- rbind(full_data, single_df)
    }
    # Time converting to POSIXct
    if (type == "web"){
      full_data[t_cols_web] <- lapply(full_data[t_cols_web], function(x){as.POSIXct(x/1000, origin = "1970-01-01 00:00:00")})
    } else {
      full_data[t_cols_physio] <- lapply(full_data[t_cols_physio], function(x){as.POSIXct(x/1000, origin = "1970-01-01 00:00:00")})
    }
    return(full_data)
    
  } else if (type == "exp") {
    for (file in files) {
      exp_data <- readLines(file)
      exp_data <- gsub("\"", "", exp_data)
      single_df <- read.csv(text = exp_data, sep = ",", quote = "\"", header = T)
      single_df$Session <- session_count
      session_count <- session_count + 1
      full_data <- rbind(full_data, single_df)
    }
    # Time converting to POSIXct
    full_data[t_cols_exp] <- lapply(full_data[t_cols_exp], function(x){as.POSIXct(x/1000, origin = "1970-01-01 00:00:00")})
    return(full_data)
  } else stop("Filetype not supported, only web, physio, exp is possible.")
}

#' importData(path, prefix)
#' 
#' Imports brownie data (of heterogenous types) from folder and adds list containing dataframes and type for each type to global environment (naming by prefix).
#' 
#' @param path, prefix
#' @return none (adds list directly to global environment)

importData <- function(path, prefix){
  file_index <- createFileIndex(path = path)
  datatypes <- unique(file_index[,2])
  for (dtype in datatypes) {
    files <- file_index[file_index[,2] == dtype, 1]
    assign(paste0(prefix, "_", dtype), list(data = data.frame(readBindFiles(files, dtype), Datatype = dtype), envir = .GlobalEnv))
  }
}
