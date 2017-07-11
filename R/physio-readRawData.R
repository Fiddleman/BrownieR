#' Read raw data into memory
#'
#' Reads the raw experiment data and returns it as a dataframe.
#' @details If there is no such file detected, then the user is asked to select the file via a dialogue box.
#' @param filename the name of the file to process.
#' @param pathRawData the path to the rawData.
#' @param remove If TRUE, then the file will be removed into "00_raw_data" under the pathRawData folder.
#' @return the raw data from the experiment in the form of data frame.
#' @examples
#' \dontrun{
#' filename <- "1101.csv"
#' pathRawData <- "abc/xyz"
#' data <- readRawData(filename, pathRawData, TRUE)
#' }
#' @export
readRawData <- function (filename, pathRawData, remove = FALSE) {
        if(!file.exists(file.path(pathRawData, filename))){
                message("No such file detected! Please choose the right file!")
                filename <- file.choose()
                filename <- unlist(strsplit(filename, .Platform$file.sep))
                pathRawData <- paste0(filename[-length(filename)], collapse = .Platform$file.sep)
                filename <- filename[length(filename)]
        }

        file_extension <- unlist(strsplit(filename, "\\."))
        file_extension <- file_extension[length(file_extension)]
        if ("csv" %in% file_extension) {

                if(remove == TRUE){
                        from <- file.path(pathRawData, filename)
                        pathRawData <- file.path(pathRawData, "00_raw_data")
                        if(!dir.exists(pathRawData)){dir.create(pathRawData)}
                        to <- file.path(pathRawData, filename)
                        file.rename(from = from,  to = to)
                }

                rawData <- readFromCSV(pathRawData, filename)

                return(rawData)
        } else {
                return("No valid inputFormat specified")
        }



}
