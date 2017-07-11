#' Wrappermethod for reading from csv
#'
#' Wrappermethod for reading from csv. This method is part of the third layer.
#'
#' @param dataDir the directory of the file
#' @param fileName the name of the file to be read
#' @param sep separator of csv file
#' @param dec decimal points symble
#' @param header whether the file contains a header or not
#' @import utils
#' @keywords internal
readFromCSV <- function (dataDir, fileName, sep = ";", dec = ",", header = TRUE) {

        dataFromCSV <- read.csv(file.path(dataDir, fileName), sep = sep , dec = dec, header = header, stringsAsFactors = FALSE)
}


