#' Wrappermethod for writing to csv
#'
#' Wrappermethod for writing to csv.
#'
#' @param dataDir the directory of the file to write.
#' @param fileName the name of the file to write.
#' @param content the content to write to the file.
#' @param sep separator of csv file.
#' @param dec decimal points symble.
#' @param col.names whether the file contains a header.
#' @param createdir whether create a new directory.
#' @import utils
#' @keywords internal
writeToCSV <- function(dataDir, fileName, content, createdir = TRUE, sep = ";", dec = ",", col.names = TRUE) {
        if(createdir){if(!dir.exists(dataDir)){dir.create(dataDir)}}
        write.table(content, file = file.path(dataDir, fileName),sep = sep, dec = dec, row.names = FALSE, col.names = col.names, quote = FALSE)

}
