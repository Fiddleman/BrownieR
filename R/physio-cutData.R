#' Cutting unneccessary entries from data and write to destination folder
#'
#' Cuts empty data fields from before experiment start and unused signal columns.
#'
#' @details Note that the cut file will be stored in e.g. "00_raw_data_cut/1101" which is a subfolder of destdir. "1101" is subject name.
#'
#' @param rawData either raw data frame or the raw data file name.
#' @param subject the subfolder name and the file name of the cut data.
#' @param destdir the folder where the cut data will be stored.
#' @param sourcedir the folder where the raw data is stored. If not given, assume sourcedir is equal to destdir.
#' @examples
#' \dontrun{
#' ## If data frame is given:
#' filename <- "1101.csv"
#' pathRawData <- "abc/xyz"
#' subject <- 1101
#' data <- readRawData(filename, pathRawData, TRUE)
#' # The cut data will be stored under "abc/xyz/00_raw_data_cut/1101"
#' cutData(data,  subject, pathRawData)
#'
#' ## If file name is given:

#' # Note that: This function will read the raw data in the pathRawData folder
#' # but save the cut file under 00_raw_data_cut/1101.
#' cutData(filename,  subject, pathRawData)
#' # Or the sourcedir is not equal to destdir:
#' cutData(filename,  subject, pathRawData,"abc/abc")
#' }
cutData <- function(rawData, subject, destdir, sourcedir = NULL){

        # read raw data
        if(is.null(sourcedir)){sourcedir <- destdir}
        if(!is.data.frame(rawData) && is.character(rawData)){
                pathRawData <- file.path(sourcedir, rawData)
                rawData <- readRawData(rawData, pathRawData)
        }else if(!is.data.frame(rawData) && !is.character(rawData)){
                stop("Wrong format of rawData! Either string or data frame!")
        }

        destfolder <- file.path(destdir, "00_raw_data_cut", subject)
        destfile <- paste0(subject, ".csv")
        if(!dir.exists(destfolder)){dir.create(destfolder, recursive = TRUE)}

        if(file.exists(file.path(destfolder, destfile))){
                stop(cat(subject, " already processed (raw data has been already cut)\n"))
        }

        # only the first row, as all rows have the same two values
        timestamp_receive <- rawData[1, "ReceiveTime"]
        timestamp_sampling <- rawData[1, "SampleTime"]

        timestamp_diff <- as.numeric(timestamp_receive)-as.numeric(timestamp_sampling)-1000
        timestamp_new <- as.numeric(timestamp_sampling) + as.integer(timestamp_diff/2)
        timestamp <- as.numeric(timestamp_sampling)

        inputDataCut <- rawData[, c("Input1",  "Input3", "Input2"), drop=FALSE]

        cutDataResult <- rbind(c(timestamp, timestamp_new, timestamp), inputDataCut)

        writeToCSV(destfolder, destfile, cutDataResult)

}
