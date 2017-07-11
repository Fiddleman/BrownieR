#' Generating timestamp offset input
#'
#' Generate timestamp offset input based on the given raw data and saved them in a csv file.
#'
#' @details Note that: the timestamp file will be stored in e.g. "01_unisens_data/1101/timestamp_offset_input.csv" which is under a subfolder of destdir. "1101" is the subject name.
#'
#' @param rawData either raw data frame or the raw data file name.
#' @param subject the subfolder name where the timestamp data should be stored.
#' @param destdir the folder where the cut data will be stored.
#' @param sourcedir the folder where the raw data is stored.If not given, assume sourcedir is equal to destdir.
#'
#' @export
#' @examples
#' \dontrun{
#' ## If data frame is given:
#' filename <- "1101.csv"
#' pathRawData <- "abc/xyz"
#' subject <- 1101
#' data <- readRawData(filename, pathRawData, TRUE)
#' # The cut data will be stored under "abc/xyz/01_unisens_data/1101"
#' generateTimestampOffsetInput(data,  subject, pathRawData)
#'
#' ## If file name is given:

#' # Note that: This function will read the raw data in the pathRawData folder
#' # but save the cut file under 01_unisens_data/1101.
#' generateTimestampOffsetInput(filename,  subject, pathRawData)
#' # Or the sourcedir is not equal to destdir:
#' generateTimestampOffsetInput(filename,  subject, pathRawData,"abc/abc")
#' }
generateTimestampOffsetInput <- function(rawData, subject, destdir, sourcedir = NULL){

        # read raw data
        if(is.null(sourcedir)){sourcedir <- destdir}
        if(!is.data.frame(rawData) && is.character(rawData)){
                pathRawData <- file.path(sourcedir, rawData)
                rawData <- readRawData(rawData, pathRawData)
        }else if(!is.data.frame(rawData) && !is.character(rawData)){
                stop("Wrong format of rawData! Either string or data frame!")
        }

        destfolder <- file.path(destdir, "01_unisens_data", subject)
        if(!dir.exists(destfolder)){dir.create(destfolder, recursive = TRUE)}


        if(file.exists(file.path(destfolder, "timestamp_offset_input.csv"))){
                stop(cat(subject, " already processed (Timestampoffset generated)\n"))
        }

        counter_second <- 0
        timestamp_receive_diff <- 0
        timestamp_receive_diff_acc <- 0
        timestamp_receive_diff_total <- 0
        last_timestamp_receive <- 0
        counter_secondVector <- c()
        timestamp_receive_diffVector <- c()
        timestamp_receive_diff_accVector <- c()
        timestamp_receive_diff_totalVector <- c()



        # find 1, 1001, ...., row
        n <- floor(nrow(rawData)/1000)
        if(nrow(rawData) %% 1000 == 0){n <- n-1}
        start <- 1
        for(i in 0:n){
                rowindex <- start + 1000 * i
                line <- rawData[rowindex, ]
                timestamp_receive <- as.numeric(line["ReceiveTime"])
                timestamp_sampling <- as.numeric(line["SampleTime"])
                #timestamp_receive_diff <- 0
                timestamp_receive_diff <- ifelse(rowindex == 1, 0, timestamp_receive - last_timestamp_receive - 1000)
                timestamp_receive_diff_acc <- as.numeric(timestamp_receive_diff_acc) + as.numeric(timestamp_receive_diff)
                timestamp_receive_diff_total <- as.numeric(timestamp_receive) - as.numeric(timestamp_sampling)-1000

                counter_second <- as.integer(rowindex/1000)
                counter_secondVector <- c(counter_secondVector, counter_second)
                timestamp_receive_diffVector <- c(timestamp_receive_diffVector, timestamp_receive_diff)
                timestamp_receive_diff_accVector <- c(timestamp_receive_diff_accVector, timestamp_receive_diff_acc)
                timestamp_receive_diff_totalVector <- c(timestamp_receive_diff_totalVector, timestamp_receive_diff_total)

                last_timestamp_receive <- timestamp_receive
        }

        timestamp_offset_input <- data.frame(counter_secondVector, timestamp_receive_diffVector, timestamp_receive_diff_accVector, timestamp_receive_diff_totalVector)
        writeToCSV(destfolder,"timestamp_offset_input.csv", timestamp_offset_input, col.names = FALSE)

}
