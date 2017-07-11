#' Inserts timestamp offset
#'
#' Insert timestamp offset entry into Unisens.xml file
#'
#' @param unisensDestDataFolder the unisens data folder where all unisens data are stored. Normally "./01_unisens_data".
#' @param subject subfolder name of the unisens data.
#' @export
#' @examples
#' \dontrun{
#' unisens_folder <- "xyz/01_unisens_data"
#' subject <- "1101"
#' insertTimestampOffsetEntry(unisens_folder, subject)
#' }
insertTimestampOffsetEntry <- function(unisensDestDataFolder,subject){

        unisensFullDestFolder <- file.path(unisensDestDataFolder, subject)

        # If timestamp_offset_input.csv not detected, the user is forced to choose it from a dialog box and the marker
        # file will be copied into the desired folder.
        if(!file.exists(file.path(unisensFullDestFolder, "timestamp_offset_input.csv"))){
                message("No timestamp_offset_input.csv detected")
                tsfile <- file.choose()
                file.copy(from = tsfile, to = file.path(unisensFullDestFolder, "timestamp_offset_input.csv"))
        }



        cat('Loading unisens for ', subject)
        j_unisensFactory <- J("org.unisens.UnisensFactoryBuilder", "createFactory")
        j_unisens <- J(j_unisensFactory, "createUnisens", unisensFullDestFolder)

        # check whether 'timestamp_offset.csv' entry exists or not
        entry_found <- 0
        j_entries <- J(j_unisens, "getEntries")
        for(i in 0:(.jcall(j_entries, "I", "size") -1)){
                j_entry_id <- J(J(j_entries, "get", as.integer(i)), "getId")

                if(tolower(j_entry_id) == tolower("timestamp_offset.csv")){
                        entry_found <- 1
                }
        }
        if(entry_found == 1){
                j_entry <- J(j_unisens, "getEntry", "timestamp_offset.csv")
                J(j_unisens, "deleteEntry", j_entry)
        }

        J(j_unisens, "save")
        J(j_unisens, "closeAll")

        #################################
        ## Event-Data ###################
        #################################
        eventEntry <- list()
        eventEntry$fileFormat    <- 'csv'
        eventEntry$contentClass  <- 'MARKERS'
        eventEntry$entryId       <- 'timestamp_offset.csv'
        eventEntry$sampleRate    <- 1
        eventEntry$commentLength <- 10
        eventEntry$entryComment  <- 'event entry data'

        #eventEntry$commentLength <- 50
        #eventEntry$contentClass  <- 'MARKERS'
        #eventEntry$entryId       <- 'marker'
        #eventEntry$typeLength    <- 1
        #eventEntry$unit          <- 'V'
        #eventEntry$entryComment  <- 'trimmed event entry data'
        #eventEntry$dataType      <- 'double'

        # laod the csv data
        csv_data <- readFromCSV(unisensFullDestFolder, "timestamp_offset_input.csv", header = FALSE)
        csv_data <- as.matrix(csv_data)
        csv_data_length <- nrow(csv_data)

        # add some random events
        data <- list()
        data$event$samplestamp <- csv_data[, 1]
        data$event$type        <- rep(0, csv_data_length)
        #data$event$comment     <- as.character(csv_data[, 3])
        data$event$comment     <- csv_data[, 3]

        # for(i in 1:csv_data_length){
        #      data$event$samplestamp[i] <- csv_data[i, 1]
        #      data$event$type[i] <- "0"
        #      data$event$comment <- as.character(csv_data[i, 3])
        # }

        eventEntry$data <- data$event
        unisens_utility_add_evententry(file.path(unisensDestDataFolder, subject), eventEntry)
}
