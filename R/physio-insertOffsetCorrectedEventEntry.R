#' Generates corrected marker file
#'
#' Generate marker_oc.csv and insert this entry into Unisens.xml file
#'
#' @details marker_oc.csv is the timestamp-offset corrected marker file.
#' @param unisensDestDataFolder the unisens data folder where all unisens data are stored. Normally "./01_unisens_data".
#' @param subject subfolder name of the unisens data.
#' @import rJava
#' @export
#' @examples
#' \dontrun{
#' unisens_folder <- "xyz/01_unisens_data"
#' subject <- "1101"
#' insertOffsetCorrectedEventEntry(unisens_folder, subject)
#'}
insertOffsetCorrectedEventEntry <- function(unisensDestDataFolder, subject){
        unisensFullDestFolder <- file.path(unisensDestDataFolder, subject)
        cat('Loading unisens for ', subject)
        j_unisensFactory <- J("org.unisens.UnisensFactoryBuilder", "createFactory")
        j_unisens <- J(j_unisensFactory, "createUnisens", unisensFullDestFolder)


        # check whether 'marker_oc.csv' entry exists or not
        entry_found <- 0
        j_entries <- J(j_unisens, "getEntries")
        for(i in 0:(.jcall(j_entries, "I", "size") -1)){
                j_entry_id <- J(J(j_entries, "get", as.integer(i)), "getId")

                if(tolower(j_entry_id) == tolower("marker_oc.csv")){
                        entry_found <- 1
                }
        }
        if(entry_found == 1){
                j_entry <- J(j_unisens, "getEntry", "marker_oc.csv")
                J(j_unisens, "deleteEntry", j_entry)
        }

        J(j_unisens, "save")
        J(j_unisens, "closeAll")


        #################################
        ## Event-Data ###################
        #################################
        # CHECK WETHER THERE ARE EVENTS AT ALL!!!
                #first gets the data from file
        j_entry <- J(j_unisens, "getEntry", 'marker.csv')
        numberOfEvents <- J(j_entry, "getCount")
        eventRaw <- J(j_entry, "read", as.integer(numberOfEvents))

        # LOAD THE CSV DATA
        csv_data <- readFromCSV(unisensFullDestFolder, "timestamp_offset.csv",header = FALSE)
        csv_data <- as.matrix(csv_data)
        csv_data_sizex <- nrow(csv_data)
        csv_data_sizey <- ncol(csv_data)

        evententry_marker <- list()
        evententry_marker$fileFormat    <- 'csv'
        evententry_marker$entryId       <- 'marker_oc.csv'
        evententry_marker$contentClass  <- 'MARKERS'
        evententry_marker$sampleRate    <- 1000
        evententry_marker$commentLength <-  50
        evententry_marker$entryComment  <- 'offset corrected event entry data'

        event<- list()

        if (numberOfEvents > 0){
                for(i in 0:(numberOfEvents-1) ){
                        raw_event <- J(eventRaw, "get", as.integer(i))
                        #print(as.character(.jcall(raw_event, "S", "getComment")))
                        event$samplestamp[i+1] <- J(raw_event, "getSamplestamp")
                        offset_second <- floor(event$samplestamp[i+1] / 1000)
                        offset <- 0

                        if (csv_data_sizex < offset_second + 1){
                                print('FILE TOO SHORT')
                                offset <-  csv_data[csv_data_sizex, 3]
                        }else{
                                offset <- csv_data[(offset_second + 1), 3]
                        }

                        event$samplestamp[i+1] <- event$samplestamp[i+1] - offset

                        event$type[i+1]        <- "0"
                        event$comment[i+1]     <- as.character(J(raw_event, "getComment"))
                        #print(data$event$comment[i+1])

                }
        }

        evententry_marker$data <- event

        unisens_utility_add_evententry(unisensFullDestFolder, evententry_marker)


}









