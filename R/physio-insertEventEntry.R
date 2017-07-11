#' Insert event entry
#'
#' Insert marker entry into Unisens.xml file
#'
#' @details This package doesn't inculde functions that generate "marker.csv". Hence "marker.csv" must be generated before. The marker file will be removed into unisens folder.
#' @param unisensDestDataFolder the unisens data folder where all unisens data are stored. Normally "./01_unisens_data".
#' @param subject subfolder name of the unisens data.
#' @param markerfile full path of markerfile
#' @export
#' @import rJava
#' @examples
#' \dontrun{
#' #markerfile is chosen by a dialog box
#' unisens_folder <- "xyz/01_unisens_data"
#' subject <- "1101"
#' insertEventEntry(unisens_folder, subject)
#' #markerfile is given
#' insertEventEntry(unisens_folder, subject, "abc/marker.csv")
#' }

insertEventEntry <- function(unisensDestDataFolder, subject, markerfile = NULL){
        unisensFullDestFolder <- file.path(unisensDestDataFolder, subject)


        cat("loading unisens for ", subject)
        j_unisensFactory <- J("org.unisens.UnisensFactoryBuilder", "createFactory")
        j_unisens <- J(j_unisensFactory, "createUnisens", unisensFullDestFolder)

        # check whether "marker.csv" entry exsits or not
        entry_found <- 0
        j_entries <- J(j_unisens, "getEntries")
        for(i in 0:(.jcall(j_entries, "I", "size") -1)){
                j_entry_id <- J(J(j_entries, "get", as.integer(i)), "getId")

                if(tolower(j_entry_id) == tolower("marker.csv")){
                        entry_found <- 1
                }
        }

        if(entry_found == 1){
                j_entry <- J(j_unisens, "getEntry", "marker.csv")
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
        eventEntry$entryId       <- 'marker.csv'
        eventEntry$sampleRate    <- 1000
        eventEntry$commentLength <-   50
        eventEntry$entryComment  <- 'event entry data'

        #eventEntry$commentLength <- 50
        #eventEntry$contentClass  <- 'MARKERS'
        #eventEntry$entryId       <- 'marker'
        #eventEntry$typeLength    <- 1
        #eventEntry$unit          <- 'V'
        #eventEntry$entryComment  <- 'trimmed event entry data'
        #eventEntry$dataType      <- 'double'


        # add some random events
        data <- list()
        data$event <- list()
        data$event$samplestamp <- c(0,0)
        data$event$type        <- c(0,0)
        data$event$comment     <- c(0,0)

        data$event$samplestamp[1] <- 500
        data$event$type[1]        <- "0"
        data$event$comment[1]     <- 'EVENT1'

        data$event$samplestamp[2] <- 1000
        data$event$type[2]        <- '0'
        data$event$comment[2]     <- 'EVENT2'

        eventEntry$data <- data$event
        unisens_utility_add_evententry(file.path(unisensDestDataFolder, subject), eventEntry)

        # If marker.csv not detected, the user is forced to choose it from a dialog box and the marker
        # file will be copied into the desired folder.
        if(!file.exists(markerfile) | is.null(markerfile)){
                message("No marker.csv detected")
                markerfile <- file.choose()
        }
        x <- file.copy(from = markerfile, to = file.path(unisensFullDestFolder), overwrite = TRUE)

}
