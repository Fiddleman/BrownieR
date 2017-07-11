#' Trim data
#'
#' Trim unnecessary data at the beginning and end of file.
#'
#' @param sourceFolder the unisens data folder.
#' @param destFolder the trimmed unisens data folder or the data folder where the results should be stored.
#' @param subject subfolder name of source folder and destfolder.
#'
#' @export
#' @examples
#' \dontrun{
#' unisens_folder <- "xyz/01_unisens_data"
#' trimmed_unisens_folder <- "xyz/02_unisens_trimmed"
#' subject <- "1101"
#' trimUnisensData(unisens_folder, trimmed_unisens_folder, subject)
#'}

trimUnisensData <- function(sourceFolder, destFolder, subject){
        fullDestFolder <- file.path(destFolder, subject)
        fullSourceFolder <- file.path(sourceFolder, subject)
        cat("Trimming unisens data for subject", subject)
        if (dir.exists(fullDestFolder)){
                stop(cat('Subject "', subject, '" already imported\n', "If not, please delete the folder: ", fullDestFolder))
        }else{
                if(!dir.exists(fullDestFolder )){dir.create(fullDestFolder , recursive = TRUE)}
        }

        ##################################
        ### Creat Unisens Instance #######
        ##################################
        j_unisensFactory <- J("org.unisens.UnisensFactoryBuilder", "createFactory")
        j_unisens <- J(j_unisensFactory, "createUnisens", fullSourceFolder)
        timestamp_start <- J(j_unisens, "getTimestampStart")

        trimmed_data <- list()
        data <- list()
        ##################################
        ### ECG-Data #####################
        ##################################
        j_entry <- J(j_unisens, "getEntry", 'ecg.bin')
        numberOfEcgValues <- J(j_entry, "getCount")

        chunksize <- floor(numberOfEcgValues/4)
        ecg1     <- J(j_entry, "read", as.integer(chunksize))
        ecg2     <- J(j_entry, "read", .jlong(chunksize), as.integer(chunksize))
        ecg3     <- J(j_entry, "read", .jlong(2*chunksize), as.integer(chunksize))
        ecg4     <- J(j_entry, "read", .jlong(3*chunksize), as.integer(numberOfEcgValues-(3*chunksize)))
        ecg1     <- .jevalArray(ecg1, simplify = TRUE)
        ecg2     <- .jevalArray(ecg2, simplify = TRUE)
        ecg3     <- .jevalArray(ecg3, simplify = TRUE)
        ecg4     <- .jevalArray(ecg4, simplify = TRUE)

        data$ecg <- c(ecg1)
        data$ecg[(chunksize + 1):(chunksize * 2)] <- c(ecg2)
        data$ecg[(chunksize * 2 + 1):(chunksize * 3)] <- c(ecg3)
        data$ecg[(chunksize * 3 + 1):numberOfEcgValues] <- c(ecg4)


        signalentry_ecg <- list()
        signalentry_ecg$fileFormat    <- 'bin'
        signalentry_ecg$entryId       <- 'ecg.bin'
        signalentry_ecg$adcResolution <- 16
        signalentry_ecg$adcZero       <- 0
        signalentry_ecg$sampleRate    <- 1000
        signalentry_ecg$lsbValue      <- 1
        signalentry_ecg$unit          <- 'V'
        signalentry_ecg$entryComment  <- 'trimmed ecg data'
        signalentry_ecg$contentClass  <- 'ECG'
        signalentry_ecg$channelNames  <- .jarray(c('Lead I'))
        signalentry_ecg$dataType      <- 'double'

        #################################
        ## EDA-Data #####################
        #################################
        j_entry <- J(j_unisens, "getEntry", "eda.bin")
        numberOfEdaValues <- J(j_entry, "getCount")

        chunksize <- floor(numberOfEdaValues/4)
        eda1     <- J(j_entry, "read", as.integer(chunksize))
        eda2     <- J(j_entry, "read", .jlong(chunksize), as.integer(chunksize))
        eda3     <- J(j_entry, "read", .jlong(2*chunksize), as.integer(chunksize))
        eda4     <- J(j_entry, "read", .jlong(3*chunksize), as.integer(numberOfEdaValues-(3*chunksize)))
        eda1     <- .jevalArray(eda1, simplify = TRUE)
        eda2     <- .jevalArray(eda2, simplify = TRUE)
        eda3     <- .jevalArray(eda3, simplify = TRUE)
        eda4     <- .jevalArray(eda4, simplify = TRUE)

        data$eda <- c(eda1)
        data$eda[(chunksize + 1):(chunksize * 2)] <- c(eda2)
        data$eda[(chunksize * 2 + 1):(chunksize * 3)] <- c(eda3)
        data$eda[(chunksize * 3 + 1):numberOfEdaValues] <- c(eda4)

        signalentry_eda <- list()
        signalentry_eda$fileFormat    <- 'bin'
        signalentry_eda$entryId       <- 'eda.bin'
        signalentry_eda$adcResolution <- 16
        signalentry_eda$adcZero       <- 0
        signalentry_eda$sampleRate    <- 1000
        signalentry_eda$lsbValue      <- 1
        signalentry_eda$unit          <- paste0("\u03BC", "S")
        signalentry_eda$entryComment  <- 'trimmed eda data'
        signalentry_eda$contentClass  <- 'EDA'
        signalentry_eda$channelNames  <- .jarray(c('EDA'))
        signalentry_eda$dataType      <- 'double'

        #################################
        ## PLETH-Data ###################
        #################################
        j_entry <- J(j_unisens, "getEntry", 'pleth.bin')
        numberOfPlethValues <- J(j_entry, "getCount")

        chunksize <- floor(numberOfPlethValues/4)
        pleth1     <- J(j_entry, "read", as.integer(chunksize))
        pleth2     <- J(j_entry, "read", .jlong(chunksize), as.integer(chunksize))
        pleth3     <- J(j_entry, "read", .jlong(2*chunksize), as.integer(chunksize))
        pleth4     <- J(j_entry, "read", .jlong(3*chunksize), as.integer(numberOfPlethValues-(3*chunksize)))
        pleth1     <- .jevalArray(pleth1, simplify = TRUE)
        pleth2     <- .jevalArray(pleth2, simplify = TRUE)
        pleth3     <- .jevalArray(pleth3, simplify = TRUE)
        pleth4     <- .jevalArray(pleth4, simplify = TRUE)

        data$pleth <- c(pleth1)
        data$pleth[(chunksize + 1):(chunksize * 2)] <- c(pleth2)
        data$pleth[(chunksize * 2 + 1):(chunksize * 3)] <- c(pleth3)
        data$pleth[(chunksize * 3 + 1):numberOfPlethValues] <- c(pleth4)

        signalentry_pleth <- list()
        signalentry_pleth$fileFormat    <- 'bin'
        signalentry_pleth$entryId       <- 'pleth.bin'
        signalentry_pleth$adcResolution <- 16
        signalentry_pleth$adcZero       <- 0
        signalentry_pleth$sampleRate    <- 1000
        signalentry_pleth$lsbValue      <- 1
        signalentry_pleth$unit          <- 'XX'
        signalentry_pleth$entryComment  <- 'trimmed pleth data'
        signalentry_pleth$contentClass  <- 'PLETH'
        signalentry_pleth$channelNames  <- .jarray(c('PLETH'))
        signalentry_pleth$dataType      <- 'double'

        #################################
        ## Event-Data ###################
        #################################
        # CHECK WETHER THERE ARE EVENTS AT ALL!!!
        #        first gets the data from file
        j_entry <- J(j_unisens, "getEntry", "marker.csv")
        numberOfEvents <- J(j_entry, "getCount")
        eventRaw <- J(j_entry, "read", as.integer(numberOfEvents))

        evententry_marker <- list()
        evententry_marker$fileFormat    <- 'csv'
        evententry_marker$entryId       <- 'marker.csv'
        evententry_marker$contentClass  <- 'MARKERS'
        evententry_marker$sampleRate    <- 1000
        evententry_marker$commentLength <-  50
        evententry_marker$entryComment  <- 'trimmed event entry data'


        j_entryoc <- J(j_unisens, "getEntry", "marker_oc.csv")
        numberOfEventsOc <- J(j_entryoc, "getCount")
        eventocRaw <- J(j_entryoc, "read", as.integer(numberOfEventsOc))

        evententryoc_marker <- list()
        evententryoc_marker$fileFormat    <- 'csv'
        evententryoc_marker$entryId       <- 'marker_oc.csv'
        evententryoc_marker$contentClass  <- 'MARKERS'
        evententryoc_marker$sampleRate    <- 1000
        evententryoc_marker$commentLength <-  50
        evententryoc_marker$entryComment  <- 'trimmed oc event entry data'

        #################################
        ## Cut out data in events #######
        #################################
        #CHECK WETHER THERE ARE EVENTS AT ALL!!!
        #data$event <- {}
        #dataoc.event <- {}
        dataoc <- list()

        if (numberOfEvents > 0){
                #firstEvent minus 30 seconds
                firstEventIndex <- round(J(J(eventocRaw, "get", as.integer(0)), "getSamplestamp") - 0.1 * 1000)
                #lastEvent plus 30 seconds
                lastEventIndex <- round(J(J(eventocRaw, "get", as.integer(numberOfEventsOc-1)), "getSamplestamp") + 30 * 1000)

                if (firstEventIndex < 1){
                        print('ACHTUNG!!! - AUFNAHME VORNE ZU KURZ')
                }


                if (lastEventIndex > numberOfEdaValues-1){
                        print('ACHTUNG!!! - AUFNAHME HINTEN ZU KURZ')

                        fillertime <- 120 * 60 * 1000
                        datafiller <- rep(0, fillertime)
                        data$ecg[(numberOfEcgValues + 1):(numberOfEcgValues + fillertime)] <- datafiller
                        data$eda[(numberOfEdaValues + 1):(numberOfEdaValues + fillertime)] <- datafiller
                        data$pleth[(numberOfPlethValues + 1):(numberOfPlethValues + fillertime)] <- datafiller
                }


                number_of_values <- lastEventIndex - firstEventIndex + 1
                trimmed_data$eda[1:number_of_values] <- data$eda[firstEventIndex:lastEventIndex] #
                trimmed_data$ecg[1:number_of_values] <- data$ecg[firstEventIndex:lastEventIndex]
                trimmed_data$pleth[1:number_of_values] <- data$pleth[firstEventIndex:lastEventIndex]


                #disp(number_of_values)
                chunksize <- floor(number_of_values/4)
                trimmed_data$eda_package1 <- trimmed_data$eda[1:chunksize]
                trimmed_data$eda_package2 <- trimmed_data$eda[(chunksize+1): (chunksize * 2)]
                trimmed_data$eda_package3 <- trimmed_data$eda[(chunksize* 2 +1):(chunksize * 3)]
                trimmed_data$eda_package4 <- trimmed_data$eda[(chunksize * 3 + 1):number_of_values]

                trimmed_data$ecg_package1 <- trimmed_data$ecg[1:chunksize]
                trimmed_data$ecg_package2 <- trimmed_data$ecg[(chunksize+1): (chunksize * 2)]
                trimmed_data$ecg_package3 <- trimmed_data$ecg[(chunksize* 2 +1):(chunksize * 3)]
                trimmed_data$ecg_package4 <- trimmed_data$ecg[(chunksize * 3 + 1):number_of_values]

                trimmed_data$pleth_package1 <- trimmed_data$pleth[1:chunksize]
                trimmed_data$pleth_package2 <- trimmed_data$pleth[(chunksize+1): (chunksize * 2)]
                trimmed_data$pleth_package3 <- trimmed_data$pleth[(chunksize* 2 +1):(chunksize * 3)]
                trimmed_data$pleth_package4 <- trimmed_data$pleth[(chunksize * 3 + 1):number_of_values]

                #data$time <- 0:1/1000:(sizex-1)/1000
                # data$event$samplestamp <- rep(0,numberOfEvents)
                # data$event$type        <- c()
                # data$event$comment     <- c()
                #
                # dataoc$event$samplestamp <- rep(0,numberOfEvents)
                # dataoc$event$type        <- c()
                # dataoc$event$comment     <- c()


                for(i in 0: (numberOfEvents-1)){
                        raw_event <- J(eventRaw, "get", as.integer(i))
                        raw_eventoc <- J(eventocRaw, "get", as.integer(i))

                        # 30000
                        dataoc$event$samplestamp[i+1] <- J(raw_eventoc, "getSamplestamp") - J(J(eventocRaw, "get", as.integer(0)), "getSamplestamp") + 100
                        dataoc$event$type[i+1]        <- '0'
                        dataoc$event$comment[i+1]     <- as.character(J(raw_eventoc, "getComment"))

                        # 30000
                        data$event$samplestamp[i+1] <- J(raw_event, "getSamplestamp") - J(J(eventocRaw, "get", as.integer(0)), "getSamplestamp") + 100
                        data$event$type[i+1]        <- '0'
                        data$event$comment[i+1]     <- as.character(J(raw_event, "getComment"))

                }



                evententry_marker$data <- data$event
                evententryoc_marker$data <- dataoc$event
        }

        #Adjust the timestamp
        timestampnew <- firstEventIndex + J(timestamp_start, "getTime")
        newtimestampdate <- .jnew("java.util.Date", .jlong(timestampnew))

        newunisens <- list()
        newunisens$path           <- destFolder
        newunisens$measurementId  <- subject
        newunisens$name           <- subject
        newunisens$timestampStart <- newtimestampdate  #noch ueberarbeiten
        newunisens$comment        <- 'Trimmed Unisens File'
        unisens_utility_create(newunisens)

        J(j_unisens, "closeAll")

        #Now we add it to our Unisens file:
        signalentry_ecg$data <- trimmed_data$ecg_package1
        unisens_utility_add_signalentry(fullDestFolder, signalentry_ecg)
        signalentry_ecg$data <- trimmed_data$ecg_package2
        unisens_utility_append_signalentry(fullDestFolder, signalentry_ecg)
        signalentry_ecg$data <- trimmed_data$ecg_package3
        unisens_utility_append_signalentry(fullDestFolder, signalentry_ecg)
        signalentry_ecg$data <- trimmed_data$ecg_package4
        unisens_utility_append_signalentry(fullDestFolder, signalentry_ecg)

        signalentry_eda$data <- trimmed_data$eda_package1
        unisens_utility_add_signalentry(fullDestFolder, signalentry_eda)
        signalentry_eda$data <- trimmed_data$eda_package2
        unisens_utility_append_signalentry(fullDestFolder, signalentry_eda)
        signalentry_eda$data <- trimmed_data$eda_package3
        unisens_utility_append_signalentry(fullDestFolder, signalentry_eda)
        signalentry_eda$data <- trimmed_data$eda_package4
        unisens_utility_append_signalentry(fullDestFolder, signalentry_eda)

        signalentry_pleth$data <- trimmed_data$pleth_package1
        unisens_utility_add_signalentry(fullDestFolder, signalentry_pleth)
        signalentry_pleth$data <- trimmed_data$pleth_package2
        unisens_utility_append_signalentry(fullDestFolder, signalentry_pleth)
        signalentry_pleth$data <- trimmed_data$pleth_package3
        unisens_utility_append_signalentry(fullDestFolder, signalentry_pleth)
        signalentry_pleth$data <- trimmed_data$pleth_package4
        unisens_utility_append_signalentry(fullDestFolder, signalentry_pleth)

        unisens_utility_add_evententry(fullDestFolder, evententry_marker)
        unisens_utility_add_evententry(fullDestFolder, evententryoc_marker)
}

























