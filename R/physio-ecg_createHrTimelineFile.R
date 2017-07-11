#' Generate event-based heart rate data
#'
#' Generate a csv file that stored event-based heart rate data.
#'
#' @details Note: This function depends highly on application!
#' @param unisensDestDataFolder the trimmed unisens data folder where all unisens data are stored. Normally "./02_unisens_trimmed".
#' @param subject subfolder name of the unisens data.
#' @param outfilename desired output file name plus extension.
#' @export
#' @examples
#' \dontrun{
#' unisens_folder <- "./02_unisens_trimmed"
#' subject <- "1101"
#' outfilename <- "hr.csv"
#' ecg_createHrTimelineFile(unisens_folder, subject, outfilename)
#' }

ecg_createHrTimelineFile <- function(unisensDestDataFolder, subject, outfilename){

        unisensFullSourceFolder <- file.path(unisensDestDataFolder, subject)

        triggerEntryId   <- 'offline_trigger.csv'
        markerEventId    <- 'marker_oc.csv'
        period <- 0
        periodbool <- FALSE
        quest <- 0
        questbool <- FALSE

        # ## Creat Unisens Instance
        markerList <- unisens_get_data(unisensFullSourceFolder, markerEventId, NULL)
        markerSampleRate <- 1000
        markerStamp <- markerList$samplestamp / markerSampleRate
        numberOfTimelineEntries <- ceiling(markerStamp[length(markerStamp)])

        triggerList <- unisens_get_data(unisensFullSourceFolder, triggerEntryId, NULL)
        triggerSampleRate <- 1000
        triggerStamp <- triggerList$samplestamp

        # Create heart beats per second
        triggerStamp <- (triggerStamp/triggerSampleRate) * 1000
        triggerStampDiff <- diff(triggerStamp)
        ep <- 1 # epoch size <- 1/ep sec
        triggerStampDiffIbi <- epoch(triggerStamp, triggerStampDiff, (triggerSampleRate/ep)) # heart period

        # Create data structure
        cell <- rep(0, numberOfTimelineEntries)
        timeline <- data.frame(id = seq(from = 1, to = numberOfTimelineEntries),
                               state = cell,
                               auction = cell,
                               round = cell,
                               offset = cell,
                               heartrate = cell,
                               stringsAsFactors = FALSE)

        # find string
        # replace ~cellfun(@isempty, regexp(text, pattern)
        findstring <- function(text, pattern){
                return(any(grepl(pattern, text, ignore.case = TRUE)))
        }
        ### Fill each second
        for(i in 1:numberOfTimelineEntries){
                ind <- which(markerList$samplestamp <= (i * markerSampleRate))
                ind <- ind[length(ind)]
                latestComment <- markerList$comment[ind]
                ### get current state
                if (is.null(latestComment) || length(latestComment) == 0 || (latestComment == "")){
                        returnValue <- 'NA'
                }else if(findstring(latestComment, 'ICD_Screen_Enter')){
                        period <- 0
                        periodbool <- TRUE
                        returnValue <- 'ICD'
                }else if(findstring(latestComment, 'InitialScreen')){
                        returnValue <- 'InitialScreen'
                        periodbool <- TRUE
                }else if(findstring(latestComment, 'ChoiceScreen')){
                        if(periodbool){
                                period <- period + 1
                                periodbool <- FALSE
                        }
                        returnValue <- paste0('ChoiceScreen_',period)
                }else if(findstring(latestComment, 'ChoiceScreenWithNonDecision')){
                        if(periodbool){
                                period <- period + 1
                                periodbool <- FALSE
                        }
                        returnValue <- paste0('ChoiceScreenWithNonDecision_',period)
                }else{
                        returnValue <- 'NA'
                }

                timeline$state[i] <- returnValue

                if(i==1 || timeline$state[i] != timeline$state[i-1]){
                        timeline$offset[i] <- 1
                }else{
                        timeline$offset[i] <- timeline$offset[i-1] + 1
                }


                # add heart rate
                if(length(triggerStampDiffIbi) >= i){
                        if(!is.nan(triggerStampDiffIbi[i])){
                                if (triggerStampDiffIbi[i] > 0 && triggerStampDiffIbi[i] <= 2000){
                                        returnValue <- (60 * 1000 / triggerStampDiffIbi[i])
                                }else{
                                        returnValue <- 0
                                }
                        }else{
                                returnValue <- 0
                        }


                }else{
                        returnValue <- -1
                }

                timeline$heartrate[i] <- round(returnValue, 2)
                #timeline.heartrateMod(i) <- fix(mod(returnValue, 1) * 10000)
        }

        writeToCSV(unisensFullSourceFolder, outfilename, timeline)

}
