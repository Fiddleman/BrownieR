#' Set trigger for ecg
#'
#' Set Triggers wherever there are peaks.
#' @param destFolder the trimmed unisens data folder.
#' @param subject the subfolder of the trimmed unisens data.
#' @details The results could be deviated slightly, as the functions in R-package "signal" may not deliver the same output as the matlab. The output of this function could also be the input of e.g. R-package "RHRV" based on some modifications. This will be considered in the future version.
#'
#' @references http://www.anslab.net/doku.php?id=openanslab
#' @import graphics
#' @import signal
#' @export
ecg_setTriggersDataOpenAnslab <- function(destFolder, subject){
        fullDestFolder <- file.path(destFolder, subject)
        cat('Setting triggers on unisens ecg data for ', subject)

        ARTIFACT_THRESHOLD <- 80
        ecgEntryId     <- 'ecg_tme.bin'
        triggerEntryId <- 'offline_trigger.csv'
        SR <- 1000

        ##################################
        ### Creat Unisens Instance #######
        ##################################
        j_unisensFactory <- J("org.unisens.UnisensFactoryBuilder", "createFactory")
        j_unisens <- J(j_unisensFactory, "createUnisens", fullDestFolder)

        #j_unisens <- j_unisensFactory.openUnisens(fullDestFolder)

        if (file.exists(file.path(fullDestFolder, triggerEntryId))){
                j_entry <- J(j_unisens ,"getEntry", triggerEntryId)
                J(j_unisens, "deleteEntry", j_entry)
        }


        J(j_unisens, "save")

        data <- list()

        ##################################
        ### ECG-Data #####################
        ##################################
        #j_entry <- j_unisens.getEntry(ecgEntryId)
        j_entry <- J(j_unisens, "getEntry", 'ecg_tme.bin')
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

        data$ecg_tme <- c(ecg1)
        data$ecg_tme[(chunksize + 1):(chunksize * 2)] <- c(ecg2)
        data$ecg_tme[(chunksize * 2 + 1):(chunksize * 3)] <- c(ecg3)
        data$ecg_tme[(chunksize * 3 + 1):numberOfEcgValues] <- c(ecg4)


        #resample everything to 400 Hz
        EC <- data$ecg_tme

        #diffrent in matlab: algorithms
        EC <- resample(EC, p = 400, q = SR )

        if (is.null(EC)){
                return()
        }

        ecglen <- length(EC)

        #*** ECG analysis settings (program FINDRTQ5)
        lowfreq <- 40     #  (40) Filter frequency cutoff for low pass filter (decrease down to 20 possible, if lower, RSA less accurate)

        # Secondary settings
        area <- 150       # (150) Peak detection window, 200 detects succeeding peaks at IBIs of 200/400 msec
        mingap <- 150     # (150) Minimal gap between peaks, if less: the higher one is chosen
        back <- 15        #  (15) A slope 'back' points before peak to peak has to be greater than 'rise'
        rise <- 10        #  (10) Fixed 'rise' in mV if 'dyn' is set to 0
        risefactor <- 0.5 # (0.5) Automatic threshold, ratio from maximal slope
        q_range <- 18     #  (18) Window for Q-point search backward from R
        q_fixed <- 10     #  (10) If qfindyes is 0: set Q-point q_fixed sample points before R-wave
        scalefact <- 1    #(100000) Scaling factor for ECG raw signal


        #*** R wave detection
        #findrtq5
        #*** Initialization
        upsrfac <- 1
        sr_orig <- 400    # samplerate of original ECG data
        ep <- 4           # epoch size  <-  1/ep sec
        srfac <- sr_orig/400   # adjust detction parameters (originally based on 400 Hz signal)
        sr <- sr_orig * upsrfac
        area <- round(area * srfac * upsrfac)
        mingap <- area
        back <- round(back * srfac * upsrfac)
        q_range <- round(q_range * srfac * upsrfac)
        q_fixed <- round(q_fixed * srfac * upsrfac)

        print('SOMETHING HAPPENING!')

        y <- EC
        print('Lowpass filter')
        y <- filthigh(y, lowfreq, sr_orig, 7)
        y <- y * scalefact


        if(length(y) > (5000 * srfac * upsrfac)){
                n <- y[(3000 * srfac * upsrfac):(5000 * srfac * upsrfac)]
                n2 <- EC[(3000 * srfac):(5000 * srfac)] * scalefact
        }else{
                n <- y[(200 * srfac * upsrfac):length(y)]
                n2 <- EC[(200 * srfac):length(y)] * scalefact
        }

        rise <- (max(n)-mean(n))/2.5
        threshold <- max(-diff(n)) * risefactor

        t <- (1:length(n))/sr
        plot(t, n, type = "l")
        axis(1, at = 0:5, labels  = seq(from = 0, to = length(n), by = sr))
        title(xlab = "sec", main = paste0('Filtered ECG.  Slope criterion =  ', rise))
        lines(t, rep(1, length(t)) * rise, type = "o", col = "blue")


        thresh_ok <- readline('Criterion ok? (1 <- yes [default], 0 <- no)   ==> ')
        if(thresh_ok != ""){
                if (as.numeric(thresh_ok) == 0){
                        rise1 <- readline('New criterion  ==>  ')
                        if(rise1 != ""){
                                rise <- as.numeric(rise1)
                        }
                }
        }

        threshold <- -threshold

        remove(EC)
        #*** Detect maxima
        print(' ')
        print('Detection of R-waves')
        max_wave <- findmax(y, area = area ,back = back, rise = rise)
        rt <- max_wave$mt
        rv  <- max_wave$mv
        print(cat(length(rt),' R-waves were detected\n'))

        triggerList  <-  rt * 2.5


        typeCell  <-  rep("N", length(triggerList))
        commentCell  <-  rep("", length(triggerList))

        triggerStruct  <-  list('samplestamp' = as.integer(triggerList), 'type' = typeCell, 'comment' = commentCell)

        # edit unisens.xml
        # write information to unisens.xml
        evententry <- list()
        evententry$fileFormat  <-  'csv'
        evententry$entryId  <-  triggerEntryId
        evententry$sampleRate  <-  1000
        evententry$entryComment  <-  'QRS trigger list by OpenANSLab at 1000 Hz'
        evententry$contentClass  <-  'TRIGGER'
        evententry$dataType  <-  J("org.unisens.DataType", "fromValue", "double")
        evententry$data  <-  triggerStruct
        evententry$source  <-  'OpenANSLab@1000Hz'

        unisens_utility_add_evententry(fullDestFolder, evententry)

}
