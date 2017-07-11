#' Filt ecg data
#'
#' Run filters, to transform the ECG data to Wavelet form
#' @param destfolder the trimmed unisens data folder
#' @param subject the subfolder of the trimmed unisens data
#' @details The results could be deviated slightly, as the functions in R-package "signal" may not deliver the same output as the matlab.
#' @export
#' @import waveslim
#' @references http://www.anslab.net/doku.php?id=openanslab

runFiltersEcgData <- function(destfolder, subject){
        cat('Running ECG filters on unisens data for subject \"', subject, '\"')
        fullDestFolder <- file.path(destfolder, subject)

        ##################################
        ### Creat Unisens Instance #######
        ##################################
        j_unisensFactory <-  J("org.unisens.UnisensFactoryBuilder", "createFactory")
        j_unisens <-  J(j_unisensFactory, "createUnisens", fullDestFolder)

        if (file.exists(file.path(fullDestFolder, 'ecg_tme.bin'))){
                j_entry <-  J(j_unisens, "getEntry", 'ecg_tme.bin')
                J(j_unisens, "deleteEntry", j_entry)
        }

        J(j_unisens, "save")

        ##################################
        ### ECG-Data #####################
        ##################################
        j_entry <-  J(j_unisens, "getEntry", "ecg.bin")
        numberOfEcgValues <-  J(j_entry, "getCount")

        data <- list()
        chunksize <-  floor(numberOfEcgValues/4)
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

        signalentry_ecgtme <- list()
        signalentry_ecgtme$fileFormat    <-  'bin'
        signalentry_ecgtme$entryId       <-  'ecg_tme.bin'
        signalentry_ecgtme$adcResolution <-  16
        signalentry_ecgtme$adcZero       <-  0
        signalentry_ecgtme$sampleRate    <-  1000
        signalentry_ecgtme$lsbValue      <-  1
        signalentry_ecgtme$unit          <-  'V'
        signalentry_ecgtme$entryComment  <-  'filtered ecg data'
        signalentry_ecgtme$contentClass  <-  'ECG'
        signalentry_ecgtme$channelNames  <-  .jarray(c('Lead I'))
        signalentry_ecgtme$dataType      <-  'double'


        #Initialize Wavelet transformation for ECG signal
        J <- 8
        wavelet <- 'haar'
        boundary_condition <- 'periodic'

        # different from the results in matlab at the beginning of the data
        data$ecg <- filthigh(data$ecg,40,1000,7)


        #wavelet transformation
        ecg.haar <- modwt(data$ecg, wavelet, J, boundary_condition)

        djt <- imodwt(ecg.haar)
        tmp_ecg_tme <- djt
        data$ecg_tme <- tmp_ecg_tme * 2 #increase amplitudes a bit

        chunksize <- floor(numberOfEcgValues/4)
        data$ecg_tme_package1 <- data$ecg_tme[1:chunksize]
        data$ecg_tme_package2 <- data$ecg_tme[(chunksize + 1):(2 * chunksize)]
        data$ecg_tme_package3 <- data$ecg_tme[(2 * chunksize + 1): (3 * chunksize)]
        data$ecg_tme_package4 <- data$ecg_tme[(3 * chunksize + 1): numberOfEcgValues]

        #plot(seq(0, 1, length.out = 1000), data$ecg_tme[1:1000], "l")


        # Now we add it to our Unisens file:
        signalentry_ecgtme$data <- data$ecg_tme_package1
        unisens_utility_add_signalentry(fullDestFolder, signalentry_ecgtme)
        signalentry_ecgtme$data <- data$ecg_tme_package2
        unisens_utility_append_signalentry(fullDestFolder, signalentry_ecgtme)
        signalentry_ecgtme$data <- data$ecg_tme_package3
        unisens_utility_append_signalentry(fullDestFolder, signalentry_ecgtme)
        signalentry_ecgtme$data <- data$ecg_tme_package4
        unisens_utility_append_signalentry(fullDestFolder, signalentry_ecgtme)

        J(j_unisens, "closeAll")


}
