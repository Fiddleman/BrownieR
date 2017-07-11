#' Prepare and generate Unisens format
#'
#' Transform cut data and generate unisens model.
#'
#' @param pathCutData the folder where cut data is stored
#' @param pathUnisensData the unisens data folder where unisens data should be stored. Normally "./01_unisens_data".
#' @param subject subfolder name of the unisens data
#' @export
#' @examples
#' \dontrun{
#' cut_data <- "./00_rwa_data_cut"
#' unisens_data <- "./01_unisens_data"
#' subject <- "1101"
#' loadRawData(cut_data, unisens_data, subject)
#' }

loadRawData <- function(pathCutData, pathUnisensData, subject){
        cat("Loading raw data for subject ", subject)

        unisensFullDestFolder <- file.path(pathUnisensData, subject)
        if(!dir.exists(unisensFullDestFolder)){dir.create(unisensFullDestFolder, recursive = TRUE)}

        # load the csv data
        csv_data <- read.csv(file.path(pathCutData,  subject, paste0(subject, ".csv")), sep = ";",
                            header = TRUE, stringsAsFactors = FALSE)
        csv_data <- as.matrix(csv_data)
        csv_data_length <- nrow(csv_data)
        #timestampmillis <- csv_data[1,1] #less accurate
        timestampmillis <- as.numeric(csv_data[1,2]) # more accurate

        data <- list()
        #################################
        ## ECG-Data #####################
        #################################
        data$ecg <- (((csv_data[2:csv_data_length, 1] * 3) / 4096) / 1019)*10

        signalentry_ecg <- list()
        signalentry_ecg$fileFormat    <- 'bin'
        signalentry_ecg$entryId       <- 'ecg.bin'
        signalentry_ecg$adcResolution <- 16
        signalentry_ecg$adcZero       <- 0
        signalentry_ecg$sampleRate    <- 1000
        signalentry_ecg$lsbValue      <- 1
        signalentry_ecg$unit          <- 'V'
        signalentry_ecg$entryComment  <- 'ECG raw data'
        signalentry_ecg$contentClass  <- 'ECG'
        signalentry_ecg$channelNames  <- .jarray(c('Lead I'))
        signalentry_ecg$dataType      <- 'double'

        ecg_length <- length(data$ecg)
        chuncksize <- floor(ecg_length/4)
        data$ecg_package1 <- data$ecg[1:chuncksize]
        data$ecg_package2 <- data$ecg[(chuncksize + 1):(2 * chuncksize)]
        data$ecg_package3 <- data$ecg[(2 * chuncksize + 1):(3 * chuncksize)]
        data$ecg_package4 <- data$ecg[(3 * chuncksize + 1):ecg_length]

        #################################
        ## eda-Data #####################
        #################################
        data$eda <- (csv_data[2:csv_data_length, 2] * 25) / 4096

        signalentry_eda <- list()
        signalentry_eda$fileFormat    <- 'bin'
        signalentry_eda$entryId       <- 'eda.bin'
        signalentry_eda$adcResolution <- 16
        signalentry_eda$adcZero       <- 0
        signalentry_eda$sampleRate    <- 1000
        signalentry_eda$lsbValue      <- 1
        signalentry_eda$unit          <- paste0("\u03BC", "S")
        signalentry_eda$entryComment  <- 'EDA raw data'
        signalentry_eda$contentClass  <- 'EDA'
        signalentry_eda$channelNames  <- .jarray(c('EDA'))
        signalentry_eda$dataType      <- 'double'

        eda_length <- length(data$eda)
        chuncksize <- floor(eda_length/4)
        data$eda_package1 <- data$eda[1:chuncksize]
        data$eda_package2 <- data$eda[(chuncksize + 1):(2 * chuncksize)]
        data$eda_package3 <- data$eda[(2 * chuncksize + 1):(3 * chuncksize)]
        data$eda_package4 <- data$eda[(3 * chuncksize + 1):eda_length]

        #################################
        ## PLETH-Data ###################
        #################################
        data$pleth <- (csv_data[2:csv_data_length, 3] * 3) / 4096

        signalentry_pleth <- list()
        signalentry_pleth$fileFormat    <- 'bin'
        signalentry_pleth$entryId       <- 'pleth.bin'
        signalentry_pleth$adcResolution <- 16
        signalentry_pleth$adcZero       <- 0
        signalentry_pleth$sampleRate    <- 1000
        signalentry_pleth$lsbValue      <- 1
        signalentry_pleth$unit          <- 'mmHg'
        signalentry_pleth$entryComment  <- 'Pleth raw data'
        signalentry_pleth$contentClass  <- 'PLETH'
        signalentry_pleth$channelNames  <- .jarray(c('PLETH'))
        signalentry_pleth$dataType      <- 'double'

        pleth_length <- length(data$pleth)
        chuncksize <- floor(pleth_length/4)
        data$pleth_package1 <- data$pleth[1:chuncksize]
        data$pleth_package2 <- data$pleth[(chuncksize + 1):(2 * chuncksize)]
        data$pleth_package3 <- data$pleth[(2 * chuncksize + 1):(3 * chuncksize)]
        data$pleth_package4 <- data$pleth[(3 * chuncksize + 1):pleth_length]

        newunisens <- list()
        newunisens$path <- pathUnisensData
        newunisens$measurementId <- subject
        newunisens$name <- subject
        newunisens$timestampStart <- .jnew("java.util.Date", .jlong(timestampmillis))
        newunisens$comment <- "Generated Unisens File"

        unisens_utility_create(newunisens)

        # Now we add it to our Unisens file:
        signalentry_ecg$data <- data$ecg_package1
        unisens_utility_add_signalentry(paste0(newunisens$path, .Platform$file.sep, newunisens$name), signalentry_ecg)
        signalentry_ecg$data <- data$ecg_package2
        unisens_utility_append_signalentry(paste0(newunisens$path, .Platform$file.sep, newunisens$name), signalentry_ecg)
        signalentry_ecg$data <- data$ecg_package3
        unisens_utility_append_signalentry(paste0(newunisens$path, .Platform$file.sep, newunisens$name), signalentry_ecg)
        signalentry_ecg$data <- data$ecg_package4
        unisens_utility_append_signalentry(paste0(newunisens$path, .Platform$file.sep, newunisens$name), signalentry_ecg)

        signalentry_eda$data <- data$eda_package1
        unisens_utility_add_signalentry(paste0(newunisens$path, .Platform$file.sep, newunisens$name), signalentry_eda)
        signalentry_eda$data <- data$eda_package2
        unisens_utility_append_signalentry(paste0(newunisens$path, .Platform$file.sep, newunisens$name), signalentry_eda)
        signalentry_eda$data <- data$eda_package3
        unisens_utility_append_signalentry(paste0(newunisens$path, .Platform$file.sep, newunisens$name), signalentry_eda)
        signalentry_eda$data <- data$eda_package4
        unisens_utility_append_signalentry(paste0(newunisens$path, .Platform$file.sep, newunisens$name), signalentry_eda)

        signalentry_pleth$data <- data$pleth_package1
        unisens_utility_add_signalentry(paste0(newunisens$path, .Platform$file.sep, newunisens$name), signalentry_pleth)
        signalentry_pleth$data <- data$pleth_package2
        unisens_utility_append_signalentry(paste0(newunisens$path, .Platform$file.sep, newunisens$name), signalentry_pleth)
        signalentry_pleth$data <- data$pleth_package3
        unisens_utility_append_signalentry(paste0(newunisens$path, .Platform$file.sep, newunisens$name), signalentry_pleth)
        signalentry_pleth$data <- data$pleth_package4
        unisens_utility_append_signalentry(paste0(newunisens$path, .Platform$file.sep, newunisens$name), signalentry_pleth)

}
