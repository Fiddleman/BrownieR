#' Anslab matlab scripts
#' @import signal
#' @keywords internal
filthigh <- function(y1, freq, samplerate = NULL, order = NULL){

        if(is.null(order)){order=5}
        if(is.null(samplerate)){samplerate=2}

        freq <- freq/(samplerate/2)
        filter <-  butter(order, freq, "low")
        y2 <- filtfilt(filter , y1)

        return(y2)
}




