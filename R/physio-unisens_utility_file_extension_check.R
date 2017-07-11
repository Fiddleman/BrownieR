#' unisens r
#' @keywords internal
#' #' @import rJava
unisens_utility_file_extension_check <- function(entryId, fileFormat){
        if(nchar(entryId) <= 3){
                r <- paste0(entryId, '.', tolower(fileFormat))
        }else if(!(tolower(substr(entryId, start = nchar(entryId) - nchar(fileFormat), stop = nchar(entryId)))==
                          tolower(paste0('.', fileFormat)))){
                r <- paste0(entryId, '.', tolower(fileFormat))
        }else{
                r <- entryId
        }
}
