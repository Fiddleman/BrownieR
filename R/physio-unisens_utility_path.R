#' unisens r
#' @keywords internal
#' @import rJava
unisens_utility_path <- function(path = NULL){
        if (is.null(path)  ||  !is.character(path)){
                print('Select Unisens folder.')
                path <- file.choose()
        }else if(grepl(paste0(.Platform$file.sep, 'unisens.xml'), path, ignore.case = TRUE)){
                path <- gsub(paste0(.Platform$file.sep, 'unisens.xml'), '', path)
        }

        # check if path exists
        if (path == 0){
                stop('Abort...')
        }else if (!dir.exists(path)){
                path <- 0
                stop(cat('Path \'\'', path, '\'\' does not exist!'))
        }

        # check if unisens file exists
        if (!file.exists(file.path(path, 'unisens.xml'))){
                path <- 0
                stop(cat('Path \'\'', path, '\'\' is no regular Unisens path!'))

        }
        return(path)
}
