#' unisens r
#' @keywords internal
#' @import rJava
unisens_utility_csv_read <- function(j_entry, pos = NULL, len = NULL){
        #UNISENS_CSV_READ reads an EventEntry from a CSV file

        nSamples <- J(j_entry, "getCount")
        if (is.null(pos) && is.null(len)){
                pos <- 0
                len <- nSamples
        }else if(!is.null(pos) && !is.null(len) && (pos + len) > nSamples){
                len <- nSamples - pos
        }

        # getting the path
        path <- paste0(J(J(j_entry, "getUnisens"), "getPath"), J(j_entry, "getId"))

        # checking the delimiter
        delimiter <- J(J(j_entry, "getFileFormat"), "getSeparator")

        # read the whole CSV file and transpose the results
        rs <- read.csv(path, header = FALSE,  sep = delimiter, stringsAsFactors = FALSE,
                       col.names = c("samplestamp", "type", "comment"))

        # trim start position
        if(pos > 0){
                rs <- rs[-(1:pos), ]

        }


        # trim length
        if(len < length(rs$samplestamp)){

                rs <- rs[-((len + 1): nrow(rs)), ]
        }
        return(rs)

}
