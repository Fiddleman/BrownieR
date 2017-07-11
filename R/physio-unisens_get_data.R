#' unisens r
#' @keywords internal
#' @import rJava
unisens_get_data <- function(path = NULL, entry = NULL, range = NULL){
        #UNISENS_GET_DATA    loads data

        if (!is.null(path)){
                path <- unisens_utility_path(path)
        }else{
                path <- unisens_utility_path()
        }

        j_unisensFactory <- J("org.unisens.UnisensFactoryBuilder", "createFactory")
        j_unisens <- J(j_unisensFactory, "createUnisens", path)
        entryId <- entry

        j_entry <- J(j_unisens, "getEntry", entryId)
        nSamples <- J(j_entry, "getCount")

        if (is.null(range)){
                pos <- 0
                nRead <- nSamples
        }else{
                stop("Need to check the scripts for further adjustments!")
        }

        if(j_entry@jclass == 'org/unisens/ri/EventEntryImpl' && J(J(j_entry, "getFileFormat"), "getFileFormatName") ==  'CSV'){
                         # EventEntry csv files can be read using unisens_utility_csv_read
                         rs <- unisens_utility_csv_read(j_entry, pos, nRead)
        }else{
                stop("Need to check the scripts for further adjustments!")
        }

        # Close the data set
        J(j_unisens, "closeAll")

        return(rs = rs)
}
