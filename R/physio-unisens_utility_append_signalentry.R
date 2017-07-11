#' unisens r
#' @keywords internal
#' @import rJava
unisens_utility_append_signalentry <- function(path, unisens){
        j_unisensFactory <- J("org.unisens.UnisensFactoryBuilder", "createFactory")
        j_unisens <- J(j_unisensFactory, "createUnisens", path)

        # file extension check
        entryId <- unisens_utility_file_extension_check(unisens$entryId, unisens$fileFormat)
        j_entry <- J(j_unisens, "getEntry", entryId)
        J(j_entry, "append", unisens$data)
        J(j_unisens, "closeAll")
}
