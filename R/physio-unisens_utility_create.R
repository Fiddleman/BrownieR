#' unisens r
#' @keywords internal
#' @import rJava
unisens_utility_create <- function(unisens){
        if(!is.null(unisens$name)){
                if (substr(unisens$path, start = nchar(unisens$path), stop= nchar(unisens$path)) != .Platform$file.sep){
                        unisens$path = paste0(unisens$path, .Platform$file.sep)

                }
                if(!dir.exists(paste0(unisens$path, unisens$name))){
                        dir.create(paste0(unisens$path, unisens$name), recursive = TRUE)}
                unisens$path <- file.path(unisens$path, unisens$name)

        }


        # create new unisens file
        j_unisensFactory <- J('org.unisens.UnisensFactoryBuilder', 'createFactory')
        j_unisens <- J(j_unisensFactory, 'createUnisens', unisens$path)

        # add information to unisens object
        .jcall(j_unisens,,'setTimestampStart', unisens$timestampStart)
        .jcall(j_unisens,, 'setMeasurementId', unisens$measurementId)
        .jcall(j_unisens,, 'setComment', unisens$comment)

        .jcall(j_unisens,, "save")
        .jcall(j_unisens,,"closeAll")

        # This is a workaround for the XMLNS problem: Read the XML file, add the
        # xmlns attribute when necessary and save the file.
        # xmlDoc <- xmlTreeParse(paste0(unisens$path, .Platform$file.sep, 'unisens.xml'))
        # d <- xmlRoot(xmlDoc)
        # d
        # if (is.null(xmlGetAttr(xmlRoot(xmlDoc), 'xmlns', NULL))){
        #         xmlAttrs(d) <- c(xmlns = 'http://www.unisens.org/unisens2.0')
        # }
        #
        # saveXML(d, paste0(unisens$path, .Platform$file.sep, 'unisens.xml'))

}



