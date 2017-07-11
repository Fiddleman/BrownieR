#' unisens r
#' @keywords internal
#' @import rJava
unisens_utility_add_signalentry <- function(path, unisens){

        j_unisensFactory <- J('org.unisens.UnisensFactoryBuilder', 'createFactory')
        j_unisens <- J(j_unisensFactory, 'createUnisens', path)

        # file extension check
        entryId <- unisens_utility_file_extension_check(unisens$entryId, unisens$fileFormat)

        # Check dataType
        if (is.character(unisens$dataType)){
                unisens$dataType = J("org.unisens.DataType", "fromValue", tolower(unisens$dataType))
        }


        # Create entry
        j_entry <- J(j_unisens, "createSignalEntry", entryId, unisens$channelNames,
                                                       unisens$dataType, unisens$sampleRate)

        # Required attributes
        J(j_entry, "setAdcResolution",  as.integer(unisens$adcResolution))
        J(j_entry, "setAdcZero", as.integer(unisens$adcZero))
        J(j_entry, "setLsbValue", unisens$lsbValue)
        J(j_entry, "setUnit", unisens$unit)
        J(j_entry, "setSampleRate", unisens$sampleRate)
        J(j_entry, "setComment", unisens$entryComment)
        J(j_entry, "setContentClass", unisens$contentClass)
        J(j_entry, "setDataType", unisens$dataType)

        # Optional attributes
        if ('sourceId' %in% names(unisens)){
                J(j_entry, "setSourceId", unisens$sourceId)
        }
        if ('source' %in% names(unisens)){
                J(j_entry, "setSource", unisens$source)
        }
        if ('baseline'%in% names(unisens)){
                J(j_entry, "setBaseline", unisens$baseline)
        }



        if (!is.null(unisens$data)){

                if (tolower(unisens$fileFormat) == 'bin'){
                        # Write BIN data

                        # j_entry$append(unisens$data)
                        # Do not use the Java method append() due to performance reasons (speed, memory)
                        file_name <- paste0(path, .Platform$file.sep, entryId)
                        # R all double type
                        f <- file(file_name, "ab")
                        writeBin(unisens$data, f)
                        close(f)
                }

        }else if(tolower(unisens$fileFormat) == 'csv'){
                # Write CSV data
                j_fileFormat = .jnew("org.unisens.ri.CsvFileFormatImpl")
                if(!('separator' %in% names(unisens))){
                        unisens$separator <- ";"
                }else if(tolower(unisens$separator) == '\t'){
                        # \t is the commen abbrevation for tabulator (ASCII 0x09), but
                        # Matlab cannot pass this value to the Java library. SPRINTF
                        # converts the string '\t' to ASCII 0x09.
                        unisens$separator <- sprintf('\t')
                }

                if (!('decimalSeparator' %in% names(unisens))){
                        unisens$decimalSeparator = '.'
                }

                J(j_fileFormat, "setSeparator", unisens$separator)
                J(j_fileFormat, "setDecimalSeparator", unisens$decimalSeparator)
                J(j_entry, "setFileFormat", unisens$j_fileFormat)
                # Do not use the Java method append() due to performance reasons (speed, memory)
                # not append??
                write.table(unisens$data, file.path(path, entryId), sep = unisens$separator,
                            dec = unisens$decimalSeparator, row.names = FALSE, col.names = FALSE, append = TRUE)
        }else if(tolower(unisens$fileFormat) == 'xml'){
                # write XML data
                j_fileFormat <- .jnew("org.unisens.ri.XmlFileFormatImpl")
                J(j_entry, "setFileFormat", j_fileFormat)
                J(j_entry, "append", unisens$data)
        }else{
                cat(sprintf('Unknown file format: %s\n', unisens$fileFormat))
        }


        J(j_unisens, "save")
        J(j_unisens, "closeAll")
        # This is a workaround for the XMLNS problem: Read the XML file, add the
        # xmlns attribute when necessary and save the file.
        # xmlDoc <- xmlTreeParse(paste0(unisens$path, .Platform$file.sep, 'unisens.xml'))
        # xmlRoot(xmlDoc)
        # if (is.null(xmlGetAttr(xmlRoot(xmlDoc), 'xmlns', NULL))){
        #         xmlAttrs(xmlRoot(xmlDoc)) <- c(xmlns = 'http://www.unisens.org/unisens2.0')
        # }
        #
        # saveXML(xmlDoc, paste0(unisens.path, .Platform$file.sep, 'unisens.xml'))

}
