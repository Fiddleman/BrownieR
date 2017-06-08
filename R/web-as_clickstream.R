#' Convert to clickstream data
#'
#' Converts brownie data into clickstream data to do further analysis with the package clickstream.
#' With the clickstream package, it's possible for e.g. possible to build markov models and plot transition graphs.
#' Additionally this function has a useful implementation of the attribution models "first" or "last". Attribution models
#' describe how to deal with occurencies of more than one objective in a session. In other words, which objective should be used
#' if the user achieves more than one objective.
#'
#' Function keeps only URLs with events "URL Change" and "first URL", after that it creates a list where each list item
#' represents a session. One session is a progression of urls a participant follow.
#' A progression ends with the label of the first or last occurrence (attribution model) of an objective else it ends with "Defer.
#'
#' @param data a brownie dataframe
#' @param objectives can be either FALSE or a labeled vector of URLs
#' @param attribution a character, either "first" or "last"
#' @return list of class "clickstreams"
#' @examples
#' df <- read_brownie_data(path = "/data", removeQuery = T)
#' 
#' 


# Needs to be splitted in several functions and is still not yet checked

as_clickstream <- function(data, objectives, attribution){
  temp_data  <- subset(data, (Event == "URL-Change") | (Event == "first URL"), select = c("URL", "Session"))
  clickstreams <- list()
  for(i in unique(temp_data$Session)){
    stream_to_add <- subset(temp_data, Session == i, "URL")
    stream_to_add <- as.vector(stream_to_add$URL)
    #Index for deleting pages after objective completion
    check <- stream_to_add %in% objectives
    pos <- c(1:length(stream_to_add))[check]
    if (attribution == "first"){
      del_pos <- pos[1] + 1
    } else if (attribution == "last"){
      del_pos <- pos[length(pos)] + 1
    } else stop("Please specify attribution model first or last", call. = FALSE)
    #Deleting every page after objective completion
    if(del_pos > length(stream_to_add)){ #Case: Ojective is last page and nothing needs to be removed only label needs to be set
      last_element <- stream_to_add[length(stream_to_add)]
      stream_to_add[length(stream_to_add)] <- names(objectives[objectives == last_element])
    } else if (!is.na(del_pos)){ #case: Their are pages the user visited after accomplished objective which needs to be removed.
      stream_to_add = stream_to_add[-c(del_pos:length(stream_to_add))]
      last_element <- stream_to_add[length(stream_to_add)]
      stream_to_add[length(stream_to_add)] <- names(objectives[objectives == last_element])
    } else { #Case: User did not reach any objective
      stream_to_add <- c(stream_to_add, "Defer")
    }
    stream_to_add <- list(stream_to_add)
    names(stream_to_add) <- paste0("Session", i)
    clickstreams <- c(clickstreams, stream_to_add)
  }
  class(clickstreams) <- "Clickstreams"
  return(clickstreams)
}
