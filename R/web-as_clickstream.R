#' as_clickstream() Convert to clickstream data
#'
#' Converts brownie data into clickstream data to do further analysis with the package clickstream.
#' With the clickstream package, it's possible to build markov models and plot transition graphs.
#' Additionally this function has a  implementation of the attribution models "first" or "last". Attribution models
#' describe how to deal with occurencies of more than one objective a subject does. In other words, which objective should be used
#' if the user completed more than one objective.
#'
#' Function keeps only URLs with events "URL Change" and "first URL", after that it creates a list where each list item
#' represents a subject. A subject is a progression of urls a participant follow.
#' A progression ends with the label of the first or last occurrence (attribution model) of an objective else it ends with "Defer".
#'
#' @param data a brownie dataframe
#' @param objectives can be either FALSE or a labeled vector of URLs
#' @param attribution a character, either "first" or "last"
#' @return list of class "clickstreams"
#' @examples
#' import("datafortestingplot/", "amz")
#' objective  <- c("Harry Potter Special angesehen!" = summary(amz_web)[41,1])
#'as_clickstream(amz_web, objective, "first")
#' 
#' 

as_clickstream <- function(data, objectives = F, attribution = "last"){
  if (!class(data) == "web") stop("data of class web is required")
  data <- as.data.frame(unclass(data), stringsAsFactors = F)
  temp_data  <- subset(data, (Event == "URL-Change") | (Event == "first URL"), select = c("URL", "SUBJECT_ID_SUBJECT"))
  clickstreams <- list()
  for(i in unique(temp_data$SUBJECT_ID_SUBJECT)){
    stream_to_add <- subset(temp_data, SUBJECT_ID_SUBJECT == i, "URL")
    stream_to_add <- as.vector(stream_to_add$URL)
    #Index for deleting pages after objective completion
    check <- stream_to_add %in% objectives
    pos <- c(1:length(stream_to_add))[check]
    if (attribution == "first"){
      del_pos <- pos[1] + 1
    } else if (attribution == "last"){
      del_pos <- pos[length(pos)] + 1
    } else stop("Please specify attribution model first or last")
    #Deleting every page after objective completion
    if (all(check == F)) { #Case: User did not reach any objective
      stream_to_add <- c(stream_to_add, "Defer")
    } else if (del_pos > length(stream_to_add)){ #Case: Ojective is last page and nothing needs to be removed only label needs to be set
      last_element <- stream_to_add[length(stream_to_add)]
      stream_to_add[length(stream_to_add)] <- names(objectives[objectives == last_element])
    } else if (!is.na(del_pos)){ #case: Their are pages the user visited after accomplished objective which needs to be removed.
      stream_to_add <- stream_to_add[-c(del_pos:length(stream_to_add))]
      last_element <- stream_to_add[length(stream_to_add)]
      stream_to_add[length(stream_to_add)] <- names(objectives[objectives == last_element])
    }
    stream_to_add <- list(stream_to_add)
    names(stream_to_add) <- paste0("SUBJECT_ID_SUBJECT", i)
    clickstreams <- c(clickstreams, stream_to_add)
  }
  class(clickstreams) <- "Clickstreams"
  return(clickstreams)
}
