#' Web Summary
#'
#' Implemented as additional method for the gerneic function summary() and all objects of class "web". Run this function to get a first overview of the behaviour of the propants during a web experiment.
#' Create a summary of type "click frequencies" for objectives equals FALSE or "objectives" for a vector
#' passed to the argument objectives.
#'
#' Summary "click frequency" has the following colums: URL, Sessions, Impressions, Impressions per Session, Duration.
#' Summary "objectives" has the colums Label, URL, Unique_Coversion, All_Conversions, Unique_Conversion-Rate,
#' and All_Conversion-Rate.
#' These metrics are an implemention of the industry standard in web analytics. In other words Google Analytics,
#' Piwik or Webtrekk for example use the same calculation techniques:
#' \itemize{
#'  \item{A Session is a unique page impression (URL call) of a propant (user) during an experiment (website visit)}
#'  \item{Impressions is the total number of page impressions}
#'  \item{Impressions per Visit is the average number of impressions per session; }
#'  \item{Duration is the average duration of a website visist per impression}
#'  \item{Unique Conversions is the number of sessions of the defined objective pages}
#'  \item{All Conversions is the number of impressions of the defined objective pages}
#'  \item{Unique Conversion-Rate is calculated by Unique Conversions divided by total number of impressions for all pages}
#'  \item{All Conversion-Rate is calculated by All Conversions divided by total number of impressions for all pages}
#' }
#'
#' @param data; a list of class web
#' @param objectives; can be either FALSE or a labeled vector of URLs
#' @return dataframe
#' @examples
#' objectives <- c(Fonds = "https://www.visualvest.de/fonds/",
#'                  Depot = "https://anlegen.visualvest.de/app/depot/")
#' summary(data = data, objectives = objectives)

summary.web <- function(data, objectives = F, ...){
  data <- as.data.frame(unclass(data), stringsAsFactors = F)
  if (!(objectives == F || is.character(objectives))){
    return(message("ERROR: Wrong declaration of objectives. Parameter Objectives has to be a boolean
                   FALSE or a labeled charater vector"))
  } else if (is.character(objectives)){
    return(summary_web_objectives(data, objectives))
  } else {
    return(summary_web_imp(data))
  }
}


#' Web Impression Summary
#' 
#' @param data, dataframe with web data
#' @return dataframe

summary_web_imp <- function(data){
  data  <- subset(data, (Event == "URL-Change") | (Event == "first URL"), select = c("URL", "Time", "SUBJECT_ID_SUBJECT"))
  urls <- unique(data$URL) #url list which is needed for further calculations
  # Calcualte Duration per Impression in Seconds
  i <- 1
  for (i in 1:nrow(data)){
    data$Duration_Imp[i] <- (data$Time[i]-data$Time[i+1])*(-1)
    i <- i + 1
  }
  # Calculate page data
  for (url in urls){
    # Subset data per url
    data_single_url <- subset(data, URL == url)
    # Impressions per url
    data$Impressions[data$URL == url] <- length(data_single_url$URL)
    # Sessions per url
    data$Sessions[data$URL == url] <- length(unique(data_single_url$SUBJECT_ID_SUBJECT))
    # Avg-duration per impression per url
    data$Duration[data$URL == url] <- round(mean(data_single_url$Duration_Imp, na.rm = T), 1)
  }
  # Remove colums and duplicated rows whiche are not longer needed
  # Reset colum enumeration, do ordering by impressions
  data <- unique(data[,-c(2:4)])
  #Calculate last KPI: Impressions per Session
  data$Imp_per_Session <- round(data$Impressions / data$Sessions, 1)
  click_frequencies <- data[order(data$Impressions, decreasing = T),]
  rownames(click_frequencies) <- NULL
  return(click_frequencies)
}

#' Web Objectives Summary
#' 
#' @param data; dataframe with web data
#' @param objectives; labeled vector of URLs
#' @return dataframe

summary_web_objectives <- function(data, objectives){
    # Select only relevant colums and pages which were defined as objective
    click_frequencies <- summary_web_imp(data)
    index_objectives <- click_frequencies$URL %in% objectives
    data <- click_frequencies[index_objectives, -c(4,5)]
    # Add label to df
    data <- cbind(Label = names(objectives), data)
    # Calculation
    sum_Impressions <- sum(click_frequencies$Impressions)
    All_Conversion_Rate <- sapply(data$Impressions, function(x){round(x/sum_Impressions, 2)})
    Unique_Conversions_Rate <- sapply(data$Sessions, function(x){round(x/sum_Impressions, 2)})
    data <- cbind(data, All_Conversion_Rate, Unique_Conversions_Rate)
    # Rename Colums
    names(data)[3] <- "Unique_Conversions"
    names(data)[4] <- "All_Conversions"
    return(data)
}
