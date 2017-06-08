#' take_screenshot(), Takes screenshots of URLs
#'
#' Takes screenshots of all URLs passed to the function arguments and saves them in a new folder
#' screenshots in the working directory. The filename is the url modified by the function
#' \code{link{url_to_filename}}.
#'
#' To get precise heatmaps with the function \code{link{plot_brownie}} it is indispensable to
#' set the width of the screenshots right. It's recommended to evalualte the max.pos in the data
#' before taking screenshots. Moreover, when executing the funciton \code{link{plot_brownie}} will
#' use the files saved in the folder screenshots.
#'
#' @param URLs a character vector; URLs must be full qualified containing schema (http or https)
#' @param width a numeric; width of screenshots in pixel. The width must be the width of the browser
#' viewport during the experiment!
#' @return vector of screenshot file names that where taken and saved in ./screenshots/
#' @examples
#' take_screenshot(c("https://www.visualvest.de/", "https://www.visualvest.de/angebot/", "https://www.visualvest.de/team/"), width = 1000)
#
take_screenshot <- function(urls = "http://www.google.de", width = 1000) {
  if (!requireNamespace("webshot", quietly = TRUE)) {
    return(stop("ERROR: The pkg webshot in conjunction with PhantomJS is
                   needed for this function to work. Please install both.
                   For further information visit webshot pkg help page.
                   To complete the installation of webshot, run the following
                   command: webshot::install_phantomjs() for screenshot functionality."))
  }
  filenames <- sapply(urls, function(x) url_to_filename(x))
  # screenshot making
  webshot::webshot(url = urls, file = filenames, vwidth = width)
  return(filenames)
  }

#' url_to_filename(), Converts a URL to a valid filename
#'
#' Function removes schema, query of URL and replace all "/" by "-".
#'
#' Function is necessary to provide a valide filename when saving the screenshots you take with
#' \code{link{take_screenshot}} of all pages propants visited.
#'
#' @param url a character; full qualified url to modify
#'
#' @return character
url_to_filename <- function(url) {
  withoutSchema <- sub(pattern = "\\https://|http://", replacement = "", x = url)
  withoutQuery <- sub(pattern = "\\?.*", replacement = "", x = withoutSchema)
  withoutSlash <- gsub("/", "-", withoutQuery)
  withoutPerc <- gsub("%", "", withoutSlash)
  return(paste("screenshots/", withoutPerc, ".png", sep = ""))
}
