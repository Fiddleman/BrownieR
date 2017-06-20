#' Heatmap plotting on website screenshot
#'
#' Function is one of the core functionalitys of the package. You can plot either the mouse motion or the clicks
#' on a website screenshot. See the Argument description for customization details.
#'
#' @param data a list of class web
#' @param url a character,pPath to the website you want to plot data points at.
#' It requires that a screenshot is taken with take_screenshots of url.
#' @param type a character, "motion" or "click". Selects the type of data to plot.
#' "motion" Selects only mouse movements, "click" only mouse clicks.
#' @param subject a integer vector including the subjects which shall be plotted
#' @param alpha a numeric, From zero to 1; to set the transparency of data points.
#' @param size a numeric, For type motion size=0.5 is recommended and for type click size=2.5 is a good setting to start.
#' It depends very much on the amount of data points.
#' @param color a color, Color of data points. Use a color which is most different than the website. See the R's color specifications for more details and possible values.
#' @return plot as png at folder "yourworkingdirectory/plots/plot.png"
#' @examples
#' plot(data, url="http://youwebsitetoplot.com/", type = "motion", subject = c(1, 2), alpha = 0.1, size = 0.5, color ="purple")
plot.web <-  function(data,  url, type = "motion", subject = c(1, 2), alpha = 0.1, size = 0.5, color ="purple", ...){
  screenshot_path <- url_to_filename(url)
  screenshot_list <- list.files(path = "screenshots", pattern = "*.png", full.names = T)
  if (!any(screenshot_list == screenshot_path)) {
    return(stop("ERROR: No screenshot of page to plot available. Please run the
                   function take_screenshot() to take a screenshot of url you want to plot"))
  }
  available_urls <- unique(data$URL)
  if (!any(available_urls == url)) {
    return(stop("ERROR: No data for url available. Please load brownie csv with (load_brownie_data) containg data to url"))
  }
  # Read in screenshot, equals backgroundimage for plot
  img <- png::readPNG(screenshot_path, info=T)
  # Determine screenshot size
  screenshot_width <- dim(img)[2]
  screenshot_height <- dim(img)[1]
  raster_img <- grid::rasterGrob(img, interpolate=T)
  # Read in data
  data_points <- transform.web(data = data, type = type, url = url, subject = subject)
  # Add min and max points to data to calibrate plot
  data_points <- rbind(data_points, c(screenshot_width, screenshot_height, subject[1]), c(0, 0, subject[1]))
  # Start plotting -----------
  heatmap <- ggplot2::ggplot(data_points, ggplot2::aes(X,Y)) +
    # xmax and ymin values are necessary for calibration again
    ggplot2::annotation_custom(raster_img, xmin=0, xmax= screenshot_width, ymin=-screenshot_height, ymax=0) +
    ggplot2::geom_point(alpha = alpha, size = size, color = color) +
    ggplot2::coord_fixed(ratio = 1) + #fix coord system
    ggplot2::scale_y_reverse() + #reverse data of y axis to start with zero at the top
    ggplot2::theme(legend.position="none",
          axis.text = ggplot2::element_blank(),
          axis.title = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_blank(),
          panel.background = ggplot2::element_blank())
  # Save plot ----------------------
  filename_heatmap <- gsub("screenshots/", "", screenshot_path)
  # Determine size of plot in inches:
  plot_width <- 0.003813559*screenshot_width #factor comes from try and error
  plot_height <- screenshot_height/screenshot_width*plot_width
  if (!dir.exists("plots")) dir.create("plots")
  ggplot2::ggsave(plot = heatmap, filename = filename_heatmap, path="plots", width = plot_width, height = plot_height)
  return(print(heatmap))
  }

#' transform.web, helper function
#'
#' Function gets called by \code{link{plot_brownie}} to calculate data points to plot on screenshot.
#'
#' Extracts data for specified subjects, URL and event. If type euqals "motion" than only data for the
#' Event MouseMotion is used (for type equals "click" only Event MouseButtonPressed is used).
#' The movement data is computed by the formulars:
#' \itemize{
#'  \item{
#'  X = H.ScrollPosition + X.Position
#'  }
#'  \item{
#'  X = H.ScrollPosition + X.Position
#'  }
#' }
#'
#' @param data a list of class web
#' @param url a character
#' @param type a character, "motion" or "click"
#' @param subject a integer vector including the subjects which should be considered for calculation
#' @return dataframe

transform.web <- function(data, url, type = "motion", subject = c(1, 2)) {
  data <- as.data.frame(unclass(data), stringsAsFactors = F)
  if (type == "click") {
    type_of_plot <- "MouseButtonPressed"
  }  else type_of_plot <- "MouseMotion"
  #if (all(session == "all")) session <- as.integer(unique(data$Session))
  data_pos <- subset(data, (URL == url) & (Event == type_of_plot) & (SUBJECT_ID_SUBJECT == subject), select = c("H.ScrollPosition", "X.Position", 
                                                                                                                "V.ScrollPosition", "Y.Position",
                                                                                                                "SUBJECT_ID_SUBJECT"))
  x <- data_pos$H.ScrollPosition + data_pos$X.Position
  y <- data_pos$V.ScrollPosition + data_pos$Y.Position
  data_points <- data.frame(X = x, Y = y, subject = data_pos$SUBJECT_ID_SUBJECT)
  return(data_points)
}