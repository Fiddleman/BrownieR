#' arrange_inputs(data, inputorder)
#' 
#' Function order the inputs as they are needed for further physio processings. Additional, all Inputs
#' which are not part of the parameter inputorder will be removed.
#' 
#' @param data a dataframe containing the tracked sensor data for one subject
#' @param inputorder a named character vector, names needs to be Input1 to Input8, values only "EKG, "BVP", or "EDA"
#' @return dataframe
#' @export
#' @examples  
#' inputorder <-c(EKG = "Input1", BVP = "Input2", EDA ="Input3")

arrange_inputs <- function(data, inputorder){
  if (!all(names(inputorder) %in% c("BVP", "EDA", "EKG"))){
    stop("Vectornames of parameter inputorder needs to be BVP, EDA or EKG, occured only one time and all three sensor types needs to be specified")
  } else if (!all(inputorder %in% c("Input1", "Input2", "Input3", "Input4","Input5", "Input6", "Input7", "Input8"))){
    stop("Values of parameter inputorder needs to be between Input1 and Input8")
  }
  else {
    # # part of new logic that doesn require all three datatypes to be tracked and specified, but their is a logic problem that it doesn sort the input yet...
    # colum_order <- c(1:3)
    # for (input in inputorder){
    #   temp <- as.integer(substr(inputorder[names(inputorder) == names(input)], 6, 6)) + 3
    #   colum_order <- c(colum_order,temp)
    # }
    EKG <- as.integer(substr(inputorder[names(inputorder) == "EKG"], 6, 6)) + 3
    BVP <- as.integer(substr(inputorder[names(inputorder) == "BVP"], 6, 6)) + 3
    EDA <- as.integer(substr(inputorder[names(inputorder) == "EDA"], 6, 6)) + 3
    colum_order <- c(1:3, EKG, BVP, EDA)
    data <- data[,colum_order]
    colnames(data)  <- c("ReceiveTime", "SampleTime", "PlugSeqNo", "Input1", "Input2", "Input3")
    return(data)
  }
}