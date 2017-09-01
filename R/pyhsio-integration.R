#' initalizeUnisens()
#' 
#' This function inialize Unisense using rJava.
#' 
#' @param no input
#' @return void

initalizeUnisens <-  function(){
  unisensdir <- file.path(system.file("java", package = "NeuroIS"))
  unisensfiles <- c("org.unisens.jar", "org.unisens.ri.jar")
  rJava::.jinit()
  # Old implementation, but unisens files are now bind to package
  # for(i in unisensfiles){
  #   rJava::.jaddClassPath(file.path(unisensdir,i))
  # }
  
  # new implementation with system.file
  rJava::.jaddClassPath(system.file("java", "org.unisens.jar", package = "NeuroIS", mustWork = T))
  rJava::.jaddClassPath(system.file("java", "org.unisens.ri.jar", package = "NeuroIS", mustWork = T))
}
