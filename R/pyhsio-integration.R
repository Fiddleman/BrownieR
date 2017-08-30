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
  for(i in unisensfiles){
    rJava::.jaddClassPath(file.path(unisensdir,i))
  }
}