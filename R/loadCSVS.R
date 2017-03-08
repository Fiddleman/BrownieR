#' load.csvs
#' @return This function loads all csv in the current working directory and saves them in a dataframe.
#' load.CSVS()
#' @export

load.csvs = function(){
  # Get the files names of all csvs in the working directory
  files = list.files(pattern="*.csv")
  # First apply read.csv, then rbind
  myfiles = do.call(rbind, lapply(files,
                                  function(file){
                                    data.temp = read.csv(file, stringsAsFactors = FALSE)
                                    data.temp = cbind(data.temp,file)
                                    data.temp} ))
}

