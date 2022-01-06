#' Parse Multi-Locus Sequence Typing output files
#' 
#' This function will parse the output of MLST predictions into a data.frame
#' @param dirpath character; path to the directory which stores mlst output in
#' .json format.
#' @return a data.frame
#' @export
parse_mlst <- function(dirpath) {
  filepaths <- dir(dirpath, full.names = TRUE)
  filepaths <- filepaths[grep(".json$", filepaths)]
  out <- unname(sapply(filepaths, function(x) {
    data <- jsonlite::fromJSON(x)
    mlst <- data$mlst$results$sequence_type
    return(mlst)
  }))
  mlst <- data.frame(file = basename(filepaths), mlst = out)
  return(mlst)
}