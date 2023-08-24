#' Flag files to keep for analysis
#' 
#' Some functions may download files that only differ in their source (e.g. GCA
#' from GenBank assemblies or GCF for RefSeq assemblies) or their version number
#' (v1, v2, etc.). This function helps remove redundant files by flagging which
#' files should be kept for further analysis.
#' @param filenames character; a character vector of filenames. Currently the
#' function only supports GCA/GCF identifiers. Look at the examples for more
#' details.
#' @return The function returns a data frame where each file is listed in the
#' first column and the recommendation to keep the file for further analysis
#' is listed in the last column.
#' @details The function first prioritises GCF over GCA and then the highest
#' version number.
#' @examples
#' # keep GCF
#' filenames <- c("GCA_003012895.2_ASM301289v2_genomic.fna",
#'                "GCF_003012895.2_ASM301289v2_genomic.fna")
#' flag_files(filenames)                
#'                
#' # keep GCF even when version number is lower
#' filenames <- c("GCA_003012895.2_ASM301289v2_genomic.fna",
#'                "GCF_003012895.1_ASM301289v1_genomic.fna")
#' flag_files(filenames) 
#'                
#' filenames <- c("GCA_003012895.1_ASM301289v1_genomic.fna",
#'                "GCA_003012895.2_ASM301289v2_genomic.fna") 
#' flag_files(filenames) 
#' @export
flag_files <- function(filenames) {
  df <- data.frame(filename = filenames)
  df$source <- sapply(df$filename,
                      function(x) strsplit(x, split = "_")[[1]][1])
  df$accession <- sapply(df$filename,
                         function(x) strsplit(x, split = "_")[[1]][2])
  df$base <- sapply(df$accession,
                    function(x) strsplit(x, "\\.")[[1]][1])
  df$version <- sapply(df$accession,
                       function(x) strsplit(x, "\\.")[[1]][2])
  df$row <- 1:nrow(df)
  df$keep <- NA
  for (i in 1:nrow(df)) {
    if (!is.na(df$keep[i])) {
      next() 
    } else {
      cr <- df[which(df$base == df$base[i]),]
      if (nrow(cr) == 1) {
        df$keep[i] = TRUE 
      } else {
        cr <- cr[order(cr$source, cr$version, decreasing = TRUE),]
        cr$keep[1] <- TRUE
        cr$keep[2:nrow(cr)] <- FALSE
        df$keep[cr$row] <- cr$keep
      }
    }
  }
  df$assembly <- mapply(function(x,y) {
    paste(x, y, sep = "_") 
  }, df$source, df$accession) 
  df <- tibble::as_tibble(df[,c(
    "filename",
    "assembly", 
    "source",
    "accession",
    "base",
    "version",
    "keep"
  )])
  return(df)
}
