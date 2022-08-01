#' Parse NCBI BioSample metadata 
#' 
#' This function parss a txt file from the NCBI BioSample database.
#' @param file character; path to a txt file.
#' @returns a tibble.
#' @examples 
#' dontrun{
#' # search for Acinetobacter baumannii within the NCBI BioSample database
#' # https://www.ncbi.nlm.nih.gov/biosample/?term=acinetobacter+baumannii
#' # upper right corner -> send to -> file -> format = full (text) -> create file
#' # parse the downloaded file
#' ncbi_parse_biosample_txt("biosample_summary.txt")
#' }
#' @export
ncbi_parse_biosample_txt <- function(file) {
  foo <- function(x) {
    x <- strsplit(x, "\n")[[1]]
    ids <- x[grep("^Identifiers", x)]
    if (length(ids) == 1) {
      ids <- gsub("^Identifiers: *", "", ids)
      ids <- strsplit(ids, "; *")[[1]]
      id_list <- list()
      for(i in 1:length(ids)) {
        id <- strsplit(ids[i], ": *")[[1]]
        id[1] <- gsub(" ", "_", id[1])
        id[1] <- tolower(id[1])
        assign(id[1], id[2])
        id_list <- c(id_list, get(id[1]))
        names(id_list)[length(id_list)] <- id[1]
      }
    } else id_list <- list()
    id_df <- as.data.frame(id_list)
    index_attributes <- grep(" +/", x)
    attribute_list <- list()
    for (i in index_attributes) {
      atr <- gsub("^ +/", "", x[i])
      atr <- strsplit(atr, "=")[[1]]
      atr[1] <- gsub(" ", "_", atr[1])
      atr[1] <- gsub("#", "", atr[1])
      atr[1] <- tolower(atr[1])
      atr[2] <- strsplit(atr[2], '"')[[1]][2]
      if (atr[1] %in% c(names(id_list)) == FALSE) {
        assign(atr[1], atr[2])
        attribute_list <- c(attribute_list, get(atr[1]))
        names(attribute_list)[length(attribute_list)] <- atr[1]
      }
    }
    attribute_df <- as.data.frame(attribute_list)
    if(nrow(id_df > 0) & nrow(attribute_df) == 0) {
      out <- id_df
    }
    if(nrow(id_df == 0) & nrow(attribute_df) > 0) {
      out <- attribute_df
    }
    if(nrow(id_df > 0) & nrow(attribute_df) > 0) {
      out <- dplyr::bind_cols(id_df, attribute_df)
    }
    out <- tibble::as_tibble(out)
    if (nrow(out) > 1) {
      warning("More than one rows returned for a biosample:")
      print(out)
    }
    return(out)
  }
  lines <- readLines(file)
  index <- c(0, which(lines == ''))
  if (length(index) == 1) {
    entry <- paste(lines, collapse = "\n")
    parsed_txt <- list(entry)
  } else {
    parsed_txt <- vector()
    for (i in 2:length(index)) {
      entry <- paste(lines[(index[i-1]+1):(index[i]-1)], collapse = "\n")
      if (entry == "\n") next() else {
        parsed_txt <- c(parsed_txt, entry)
      }
    }
  }
  out <- lapply(parsed_txt, foo)
  out_consistent <- mean(sapply(out, nrow)) == 1
  if (out_consistent == FALSE) {
    warning("Number of rows may not match number of biosamples. Check.")
  }
  out <- dplyr::bind_rows(out)
  na_terms <- c(
    "missing", "na", "n/a", "none", "not applicable", "not collected", 
    "not_determined", "unknown", "NA")
  geo <- tolower(out$geographic_location)
  geo <- ifelse(geo %in% na_terms == FALSE, geo, NA)
  geo <- sapply(geo, function(x) {
    ifelse(!is.na(x), strsplit(x, ": *")[[1]][1], NA)
  })
  geo <- gsub(" +", "_", geo)
  out$country <- geo
  return(out)
}
