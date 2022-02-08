#' Parse NCBI assembly metadata 
#' 
#' This function can be used to parse an xml file from the NCBI assembly
#' database into a tibble.
#' @param file character; path to an xml file.
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
    biosample <- x[grep("^Identifiers: BioSample", x)]
    if (length(biosample) == 1) {
      biosample <- stringi::stri_extract(
        biosample, regex = "BioSample: +[a-zA-Z]+[0-9]+")
      biosample <- strsplit(biosample, "BioSample: +")[[1]][[2]]
    } else biosample <- NA
    sra <- x[grep("SRA: ", x)]
    if (length(sra) == 1) {
      sra <- stringi::stri_extract(sra, regex = "SRA: +[a-zA-Z]+[0-9]+")
      sra <- strsplit(sra, "SRA: +")[[1]][[2]]
    } else sra <- NA
    organism <- x[grep("^Organism", x)]
    if (length(organism) == 1) {
      organism <- strsplit(organism, "^Organism: +")[[1]][[2]]
    } else orgamism <- NA
    hit <- x[grep("strain", x)]
    strain <- ifelse(length(hit) == 1, strsplit(hit, '"')[[1]][2], NA)
    hit <- x[grep("host", x)]
    host <- ifelse(length(hit) == 1, strsplit(hit, '"')[[1]][2], NA)
    hit <- x[grep("collection date", x)]
    collection_date <- ifelse(length(hit) == 1, strsplit(hit, '"')[[1]][2], NA)
    collection_day <- as.Date(collection_date, format = "%Y-%m-%d")
    hit <- x[grep("geographic location", x)]
    geographic_location <- ifelse(length(hit) == 1, strsplit(hit, '"')[[1]][2], NA)
    out <- tibble::tibble(
      biosample = biosample,
      sra = sra,
      organism = organism,
      strain = strain,
      host = host,
      collection_date = collection_date,
      collection_day = collection_day,
      geographic_location = geographic_location
    )
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
