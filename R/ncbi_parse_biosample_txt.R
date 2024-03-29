#' Parse NCBI BioSample metadata 
#' 
#' This function parses a txt file from the NCBI BioSample database.
#' @param file character; path to a txt file.
#' @param resolve_na logical; replace strings that match NA terms with NA.
#' @param verbose logical; should verbose output be printed to console?
#' @returns a tibble.
#' @examples
#' \dontrun{
#' # search for Acinetobacter baumannii within the NCBI BioSample database
#' # https://www.ncbi.nlm.nih.gov/biosample/?term=acinetobacter+baumannii
#' # upper right corner -> send to -> file -> format = full (text) -> create file
#' # parse the downloaded file
#' ncbi_parse_biosample_txt("biosample_summary.txt")
#' }
#' @export
ncbi_parse_biosample_txt <- function(file,
                                     resolve_na = TRUE,
                                     verbose = getOption("verbose")) {
  foo <- function(x) {
    x <- strsplit(x, "\n")[[1]]
    # extract title
    title <- strsplit(x[1], ": +")[[1]]
    title <- paste(title[2:length(title)], collapse = ": ")
    # extract identifiers
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
    # extract organism
    org <- x[grep("^Organism", x)]
    if (length(org) > 0) {
      org <- gsub("^Organism: *", "", org)
      org_df <- data.frame(organism = org)
    }
    # extract attributes
    index_attributes <- grep(" +/", x)
    attribute_list <- list()
    for (i in index_attributes) {
      atr <- gsub("^ +/", "", x[i])
      atr <- strsplit(atr, "=")[[1]]
      atr[1] <- gsub(" ", "_", atr[1])
      atr[1] <- gsub("#", "", atr[1])
      atr[1] <- tolower(atr[1])
      atr[1] <- paste0("attr_", atr[1])
      atr[2] <- strsplit(atr[2], '"')[[1]][2]
      if (atr[1] %in% c(names(id_list)) == FALSE) {
        assign(atr[1], atr[2])
        attribute_list <- c(attribute_list, get(atr[1]))
        names(attribute_list)[length(attribute_list)] <- atr[1]
      }
    }
    if (length(attribute_list) > 0) {
      attribute_df <- as.data.frame(attribute_list)
    } else {
      attribute_df <- data.frame()
    }
    # extract description
    index_desc <- grep("^Description", x)
    if (length(index_desc) > 0) {
      index_accn <- grep("^Accession", x)
      desc <- paste(
        x[(index_desc + 1):(index_accn - 1)],
        collapse = " "
      )
      desc_df <- data.frame(description = desc)
    } else {
      desc_df <- data.frame()
    }
    out <- data.frame(title = title)
    if (nrow(id_df) > 0) {
      out <- dplyr::bind_cols(out, id_df)
    }
    if (nrow(org_df) > 0) {
      out <- dplyr::bind_cols(out, org_df)
    }
    if (nrow(attribute_df) > 0) {
      out <- dplyr::bind_cols(out, attribute_df)
    }
    if (nrow(desc_df) > 0) {
      out <- dplyr::bind_cols(out, desc_df)
    }
    out <- tibble::as_tibble(out)
    if (nrow(out) > 1) {
      warning("More than one rows returned for a biosample:")
      print(out)
    }
    return(out)
  }
  if (verbose) message("Parsing file.")
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
  if (resolve_na) {
    if (verbose) message("Resolving NA terms.")
    na_terms <- c(
      "missing", "na", "n/a", "none", "not applicable", "not collected", 
      "not determined", "not provided", "null", "restricted access",
      "unknown")
    out <- tibble::as_tibble(data.frame(lapply(out, function(x) {
      ifelse(tolower(x) %in% na_terms == FALSE, x, NA)
    })))
  }
  if (verbose) message("Done.")
  return(out)
}
