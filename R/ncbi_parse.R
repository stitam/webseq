#' Parse NCBI sequence metadata
#' 
#' This function can be used to parse various retrieved non-sequence data sets
#' from NCBI into a tibble. These data sets usually accompany the biological
#' sequences and contain additional information e.g. identifiers, information
#' about the sample, the sequencing platform, etc.
#' @param meta character; either an unparsed metadata object returned by
#'  \code{ncbi_get_meta()} or the path to a file that was downloaded from NCBI.
#' @param db character; the NCBI database from which the data was retrieved.
#' @param format character; the format of the data set. Currently only
#' \code{"xml"} is supported.
#' @param verbose logical; Should verbose messages be printed to console?
#' @return a tibble.
#' @details This function is integrated into \code{ncbi_get_meta()} and is 
#' called automatically if \code{parse = TRUE} (default). However, it can also
#' be used  separately e.g. when you want to examine the unparsed metadata
#' object before parsing, or when you already downloaded the metadata manually
#' and you just want to parse it into a tabular format.
#' @details If \code{meta} is an unparsed \code{ncbi_meta} object returned by 
#' \code{ncbi_get_meta()} then the \code{db} argument is optional. If \code{db} 
#' is not specified, the function will extract it automatically. However, if it
#' is specified, it must be identical to the \code{db} attribute of the metadata
#' object. If \code{meta} is not an \code{ncbi_meta} object, the \code{db}
#' argument is required.
#' @examples
#' \dontrun{
#' data(examples)
#' #' 
#' # NCBI Assembly, download XML file from NCBI and parse
#' 
#' # Manually download the XML file
#' # https://www.ncbi.nlm.nih.gov/assembly/GCF_000299415.1
#' # upper right corner -> send to -> file -> format = xml -> create file
#' # Parse XML
#' ncbi_parse(meta = "assembly_summary.xml", db = "assembly", format = "xml")
#' 
#' # NCBI BioSample, fully programmatic access, separate retrieval and parsing
#' 
#' # Get metadata but do not parse
#' meta <- ncbi_get_meta(examples$biosample, db = "biosample", parse = FALSE)
#' # Parse metadata separately, the function will extract 'db' automatically.
#' ncbi_parse(meta = meta)
#' 
#' # NCBI BioSample, download XML file from NCBI and parse
#' 
#' # Manually download the XML file
#' # https://www.ncbi.nlm.nih.gov/biosample/?term=SAMN02714232
#' # upper right corner -> send to -> file -> format = full (xml) -> create file
#' # Parse XML
#' ncbi_parse(meta = "biosample_result.xml", db = "biosample", format = "xml") 
#' }
#' @export
ncbi_parse <- function(
  meta,
  db = NULL,
  format = "xml",
  verbose = getOption("verbose")
) {
  if ("ncbi_meta" %in% class(meta)) {
    if (is.null(db)) {
      db <- attributes(meta)$db
    } else {
      if (db != attributes(meta)$db) {
        msg <- paste0(
          "'db' for retrieved meta data does not match 'db' argument.\n",
          "Provide identical values or use db = NULL (default)."
        )
        stop(msg)
      }
    }
  }
  db <- match.arg(db, choices = c("assembly", "biosample"))
  format <- match.arg(format, choices = c("xml"))
  f <- get(paste("ncbi_parse", db, format, sep = "_"))
  if (db == "assembly" && format == "xml") {
    out <- f(meta, verbose = verbose)
  } else if (db == "biosample" && format == "xml") {
    out <- f(meta, verbose = verbose)
  } else {
    if (verbose) message("Parsing is not supported.")
    out <- NA_character_
  }
  return(out)
}
