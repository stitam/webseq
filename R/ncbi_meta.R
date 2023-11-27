#' Parse NCBI sequence metadata
#' 
#' This function can be used to parse various non-sequence data sets from NCBI
#' into a tibble. The function currently supports parsing NCBI BioSample data
#' from XML format.
#' @param meta character; either a character vector containing a data set that
#' was retrieved through \code{rentrez::entrez_fetch()} or a path to an file
#' that was downloaded from NCBI.
#' @param db character; the NCBI database from which the data was retrieved.
#' Currently only \code{"biosample"} is supported.
#' @param format character; the format of the data set. Currently only
#' \code{"xml"} is supported.
#' @param verbose logical; Should verbose messages be printed to console?
#' @examples
#' \dontrun{
#' data(examples)
#' 
#' # NCBI BioSample, fully programmatic access
#' 
#' # Get internal BioSample UID for BioSample ID
#' biosample_uid <- get_uid(examples$biosample, db = "biosample")
#' # Get metadata in XML format
#' meta_xml <- rentrez::entrez_fetch(
#'   db = "biosample",
#'   id = biosample_uid$uid,
#'   rettype = "full",
#'   retmode = "xml"
#' )
#' # Parse XML
#' ncbi_meta(meta = meta_xml, db = "biosample", format = "xml")
#' 
#' # NCBI BioSample, download XML file from NCBI and parse
#' 
#' # Manually download the XML file
#' # https://www.ncbi.nlm.nih.gov/biosample/?term=SAMN02714232
#' # upper right corner -> send to -> file -> format = full (xml) -> create file
#' # Parse XML
#' ncbi_meta(meta = "biosample_result.xml", db = "biosample", format = "xml")
#' }
#' @export
ncbi_meta <- function(
  meta,
  db,
  format = "xml",
  verbose = getOption("verbose")
) {
  f <- get(paste("ncbi_meta", db, format, sep = "_"))
  if (db == "biosample" & format == "xml") {
    out <- f(meta, verbose)
  } else {
    out <- tibble::tibble()
  }
  return(out)
}
