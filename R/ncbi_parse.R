#' Parse NCBI sequence metadata
#' 
#' This function can be used to parse various non-sequence data sets from NCBI
#' into a tibble. These data sets usually accompany the biological sequences and
#' contain additional information e.g. identifiers, information about the
#' sample, the sequencing platform, etc. The function currently supports parsing
#' NCBI BioSample data from XML format.
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
#' #' 
#' # NCBI Assembly, download XML file from NCBI and parse
#' 
#' # Manually download the XML file
#' # https://www.ncbi.nlm.nih.gov/assembly/GCF_000299415.1
#' # upper right corner -> send to -> file -> format = xml -> create file
#' # Parse XML
#' ncbi_parse(meta = "assembly_summary.xml", db = "assembly", format = "xml")
#' 
#' # NCBI BioSample, fully programmatic access
#' 
#' # Get internal BioSample UID for BioSample ID
#' biosample_uid <- get_uid(examples$biosample, db = "biosample")
#' # Get metadata in XML format
#' meta <- ncbi_get_meta(uids$uid, db = "biosample")
#' # Parse XML
#' ncbi_parse(meta = meta_xml, db = "biosample", format = "xml")
#' 
#' # NCBI BioSample, download XML file from NCBI and parse
#' 
#' # Manually download the XML file
#' # https://www.ncbi.nlm.nih.gov/biosample/?term=SAMN02714232
#' # upper right corner -> send to -> file -> format = full (xml) -> create file
#' # Parse XML
#' ncbi_parse(meta = "biosample_result.xml", db = "biosample", format = "xml")#' 
#' }
#' @export
ncbi_parse <- function(
  meta,
  db,
  format = "xml",
  verbose = getOption("verbose")
) {
  db <- match.arg(db, choices = c("assembly", "biosample"))
  format <- match.arg(format, choices = c("xml"))
  f <- get(paste("ncbi_parse", db, format, sep = "_"))
  if (db == "assembly" && format == "xml") {
    out <- f(meta, verbose)
  } else if (db == "biosample" && format == "xml") {
    out <- f(meta, verbose)
  } else {
    out <- tibble::tibble()
  }
  return(out)
}
