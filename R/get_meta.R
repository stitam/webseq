#' Get metadata for biological sequences
#' 
#' This function accesses metadata that usually accompanies the biological
#' sequences. Depending on the database queried this metadata can contain a
#' number of identifiers of information about the sample e.g. when and where it
#' was collected. Currently the function works with a few NCBI databases.
#' @param uid character; the internal UID of an entry inside a database.
#' @param db character; the database to search in. Can be either 
#' \code{"assembly"}, \code{"biosample"} or \code{"sra"}.
#' @return a data frame
#' @examples
#' \dontrun{
#' # search for the species in the NCBI Assembly database
#' assembly_uid <- get_uid("Dechloromonas phosphoritropha", db = "assembly")
#' # get assembly metadata for the first two hits
#' assembly_meta <- get_meta(assembly_uid$uid[1:2], db = "assembly")
#' # convert UIDs in the Assembly database to UIDs in the Biosample database
#' biosample_uid <- link_uid(assembly_uid$uid, from = "assembly", to = "biosample")
#' # get biosample metadata for the first two hits
#' biosample_meta <- get_meta(biosample_uid$result[1:2], db = "biosample")
#' }
#' @export
get_meta <- function(uid, db = "assembly") {
  if (db == "assembly") {
    ncbi_meta_assembly(uid)
  } else if (db == "biosample") {
    ncbi_meta_biosample(uid)
  }
}