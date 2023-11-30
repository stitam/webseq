#' Get sequence metadata from NCBI
#' 
#' This function is a wrapper for \code{rentrez::entrez_fetch()} that retrieves
#' metadata from a given NCBI sequence database. The function currently works
#' with the following databases: \code{"assembly"}, \code{"biosample"}.
#' @param id integer; an integer vector of database specific NCBI UIDs.
#' @param db character; the database to search in. For options see
#' \code{rentrez::entrez_dbs()}.
#' @param verbose logical; Should verbose messages be printed to console?
#' @examples
#' \dontrun{
#' data(examples)
#' uids <- get_uid(examples$biosample, db = "biosample")
#' meta <- ncbi_get_meta(uids$uid, db = "biosample")
#' }
#' @export
ncbi_get_meta <- function(
    id,
    db,
    batch_size = 250,
    verbose = getOption("verbose")
  ) {
  if (!"integer" %in% class(id)) {
    stop("id must be an integer vector.")
  }
  idlist <- list()
  if (length(id) > batch_size) {
    nbatch <- ceiling(length(id)/batch_size)
    for (i in 1:nbatch) {
      idlist[[i]] <- id[((i-1)*batch_size + 1):min(i*batch_size, length(id))]
    }
  } else {
    idlist[[1]] <- id
  }
  if (db == "assembly") {
    rettype <- "docsum"
    retmode <- "xml"
  }
  if (db == "biosample") {
    rettype <- "full"
    retmode <- "xml"
  }
  out <- lapply(idlist, function(x) {
    rentrez::entrez_fetch(
      db = db,
      id = x,
      rettype = rettype,
      retmode = retmode
    ) 
  })
  class(out) <- c(paste("ncbi", db, "meta", sep = "_"), class(out))
  return(out)
}
