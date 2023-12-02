#' Get sequence metadata from NCBI
#' 
#' This function retrieves metadata from a given NCBI sequence database. The
#' function currently works with the following databases: \code{"assembly"},
#' \code{"biosample"}.
#' @param id integer; an integer vector of database specific NCBI UIDs.
#' @param db character; the database to search in. For options see
#' \code{rentrez::entrez_dbs()}.
#' @param batch_size integer; the number of search terms to query at once. If the
#' number of search terms is larger than \code{batch_size}, the search terms
#' are split into batches and queried separately.
#' @param verbose logical; Should verbose messages be printed to console?
#' @examples
#' \dontrun{
#' data(examples)
#' uids <- ncbi_get_uid(examples$biosample, db = "biosample")
#' meta <- ncbi_get_meta(uids$uid, db = "biosample")
#' }
#' @export
ncbi_get_meta <- function(
    term,
    db,
    batch_size = 100,
    use_history = TRUE,
    verbose = getOption("verbose")
  ) {
  uids <- ncbi_get_uid(
    term = term,
    db = db,
    batch_size = batch_size,
    verbose = verbose
  )
  if (db == "assembly") {
    rettype <- "docsum"
    retmode <- "xml"
  }
  if (db == "biosample") {
    rettype <- "full"
    retmode <- "xml"
  }
  if (use_history) {
    out <- lapply(uids$web_history, function(x) {
      WH <- list("WebEnv" = x,"QueryKey" = "1")
      class(WH) <- c("web_history", "list")
      rentrez::entrez_fetch(
        db = db,
        web_history = WH,
        rettype = rettype,
        retmode = retmode
      ) 
    })
  } else {
    idlist <- list()
    if (length(uids$uid) > batch_size) {
      nbatch <- ceiling(length(uids$uid)/batch_size)
      for (i in 1:nbatch) {
        index_min <- (i-1)*batch_size + 1
        index_max <- min(i*batch_size, length(uids$uid))
        idlist[[i]] <- uids$uid[index_min:index_max]
      }
    } else {
      idlist[[1]] <- id
    }
    out <- lapply(idlist, function(x) {
      rentrez::entrez_fetch(
        db = db,
        id = x,
        rettype = rettype,
        retmode = retmode
      )
    })
  }
  class(out) <- c(paste("ncbi", db, "meta", sep = "_"), class(out))
  return(out)
}
