#' Get sequence metadata from NCBI
#' 
#' This function retrieves metadata from a given NCBI sequence database.
#' @param term character; one or more search terms.
#' @param db character; the database to search in. For options see
#' \code{rentrez::entrez_dbs()}. Not all databases are supported.
#' @param batch_size integer; the number of search terms to query at once. If
#' the number of search terms is larger than \code{batch_size}, the search terms
#' are split into batches and queried separately.
#' @param use_history logical; should the function use web history for faster
#' API queries?
#' @param parse logical; Should the function attempt to parse the output into a
#' tibble? If unsuccessful, the function will return the unparsed output.
#' @param verbose logical; Should verbose messages be printed to console?
#' @return The function returns a list with two elements:
#' \itemize{
#' \item \code{meta}: if \code{parse = TRUE} then either a tibble with the 
#' metadata or if parsing is unsuccessful, the unparsed metadata. If
#' \code{parse = FALSE} the unparsed metadata.
#' \item \code{history}: if \code{web_history = TRUE}, a character vector with
#' the web history IDs of the individual batches, otherwise \code{NULL}.
#' }
#' @examples
#' \dontrun{
#' data(examples)
#' meta <- ncbi_get_meta(examples$biosample, db = "biosample")
#' }
#' @export
ncbi_get_meta <- function(
    term,
    db,
    batch_size = 100,
    use_history = TRUE,
    parse = TRUE,
    verbose = getOption("verbose")
  ) {
  uids <- ncbi_get_uid(
    term = term,
    db = db,
    batch_size = batch_size,
    use_history = use_history,
    verbose = verbose
  )
  if (db == "assembly") {
    rettype <- "docsum"
    retmode <- "xml"
  } else if (db == "biosample") {
    rettype <- "full"
    retmode <- "xml"
  } else {
    stop("Database is not yet supported.")
  }
  if (use_history) {
    res <- lapply(uids$web_history, function(x) {
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
      idlist[[1]] <- uids$uid
    }
    res <- lapply(idlist, function(x) {
      rentrez::entrez_fetch(
        db = db,
        id = x,
        rettype = rettype,
        retmode = retmode
      )
    })
  }
  if (parse) {
    if (verbose) {
      message("Attempting to parse retrieved metadata.")
    }
    res_parsed <- lapply(res, function(x) {
      ncbi_parse(meta = x, db = db, verbose = verbose)
    })
    if ("data.frame" %in% class(res_parsed[[1]])) {
      res_parsed <- dplyr::bind_rows(res_parsed)
    } else {
      res_parsed <- res
    }
  } else {
    res_parsed <- res
  }
  out <- list(
    meta = res_parsed,
    web_history = if (use_history) uids$web_history else NULL
  )
  class(out) <- c(paste("ncbi", db, "meta", sep = "_"), class(out))
  return(out)
}
