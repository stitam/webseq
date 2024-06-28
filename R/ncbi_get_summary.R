#' Get object summary from NCBI
#' 
#' This function retrieves object summary data from a given NCBI database.
#' @param query either an object of class \code{ncbi_uid} or an integer vector 
#' of UIDs. See Details for more information.
#' @param db character; the database to search in. For options see
#' \code{ncbi_dbs()}.
#' @param batch_size integer; the number of items to query at once. If query 
#' length is larger than `batch_size`, the query will be split into batches.
#' @param verbose logical; should verbose messages be printed to the console?
#' @return A list of rentrez summary objects.
#' @details If query is an `ncbi_uid` object, the `db` argument is optional. If 
#' `db` is not specified, the function will retrieve it from the query object. 
#' However, if it is specified, it must be identical to the `db` attribute of 
#' the query.
#' @examples
#' \dontrun{
#' assemblies <- c("GCF_000002435.2", "GCF_000299415.1")
#' uids <- ncbi_get_uid(assemblies, db = "assembly")
#' ncbi_get_summary(uids)
#' }
#' @export
ncbi_get_summary <- function(
    query,
    db = NULL,
    batch_size = 100,
    verbose = getOption("verbose")
  ) {
  if ("ncbi_uid" %in% class(query)) {
    if (is.null(db)) {
      db <- query$db
    } else {
      if (db != query$db) {
        msg <- paste0(
          "Database for queried UIDs does not match 'db' argument.\n",
          "Provide identical values or use db = NULL (default)."
        )
        stop(msg)
      }
    }
  } else {
    if (is.null(db)) {
      msg <- "Specify a 'db' argument or query an object of class 'ncbi_uid'."
      stop(msg)
    }
  }
  db <- match.arg(db, ncbi_dbs())
  if ("ncbi_uid" %in% class(query) && nrow(query$web_history) > 0) {
    res <- list()
    for (i in 1:nrow(query$web_history)) {
      WH <- list(
        "WebEnv" = query$web_history$WebEnv[i],
        "QueryKey" = query$web_history$QueryKey[i]
      )
      class(WH) <- c("web_history", "list")
      hit <- wrap(
        "entrez_summary",
        package = "rentrez",
        verbose = verbose,
        db = "assembly",
        web_history = WH
      )
      if ("esummary" %in% class(hit)) {
        hit <- list(hit)
      }
      res[[i]] <- hit
    }
  } else {
    # implement solution with batches of ids
    foo_from_ids <- function(x, db, verbose) {
      if (length(x) == 1 && is.na(x)) return(NA)
      res <- wrap(
        "entrez_summary",
        package = "rentrez",
        verbose = verbose,
        db = db,
        id = x
      )
      if ("esummary" %in% class(res)) {
        res <- list(res)
      }
      return(res)
    }
    if (!is.numeric(query)) {
      stop("Query must be an ncbi_uid object or a numeric vector or UIDs.")
    }
    idlist <- get_idlist(query, batch_size = batch_size, verbose = verbose)
    res <- lapply(idlist, function(x) {
      foo_from_ids(x, db = db, verbose = verbose)
    })
  }
  res <- unlist(res, recursive = FALSE)
  names(res) <- unname(sapply(res, function(x) x$uid))
  return(res)
}
