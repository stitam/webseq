#' Recover NCBI IDs from NCBI UIDs
#' 
#' Many functions that interact with NCBI return UIDs. This function converts
#' the UIDs to NCBI IDs.
#' @param query either an object of class \code{ncbi_uid} or an integer vector 
#' of UIDs. See Details for more information.
#' @param db character; the database to search in. For options see
#' \code{ncbi_dbs()}.
#' @param batch_size integer; the number of items to query at once. If query 
#' length is larger than `batch_size`, the query will be split into batches.
#' @param verbose logical; should verbose messages be printed to the console?
#' @return A vector of matching IDs.
#' @details If query is an `ncbi_uid` object, the `db` argument is optional. If 
#' `db` is not specified, the function will retrieve it from the query object. 
#' However, if it is specified, it must be identical to the `db` attribute of 
#' the query.
#' @examples
#' \dontrun{
#' uid <- ncbi_get_uid("GCF_000002435.2", db = "assembly")
#' ncbi_recover_id(uid)
#' }
#' @export
ncbi_recover_id <- function(
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
  summaries <- ncbi_get_summary(
    query,
    db = db,
    batch_size = batch_size,
    verbose = verbose
  )
  if (db == "assembly") {
    id <- unname(sapply(summaries, function(x) x$assemblyaccession))
  } else if (db == "biosample") {
    id <- unname(sapply(summaries, function(x) x$accession))
  } else if (db == "bioproject") {
    id <- unname(sapply(summaries, function(x) x$project_acc))
  } else if (db == "gene") {
    id <- unname(sapply(summaries, function(x) x$uid))
  } else if (db == "protein") {
    id <- unname(sapply(summaries, function(x) x$accessionversion))
  } else {
    stop("Not supported.")
  }
  out <- tibble::tibble(
    uid = as.numeric(unname(sapply(summaries, function(x) x$uid))),
    id = id
  )
  if ("ncbi_uid" %in% class(query)) {
    out <- dplyr::left_join(
      tibble::tibble(query = query$uid), 
      out, 
      by = c("query" = "uid")
    )
    testthat::expect_equal(length(query$uid), nrow(out))
    testthat::expect_equal(query$uid, out$query)
  } else {
    out <- dplyr::left_join(
      tibble::tibble(query = query), 
      out, 
      by = c("query" = "uid")
    )
    testthat::expect_equal(length(query), nrow(out))
    testthat::expect_equal(query, out$query)
  }
  return(out$id)
}
