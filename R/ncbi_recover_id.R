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
#' @importFrom dplyr %>%
#' @importFrom rlang .data
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
    query <- as_numeric(query)
  }
  summaries <- ncbi_get_summary(
    query,
    db = db,
    batch_size = batch_size,
    verbose = verbose
  )
  # TODO add more checks. for example, ncbi_get_summary should return a list
  # maybe add this check in ncbi_get_summary?
  if (db == "assembly") {
    id <- unname(sapply(summaries, function(x) x$assemblyaccession))
  } else if (db == "biosample") {
    id <- unname(sapply(summaries, function(x) x$accession))
  } else if (db == "bioproject") {
    id <- unname(sapply(summaries, function(x) x$project_acc))
  } else if (db == "gene") {
    id <- unname(sapply(summaries, function(x) x$uid))
  } else if (db == "nuccore") {
    id <- unname(sapply(summaries, function(x) x$accessionversion))
  } else if (db == "protein") {
    id <- unname(sapply(summaries, function(x) x$accessionversion))
  } else if (db == "pubmed") {
    id <- unname(sapply(summaries, function(x) {
      if (!is.list(x)) {
        if (verbose) message("Summary is not a list.")
        return(NA_character_)
      }
      if (!"articleids" %in% names(x)) {
        if (verbose) message("Missing list element: 'articleids'.")
        return(NA_character_)
      }
      ids <- x$articleids
      if (!inherits(ids, "data.frame")) {
        if (verbose) message("Element 'articleids' is not a data.frame.")
        return(NA_character_)
      }
      if (!all(c("idtype", "value") %in% names(ids))) {
        if (verbose) message(
          "Element 'articleids' must contain variables 'idtype' and 'value'"
        )
        return(NA_character_)
      }
      doi_vals <- ids |>
        dplyr::filter(tolower(.data$idtype) == "doi") |>
        dplyr::pull(.data$value) |>
        as.character() |>
        unique()
      if (length(doi_vals) == 0 || all(is.na(doi_vals))) {
        if (verbose) message("Could not recover DOI.")
        return(NA_character_)
      }
      if (length(doi_vals) > 1) {
        if (verbose) message("pubmed: multipe DOIs found. Returning all.")
      }
      paste0("https://doi.org/", doi_vals)
    }))
  } else {
    stop("Not supported.")
  }
  out <- tibble::tibble(
    uid = as.numeric(unname(sapply(summaries, function(x) x$uid))),
    id = id
  ) %>% dplyr::distinct()
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
