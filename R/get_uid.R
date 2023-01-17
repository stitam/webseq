#' Collect UID-s from NCBI databases
#'
#' This function replicates the NCBI website's search utility. One or more
#' search terms are matched against the chosen database and the function returns
#' a tibble of internal NCBI UID-s that can be used e.g. to link NCBI entries
#' with entries in other NCBI databases.
#' @param term character; one or more search terms.
#' @param db character; the database to search in. For options see
#' \code{rentrez::entrez_dbs()}
#' @param verbose logical; should verbos messages be printed to the console?
#' @return A tibble.
#' @examples
#' \dontrun{
#' get_uid("GCA_003012895.2")
#' get_uid("Autographiviridae OR Podoviridae", db = "assembly")
#' get_uid(c("WP_093980916.1", "WP_181249115.1"), db = "protein")
#' }
#' @export
get_uid <- function(term, db = "assembly", verbose = getOption("verbose")) {
  db <- match.arg(db, rentrez::entrez_dbs())
  foo <- function(x) {
    if (verbose) seqdb_message("query", x, appendLF = FALSE)
    r <- NULL
    attempt <- 1
    while(is.null(r) && attempt <= 5) {
      hit <- try(rentrez::entrez_search(db, term = x), silent = TRUE)
      if (inherits(hit, "try-error")) {
        attempt <- attempt + 1
      } else r <- 1
    }
    if (inherits(hit, "try-error")) {
      if (verbose) seqdb_message("service_down")
      return(tibble::tibble(term = x, db = db, uid = NA))
    }
    if (hit$count > hit$retmax) {
      r <- NULL
      attempt <- 1
      while(is.null(r) && attempt <= 5) {
        hit <- try(rentrez::entrez_search(db, term = x, retmax = hit$count),
                   silent = TRUE)
        if (inherits(hit, "try-error")) {
          attempt <- attempt + 1
        } else r <- 1
      }
    }
    if (inherits(hit, "try-error")) {
      if (verbose) seqdb_message("service_down")
      return(tibble::tibble(term = x, db = db, uid = NA))
    }
    if (length(hit$ids) > 0) {
      if (verbose) message("OK.")
      return(tibble::tibble(term = x, db = db, uid = hit$ids))
    } else {
      if (verbose) message("Not found. Returning NA.")
      return(tibble::tibble(term = x, db = db, uid = NA))
    }
  }
  res <- lapply(term, foo)
  res <- dplyr::bind_rows(res)
  return(res)
}
