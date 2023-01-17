#' Collect UID-s from NCBI databases
#'
#' This function replicates the NCBI website's search utility. One or more
#' search terms are matched against the chosen database and the function returns
#' a tibble of internal NCBI UID-s that can be used e.g. to link NCBI entries
#' with entries in other NCBI databases.
#' @param term character; one or more search terms.
#' @param db character; the database to search in. For options see
#' \code{rentrez::entrez_dbs()}
#' @return A tibble.
#' @examples
#' \dontrun{
#' get_uid("GCA_003012895.2")
#' get_uid("Autographiviridae OR Podoviridae", db = "assembly")
#' get_uid(c("WP_093980916.1", "WP_181249115.1"), db = "protein")
#' }
#' @export
get_uid <- function(term, db = "assembly") {
  db <- match.arg(db, rentrez::entrez_dbs())
  foo <- function(x) {
    r <- NULL
    attempt <- 1
    while(is.null(r) && attempt <= 5) {
      hit <- try(rentrez::entrez_search(db, term = x), silent = TRUE)
      if (inherits(hit, "try-error")) {
        attempt <- attempt + 1
      } else r <- 1
    }
    if (inherits(hit, "try-error")) {
      stop("Query failed after 5 tries.")
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
      stop("Query failed after 5 tries.")
    }
    if (length(hit$ids) > 0) {
      return(tibble::tibble(term = x, db = db, uid = hit$ids))
    } else {
      return(tibble::tibble(term = x, db = db, uid = NA))
    }
  }
  res <- lapply(term, foo)
  res <- dplyr::bind_rows(res)
  return(res)
}
