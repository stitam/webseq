#' Collect internal IDs from NCBI databases
#'
#' This function replicates the NCBI website's search utility. The search term
#' is matched against the chosen database and the function returns a character
#' vector of internal IDs that can be used e.g. to link NCBI entries with
#' entries in other NCBI databases.
#' @param term character; the search term.
#' @param db character; the database to search in. For options see
#' \code{rentrez::entrez_dbs()}
#' @return a character vector of ID-s.
#' @examples
#' \dontrun{
#' get_uid("GCA_003012895.2")
#' get_uid(c("Autographiviridae OR Podoviridae"), db = "assembly")
#' }
#' @export
get_uid <- function(term, db = "assembly") {
  db <- match.arg(db, rentrez::entrez_dbs())
  term <- paste(term, collapse = " OR ")
  hit <- NULL
  attempt <- 1
  while(is.null(hit) && attempt <= 5) {
    hit <- try(rentrez::entrez_search(db, term = term), silent = TRUE)
    if (inherits(hit, "try-error")) {
      hit <- NULL
      attempt <- attempt + 1
    }
  }
  if (hit$count > hit$retmax) {
    hit <- NULL
    attempt <- 1
    while(is.null(hit) && attempt <= 5) {
      hit <- try(rentrez::entrez_search(db, term = term, retmax = hit$count),
                 silent = TRUE)
      if (inherits(hit, "try-error")) {
        hit <- NULL
        attempt <- attempt + 1
      }
    }
  }
  out <- ifelse(length(hit$ids) > 0, hit$ids, NA)
  return(out)
}
