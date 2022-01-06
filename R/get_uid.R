#' Collect internal IDs from NCBI databases
#'
#' This function replicates the NCBI website's search utility. The search term
#' is matched against the chosen database and the function returns a list of
#' internal IDs that can be used e.g. to link NCBI entries with entries in other
#' NCBI databases.
#' @param term character; the search term. Check details for more information.
#' \code{term} takes a vector of strings and collapses them with " OR ".
#' @param db character; the database to search in. For options see
#' \code{rentrez::entrez_dbs()}
#' @return an object of class \code{esearch}.
#' @note This is the first step within the pipeline for downloading GenBank
#' files.
#' @seealso
#' \code{get_report_url()},
#' \code{download_report()},
#' \code{parse_report()},
#' \code{extract_accn()},
#' \code{download_gb()}
#' @examples
#' \dontrun{
#' get_uid(c("Autographiviridae", "Podoviridae"), db = "assembly")
#' }
#' @export
get_uid <- function(term,
                         db = "assembly") {
  db <- match.arg(db, rentrez::entrez_dbs())
  term <- paste(term, collapse = " OR ")
  hit <- rentrez::entrez_search(db, term = term)
  if (hit$count > hit$retmax) {
    hit <- rentrez::entrez_search(db, term = term, retmax = hit$count)
  }
  return(hit$ids)
}
