#' Link NCBI UID-s
#' 
#' Each entry in an NCBI database has its unique internal id. Entries in
#' different databases may be linked. For example, an assembly in the NCBI
#' Assembly database may be linked with metadata in the NCBI BioSample database.
#' This function links uids from one database with uids from another.
#' @param query character; a vector of uids.
#' @param from character; the database the queried uids come from.
#' \code{rentrez::entrez_dbs()} lists all available options.
#' @param to character; the database in which the function should look for links.
#' \code{rentrez::entrez_dbs()} lists all available options.
#' @return A tibble
#' @examples 
#' \dontrun{
#' link_uids("4253631", "assembly", "biosample")
#' link_uids(c("1226742659", "1883410844"), "protein", "nuccore")
#' }
#' @export
link_uids <- function(query, from, to) {
  query <- as.numeric(query)
  from <- match.arg(from, rentrez::entrez_dbs())
  to <- match.arg(to, rentrez::entrez_dbs())
  foo <- function(x) {
    res <- NULL
    attempt <- 1
    while(is.null(res) && attempt <= 5) {
      res <- try(
        rentrez::entrez_link(
          id = x, dbfrom = from, db = to), silent = TRUE)
      if (inherits(res, "try-error")) {
        res <- NULL
        attempt <- attempt + 1
      }
    }
    if (length(res$links) == 0) {
      tbl <- tibble::tibble(
        query = x,
        query_db = from,
        result = NA,
        result_db = to)
    } else {
      tbl <- tibble::tibble(
        query = x,
        query_db = from,
        result = res$links[[paste(from, to, sep = "_")]],
        result_db = to)
    }
    return(tbl)
  }
  out <- lapply(query, foo)
  out <- dplyr::bind_rows(out)
  return(out)
}
