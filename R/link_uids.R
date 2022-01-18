#' Convert between NCBI UIDs
#' 
#' Each entry in an NCBI database has its unique internal id. Entries in
#' different databases may be linked. For example, an assembly in the NCBI
#' Assembly database may be linked with metadata in the NCBI BioSample database.
#' This function links uids from one database with uids from another.
#' @param uids character; a vector of uids.
#' @param from character; the database the queried uids come from.
#' \code{rentrez::entrez_dbs()} lists all available options.
#' @param to character; the database in which the function should look for links.
#' \code{rentrez::entrez_dbs()} lists all available options.
#' @return A character vector of linked uids from the new database.
#' @examples 
#' \dontrun {
#' link_uids("4253631", "assembly", "biosample")
#' }
#' @export
link_uids <- function(uids, from, to) {
  uids <- as.numeric(uids)
  from <- match.arg(from, rentrez::entrez_dbs())
  to <- match.arg(to, rentrez::entrez_dbs())
  out <- unname(sapply(uids, function(x) {
    res <- rentrez::entrez_link(id = x, dbfrom = from, db = to)
    if (length(res$links) == 0) return(NA) else {
      new_uid <- unlist(res$links[paste(from, to, sep = "_")])
      return(new_uid)
    }
  }))
  return(out)
}
