#' Which NCBI databases can a database be linked to?
#'
#' This function is a wrapper around \code{rentrez::entrez_db_links()}. Results
#' are cached and reused to validate input for \code{ncbi_link_uid()}.
#' @param db character; the name of an NCBI database
#' @return a character vector of NCBI database names the original database can
#' be linked to.
#' @examples
#' ncbi_db_links("assembly")
#' @noRd
ncbi_db_links <- function(db) {
  if (!dir.exists(tempdir())) dir.create(tempdir())
  filename <- paste0(
    tempdir(),
    "/webseq_ncbi_db_links_",
    db,
    ".rds"
  )
  if (!file.exists(filename)) {
    db_links <- names(rentrez::entrez_db_links(db))
    saveRDS(db_links, file = filename)
  } else {
    db_links <- readRDS(filename)
  }
  return(db_links)
}