#' MGnify endpoints
#' 
#' This functions queries MGnify for all available endpoints
#' @param verbose boolean; should verbose messages be printed to console?
#' @return a tibble of API-s and their respective endpoints
#' @note The function prints contents of the following url:
#' \url{https://www.ebi.ac.uk/metagenomics/api/v1/}
#' @examples
#' \dontrun{
#' mgnify_endpoints()
#' }
#' @export
mgnify_endpoints <- function(verbose = options("verbose")) {
  tmpdir <- tempdir()
  if (!dir.exists(tmpdir)) {
    if (verbose) message("Creating cache directory ", tmpdir, ".")
    dir.create(tmpdir)
  }
  if (!file.exists(paste0(tmpdir, "/mgnify_endpoints.rda"))) {
    res <- httr::GET("https://www.ebi.ac.uk/metagenomics/api/v1/")
    cont <- httr::content(res)$data
    apis <- tibble::tibble(
      api = names(cont),
      endpoint = unlist(cont)
    )
    others <- tibble::tibble(
      api = "downloads",
      endpoint = NA
    )
    out <- rbind(apis, others)
    if (verbose) message("Saving endpoints to cache.")
    save(out, file = paste0(tmpdir, "/mgnify_endpoints.rda"))
  } else {
    if (verbose) message("Loading endpoints from cache.")
    load(paste0(tmpdir, "/mgnify_endpoints.rda"))
  }
  return(out)
}