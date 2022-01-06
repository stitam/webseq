#' MGnify endpoints
#' 
#' This functions queries MGnify for all available endpoints
#' @return a tibble of API-s and their respective endpoints
#' @note The function prints contents of the following url:
#' \url{https://www.ebi.ac.uk/metagenomics/api/v1/}
#' @examples
#' \dontrun{
#' mgnify_endpoints()
#' }
#' @export
mgnify_endpoints <- function() {
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
  return(out)
}