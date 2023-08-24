#' Search for a specific entry in MGnify
#' 
#' This function can be used for searching MGnify using an identifier.
#' @param query character; the indentifier
#' @param from character; the api which contains this identifier. See
#' \code{mgnify_endpoints()} for a list of possible apis. 
#' @return a list
#' @examples
#' \dontrun{
#' # look up an assembly
#' mgnify_instance("ERZ477576", from = "assemblies")
#' }
#' @export
mgnify_instance <- function(query, from) {
  query <- utils::URLencode(query)
  from <- match.arg(from, mgnify_endpoints()$api)
  url <- paste("https://www.ebi.ac.uk/metagenomics/api/v1",
               from,
               query,
               sep = "/")
  res <- httr::GET(url)
  cont <- httr::content(res)
  return(cont)
}