#' @export
mgnify_instance <- function(query, from) {
  query <- URLencode(query)
  from <- match.arg(from, mgnify_endpoints()$api)
  url <- paste("https://www.ebi.ac.uk/metagenomics/api/v1",
               from,
               query,
               sep = "/")
  res <- httr::GET(url)
  cont <- httr::content(res)
  return(cont)
}