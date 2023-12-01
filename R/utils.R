#' Try an URL with default parameters
#' 
#' This is a convenience wrapper for trying URLs
#' @param verb character; Name of the verb to use.
#' @param qurl character; Query URL.
#' @importFrom httr RETRY
#' @noRd
try_url <- function(verb, qurl, ...){
  try(httr::RETRY(verb,
                  qurl,
                  terminate_on = 404,
                  quiet = TRUE,
                  ...), silent = TRUE)
}

webseq_message <- function(action = c("na",
                                     "query",
                                     "query_all",
                                     "not_found",
                                     "not_available",
                                     "service_down"),
                            appendLF = TRUE,
                            ...) {
  action <- match.arg(action)
  string <- switch(
    action,
    na = "Query is NA. Returning NA.",
    query = paste0("Querying ", ..., ". "),
    query_all = "Querying. ",
    not_found = "Not found. Returning NA.",
    not_available = "Not available. Returning NA.",
    service_down = "Service not available. Returning NA."
  )
  message(string, appendLF = FALSE)
  if (appendLF) message("")
}

#' Function to wait between every web-service query
#'
#' @param time numeric; Wait time in seconds.
#' @param type character; Will be an API queried or a website scraped?
#' @noRd
#'
webseq_sleep <- function(time = NULL, type = c('API', 'scrape')) {
  type <- match.arg(type)
  if (is.null(time)) {
    if (type == 'API') {
      time <- 0.2
    }
    if (type == 'scrape') {
      time <- 0.3
    }
  } else {
    if (!is.numeric(time)) {
      stop('Set time is not numeric.')
    }
  }
  Sys.sleep(time)
}
