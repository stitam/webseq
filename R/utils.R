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
webseq_sleep <- function(time = NULL, type = c("API", "FTP", "scrape")) {
  type <- match.arg(type)
  if (is.null(time)) {
    if (type == 'API') {
      time <- 0.2
    }
    if (type == "FTP") {
      time <- 0.1
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

#' Wrap web-service queries
#' 
#' @param function_name character; Name of the function to wrap.
#' @param package character; Name of the package in which the function is.
#' @param verbose logical; Should verbose messages be printed to console?
#' @param ... arguments to pass to the function.
#' @noRd
wrap <- function(
  function_name, 
  package, 
  verbose = getOption("verbose"), 
  ...
  ) {
  f <- get(function_name, envir = asNamespace(package))
  r <- NULL
  attempt <- 1
  while(is.null(r) && attempt <= 5) {
    webseq_sleep(type = "API")
    hit <- try(
      f(...),
      silent = TRUE
    )
    if (inherits(hit, "try-error")) {
      attempt <- attempt + 1
    } else r <- 1
  }
  if (inherits(hit, "try-error")) {
    if (verbose) webseq_message("service_down")
    return(NA)
  } else {
    if (verbose) {
      message("Query successful.")
    }
    return(hit)
  }
}

#' Split a vector of IDs into batches
#' 
#' @param ids character; Vector of IDs.
#' @param batch_size integer; Number of IDs per batch.
#' @param verbose logical; Should verbose messages be printed to console?
#' @noRd
get_idlist <- function(ids, batch_size, verbose = getOption("verbose")) {
  ids <- as.numeric(ids)
  if (all(is.na(ids))) {
    if (verbose) message("No valid UIDs.")
    ids <- NA
  } else if (any(is.na(ids))){
    if (verbose) message("Removing NA-s from UIDs.")
    ids <- ids[which(!is.na(ids))]
  }
  idlist <- list()
  if (length(ids) > batch_size) {
    nbatch <- ceiling(length(ids)/batch_size)
    if (verbose) message("Splitting ids into ", nbatch, " batches.")
    for (i in 1:nbatch) {
      index_min <- (i-1)*batch_size + 1
      index_max <- min(i*batch_size, length(ids))
      idlist[[i]] <- ids[index_min:index_max]
    }
  } else {
    idlist[[1]] <- ids
  }
  return(idlist)
}

#' Validate the structure of a webseq object
#' 
#' @param x a webseq object
#' @noRd
validate_webseq_class <- function(x) {
  if ("ncbi_uid" %in% class(x)) {
    testthat::expect_equal(length(names(x)), 3)
    testthat::expect_true(names(x)[1] == "uid")
    testthat::expect_true(names(x)[2] == "db")
    testthat::expect_true(names(x)[3] == "web_history")
    testthat::expect_true(class(x$uid) == "integer")
    testthat::expect_true(class(x$db) == "character")
    testthat::expect_s3_class(x$web_history, c("tbl_df", "tbl", "data.frame"))
  }
  if ("ncbi_meta" %in% class(x)) {
    testthat::expect_true(attr(x, "db") %in% ncbi_dbs())
  }
}
